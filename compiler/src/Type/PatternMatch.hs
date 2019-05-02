{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
  
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Type.PatternMatch where

import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad
import Data.Foldable (foldrM, toList)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import qualified Data.Maybe as Maybe

import Data.Text (unpack)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Reporting.Region as R
import qualified Type.Error as ET 
import qualified Type.UnionFind as UF

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import qualified Type.Type as Type
import Control.Monad.Writer

import Data.Semigroup
import Data.Monoid

import qualified Data.List as List

type Variable = UF.Point ()

data TypeEffect_ typeEffect =
    PlaceHolder N.Name
    | Alias ModuleName.Canonical N.Name [(N.Name, typeEffect)] typeEffect
    -- | Var Variable
    | App ModuleName.Canonical N.Name [typeEffect]
    | Fun typeEffect typeEffect
    | EmptyRecord
    | Record (Map.Map N.Name typeEffect) typeEffect
    | Unit
    | Tuple typeEffect typeEffect (Maybe typeEffect)
    deriving (Functor, Traversable, Foldable)


newtype Fix f = Fix {unfix :: f (Fix f)}

infix 9 :@
data TypeEffect =  (TypeEffect_ TypeEffect) :@ Variable

data LitPattern_ self =
    SetVar_ Variable
    | Ctor_ String [self]
    | Proj_ String Int self
    | Top_
    | Bottom_
    | Intersect_ self self
    | Union_ self self
    deriving (Functor, Traversable, Foldable)

pattern SetVar v = Fix ( SetVar_ v)
pattern Ctor s l = Fix (Ctor_ s l)
pattern Proj s i p = Fix (Proj_ s i p)
pattern Top = Fix Top_
pattern Bottom = Fix Bottom_
pattern Union x y = Fix (Union_ x y)

data Constraint_ pat self =
    CAnd_ [self]
    | COr_ [self]
    | CImplies_ self self
    | CSubset_ pat pat
    | CTrue_
    | CNot_ self
    deriving (Functor, Traversable, Foldable)

pattern CAnd l = Fix (CAnd_ l)
pattern COr l = Fix (COr_ l)
pattern CImplies x y = Fix (CImplies_ x y)
pattern CSubset p1 p2 = Fix (CSubset_ p1 p2)
pattern CTrue = Fix CTrue_
pattern CNot x = Fix (CNot_ x)

type LitPattern = Fix LitPattern_
type Constraint = Fix (Constraint_ LitPattern)

newtype Safety = Safety [(Constraint, R.Region)]
    deriving (Monoid, Semigroup)

(====) :: (Subsetable a, Subsetable b) => a -> b -> Constraint
p1 ==== p2 =
    let l1 = toLit p1
        l2 = toLit p2
    in
    (l1 << l2) /\ (l2 << l1)

unions :: (Subsetable a) => [a] -> LitPattern
unions = foldr (\ a b ->  (toLit a) `union` b) Bottom

intersects :: (Subsetable a) => [a] -> LitPattern
intersects = foldr (\ a b ->  (toLit a) `intersect` b) Bottom

union a b = unions [toLit a, toLit b]
intersect a b = intersects [toLit a, toLit b]

class Subsetable a where
    toLit :: a -> LitPattern

instance Subsetable LitPattern where
    toLit = id

instance Subsetable Variable where
    toLit = SetVar

instance Subsetable TypeEffect where
    toLit (_ :@ v) = toLit v

instance Subsetable (LitPattern_ LitPattern) where
    toLit = Fix

(<<) :: (Subsetable a, Subsetable b ) => a -> b -> Constraint
v1 << v2 = CSubset (toLit v1) (toLit v2)

(</<) :: (Subsetable a, Subsetable b ) => a -> b -> Constraint
v1 </< v2 = CNot $ CSubset (toLit v1) (toLit v2)

instance Semigroup Constraint where
    CTrue <> c = c
    c <> CTrue = c
    c1 <> (CAnd cl) = CAnd (c1 : cl)
    c1 <> c2 = CAnd [c1, c2]

instance Monoid Constraint where
    mempty =  CTrue
    mappend = (Data.Semigroup.<>)

data EffectScheme =
    Forall [Variable] TypeEffect Constraint

type Gamma = Map.Map N.Name EffectScheme

monoscheme :: TypeEffect -> LitPattern -> EffectScheme
monoscheme tipe@(_ :@ var) lit =
    Forall [] tipe (var ==== lit)

monoschemeVar :: TypeEffect -> EffectScheme
monoschemeVar tipe = Forall [] tipe CTrue



freeConstraintVars :: TypeEffect -> [Variable]
freeConstraintVars ty = [] --TODO implement

traverseTE :: (TypeEffect -> TypeEffect) -> TypeEffect -> TypeEffect
traverseTE f (t :@ e) = f ((f <$> t) :@ e)

oneTypeSubst :: (Variable -> Variable) -> TypeEffect -> TypeEffect
oneTypeSubst sub (t :@ e) = t :@ (sub e)

oneConstrSubst :: (Variable -> Variable) -> Constraint -> Constraint
oneConstrSubst sub (CSubset p1 p2) = CSubset (litSubsts sub p1) (litSubsts sub p2)
oneConstrSubst sub c = c

oneLitSubst :: (Variable -> Variable) -> LitPattern -> LitPattern
oneLitSubst sub (SetVar v) = SetVar $ sub v
oneLitSub sub l = l

typeSubsts :: (Variable -> Variable) -> TypeEffect -> TypeEffect
typeSubsts sub (t :@ e) = oneTypeSubst sub ((oneTypeSubst sub <$> t) :@ e)

constrSubsts :: (Variable -> Variable) -> Constraint -> Constraint
constrSubsts sub (Fix c) =  oneConstrSubst sub (Fix $ (oneConstrSubst sub <$> c))

litSubsts :: (Variable -> Variable) -> LitPattern -> LitPattern
litSubsts sub (Fix l) = oneLitSubst sub (Fix $ (oneLitSubst sub <$> l))

typeLocalVars (t :@ e) = [e]
constrLocalVars (CSubset p1 p2) = (litFreeVars p1) ++ (litFreeVars p2)
constrLocalVars c = []
litLocalVars (SetVar v) = [v]
litLovalVars l = []

typeFreeVars (t :@ e) = List.nub $ e: concatMap typeLocalVars (toList t)
constrFreeVars (Fix c) = List.nub $ concatMap constrLocalVars ((Fix c) : toList c)
litFreeVars (Fix l) = List.nub $ concatMap litLocalVars ((Fix l) : toList l)
schemeFreeVars (Forall v t c) = List.nub $ (typeFreeVars t) ++ (constrFreeVars c)

instantiate :: EffectScheme -> IO (TypeEffect, Constraint)
instantiate (Forall boundVars tipe constr) = do
    freshVars <- forM [1 .. length boundVars] $ \ _ -> UF.fresh ()
    let subList =  zip boundVars freshVars
    let substFun x = Maybe.fromMaybe x (lookup x subList)
    return $ (typeSubsts substFun tipe, constrSubsts substFun constr)


generalize :: Gamma -> TypeEffect -> Constraint  -> EffectScheme
generalize _Gamma tipe constr =
    let
        allFreeVars = List.nub $ (typeFreeVars tipe) ++ (constrFreeVars constr)
        gammaFreeVars = List.nub $ concatMap schemeFreeVars (Map.elems _Gamma)
    in
        Forall (allFreeVars List.\\ gammaFreeVars) tipe constr



(==>) ::  Constraint -> Constraint -> Constraint
x ==> y =  CImplies x y

--Smart constructor for AND
(/\) :: Constraint -> Constraint -> Constraint
(CAnd c1) /\  (CAnd c2) =   CAnd (c1 ++ c2)
c1 /\  (CAnd c2) =  CAnd (c1 : c2)
(CAnd c1) /\ c2 =  CAnd (c2 : c1 )
c1 /\ c2 =  CAnd [c1, c2]

c1 \/ c2 = COr [c1, c2]

addEffectVars :: Type.Type -> IO TypeEffect
addEffectVars (Type.AliasN t1 t2 t3 actualType) = addEffectVars actualType
addEffectVars t = do
    innerType <-
        case t of
            (Type.PlaceHolder t) ->
                return $ PlaceHolder t
            (Type.VarN t) ->
                 (error "TODO var conversion")
            (Type.AppN t1 t2 t3) ->
                (App t1 t2) <$> (mapM addEffectVars  t3)
            (Type.FunN t1 t2) ->
                Fun <$> addEffectVars t1 <*> addEffectVars t2
            Type.EmptyRecordN -> return EmptyRecord
            (Type.RecordN t1 t2) ->
                Record <$> (mapM addEffectVars t1) <*> addEffectVars t2
            Type.UnitN -> return Unit
            (Type.TupleN t1 t2 t3) -> do
                mt3 <- case t3 of
                    Nothing -> return Nothing
                    Just t -> Just <$> addEffectVars t
                Tuple <$> addEffectVars t1 <*> addEffectVars t2 <*> (return mt3)
    constraintVar <- UF.fresh ()
    return $ innerType :@ constraintVar

-- freshTE :: IO TypeEffect
-- freshTE = do
--     tipe <- Var <$> UF.fresh () --TODO what descriptor?
--     effect <- UF.fresh () --TODO what descriptor?
--     return $ tipe :@ effect


unifyTypes :: (ConstrainM m) => TypeEffect -> TypeEffect -> m ()
unifyTypes (_ :@ v1) (_ :@ v2) =
    liftIO $ UF.union v1 v2 ()
    --TODO do we need to unify sub-vars?


type ConstrainM m = (MonadIO m, MonadWriter Safety m )

newtype ConstrainMonad a = CM {
    runCM :: WriterT Safety IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadWriter Safety)

runCMIO :: ConstrainMonad a -> IO (a, Safety)
runCMIO c = runWriterT (runCM c)

tellSafety pathConstr x r = tell $ Safety [(pathConstr ==> x, r)]

--Given a type and an  effect-variable for each expression,
-- a mapping (Gamma) from free variables to effect schemes,
--A "path constraint" of truths that must hold for us to reach this point in the program,
-- and an expression,
--Traverse that expression and generate the constraints that, when solved,
-- give the possible patterns for each effect variable.
-- Emits "safety constraints" using the Writer monad
constrainExpr :: (ConstrainM m) => Map.Map R.Region TypeEffect -> (Gamma, Constraint) -> Can.Expr ->   m (TypeEffect,  Constraint)
constrainExpr tyMap _GammaPath (A.At region expr)  =
    case Map.lookup region tyMap of
        Just ty -> do
            c <- constrainExpr_ expr ty _GammaPath
            return (ty, c)
        Nothing -> error "Region not in type map"
  where
    self :: (ConstrainM m) => (Gamma, Constraint) -> Can.Expr ->   m (TypeEffect,  Constraint)
    self  = constrainExpr tyMap
    constrainExpr_ ::  (ConstrainM m) => Can.Expr_ -> TypeEffect -> (Gamma, Constraint) -> m Constraint
    constrainExpr_ (Can.VarLocal name) t (_Gamma, _) = do
        (tipe, constr) <- liftIO $ instantiate (_Gamma Map.! name)
        unifyTypes t tipe
        return CTrue
    constrainExpr_ (Can.VarTopLevel expr1 expr2) t _GammaPath = error "TODO get type from imports"
    constrainExpr_ (Can.VarCtor _ _ ctorName _ _) t _GammaPath =
      --Traverse the simple type of the constructor until it's not an arrow type
      --Collect the argument patterns, then apply the Ctor name to those patterns
      return $ ctorLoop t [] CTrue
        where
            ctorLoop ((Fun dom cod) :@ v3) reverseAccum condAccum = ctorLoop cod (dom : reverseAccum) (condAccum /\ (v3 ==== litLambda))
            ctorLoop (_ :@ ctorVar ) reverseAccum condAccum = condAccum /\ (ctorVar ==== (Ctor (N.toString ctorName) (map toLit $ reverse reverseAccum)) )
    constrainExpr_ (Can.VarOperator expr1 expr2 expr3 expr4) t _GammaPath = return CTrue --TODO built-in types for operators 
    constrainExpr_ (Can.Binop expr1 expr2 expr3 expr4 expr5 expr6) t _GammaPath = return CTrue --TODO built-in types for operators  
    constrainExpr_ (Can.Case discr branches) resultType (_Gamma, pathConstr) = do
        (inputPatterns, inputConstrs) <- self (_Gamma, pathConstr) discr
        --TODO negate previous branches
        let litBranches = map (\ (Can.CaseBranch pat rhs) -> (pat, canPatToLit pat, rhs) ) branches
        --Emit a safety constraint: must cover all possible inputs by our branch patterns
        tellSafety pathConstr (inputPatterns << unions (map (\(_,b,_) -> b) litBranches)) region
        (patVars, branchConstrs) <- unzip <$>
            forM litBranches (
                \(pat, lit, rhs) -> do
                    v <- liftIO $ UF.fresh ()
                    let canBeInBranch = ( toLit inputPatterns `intersect` lit) </< Bottom
                    let newEnv = envAfterMatch tyMap (toLit inputPatterns) pat
                    let newPathConstr = canBeInBranch /\ pathConstr
                    (rhsTy, rhsConstrs) <- self (Map.union newEnv _Gamma, newPathConstr) rhs
                    return (v,
                        (canBeInBranch ==> (v ==== rhsTy))
                        /\ ((CNot canBeInBranch) ==> (v ==== Bottom))))
        --The result of the whole thing is the union of all the results of the branches
        let resultConstr = resultType ==== (unions patVars)
        return (inputConstrs /\ resultConstr /\ CAnd branchConstrs)
    --Lambda base case: just typecheck the body if there are 0 args

    constrainExpr_ (Can.Lambda allArgPats body) t (_Gamma, pathConstr) =
        lamHelper allArgPats t (_Gamma, pathConstr)
            where
                --TODO need to alter path condition?
                lamHelper [] t _GammaPath = do
                    --TODO need to unify types more?
                    (bodyType, bodyConstr) <- self _GammaPath body
                    unifyTypes t  bodyType
                    return bodyConstr
                lamHelper (argPat:argPats) t@( (Fun dom cod) :@ v3 ) (_Gamma, pathConstr) = do
                    --Emit a safety constraint saying that the argument must match the pattern
                    let litPat = canPatToLit argPat
                    tellSafety pathConstr (dom << litPat) region
                    --All values of function types must be lambdas, so we have a trivial constraint on v3
                    let lamConstr = (v3 ==== litLambda)
                    --Get the environment to check the body
                    let newEnv = envAfterMatch tyMap (toLit dom) argPat
                    --Check the body --TODO path constr?
                    bodyConstr <- lamHelper argPats cod (Map.union newEnv _Gamma, pathConstr)
                    return $ bodyConstr /\ lamConstr
    constrainExpr_ (Can.Call fun args) retEffect _GammaPath = do
         (funTy, funConstr) <- self _GammaPath fun
         (argTEs, argConstrs) <- unzip <$> mapM (self _GammaPath) args
         return $ argHelper funTy argTEs ( funConstr /\ CAnd argConstrs)
            where
                --Loop to compute the return type
                --At each application, instantiate the function's argument type to be the type of the given argument
                --Finally, make sure the patterns of the whole expression is the pattern the function returns
                argHelper funEffect [] accum = funEffect ==== retEffect  /\ accum
                argHelper (Fun (tdom :@ edom) cod :@ efun) (targ :@ earg : rest) accum =
                    argHelper cod rest (earg ==== edom /\ accum )
    constrainExpr_ (Can.If conds elseExpr) t _GammaPath = do
        (elseType, elseCond) <- self _GammaPath elseExpr
        (branchTypes, branchConds) <-
            unzip <$>
                forM conds (
                    \ (ifExp, thenExp) -> do
                        retType <- liftIO $ UF.fresh ()
                        (ifType, ifCond) <- self _GammaPath ifExp
                        (thenType, thenCond) <- self _GammaPath thenExp
                        return (retType,
                            ifCond
                            /\ thenCond
                            /\ ((litTrue << ifType ) ==> (retType ==== thenType) )
                            /\ ((((litTrue `intersect` (toLit ifType)) ==== Bottom)) ==> (retType ==== Bottom))))
        return $ elseCond /\ (CAnd branchConds) /\ (t ==== unions ((toLit elseType) : (map toLit branchTypes)))
    constrainExpr_ (Can.Let def inExpr)  t _GammaPath@(_Gamma, pathConstr) = do
        --We don't generate any constraints when we constrain a definition
        --Because those constraints get wrapped up in the EffectScheme
        --And are instantiated at each use of x
        envExt <- liftIO $ constrainDef tyMap _GammaPath def
        (bodyType, bodyConstr) <- self (Map.union envExt _Gamma, pathConstr) inExpr
        unifyTypes bodyType t
        return bodyConstr
    constrainExpr_ (Can.LetDestruct pat letExp inExp) t _GammaPath@(_Gamma, pathConstr) = do
        --TODO need to generalize?
        let lit = canPatToLit pat
        --Can't have a recursive let on a pattern-match, since won't be behind a lambda
        --TODO is this right?
        (letType, letConstr) <- self _GammaPath letExp
        --Safety constraint: must match whatever we are binding
        tellSafety pathConstr (letType << lit) region
        let envExt = envAfterMatch tyMap (toLit letType) pat
        --TODO extend path cond
        (bodyType, bodyConstr) <- self (Map.union envExt _Gamma, pathConstr) inExp
        --Whole expression has type of body
        unifyTypes bodyType t
        return $ letConstr /\ bodyConstr
    constrainExpr_ (Can.Accessor expr) t _GammaPath = error "TODO records"
    constrainExpr_ (Can.Access expr1 expr2) t _GammaPath = error "TODO records"
    constrainExpr_ (Can.Update expr1 expr2 expr3) t _GammaPath = error "TODO records"
    constrainExpr_ (Can.Record expr) t _GammaPath = error "TODO records"
    constrainExpr_ (Can.Tuple expr1 expr2 Nothing) (_ :@ vTuple) _GammaPath = do
        (elemEffect1, constr1) <- self _GammaPath expr1
        (elemEffect2, constr2) <- self _GammaPath expr2
        return $ CAnd [constr1, constr2, (litPair elemEffect1 elemEffect2) ==== vTuple]
    constrainExpr_ (Can.Tuple expr1 expr2 (Just expr3)) (_ :@ vTuple) _GammaPath = do
        (elemEffect1, constr1) <- self _GammaPath expr1
        (elemEffect2, constr2) <- self _GammaPath expr2
        (elemEffect3, constr3) <- self _GammaPath expr3
        return $ CAnd [constr1, constr2, constr3, (litTriple elemEffect1 elemEffect2 elemEffect3) ==== vTuple]
    constrainExpr_ (Can.List expr) t _GammaPath = do
        (typeEffects, constrs) <- unzip <$> forM expr (self _GammaPath)
        return $ (CAnd constrs) /\ (t ==== litList typeEffects )
    constrainExpr_ (Can.Chr c) typeEffect _GammaPath = return $ (litChar c) ====  typeEffect
    constrainExpr_ (Can.Str s) typeEffect  _GammaPath = return $ (litString s) ==== typeEffect
    constrainExpr_ (Can.Int i) typeEffect  _GammaPath =  return $ (litInt i) ==== typeEffect
    constrainExpr_ (Can.Float expr) t _GammaPath = return CTrue
    constrainExpr_ (Can.Negate expr) t _GammaPath = return (t ==== Top) --TODO something smart for negation
    constrainExpr_ (Can.VarKernel expr1 expr2) t _GammaPath = return (t ==== Top)
    constrainExpr_ (Can.VarForeign expr1 expr2 expr3) t _GammaPath = return (t ==== Top)
    constrainExpr_ (Can.VarDebug expr1 expr2 expr3) t _GammaPath = return (t ==== Top)
    constrainExpr_ Can.Unit (_:@v) _GammaPath = return $ litUnit ==== v
    constrainExpr_ (Can.Shader expr1 expr2 expr3) t _GammaPath = return (t ==== Top )
    constrainExpr_ _ _ _ = error $ "Impossible type-expr combo "


--Takes place in the IO monad, not our ConstrainM
--Because we want to generate a separate set of safety constraints for this definition
--TODO check all this
constrainDef ::  Map.Map R.Region TypeEffect -> (Gamma, Constraint) -> Can.Def ->   IO Gamma
constrainDef tyMap _GammaPath@(_Gamma, pathConstr) def = do
    (x, defType, defConstr) <- case def of
        --Get the type of the body, and add it into the environment as a monoscheme
        --To start our argument-processing loop
        (Can.Def (A.At _ x) funArgs body@(A.At exprRegion _ )) -> do
            let exprType = (tyMap Map.! exprRegion)
            (exprConstr, safety) <- runCMIO $ constrainDef_  funArgs body exprType (Map.insert x (monoschemeVar exprType) _Gamma)
            return (x, exprType, exprConstr)
        (Can.TypedDef (A.At _ x) _ patTypes body@(A.At exprRegion _) _) -> do
            let exprType = (tyMap Map.! exprRegion)
            (exprConstr, safety) <- runCMIO $ constrainDef_  (map fst patTypes) body exprType (Map.insert x (monoschemeVar exprType) _Gamma)
            return (x, exprType, exprConstr)
    --Now that we have types and constraints for the body, we generalize them over all free variables
    --not  occurring in Gamma, and return an environment mapping the def name to this scheme
    --Generalize should check that the safety constraints from the body will always hold
    --If the constraints on the types hold
    let scheme = generalize (fst _GammaPath) defType defConstr
    return $ Map.singleton x scheme
    where
        constrainDef_  (argPat : argList) body ((Fun dom cod) :@ vFun) _Gamma =
            --Add the argument pattern vars to the dictionary, then check the rest at the codomain type
            constrainDef_ argList body cod (Map.union (envAfterMatch tyMap (toLit dom) argPat) _Gamma)
        constrainDef_  [] body exprType _Gamma = do
            (bodyType, bodyConstr) <- constrainExpr tyMap (_Gamma, pathConstr) body
            unifyTypes bodyType exprType
            --Now that we have the type and constraints for our definition body
            --We can generalize it into a type scheme and return a new environment 
            --TODO run constraint solver at this point
            return bodyConstr

--Convert to our pattern format
--Used to generate safety constraints for pattern matches 
canPatToLit ::  Can.Pattern -> LitPattern
canPatToLit  (A.At info pat) =
    case pat of
        Can.PAnything -> Top
        (Can.PVar x) -> Top
        (Can.PRecord p) -> error "TODO records"
        (Can.PAlias p1 p2) -> error "TODO Pattern alias"
        Can.PUnit -> litUnit
        (Can.PTuple p1 p2 (Just p3)) -> litTriple (canPatToLit p1) (canPatToLit p2) (canPatToLit p3)
        (Can.PTuple p1 p2 Nothing) -> litPair (canPatToLit p1) (canPatToLit p2)
        (Can.PList plist) -> litList (map canPatToLit plist)
        (Can.PCons p1 p2) -> litCons (canPatToLit p1) (canPatToLit p2)
        (Can.PBool _ b) -> if b then litTrue else litFalse
        (Can.PChr c) -> litChar c
        (Can.PStr s) -> litString s
        (Can.PInt i) -> litInt i
        Can.PCtor { Can._p_name = ctorName, Can._p_args = ctorArgs } -> Ctor (N.toString ctorName) (map (canPatToLit . Can._arg) ctorArgs)

envAfterMatch :: Map.Map R.Region TypeEffect ->  LitPattern  -> Can.Pattern -> Gamma
envAfterMatch typesForRegions matchedPat (A.At region pat)  =
    let
        ourType = typesForRegions Map.! region
        ctorProjectionEnvs  topPat nameString ctorArgPats =
            let
                subPats = map ((flip $ Proj nameString) topPat) [1.. (length ctorArgPats)]
                subDicts = zipWith ((envAfterMatch typesForRegions) ) subPats ctorArgPats
             in Map.unions subDicts
    in case pat of
        Can.PVar x -> Map.singleton x (monoscheme ourType matchedPat)
        Can.PCtor { Can._p_name = ctorName, Can._p_args = ctorArgs } ->
            ctorProjectionEnvs matchedPat (N.toString ctorName) $ map Can._arg ctorArgs
        (Can.PTuple p1 p2 (Just p3)) -> ctorProjectionEnvs matchedPat ctorTriple [p1, p2, p3]
        (Can.PTuple p1 p2 Nothing) -> ctorProjectionEnvs matchedPat ctorPair [p1, p2]
        (Can.PList []) -> Map.empty --Nothing to add to env for Null
        --For lists, we get split into head and tail and handle recursively
        (Can.PList (p1 : pList)) -> ctorProjectionEnvs matchedPat ctorCons [p1, A.At region (Can.PList pList)]
        (Can.PCons p1 p2) -> ctorProjectionEnvs matchedPat ctorCons [p1, p2]
        _ -> Map.empty



--Helpers for generating literal patterns from builtin data types
ctorLam = "--Lambda"
litLambda = Ctor ctorLam []
ctorUnit = "--Unit"
litUnit = Ctor ctorUnit []

ctorPair = "--Pair"
litPair l1 l2 = Ctor ctorPair  [(toLit l1), (toLit l2)]
ctorTriple = "--Triple"
litTriple l1 l2 l3 = Ctor ctorTriple  (map toLit [l1, l2, l3])

litList l = case l of
    [] -> litNull
    (h : t) -> litCons h (litList t)
ctorNull = "--Null"
ctorCons = "--Cons"
litNull = Ctor ctorNull []
litCons h t = Ctor ctorCons [toLit h, toLit t]

ctorTrue = "--True"
litTrue = Ctor ctorTrue []
ctorFalse = "--False"
litFalse = Ctor ctorFalse []
ctorChar c = "--CHAR_" ++ unpack c
litChar c = Ctor (ctorChar c) []
ctorString s = "--STRING" ++ show s
litString s = Ctor (ctorString s) []

ctorZero = "--ZERO"
ctorPos = "--POSITIVE"
ctorNeg = "--NEGATIVE"
ctorSucc = "--SUCC"
litInt i = case i of
    0 -> Ctor ctorZero []
    _ | i < 0 -> Ctor ctorPos [litNat (-i)]
    _ | i > 0 -> Ctor ctorNeg [litNat i]

litNat n = case n of
    0 -> Ctor ctorZero []
    i | i > 0 -> Ctor ctorSucc [litNat (i-1)]