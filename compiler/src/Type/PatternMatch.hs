{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Type.PatternMatch where

import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

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

type Variable = UF.Point ()

data TypeEffect_ = 
    PlaceHolder N.Name
    | Alias ModuleName.Canonical N.Name [(N.Name, TypeEffect)] TypeEffect
    | Var Variable
    | App ModuleName.Canonical N.Name [TypeEffect]
    | Fun TypeEffect TypeEffect
    | EmptyRecord
    | Record (Map.Map N.Name TypeEffect) TypeEffect
    | Unit
    | Tuple TypeEffect TypeEffect (Maybe TypeEffect)
    


infix 9 :@    
data TypeEffect =  TypeEffect_ :@ Variable  

data LitPattern = 
    SetVar Variable
    | Ctor String [LitPattern] 
    | Proj Int LitPattern 
    | Top
    | Bottom 

data Constraint =
    CAnd [Constraint]
    | COr [Constraint]
    | CImplies Constraint Constraint
    | CSubset LitPattern LitPattern
    | CTrue

p1 === p2 = 
    CSubset p1 p2 /\ CSubset p2 p1

class Subsetable a where
    toLit :: a -> LitPattern
     
instance Subsetable LitPattern where
    toLit = id 
    
instance Subsetable Variable where
    toLit = SetVar

(<<) :: (Subsetable a, Subsetable b ) => a -> b -> Constraint
v1 << v2 = CSubset (toLit v1) (toLit v2)  

instance Semigroup Constraint where
    CTrue <> c = c
    c <> CTrue = c
    c1 <> (CAnd cl) = CAnd (c1 : cl) 
    c1 <> c2 = CAnd [c1, c2]

instance Monoid Constraint where
    mempty = CTrue

data EffectScheme =
    Forall [Variable] TypeEffect Constraint

type Gamma = Map.Map N.Name EffectScheme

freeConstraintVars :: TypeEffect -> [Variable]
freeConstraintVars ty = [] --TODO implement

instantiate :: EffectScheme -> (TypeEffect, Constraint)
instantiate = error "TODO instantiate"

generalize :: Gamma -> TypeEffect -> EffectScheme
generalize = error "TODO generalize"

(==>) = CImplies

--Smart constructor for AND
CAnd c1 /\ CAnd c2 = CAnd (c1 ++ c2)
c1 /\ CAnd c2 = CAnd (c1 : c2)
CAnd c1 /\ c2 = CAnd (c2 : c1 )
c1 /\ c2 = CAnd [c1, c2]

c1 \/ c2 = COr [c1, c2]

addEffectVars :: Type.Type -> IO TypeEffect
addEffectVars t = do
    (innerType :: TypeEffect_ ) <- 
        case t of
            (Type.PlaceHolder t) -> 
                return $ PlaceHolder t
            (Type.AliasN t1 t2 t3 t4) -> 
                error "TODO type aliases" -- (AliasN  t1 t2 ) <$> addEffectVars t3 <*> addEffectVars t4
            (Type.VarN t) -> 
                return $ Var (error "TODO var conversion")
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

freshTE :: IO TypeEffect
freshTE = do
    tipe <- Var <$> UF.fresh () --TODO what descriptor?
    effect <- UF.fresh () --TODO what descriptor?
    return $ tipe :@ effect

unifyTypes :: TypeEffect -> TypeEffect -> IO ()
unifyTypes (_ :@ v1) (_ :@ v2) =
    UF.union v1 v2 ()
    --TODO do we need to unify sub-vars?




constrainExpr :: Map.Map R.Region TypeEffect -> Gamma -> Can.Expr ->   IO (TypeEffect,  Constraint) 
constrainExpr tyMap _Gamma (A.At region expr)  =  
    case Map.lookup region tyMap of
        Just ty -> do
            c <- constrainExpr_ expr ty _Gamma
            return (ty, c)
        Nothing -> error "Region not in type map"
  where
    self  = constrainExpr tyMap 
    constrainExpr_ :: Can.Expr_ -> TypeEffect -> Gamma -> IO Constraint 
    constrainExpr_ (Can.VarLocal name) t _Gamma = do
        let (tipe, constr) = instantiate (_Gamma Map.! name)
        unifyTypes t tipe
        return CTrue
    constrainExpr_ (Can.VarTopLevel expr1 expr2) t _Gamma = error "TODO get type from imports"
    constrainExpr_ (Can.VarCtor expr1 expr2 expr3 expr4 expr5) t _Gamma = _ 
    --TODO: for ctor, get its (simple) type scheme and instantiate
    --Turn into TypeEffect
    constrainExpr_ (Can.VarOperator expr1 expr2 expr3 expr4) t _Gamma = return CTrue --TODO built-in types for operators 
    constrainExpr_ (Can.Binop expr1 expr2 expr3 expr4 expr5 expr6) t _Gamma = return CTrue --TODO built-in types for operators  
    constrainExpr_ (Can.Case discr branches) (discType :@ inputPatterns) _Gamma = do
        let coveredPatterns = (_ :: LitPattern)
        let coveredConstr =  inputPatterns << coveredPatterns
        branchConstr <- CAnd <$> 
            (forM branches $ 
                \(Can.CaseBranch pat rhs) -> do
                    return _ ) 
        return $ coveredConstr /\ branchConstr --TODO safety constraint separately?
    constrainExpr_ (Can.Lambda expr1 expr2) ( (Fun (t1 :@ e1) (t2 :@ v2)) :@ v3 ) _Gamma = _ 
    constrainExpr_ (Can.Call fun args) (_ :@ retEffect) _Gamma = do
         (funTy, funConstr) <- self _Gamma fun 
         (argTEs, argConstrs) <- unzip <$> mapM (self _Gamma) args  
         return $ argHelper funTy argTEs ( funConstr /\ CAnd argConstrs) 
            where
                --Loop to compute the return type
                --At each application, make sure the possible arg patterns are less than the patterns the function expects
                --Finally, make sure the patterns of the whole expression contain every possible pattern the functions return
                argHelper (_ :@ funEffect) [] accum = funEffect << retEffect  /\ accum
                argHelper (Fun (tdom :@ edom) cod :@ efun) (targ :@ earg : rest) accum = 
                    argHelper cod rest (earg << edom /\ accum ) 
    constrainExpr_ (Can.If expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.Let def inExpr)  t _Gamma = 
        snd<$>constrainDef tyMap _Gamma def 
    constrainExpr_ (Can.LetDestruct expr1 expr2 expr3) t _Gamma = _ 
    constrainExpr_ (Can.Accessor expr) t _Gamma = _ 
    constrainExpr_ (Can.Access expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.Update expr1 expr2 expr3) t _Gamma = _ 
    constrainExpr_ (Can.Record expr) t _Gamma = _ 
    constrainExpr_ (Can.Tuple expr1 expr2 expr3) t _Gamma = _ 
    constrainExpr_ (Can.List expr) t _Gamma = _ --TODO fold over list type 
    constrainExpr_ (Can.Chr c) (_:@v) _Gamma = return $ (litChar c) << (SetVar v) 
    constrainExpr_ (Can.Str s) t _Gamma = return CTrue
    constrainExpr_ (Can.Int i) t _Gamma =  return CTrue
    constrainExpr_ (Can.Float expr) t _Gamma = return CTrue
    constrainExpr_ (Can.Negate expr) t _Gamma = return CTrue --TODO ints?
    constrainExpr_ (Can.VarKernel expr1 expr2) t _Gamma = return CTrue 
    constrainExpr_ (Can.VarForeign expr1 expr2 expr3) t _Gamma = return CTrue
    constrainExpr_ (Can.VarDebug expr1 expr2 expr3) t _Gamma = return CTrue
    constrainExpr_ Can.Unit t _Gamma = return CTrue 
    constrainExpr_ (Can.Shader expr1 expr2 expr3) t _Gamma = return CTrue     
    constrainExpr_ _ _ _ = error $ "Impossible type-expr combo "

constrainDef :: Map.Map R.Region TypeEffect -> Gamma -> Can.Def ->   IO (TypeEffect,  Constraint)
constrainDef tipes _Gamma def = _


--Given a variable V for a pattern expression, and the LHS of a case branch,
--Generate the constraint that the variable 
canPatToLit ::  Can.Pattern -> LitPattern
canPatToLit  (A.At info pat) = 
    case pat of 
        Can.PAnything -> Top
        (Can.PVar x) -> _ Top
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
        (Can.PCtor { Can._p_home = p__p_home, Can._p_type = p__p_type, Can._p_union = p__p_union, Can._p_name = ctorName, Can._p_index = p__p_index, Can._p_args = ctorArgs }) -> Ctor (N.toString ctorName) (map (canPatToLit . Can._arg) ctorArgs)  

litUnit = Ctor "--Unit" []
litPair l1 l2 = Ctor "--Pair" [l1, l2]
litTriple l1 l2 l3 = Ctor "--Triple" [l1, l2, l3]
litList l = case l of
    [] -> litNull
    (h : t) -> litCons h (litList t)
litNull = Ctor "--Null" []
litCons h t = Ctor "--Cons" [h, t]
litTrue = Ctor "--True" []
litFalse = Ctor "--False" []
litChar c = Ctor ("--CHAR_" ++ unpack c) []
litString s = Ctor ("--STRING" ++ show s) []
litInt i = case i of
    0 -> Ctor "--ZERO" []
    _ | i < 0 -> Ctor "--NEGATIVE" [litNat (-i)]
    _ | i > 0 -> Ctor "--POSITIVE" [litNat i]

litNat n = case n of
    0 -> Ctor "--ZERO" []
    i | i > 0 -> Ctor "--SUCC" [litNat (i-1)]