{-# OPTIONS_GHC -fwarn-unused-binds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE BangPatterns #-}

module Type.PatternMatch (patternMatchAnalysis) where

import qualified Reporting.Error

import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad
import Data.Foldable (foldrM, toList)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import Data.Char (isAlphaNum)

import qualified Nitpick.PatternMatches as PatError
 
import Control.Monad.Fail
import Control.Monad.Except (ExceptT, MonadError, throwError, runExceptT)

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

import Control.Monad.Reader as Reader

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import qualified Type.Type as Type

import Control.Monad.Writer.Strict 

import Data.Semigroup
import Data.Monoid

import qualified Data.List as List
import qualified Data.Set as Set

import qualified Reporting.Result as Result

import qualified SetConstraints.Solve as SC

import Data.Index (ZeroBased(..))
import qualified AST.Utils.Shader

import qualified Debug.Trace as Trace

import qualified Data.Graph as Graph
import qualified Data.Tree as Tree

import Data.Bifunctor (first, bimap)

-- {-# INLINE verbose #-}
(verbose :: Bool, verboseSMT :: Bool, unrollLevel :: Int) = read $ unsafePerformIO  $ readFile "/home/joey/gh/elm-compiler/verbose.txt"
-- verboseSMT = True --verbose && True

{-# INLINE trace #-}
-- trace = Trace.trace
trace s x = if verbose then (Trace.trace s x) else x

-- {-# INLINE doLog #-}
-- doLog s = when verbose $ putStrLn s
-- doLog s = return ()

{-# INLINE logIO #-}
logIO s = when verbose $ liftIO $ putStrLn s
-- doLog s = return ()

type Variable = UF.Point (String, Maybe LitPattern)

instance Ord Variable where
    v1 <= v2 = (unsafePerformIO $ UF.get v1) <= (unsafePerformIO $ UF.get v2 )

newtype Arity = Arity {getArity :: Int} deriving (Show)


deriving instance (Show a) => Show (A.Located a)
deriving instance Show Can.Expr_
deriving instance Show Can.Pattern_
deriving instance Show Can.PatternCtorArg
-- deriving instance Show Can.Union
instance Show Can.Union where
    show _ = "UNION"
instance Show ZeroBased where
    show z = "ZB"
deriving instance Show Can.CtorOpts
deriving instance Show Can.Def
deriving instance Show Can.CaseBranch
deriving instance Show Can.FieldUpdate
instance Show AST.Utils.Shader.Shader where
    show _ = "SHADER"

-- deriving instance Show Can.Pattern_



data TypeEffect_ typeEffect =
    TypeVar N.Name
    | Alias ModuleName.Canonical N.Name [(N.Name, typeEffect)] typeEffect
    -- | Var Variable
    | App ModuleName.Canonical N.Name [typeEffect]
    | Fun typeEffect typeEffect
    | EmptyRecord
    | Record (Map.Map N.Name typeEffect)
    | Unit
    | Tuple typeEffect typeEffect (Maybe typeEffect)
    deriving (Functor, Traversable, Foldable, Show)

instance Show Variable where
    show v = unsafePerformIO $ do
        v' <- UF.repr v
        fst <$> UF.get v'

newtype Fix f = Fix (f (Fix f))

infix 9 :@
data TypeEffect =  (TypeEffect_ TypeEffect) :@ LitPattern
    deriving Show

data LitPattern_ self =
    SetVar_ Variable
    | Ctor_ String [self]
    -- | Proj_ String Arity Int self
    | Top_
    | Bottom_
    | Intersect_ self self
    | Union_ self self
    | Neg_ self
    deriving (Functor, Traversable, Foldable,  Eq, Ord)


pattern SetVar v = Fix ( SetVar_ v)
pattern Ctor s l = Fix (Ctor_ s l)
-- pattern Proj s a i p = Fix (Proj_ s a i p)
pattern Top = Fix Top_
pattern Bottom = Fix Bottom_
pattern Union x y = Fix (Union_ x y)
pattern Intersect x y = Fix (Intersect_ x y)
pattern Neg x = Fix (Neg_ x)

instance Show LitPattern where
    show (SetVar v) = show v
    show (Ctor s l) = s ++  "(" ++ List.intercalate ", " (map show l) ++ ")"
    show Top = "⊤"
    show Bottom = "⊥"
    show (Union x y) = "(" ++ show x ++ " ∪ " ++ show y ++ ")"
    show (Intersect x y) = "(" ++ show x ++ " ∩ " ++ show y ++ ")"
    show (Neg x) = "(" ++"¬" ++ show x ++ ")"

data Constraint_ self =
    CAnd_ [self]
    -- | COr_ [self]
    | CImplies_ self self
    -- | CIff_ self self
    | CSubset_ LitPattern LitPattern
    | CEqual_ LitPattern LitPattern
    | CTrue_
    | CNonEmpty_ LitPattern
    deriving (Functor, Traversable, Foldable, Show, Eq, Ord)

pattern CAnd l = Fix (CAnd_ l)
-- pattern COr l = Fix (COr_ l)
pattern CImplies x y = Fix (CImplies_ x y)
-- pattern CIff x y = Fix (CIff_ x y)
pattern CSubset x y = Fix (CSubset_ x y)
pattern CEqual x y = Fix (CEqual_ x y)
pattern CTrue = Fix CTrue_
pattern CFalse = Fix (CSubset_ Top Bottom)
pattern CNonEmpty x = Fix (CNonEmpty_ x)

instance Show Constraint where
    show (CAnd l) = "(" ++ List.intercalate " ∧ " (map show l) ++ ")"
    -- show (COr l) = "(" ++ List.intercalate " ∨ " (map show l) ++ ")"
    show (CImplies x y) = "(" ++ show x ++ " ⇒ " ++ show y ++ ")"
    -- show (CIff x y) = "(" ++ show x ++ " ⇔ " ++ show y ++ ")"
    show (CNonEmpty x) = "(" ++ show x ++ " ⊈ " ++ show Bottom ++ ")"
    show (CSubset x y) = "(" ++show x ++ " ⊆ " ++ show y ++ ")"
    show (CEqual x y) = show x ++ " ≡ " ++ show y
    show CTrue = "TRUE"

type LitPattern = Fix LitPattern_
deriving instance Ord LitPattern

type Constraint = Fix Constraint_
deriving instance Ord Constraint


-- instance (Show (f (Fix f))) => (Show (Fix f))
--   where
--     show (Fix x) = "(" ++ show x ++ ")"
-- deriving instance (Show a) => Show (LitPattern_ a )
-- deriving instance (Show a) => Show (Constraint_ a)

instance (Eq (f (Fix f))) => (Eq (Fix f)) where
    (Fix a) == (Fix b) = a == b



newtype Safety = Safety {unSafety :: [(Constraint, (R.Region, PatError.Context, [Can.Pattern]) )]}
    deriving (Monoid, Semigroup)


instance Show Safety where
    show (Safety l) = show $ map fst l

instance Eq Safety where
    (Safety l1) == (Safety l2) = (map fst l1) == (map fst l2)

getSafetyConstrs :: Safety -> [Constraint]
getSafetyConstrs (Safety l) = map fst l

(====) :: (Subsetable a, Subsetable b) => a -> b -> Constraint
p1 ==== p2 =
    let l1 = toLit p1
        l2 = toLit p2
    in case (l1, l2 ) of
        -- (_,Top) -> Top << l1
        -- (Top, _) ->  Top << l2
        -- (_,Bottom) ->   l1 << Bottom
        -- (Bottom, _) -> l2 << Bottom
        _ -> CEqual l1 l2

--When setting a variable type to top, we only unify its positively occuring variables with top
--This ensures that it can accept any input, but we know nothing about its output
--TODO: get rid of this when we support modules

deepUnifyTop :: TypeEffect -> Constraint
deepUnifyTop te = CAnd $ (flip map) (typePositiveVars te) (==== Top)

doDeepUnifyTop :: ConstrainM m => TypeEffect -> m ()
doDeepUnifyTop te = forM_ (typePositiveVars te) $ \var -> unifyEffects (SetVar var) Top


unions :: (Subsetable a, Ord a) => [a] -> LitPattern
-- unions [] = Bottom
unions (a : l) = foldr (\ a b ->  (toLit a) `unionPat` b) (toLit a) (List.sort l)

-- intersects :: (Subsetable a, Ord a) => [a] -> LitPattern
-- -- intersects [] = Top
-- intersects (a : l) = foldr (\ a b ->  (toLit a) `intersectPat` b) (toLit a) (List.sort l)

-- union :: (Subsetable a, Subsetable b) => a -> b -> LitPattern
-- union a b = unionPat (toLit a) (toLit b)

intersect :: (Subsetable a, Subsetable b) => a -> b -> LitPattern
intersect a b = intersectPat (toLit a) (toLit b)


unionPat :: LitPattern -> LitPattern -> LitPattern
unionPat Bottom b = b
unionPat a Bottom =  a
unionPat Top b = Top
unionPat a Top = Top
-- unionPat (Ctor nm args) (Ctor nm' args') | nm == nm' = 
--     Ctor nm (zipWith unionPat args args')
unionPat a b =   a `Union`  b


intersectPat :: LitPattern -> LitPattern -> LitPattern
intersectPat Top b = b
intersectPat a Top = a
intersectPat Bottom b = Bottom
intersectPat a Bottom = Bottom
-- intersectPat (Ctor nm args) (Ctor nm' args') | nm == nm' = 
--     Ctor nm (zipWith intersectPat args args')
intersectPat a b = a `Intersect`  b

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

-- (</<) :: (Subsetable a, Subsetable b ) => a -> b -> Constraint
-- v1 </< v2 = CNot $ CSubset (toLit v1) (toLit v2)

instance Semigroup Constraint where
    CTrue <> c = c
    c <> CTrue = c
    c1 <> (CAnd cl) = CAnd (c1 : cl)
    c1 <> c2 = CAnd [c1, c2]

instance Monoid Constraint where
    mempty =  CTrue
    mappend = (Data.Semigroup.<>)

data EffectScheme =
    Forall [Variable] TypeEffect Constraint Safety
    deriving (Show)

type Gamma = Map.Map N.Name EffectScheme

-- monoscheme :: TypeEffect -> LitPattern -> EffectScheme
-- monoscheme tipe@(_ :@ var) lit =
--     Forall [] tipe (var ==== lit) (Safety [])

monoschemeVar :: TypeEffect -> EffectScheme
monoschemeVar tipe = Forall [] tipe CTrue (Safety [])



-- freeConstraintVars :: TypeEffect -> [Variable]
-- freeConstraintVars ty = [] --TODO implement

-- traverseTE :: (TypeEffect -> TypeEffect) -> TypeEffect -> TypeEffect
-- traverseTE f (t :@ e) = f ((f <$>  t) :@ e)

typeSubsts :: (Variable -> Variable) -> TypeEffect -> TypeEffect
typeSubsts sub (t :@ e) =
    ((typeSubsts sub <$> t) :@ (litSubsts sub e))

constrSubsts :: (Variable -> Variable) -> Constraint -> Constraint
constrSubsts sub c@(CSubset p1 p2) =  CSubset (litSubsts sub p1) (litSubsts sub p2)
constrSubsts sub c@(CEqual p1 p2) =  CEqual (litSubsts sub p1) (litSubsts sub p2)
constrSubsts sub c@(CNonEmpty  p1 ) =  CNonEmpty (litSubsts sub p1) 
constrSubsts sub (Fix c) =   Fix (constrSubsts sub <$> c)

safetySubsts sub (Safety l) = Safety $ (fmap . first)  (constrSubsts sub ) l

litSubsts :: (Variable -> Variable) -> LitPattern -> LitPattern
litSubsts sub l@(SetVar v) =  (SetVar $ sub v) 
litSubsts sub (Fix l) =   Fix (litSubsts sub <$> l) 

-- typeSubsts :: (Variable -> Variable) -> TypeEffect -> TypeEffect
-- typeSubsts sub (t :@ e) = oneTypeSubst sub ((oneTypeSubst sub <$> t) :@ e)

-- constrSubsts :: (Variable -> Variable) -> Constraint -> Constraint
-- constrSubsts sub (Fix c) =  oneConstrSubst sub (Fix $ (oneConstrSubst sub <$> c))

-- litSubsts :: (Variable -> Variable) -> LitPattern -> LitPattern
-- litSubsts sub (Fix l) = oneLitSubst sub (Fix $ (oneLitSubst sub <$> l)) 

typeLocalVars (t :@ e) = litFreeVars e
constrLocalVars (CSubset p1 p2) = (litFreeVars p1) ++ (litFreeVars p2)
constrLocalVars (CNonEmpty p1) = (litFreeVars p1)
constrLocalVars (CEqual p1 p2) = (litFreeVars p1) ++ (litFreeVars p2)
constrLocalVars c = []
-- litLocalVars (SetVar v) = unsafePerformIO $ do
--     (_, desc) <- UF.get v
--     case desc of
--         Nothing -> return [v]
--         Just e -> return $ [v] ++ litFreeVars e
-- litLocalVars l = []

typeFreeVars ty@(t :@ e) =  (typeLocalVars ty) ++ concatMap typeFreeVars (toList t)
constrFreeVars (Fix c) =  (constrLocalVars (Fix c)) ++ concatMap constrFreeVars (toList c)
-- litFreeVars (Fix l) =  litLocalVars (Fix l) ++ concatMap litFreeVars ( toList l)
safetyFreeVars (Safety l) = concatMap  (constrFreeVars . fst ) l

litLocalVars :: LitPattern -> [Variable]
litLocalVars (SetVar v) = [v]
litLocalVars l = []

litFreeVars :: LitPattern -> [Variable]
litFreeVars (Fix l) =  litLocalVars (Fix l) ++ concatMap litFreeVars ( toList l)

typePositiveVars (Fun cod dom :@ e) = typePositiveVars dom ++ typeNegativeVars cod
typePositiveVars ty@(t :@ e) =  (typeLocalVars ty) ++ concatMap typePositiveVars (toList t)

--Function arrow flips the polarity
typePositiveVars (Fun cod dom :@ e) = typeNegativeVars dom ++ typePositiveVars cod
--Local variables never occur negatively in a type
typeNegativeVars ty@(t :@ e) = concatMap typeNegativeVars (toList t)


--Like free vars, but only get ones on RHS of implication
constrPosVars (CImplies _ c) =  constrPosVars c
constrPosVars (Fix c) =  (constrLocalVars (Fix c)) ++ concatMap constrPosVars (toList c)

-- schemeFreeVars (Forall v t c  ) =  (typeFreeVars t) ++ (constrFreeVars c) 

-- typeFreeTypeVars :: TypeEffect -> [(N.Name, LitPattern)]
-- typeFreeTypeVars ty@(t :@ e) =  (typeLocalTypeVars ty) ++ concatMap typeFreeTypeVars (toList t)
-- typeLocalTypeVars t@(TypeVar n :@ e) = [(n,e)]
-- typeLocalTypeVars (t :@ e) = []



--Does this pattern match at most one element?
isSingleton :: LitPattern -> Bool
isSingleton (Ctor _ args) = all isSingleton args
isSingleton _ = False

--Is this pattern for sure not empty
forSureNotEmpty :: LitPattern -> Bool
forSureNotEmpty (Ctor _ args) = all forSureNotEmpty args
forSureNotEmpty Top = True
forSureNotEmpty (Union a b) = forSureNotEmpty a && forSureNotEmpty b
forSureNotEmpty _ = False

freshName :: (ConstrainM m) => String -> m String
freshName s = do
    ioref <- State.get
    i <- liftIO $ readIORef ioref
    liftIO $ writeIORef ioref (i + 1)
    return $ s ++ "_" ++  show i

-- varNamed :: (ConstrainM m) => String -> m Variable
-- varNamed s =
--     liftIO $ UF.fresh (s, Nothing)

freshVar :: (ConstrainM m) => m Variable
freshVar = do
    desc <- freshName "SetVar"
    liftIO $ UF.fresh (desc, Nothing)

instantiate :: (ConstrainM m) => EffectScheme -> m (TypeEffect, Constraint)
instantiate (Forall boundVars tipe constr safety) = do
    freshVars <- forM [1 .. length boundVars] $ \ _ -> freshVar
    let subList =  zip boundVars freshVars
    case subList of
        [] -> do
            --Should be empty for monoscheme
            tell safety
            return (tipe, constr)
        _ -> do
            -- logIO $ "Instantiating with SubList" ++ (show subList)
            let typeFVs = typeFreeVars tipe
            let constrFVs = constrFreeVars constr
            let safetyFVs = safetyFreeVars safety
            subPairList <-  forM (typeFVs ++ constrFVs ++ safetyFVs) $ \oldVar -> forM subList $ \(boundVar, freshVar) -> do
              equiv <- liftIO $ UF.equivalent oldVar boundVar
              case equiv of
                False -> return []
                True -> return [(oldVar, freshVar)]
            let subPairs = List.nub (subList ++  concatMap concat subPairList  )
            -- logIO $ "Got total subPair list " ++ show subPairs
            let substFun x =
                    case lookup x subPairs of
                        Nothing -> x
                        Just y -> y
            let subbedSafety = safetySubsts substFun safety
            --Add whatever safety constraints came from the instantiation
            -- to our current safety constraints
            tell subbedSafety
            let subbedType = typeSubsts substFun tipe
                subbedConstr = constrSubsts substFun constr
            -- logIO $ "Subbed type for instantiation " ++ show subbedType
            -- logIO $ "Subbed constr for instantiation " ++ show subbedConstr
            return $ (subbedType, subbedConstr )


generalize :: (ConstrainM m) => Gamma -> TypeEffect -> Constraint -> Safety -> m EffectScheme
generalize _Gamma tipe constr safety = do
    let allFreeVars_dupes = (typeFreeVars tipe) ++ constrFreeVars constr ++ safetyFreeVars safety
    -- logIO $ "GENERALIZE: free vars with duples" ++ show allFreeVars_dupes
    allFreeVars <- liftIO $ List.nub <$> mapM UF.repr allFreeVars_dupes
    -- logIO $ "GENERALIZE: free vars " ++ show allFreeVars
    let schemeVars (Forall bnd (t :@ lit) sconstr ssafety) = liftIO $ do
            let vEff = litFreeVars lit
            let vConstr = constrFreeVars sconstr
            let vSafety = safetyFreeVars ssafety
            reprs <- mapM UF.repr (vEff ++ vConstr ++ vSafety)
            boundReprs <- mapM UF.repr bnd
            return $ reprs List.\\ boundReprs
    gammaFreeVars <- (List.nub . concat) <$> mapM schemeVars (Map.elems _Gamma)
    -- logIO $ "GENERALIZE: gamma free vars " ++ show gammaFreeVars
    return $  Forall (allFreeVars List.\\ gammaFreeVars) tipe constr safety


subConstrVars :: ConstrainM m => Constraint -> WriterT [Constraint] m Constraint
subConstrVars (Fix c) = do
    ret <- case c of
        (CSubset_ l1 l2) -> CSubset <$> (subLitVars l1) <*> (subLitVars l2)
        (CEqual_ l1 l2) -> CEqual <$> (subLitVars l1) <*> (subLitVars l2)
        (CNonEmpty_ l1) -> CNonEmpty <$> (subLitVars l1) 
        _ -> Fix <$> mapM subConstrVars c
    return ret

subLitVars :: ConstrainM m => LitPattern -> WriterT [Constraint] m (LitPattern)
subLitVars (Fix l) =
    case l of
        (SetVar_ v) -> do
            (nm,ty) <- liftIO $ UF.get v
            case ty of
                Nothing -> return (SetVar v)
                (Just lnew) -> do
                    when (v `elem` litFreeVars lnew) $ do
                            liftIO $ UF.set v (nm, Nothing)
                            tell [CSubset (SetVar v) lnew, CSubset lnew (SetVar v)]
                    subLitVars lnew
                    
        _ -> do
            Fix <$> mapM subLitVars l

subTypeVars (t :@ e) = do
    tnew <-  (mapM subTypeVars t)
    enew <- (subLitVars e)
    return $ tnew :@ enew

simplifyConstraint :: Constraint -> Constraint
simplifyConstraint (CAnd l) =  
    let
        cl =   (flip concatMap) l $ \c -> case simplifyConstraint c of
            CAnd l -> l
            CTrue -> []
            cs -> [cs]
    in case cl of
        [] -> CTrue
        [c] -> c
        _ -> if CFalse `elem` cl then CFalse else CAnd cl 
simplifyConstraint (CImplies x y ) =  case (simplifyConstraint x, simplifyConstraint y) of
    (CTrue, sy) -> simplifyConstraint y
    (CFalse, _) -> CTrue
    (sx, CImplies sx2 sy) -> simplifyConstraint (CImplies (sx /\ sx2) sy)
    (sx, CAnd sys) -> simplifyConstraint $ CAnd $ map (CImplies sx) sys
    (sx, sy) -> CImplies sx sy
simplifyConstraint (CNonEmpty x) = case simplifyLit x of
    Top -> CTrue
    Bottom -> CFalse
    Intersect a b | isSingleton a -> simplifyConstraint $ CSubset a b
    Intersect b a | isSingleton a -> simplifyConstraint $ CSubset a b
    Ctor _ args -> simplifyConstraint $ CAnd $ map CNonEmpty args

    xs -> if forSureNotEmpty xs then CTrue else CNonEmpty xs
simplifyConstraint (CSubset x y) =  case (simplifyLit x, simplifyLit y) of
    (Top, rhs@(SetVar _)) -> CEqual rhs Top
    (lhs@(SetVar _), Bottom) -> CEqual lhs Bottom
    (_, Top) -> CTrue
    (Intersect a b, a') | a == a' -> CTrue 
    (Intersect b a, a') | a == a' -> CTrue
    (Ctor s1 _, Ctor s2 _) | s1 /= s2 -> CFalse
    (Ctor nm xl, Ctor nm' yl) | nm == nm' -> simplifyConstraint (CAnd $ zipWith CSubset xl yl)
    (xs, ys) -> if  xs == ys then CTrue else CSubset xs ys
simplifyConstraint (CEqual x y) =  case (simplifyLit x, simplifyLit y) of
    (Ctor nm xl, Ctor nm' yl) | nm == nm' -> simplifyConstraint (CAnd $ zipWith CEqual xl yl)
    (xs, ys) -> if  xs == ys then CTrue else CEqual xs ys
simplifyConstraint CTrue =  CTrue

simplifyLit :: LitPattern -> LitPattern

simplifyLit (Ctor s l) = 
    case map simplifyLit l of
        [a `Union` b] -> simplifyLit $ (Ctor s [a]) `Union` (Ctor s [b])
        -- [a `Intersect` b] -> simplifyLit $ (Ctor s [a]) `Intersect` (Ctor s [b])
        sl | Bottom `elem` sl -> Bottom
        sl -> Ctor s sl
simplifyLit (Union x y) = 
    case (simplifyLit x, simplifyLit y) of
        (Top, b) -> Top
        (a, Top) -> Top
        (Bottom, b) -> b
        (a, Bottom) -> a
        -- (Ctor nm xl, Ctor nm' yl) | nm == nm' -> 
        --     Ctor nm $ map simplifyLit $ zipWith Union xl yl
        (xs, ys) -> Union xs ys
simplifyLit (Intersect x y) = 
    case (simplifyLit x, simplifyLit y) of
        (Bottom, b) -> Bottom
        (a, Bottom) -> Bottom
        (Top, b) -> b
        (a, Top) -> a
        (Ctor nm xl, Ctor nm' yl)  -> 
            if nm == nm' then
                Ctor nm $ map simplifyLit $ zipWith Intersect xl yl
            else
                Bottom
        (xs, ys) -> Intersect xs ys
simplifyLit (Neg l) = case simplifyLit l of
    Neg sl -> sl
    Top -> Bottom
    Bottom -> Top
    sl -> Neg sl
simplifyLit l = l

pairsToGraphMap :: (Ord a) => [a] -> [(a,a)] -> [(a,a,[a])]
pairsToGraphMap vertices edges = 
    let
        startPairs = (map (\(a) -> (a,Set.singleton a)) vertices) ++ (map  (\(a,b) -> (a,Set.singleton b)) edges)
        edgeMap = Map.fromListWith Set.union startPairs 
    in  map (\(a,b) -> (a,a, Set.toList b)) $ Map.toList edgeMap

removeUnreachableConstraints :: (ConstrainM m) => [Variable] -> [Constraint] -> [Constraint] ->  m [(Constraint)]
removeUnreachableConstraints initial candidateConstrs allConstrs = do
    logIO "Starting graph constraints"
    --TODO okay to assume that all vars live, even if replaced by expr?
    --Variable names for all types reachable in our "initial variables"
    --i.e. those referenced by the TypeEffect we're dealing with
    initialStrings <- fmap (map fst) $  liftIO $ mapM UF.get initial
    --All varaibles in our constraint set
    allVars <- fmap (map fst) $ liftIO $ mapM UF.get $  List.nub $ initial ++ (concatMap constrFreeVars allConstrs)
    --We are looking for the set of variables that can possibly influence the values of our "initial" variable set
    --We add an edge (u,v) whenever v can influence the value of u
    --This happens whenever u occurs positively (right hand side of an implication)
    --in a constraint in which v is free
    --TODO do this
    let constrVarTriples = map (\c -> (c, constrFreeVars c, constrPosVars c)) allConstrs
    let edgesFor (c, nodesForCRaw, posForCRaw) = do
        nodesForC <- forM nodesForCRaw (liftIO . UF.get)
        posForC <- forM posForCRaw (liftIO . UF.get)
        return [(fst c1, fst c2) | c1 <- posForC, c2 <- nodesForC]
    --Combine all the edges from our different constraints
    allEdges <-  mapM edgesFor  constrVarTriples
    -- logIO $ " All edges: " ++ show allEdges
    --Get our edges into the format Data.Graph expects
    --i.e. an adjacency list for each vertex (variable)
    -- logIO $ "Got graph edges " ++ show (pairsToGraphMap allVertices allEdges)
    let (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges $ pairsToGraphMap allVars $ concat allEdges 
    logIO $ "Vertex values for each initial node " ++ show (zip initialStrings (map vertexFromKey initialStrings))
    let initialVertices = map (Maybe.fromJust . vertexFromKey) initialStrings
        toNode = ((\(a,_,_)->a) . nodeFromVertex )
        reachabilityMap = Map.fromList [ (toNode v, map toNode $ Graph.reachable graph v) | v <- initialVertices]
        reachableVertices = List.nub $ concatMap (\v -> Maybe.fromMaybe [] $ Map.lookup v reachabilityMap  )  allVars
        -- reachableVertices = concatMap (map ( (\(a,_,_)->a) . nodeFromVertex )) $ map toList $   Graph.dfs graph initialVertices 

        -- reachableVertices = map ( (\(a,_,_)->a) . nodeFromVertex . nodeOf) $ toList canReachForest
    --Now, we filter our constraints
    --Keep a constraint if any of its free variables are reachable
    -- logIO $ "Reachable vertices: " ++ show reachableVertices
    --A constraint is reachable if one of its variables is reachable from the initial set
    let constraintReachable c = do
        let vars = constrPosVars c
        varStrings <- fmap (map fst) $ liftIO $ forM vars UF.get
        return $ not $ null $ List.intersect  varStrings reachableVertices
    filterM constraintReachable  candidateConstrs


-- dischargeSafety comp = do
--         logIO $ "DISCHARGING: " ++ (show $ map fst comp)
--         unreachSoln <- solveConstraint (CAnd  ((CNonEmpty Top ) : map fst comp))
--         case unreachSoln of
--             Right _ -> return ()
--             Left _ ->
--                 forM_ comp $ \ (_, (region, context, pats) ) -> do
--                     logIO "FAILED TO DISCHARGE"
--                     throwError $ PatError.Incomplete region context (map PatError.simplify pats )

getVarName :: forall m . (ConstrainM m) => Variable -> m String 
getVarName v = fst <$> (liftIO $ UF.get v)          

constrToList :: Constraint -> [Constraint]
constrToList (CAnd l) = concatMap constrToList l
constrToList c = [c]

safetyToList :: Safety -> [(Constraint, _)]
safetyToList s = 
    concatMap (\(c,info) -> map (\c' -> (c', info)) (constrToList c) ) $ unSafety s

data InterLHS = 
    SimpleLHS LitPattern
    | ImplicationLHS Constraint LitPattern
    deriving (Show)

makeLHSConstraint (SimpleLHS lhs) rhs = CSubset lhs rhs
makeLHSConstraint (ImplicationLHS cond lhs) rhs = CImplies cond (CSubset lhs rhs)

optimizeConstr :: forall m . (ConstrainM m) => Bool -> TypeEffect  -> Constraint -> Safety -> m (TypeEffect, [Constraint], Safety)
optimizeConstr graphOpts topTipe ordConstrs safety = optimizeConstr_ graphOpts topTipe (constrToList ordConstrs) (safetyToList safety)


optimizeConstr_ :: forall m . (ConstrainM m) => Bool -> TypeEffect  -> [Constraint] -> _ -> m (TypeEffect, [Constraint], Safety)
optimizeConstr_ graphOpts topTipe constrList safetyList = do
    let tfVars = typeFreeVars topTipe
    (pairsInter, safetyInter, tInter) <-  doOpts graphOpts tfVars topTipe (map (,()) constrList) safetyList
    (optimizedSafetyList, preGraphOptimizedPairs, tRet) <- doOpts graphOpts tfVars tInter safetyInter pairsInter
    let preGraphOptimized = map fst preGraphOptimizedPairs
    -- let optimizedSafety = Safety optimizedSafetyList
    --Remove constraints that aren't "connected" to either the variables of our type
    --Our our current safety constraints
    optimized <- case graphOpts of 
        True ->
            removeUnreachableConstraints (tfVars ++ safetyFreeVars (Safety optimizedSafetyList)) preGraphOptimized (preGraphOptimized ++ map fst optimizedSafetyList)
        False -> return $ map fst preGraphOptimizedPairs
    -- logIO $ "Comparing " ++ show optimized ++ "\nto " ++ show constrList ++ "\nAND " ++ show (map fst optimizedSafetyList) ++ "\nto " ++ show  (map fst safetyList)
    case (optimized == constrList && (map fst optimizedSafetyList) ==  (map fst safetyList)) of
        True -> return $ (tRet, optimized, Safety optimizedSafetyList)
        False -> optimizeConstr_ graphOpts tRet optimized optimizedSafetyList
  where
    subConstrPair (c, info) = do
        csub <- subConstrVars c
        return (simplifyConstraint csub, info)
    subConstrPairs cList otherList tipe = do
        csub <- forM cList subConstrPair
        otherSub <- forM otherList subConstrPair
        newType <- subTypeVars tipe
        return (csub, otherSub, newType)
    doOpts :: forall a b. 
        Bool
        -> [Variable] 
        -> TypeEffect 
        -> [(Constraint, a)] 
        -> [(Constraint, b)] 
        -> m ([(Constraint, a)], [(Constraint, b)], TypeEffect)
    doOpts graphOpts tfVars tipe l others  = do
        logIO ("Initial list:\n" ++ show (map fst l))
        ((lSubbed, oSubbed, tSubbed), newConstrs1) <- runWriterT $ subConstrPairs l others tipe
        logIO ("After subbed:\n" ++ show (map fst lSubbed))
        optimized <- helper lSubbed []
        logIO ("After opt:\n" ++ show (map fst optimized))
        ((lRetWithoutNew, oRet, tRet), newConstrs2) <- runWriterT $ subConstrPairs optimized oSubbed tSubbed
        let defaultInfo = snd $ head lRetWithoutNew
        let lRet = (map (,defaultInfo) newConstrs1) ++ (map (,defaultInfo) newConstrs2) ++ lRetWithoutNew  
        logIO ("After second sub:\n" ++ show (map fst lRet))
        let totalList = (map fst lRet) ++ (map fst oRet)
        deadRemoved <- removeDead graphOpts tfVars lRet totalList tRet 
        return (deadRemoved, oRet, tRet)


    removeDead :: Bool -> [Variable] -> [(Constraint, a)] -> [Constraint] -> TypeEffect -> m [(Constraint, a)]
    removeDead graphOpts tfVars clistWithInter totalList tp = do
        logIO $ "Starting removeDead"
        let
            -- tfVars = typeFreeVars tp
            --First we remove constraints of the form
            -- X << pat 
            -- or the form (pat << X)
            --where X occurs in no other constraints
            --Since these are always trivially solveable 
            occurrences = map constrFreeVars totalList
        -- logIO $ "All occurrences: " ++ show (zip3 totalList (map (\(Fix c) -> toList c) totalList) occurrences)
        -- logIO $ "Type free vars: " ++ show tfVars ++ "  for  " ++ show tp
        tfReps <- forM tfVars (\x -> getVarName x)
        theOccList <- forM (tfVars ++ concat occurrences) (\ pt -> (fmap fst ) $ liftIO $ UF.get pt)
        let allVars = List.nub theOccList
            occurenceCountMap = Map.fromList $ map (\l -> (head l, length l)) $ List.group $ List.sort theOccList
        -- logIO $ "Bare occurreces: " ++ show occurrences
        logIO $ "OCList " ++ show theOccList
        let


            cLHS (CSubset (SetVar v) _) = Just v
            cLHS (CImplies _ (CSubset (SetVar v) _)) = Just v
            cLHS _ = Nothing
            

            cRHS (CSubset other (SetVar v)) = Just (v, SimpleLHS other)
            cRHS (CImplies cond (CSubset other (SetVar v))) = Just (v, ImplicationLHS cond other)
            cRHS _ = Nothing

        filteredLHSList <- (fmap Maybe.catMaybes ) $ forM totalList $ \c -> 
                case cLHS c of
                    Nothing -> return Nothing
                    Just v -> Just <$> getVarName v
        let lhsMap = Map.fromList $ map (\l -> (head l, length l)) $ List.group $ List.sort filteredLHSList
            

        filteredRHSList <- (fmap Maybe.catMaybes ) $ forM totalList $ \c -> 
            case cRHS c of
                Nothing -> return Nothing
                Just (v, rhs) -> do
                    repr <-  getVarName v
                    return $ Just (repr, rhs)
        let rhsGroups = List.groupBy (\ x y -> fst x == fst y) $ List.sortOn fst filteredRHSList
        let 
            rhsMap = Map.fromList $ (flip map) rhsGroups $ \l -> 
                let (v:_, lhses) = unzip l
                in (v, lhses)


            varIsIntermediate v = 
                let
                    numOccs = Maybe.fromMaybe 0 $ Map.lookup v occurenceCountMap
                    numLHS = Maybe.fromMaybe 0 $ Map.lookup v lhsMap
                    lhsForRHSOccs = Maybe.fromMaybe [] $ Map.lookup v rhsMap
                in trace ("Var " ++ v ++ " lhs  " ++ show numLHS ++ "  rhs  " ++ show lhsForRHSOccs) $ case (numOccs == numLHS + length lhsForRHSOccs, v `elem` tfReps) of
                    (True, False) -> Just (v, lhsForRHSOccs)  
                    _ -> Nothing

            varIsDead v =  do
                r1 <- fst <$> (liftIO $ UF.get v)
                return $ Map.lookup r1 occurenceCountMap  == Just 0


            constrIsDead (CSubset (SetVar v) (SetVar v2)) = do
                b1 <- (varIsDead v) 
                if b1 then return True else (varIsDead v2)
            constrIsDead (CSubset (SetVar v) l) = varIsDead v
            constrIsDead (CSubset l (SetVar v)) =  varIsDead v
            constrIsDead (CEqual (SetVar v) (SetVar v2)) = do
                b1 <- (varIsDead v) 
                if b1 then return True else (varIsDead v2)
            constrIsDead (CEqual (SetVar v) l) = varIsDead v
            constrIsDead (CEqual l (SetVar v)) = varIsDead v
            constrIsDead (CImplies c1 c) = constrIsDead c --Implication is dead if conclusion is trivial
            constrIsDead (CImplies _ CTrue) = return True
            constrIsDead c = return False
        let varInterMap =  Map.fromList $ Maybe.catMaybes  $ map varIsIntermediate allVars
        logIO $ "Got varInterMap " ++ show varInterMap
        --If a variable only serves as an intermediate (i.e. A < B, A' < B, B < C)
        --then eliminate it (i.e. into A < C, A' < C)
        let adjustIntermediate (c,info) = 
                case c of
                    --If both are variables, then we check if we can delete
                    --Because the RHS is intermediate. If so, we do, otherwise we check lhs
                    CSubset (SetVar vlhs) (SetVar vrhs) -> do 
                        rlhs <- getVarName vlhs
                        rrhs <- getVarName vrhs
                        case Map.lookup rrhs varInterMap of
                            Just _ -> return []
                            Nothing -> 
                                case Map.lookup rlhs varInterMap of
                                    Nothing -> return [(c, info)]
                                    Just lhses -> do
                                        newConstrs <- forM lhses $ \lhs ->  adjustIntermediate (makeLHSConstraint lhs (SetVar vrhs), info)
                                        return $ concat newConstrs
                    CImplies cond (CSubset (SetVar vlhs) (SetVar vrhs)) -> do 
                        rlhs <- getVarName vlhs
                        rrhs <- getVarName vrhs
                        case Map.lookup rrhs varInterMap of
                            Just _ -> return []
                            Nothing ->
                                case Map.lookup rlhs varInterMap of
                                    Nothing -> return [(c, info)]
                                    Just lhses -> do
                                        newConstrs <- forM lhses $ \lhs ->  adjustIntermediate (makeLHSConstraint lhs (SetVar vrhs), info)
                                        return $ map (\(c,_) -> (CImplies cond c , info)) $ concat newConstrs
                    CSubset (SetVar var) rhs -> do 
                        v <- getVarName var
                        case Map.lookup v varInterMap of
                            Nothing -> return [(c, info)]
                            Just lhses -> do
                                newConstrs <- forM lhses $ \lhs ->  adjustIntermediate (makeLHSConstraint lhs rhs, info)
                                return $ concat newConstrs
                    CImplies cond (CSubset (SetVar var) rhs) -> do 
                        v <- getVarName var
                        case Map.lookup v varInterMap of
                            Nothing -> return [(c, info)]
                            Just lhses -> do
                                newConstrs <- forM lhses $ \lhs ->  adjustIntermediate (makeLHSConstraint lhs rhs, info)
                                return $ map (\(c,_) -> (CImplies cond c , info)) $ concat newConstrs
                    CSubset _ (SetVar var) -> do
                        v <- getVarName var
                        return $ case Map.member v varInterMap of
                            True -> []
                            False -> [(c,info)]
                    CImplies _ (CSubset _ (SetVar var)) -> do
                        v <- getVarName var
                        return $ case Map.member v varInterMap of
                            True -> []
                            False -> [(c,info)]
                    _ -> return [(c,info)] 
        clist <- (fmap concat ) $ forM clistWithInter adjustIntermediate
        boolList <- forM clist (\x -> (fmap not) $ constrIsDead (fst x))
        let boolPairList = zip boolList clist
        let (easyFiltered, easyDeleted) = List.partition fst boolPairList
        --Then, we remove constraints contianing only variables
        --that are unreachable from the reference graph of the type's free variables
        -- i.e. we only want constraints relevant to the typeEffect
        logIO $ "Easy filter deleting: " ++ show (map (fst . snd) easyDeleted)
        logIO $ "After filtering, remaining are: " ++ show (map (fst . snd) easyFiltered)
        return $ map snd easyFiltered


    -- subVars :: Constraint -> m Constraint

    helper :: [(Constraint, a)] -> [(Constraint,a)] -> m [(Constraint, a)]
    helper [] accum =  do
        return $ reverse accum
    helper ((CTrue,_) : rest) accum =  helper rest accum
    --basically modus-ponens
    helper ((CImplies lhs rhs, info) : (lhs', info') : rest ) accum | lhs == lhs' =  helper ((rhs, info) : (lhs, info') : rest) accum
    helper ((CEqual (SetVar l1) (SetVar l2), info) : rest) accum =  do
        eq <- liftIO $ UF.equivalent l1 l2
        case eq of
            True -> helper rest accum
            False -> do
                constr <- unifyEffectVars l1 l2
                helper ((constr,info): rest) accum
    helper ((CEqual l1 (SetVar v), info) : rest) accum =  do
        (name, mval) <- liftIO $ UF.get v
        case mval of
            Nothing -> do
                liftIO $ UF.set v (name, Just l1)
                helper rest accum
            Just l2 -> helper ((CEqual l1 l2,info ) : rest) accum
    helper ((CEqual (SetVar v) l1, info ): rest) accum =  do
        (name, mval) <- liftIO $ UF.get v
        case mval of
            Nothing -> do
                liftIO $ UF.set v (name, Just l1)
                helper rest accum
            Just l2 -> helper ((CEqual l1 l2, info ) : rest) accum


    -- helper ((CSubset (SetVar l1) (SetVar l2), info) : (CSubset (SetVar l2') (SetVar l1'), info2) : rest) accum | l1 == l1' && l2 == l2' = do
    --     desc <- liftIO $ UF.get l1
    --     liftIO $ UF.union l1 l2 desc
    --     helper rest accum
    helper ((CSubset Top pat@(SetVar _), info) : rest) accum =  helper ((CEqual Top pat, info):rest) accum
    helper ((CSubset pat@(SetVar _) Bottom, info) : rest) accum =  helper ((CEqual Bottom pat, info):rest) accum
    helper ((CAnd l,info) : rest) accum =  helper ( (map (,info) l) ++ rest) accum

    -- helper ((CEqual a (Intersect b a'), info): rest) accum | a == a' = helper ((CSubset a b,info):rest) accum
    -- helper ((CSubset a (Intersect b a'), info): rest) accum | a == a' = helper ((CSubset a b,info):rest) accum

    --If we have constructors that we know are the only constructor for the type
    -- i.e. tuples, then this gives us more information about subset constraints
    helper ((CImplies cond (CSubset lhs@(Ctor tupleName vars) rhs@(SetVar rhsVar)), info ): rest) accum | tupleName `elem` ["PatMatchPair", "PatMatchTriple"] = trace "H10" $ do
        freshVars <- forM vars (\ _ -> SetVar <$> freshVar)
        let newRHS = (Ctor tupleName freshVars)
        helper ((CEqual rhs newRHS, info) : (CImplies cond (CSubset lhs newRHS), info) : rest) accum

        
    helper (h : rest) accum = do
        helper rest (h : accum)


toSC :: (ConstrainM m) => Constraint -> m SC.CExpr
toSC c = case c of
    CTrue -> return $ SC.CSubset SC.Top SC.Top
    CAnd cl -> SC.CAnd <$> mapM toSC cl
    -- COr cl -> SC.COr <$> mapM toSC cl
    CNonEmpty l -> SC.CNot <$> toSC (CSubset l Bottom)
    (CEqual l1 l2) -> toSC ((CSubset l1 l2) /\ (CSubset l2 l1))
    CSubset l1 l2 -> SC.CSubset <$> toSCLitNoCycle [] l1 <*> toSCLitNoCycle [] l2
    CImplies c1 c2 -> SC.CImplies <$> (toSC c1) <*> (toSC c2)
    -- CIff c1 c2 -> SC.CIff <$> (toSC c1) <*> (toSC c2)

toSCLitNoCycle :: (ConstrainM m) => [Variable] -> LitPattern -> m SC.Expr
toSCLitNoCycle seen l = do
    let toSCLit  = toSCLitNoCycle seen
    case l of
        SetVar uf -> do
            ufRepr <- liftIO $ UF.repr uf
            (reprName, reprVal) <- liftIO $ UF.get $ ufRepr
            case (ufRepr `elem` seen || Maybe.isNothing reprVal) of
                True -> return $ SC.Var reprName
                False -> toSCLitNoCycle (ufRepr : seen) (Maybe.fromJust reprVal)

        --Our theory doesn't support projections as expressions 
        --So we generate a fresh variable and constrain that it must contain exactly the projection
        -- Proj ctor arity i l -> do
        --     projName <-  freshName "ProjVar"
        --     tform <- toSCLit l
        --     -- simpleReturn $ SC.Var projName
        --     return $ \ f -> tform $ \ e -> SC.withProjection projName (getArity arity) (SC.Projection ctor (i-1) e) f
        Union l1 l2 ->
            SC.Union <$> toSCLit l1 <*> toSCLit l2
        Intersect l1 l2 -> SC.Intersect <$> toSCLit l1 <*> toSCLit l2
        Neg l1 -> SC.Neg <$> toSCLit l1
        Top -> return $ SC.Top
        Bottom -> return $ SC.Bottom
        Ctor name args -> SC.FunApp name <$> (mapM toSCLit args)
        l -> error $ "Missing case for lit" ++ show l

solveConstraint :: ConstrainM m => Constraint -> m (Either String ())
-- solveConstraint !x = return $ Right ()
solveConstraint CTrue = return $ Right ()
solveConstraint c = do
    logIO "In solveConstraint, passing off to SBV"
    logIO ("Flattened at top level:\n" ++ show c ++ "\n")
    sc <- toSC c
    -- liftIO $ putStrLn "Solving pattern match constraints"
    liftIO $ SC.solve (SC.Options "" verboseSMT "z3" False False False) sc
    -- liftIO $ putStrLn "Solved Pattern Match constraints"
    -- return ret

-- (<==>) ::  Constraint -> Constraint -> Constraint
-- CTrue <==> y = y
-- x <==> y =  CIff x y

(==>) ::  Constraint -> Constraint -> Constraint
CTrue ==> y = y
x ==> y =  CImplies x y

--Smart constructor for AND
(/\) :: Constraint -> Constraint -> Constraint
CTrue /\ x = x
x /\ CTrue = x
(CAnd c1) /\  (CAnd c2) =   CAnd (c1 ++ c2)
c1 /\  (CAnd c2) =  CAnd (c1 : c2)
(CAnd c1) /\ c2 =  CAnd (c2 : c1 )
c1 /\ c2 =  CAnd [c1, c2]

-- (\/) :: Constraint -> Constraint -> Constraint
-- (COr c1) \/  (COr c2) =   COr (c1 ++ c2)
-- c1 \/  (COr c2) =  COr (c1 : c2)
-- (COr c1) \/ c2 =  COr (c2 : c1 )
-- c1 \/ c2 =  COr [c1, c2]


addEffectVars :: (ConstrainM m) => Can.Type -> m TypeEffect
addEffectVars (Can.TAlias t1 t2 t3 actualType) = case actualType of
    Can.Holey t -> addEffectVars t
    Can.Filled t -> addEffectVars t
addEffectVars t = do
    innerType <- case t of
        (Can.TLambda t1 t2) -> Fun <$> addEffectVars t1 <*> addEffectVars t2
        (Can.TVar n) -> return $ TypeVar n
        (Can.TType t1 t2 t3) -> (App t1 t2) <$> (mapM addEffectVars  t3)
        (Can.TRecord t1 t2) ->
            Record <$> (mapM (\ (Can.FieldType _ t) -> addEffectVars t) t1)
        Can.TUnit -> return $ Unit
        (Can.TTuple t1 t2 t3) -> do
            mt3 <- case t3 of
                Nothing -> return Nothing
                Just t -> Just <$> addEffectVars t
            Tuple <$> addEffectVars t1 <*> addEffectVars t2 <*> (return mt3)
    constraintVar <- SetVar <$> freshVar
    return $ innerType :@ constraintVar
-- addEffectVars (Can.Alias t1 t2 t3 actualType) = addEffectVars actualType
-- addEffectVars t = do
--     innerType <-
--         case t of
--             (Type.PlaceHolder t) ->
--                 return $ PlaceHolder t
--             (Type.VarN t) ->
--                  
--             (Type.AppN t1 t2 t3) ->
--                 (App t1 t2) <$> (mapM addEffectVars  t3)
--             (Type.FunN t1 t2) ->
--                 Fun <$> addEffectVars t1 <*> addEffectVars t2
--             Type.EmptyRecordN -> return EmptyRecord
--             (Type.RecordN t1 t2) ->
--                 Record <$> (mapM addEffectVars t1) <*> addEffectVars t2
--             Type.UnitN -> return Unit
--             (Type.TupleN t1 t2 t3) -> do
--                 mt3 <- case t3 of
--                     Nothing -> return Nothing
--                     Just t -> Just <$> addEffectVars t
--                 Tuple <$> addEffectVars t1 <*> addEffectVars t2 <*> (return mt3)
--     

-- freshTE :: IO TypeEffect
-- freshTE = do
--     tipe <- Var <$> freshVar --TODO what descriptor?
--     effect <- freshVar --TODO what descriptor?
--     return $ tipe :@ effect

unifyEffects :: (ConstrainM m) => LitPattern -> LitPattern -> m Constraint
unifyEffects (SetVar v1) (SetVar v2) = unifyEffectVars v1 v2
unifyEffects (SetVar v) l1 = do
    (name, ml2 ) <- liftIO $ UF.get v
    case ml2 of
        Just l2 -> return $ l1 ==== l2
        Nothing -> do
            liftIO $ UF.set v (name, Just l1)
            return CTrue
unifyEffects l1 (SetVar v) = do
    (name, ml2 ) <- liftIO $ UF.get v
    case ml2 of
        Just l2 -> return $ l1 ==== l2
        Nothing -> do
            liftIO $ UF.set v (name, Just l1)
            return CTrue
unifyEffects l1 l2 = return (l1 ==== l2)

unifyEffectVars :: (ConstrainM m) => Variable -> Variable -> m Constraint
unifyEffectVars v1 v2 = do
    (name1, eff1) <- liftIO $ UF.get v1
    (name2, eff2) <- liftIO $ UF.get v2
    case (eff1, eff2) of
        (Nothing, _) -> do
            liftIO $ UF.union v1 v2 (name2, eff2)
            return CTrue
        (_,Nothing) -> do
            liftIO $ UF.union v1 v2 (name1, eff1)
            return CTrue
        (Just e1, Just e2) -> do
            liftIO $ UF.union v1 v2 (name1, Just e1)
            return (e1 ==== e2)


unifyTypes :: (ConstrainM m) => TypeEffect -> TypeEffect -> m Constraint
unifyTypes (t1 :@ v1) (t2 :@ v2) = do
    constr1 <- unifyEffects v1 v2
    constr2 <- case (t1, t2) of
        ((TypeVar v1), _) -> return CTrue
        (_, (TypeVar v1)) -> return CTrue
        ((Alias t1 t2 t3 t4), _) -> return CTrue --TODO alias unify
        ((App t1 t2 t3), (App t1' t2' t3')) ->
            CAnd <$> zipWithM  unifyTypes t3 t3'
        ((Fun t1 t2), (Fun t1' t2')) -> do
            c1 <- unifyTypes t1 t1'
            c2 <- unifyTypes t2 t2'
            return (c1 /\ c2)
        (EmptyRecord, EmptyRecord) -> return CTrue
        ((Record fields), (Record fields')) ->
            CAnd <$>( forM (Map.toList fields) $ \(key, ty1) -> do
                let mTy2 = Map.lookup key fields'
                case mTy2 of
                    Nothing -> return CTrue
                    Just ty2 -> unifyTypes ty1 ty2 )
        (Unit, Unit) -> return CTrue
        ((Tuple t1 t2 mt3), (Tuple t1' t2' mt3')) -> do
            c1 <- unifyTypes t1 t1'
            c2 <- unifyTypes t2 t2'
            c3 <- case (mt3, mt3') of
                (Nothing, Nothing) -> return CTrue
                (Just t3, Just t3') -> unifyTypes t3 t3'
            return (c1 /\ c2 /\ c3)
        _ -> error $ "Can't unify types " ++ (show t1) ++ " and " ++ (show t2 )
    return (constr1 /\ constr2)


type ConstrainM m = (State.MonadState (IORef Int) m, MonadIO m, MonadWriter Safety m, MonadError PatError.Error m )

-- eitherToPatError :: (ConstrainM m) => m (Either String a) -> m a
-- eitherToPatError comp = do
--     eitherVal <- comp
--     case eitherVal of
--         Right x -> return x
--         Left s -> error s
--         --PatError.Incomplete _ PatError.BadArg [Pattern] 

unpackEither :: (ConstrainM m ) => m (Either PatError.Error b) -> m b
unpackEither ec = do
    e <- ec
    case e of
        Left s -> throwError s
        Right b -> return b

newtype ConstrainMonad a = CM {
    runCM :: ExceptT PatError.Error  (WriterT Safety (StateT (IORef Int) IO)) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadWriter Safety, MonadError PatError.Error, State.MonadState (IORef Int))

runCMIO :: IORef Int -> ConstrainMonad a ->  IO (Either PatError.Error (a, Safety))
runCMIO ioref c = do
    (maybeResult, safety) <- flip State.evalStateT ioref $  runWriterT $ runExceptT $ runCM c
    return $ do
        result <- maybeResult
        return (result, safety)

tellSafety branches pathConstr x r context pats =
  let 
    patErrors = PatError.checkCases r branches []
  in unless (  null $ patErrors) $ 
    tell $ Safety [(pathConstr ==> x, (r, context, pats))]

--Given a type and an  effect-variable for each expression,
-- a mapping (Gamma) from free variables to effect schemes,
--A "path constraint" of truths that must hold for us to reach this point in the program,
-- and an expression,
--Traverse that expression and generate the constraints that, when solved, 
-- give the possible patterns for each effect variable.
-- Emits "safety constraints" using the Writer monad
constrainExpr :: (ConstrainM m) => Map.Map R.Region Can.Type -> (Gamma, Constraint) -> Can.Expr ->   m (TypeEffect,  Constraint)
constrainExpr tyMap _GammaPath (A.At region expr)  = 
    -- logIO ("Constraining exprssion " ++ show expr)
    case Map.lookup region tyMap of
        Just rawty -> do
            ty <- addEffectVars rawty
            c <- constrainExpr_ expr ty _GammaPath
            logIO ("Constrained exprssion " ++ show expr ++ "  to " ++ show ty ++ "  WITH CONSTR " ++ show c)
            return (ty, c)
        Nothing -> error "Region not in type map"
  where
    self :: (ConstrainM m) => (Gamma, Constraint) -> Can.Expr ->   m (TypeEffect,  Constraint)
    self  = constrainExpr tyMap
    constrainExpr_ ::  (ConstrainM m) => Can.Expr_ -> TypeEffect -> (Gamma, Constraint) -> m Constraint
    constrainExpr_ (Can.VarLocal name) t (_Gamma, pathConstr) =
        case Map.lookup name _Gamma of
            Nothing ->  do
                logIO $ "VAR NOT FOUND: " ++ N.toString name ++ "  at  " ++ show region
                return (deepUnifyTop t ) --Might hit this for record variables
                        -- error $ "constrainExpr: name " ++ (show name) ++ "at region " ++ (show region) ++ " not found in " ++ (show _Gamma)
            (Just sigma) -> do
                (tipe, constr) <- instantiate sigma
                logIO $ "Instantiating " ++ N.toString name ++ "  type  " ++ (show sigma) ++ " into " ++ (show (tipe, constr)) ++ " for var " ++ (N.toString name)
                logIO $ "Unifying types" ++ (show t) ++ "\n  and " ++ show tipe
                unifyTypes t tipe
                return constr
    constrainExpr_ e@(Can.VarTopLevel _ name) t (_Gamma, pathConstr) = --TODO what about other modules?
        case Map.lookup name _Gamma  of
            Nothing -> do
                logIO ("TOP for Setting type for " ++ (show e) ++ " imported, var " ++ show t)
                return $ deepUnifyTop t
            Just sigma -> do
                --TODO reduce duplication
                (tipe, constr) <- instantiate sigma
                logIO $ "Instantiating " ++ N.toString name ++ "  type  " ++ (show sigma) ++ " into " ++ (show (tipe, constr)) ++ " for var " ++ (N.toString name)
                logIO $ "Unifying types" ++ (show t) ++ "\n  and " ++ show tipe
                unifyTypes t tipe
                return constr
    constrainExpr_ (Can.VarCtor _ _ ctorName _ _) t _GammaPath =
      --Traverse the simple type of the constructor until it's not an arrow type
      --Collect the argument patterns, then apply the Ctor name to those patterns
      return $ ctorLoop t [] CTrue
        where
            ctorLoop ((Fun dom cod) :@ v3) reverseAccum condAccum = ctorLoop cod (dom : reverseAccum) (condAccum /\ (v3 ==== litLambda))
            ctorLoop (_ :@ ctorVar ) reverseAccum condAccum = condAccum /\ (ctorVar ==== (Ctor (N.toString ctorName) (map toLit $ reverse reverseAccum)) )
    constrainExpr_ e@(Can.VarOperator _ _ _ _) t _GammaPath = do
        logIO ("TOP for var operator " ++ show e ++ ", var " ++ show t)
        return $ deepUnifyTop t
    constrainExpr_ e@(Can.Binop name _ _ _ e1 e2) t _GammaPath = do
        (ty1, constr1) <- self _GammaPath e1
        (ty2, constr2) <- self _GammaPath e2
        opConstr <- case N.toString name of
            "::" -> return $ t ==== litCons ty1 ty2
            _ -> do
                logIO ("TOP for binary operator " ++ show e ++ ", var " ++ show t)
                return (deepUnifyTop t)
        return $ constr1 /\ constr2 /\ opConstr --TODO built-in types for operators  
    constrainExpr_ (Can.Case discr@(A.At dloc _) branches) resultType (_Gamma, pathConstr) = do
        (inputPatterns, inputConstrs) <- self (_Gamma, pathConstr) discr

        let
            discrType =
                case Map.lookup dloc tyMap  of
                    Just tipe -> tipe
                    Nothing -> Can.TUnit
            getBranchPat (Can.CaseBranch pat _) = pat
            patPairs = concatMap (\(Can.CaseBranch pat _) -> unionTypePairs 1 pat) branches
            (depths, pairs) = unzip patPairs
            maxDepth = maximum (0 : depths)
            unionMap = Map.fromList pairs
            unionTypes =
                Maybe.catMaybes $ (flip map) branches ( \(Can.CaseBranch pat _) ->
                        case pat of
                            A.At _ (Can.PCtor _ _ union _ _ _) -> Just union
                            _ -> Nothing )
            theUnionType =
                case unionTypes of
                    [] -> Nothing
                    (h:_) -> Just h
        logIO $ "CASE: Union types " ++ show (map (\(Can.CaseBranch pat _) -> pat) branches )
        logIO $ "Found unions num " ++ show (length unionTypes)
        logIO $ "Max depth " ++ show maxDepth ++ " with pair map " ++ show unionMap ++ "for patterns " ++ (show $ map getBranchPat branches) ++ "\n"

        --TODO negate previous branches
        let litBranches = map (\ (Can.CaseBranch pat rhs) -> (pat, canPatToLit pat, rhs) ) branches
        --Emit a safety constraint: must cover all possible inputs by our branch patterns
        let safetyRHS = unions (map (\(_,b,_) -> b) litBranches)
        --We only need to cover patterns that are possible for the given datatypes
        let theSafetyConstr =
                case theUnionType of
                    Nothing -> CTrue -- inputPatterns << safetyRHS 
                    Just u -> (inputPatterns `intersect` unionToLitPattern maxDepth unionMap u discrType) << (safetyRHS )
        logIO $ "Case branch telling safety " ++ show theSafetyConstr
        tellSafety branches pathConstr theSafetyConstr region PatError.BadCase (map (\(a,_,_)->a) litBranches)
        branchConstrs <-
            forM litBranches (
                \(pat, lit, rhs) -> do
                    -- v <- freshVar
                    let canBeInBranch = CNonEmpty ( inputPatterns `intersect` lit) 
                    (newEnv, newEnvConstr) <- envAfterCaseMatch tyMap inputPatterns pat
                    let newPathConstr = canBeInBranch /\ pathConstr
                    (rhsTy, rhsConstrs) <- self (Map.union newEnv _Gamma, newPathConstr) rhs
                    --If this branch is reachable, then we emit a constraint saying
                    --That the overall result of the case expression contains the result of this branch
                    --TODO make paper match this
                    logIO $ "New env constraint: " ++ show newEnvConstr
                    logIO $ "rshConstrs: " ++ show rhsConstrs
                    logIO $ "CanBeInBranchResult: " ++ show (canBeInBranch ==> ( rhsTy << resultType)) 
                    return $
                        newEnvConstr /\ rhsConstrs /\ (canBeInBranch ==> ( rhsTy << resultType)))
        return (inputConstrs  /\ CAnd branchConstrs)
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
                    tellSafety [Can.CaseBranch argPat body] pathConstr (dom << litPat) region PatError.BadArg [argPat]
                    --All values of function types must be lambdas, so we have a trivial constraint on v3
                    let lamConstr = (v3 ==== litLambda)
                    --Get the environment to check the body
                    (newEnv) <- envAfterDefMatch tyMap dom argPat
                    --Check the body --TODO path constr?
                    bodyConstr <- lamHelper argPats cod (Map.union newEnv _Gamma, pathConstr)
                    return $ bodyConstr /\ lamConstr
    constrainExpr_ (Can.Call fun args) retEffect _GammaPath@(_Gamma, pathConstr) = do
         (funTy, funConstr) <- self _GammaPath fun
         (argTEs, argConstrs) <- unzip <$> mapM (self _GammaPath) args
         ret <- argHelper funTy argTEs ( funConstr /\ CAnd argConstrs)
         logIO ("Function call " ++ show fun ++ "\n    generates constr " ++ show ret)
         return ret
            where
                --Loop to compute the return type
                --At each application, instantiate the function's argument type to be the type of the given argument
                --Finally, make sure the patterns of the whole expression is the pattern the function returns
                argHelper funEffect [] accum = do
                    let retConstr = (funEffect << retEffect) /\ accum
                    -- logIO $ "Constraining that returnTy " 
                    return $  retConstr
                argHelper (Fun dom cod :@ _) (argTy : rest) accum = do
                    --Domain effects only show up in safety constraints
                    --So unifying the argument type with the domain just asserts that the safety constraints apply to the argument 
                    logIO $ "Constraining that argTy " ++ (show argTy) ++ ("is " ++ show dom)
                    unifyTypes argTy  dom 
                    argHelper cod rest accum
                argHelper fun tyList _ = error $ "Bad fun ty combo " ++ (show fun) ++ "\nAND\n" ++ (show tyList)
    constrainExpr_ (Can.If conds elseExpr) t _GammaPath = do
        (elseType, elseCond) <- self _GammaPath elseExpr
        (branchTypes, branchConds) <-
            unzip <$>
                forM conds (
                    \ (ifExp, thenExp) -> do
                        retType <- freshVar
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
        envExt <- constrainDef tyMap _GammaPath def
        (bodyType, bodyConstr) <- self (Map.union envExt _Gamma, pathConstr) inExpr
        unifyTypes bodyType t
        return bodyConstr
    constrainExpr_ (Can.LetRec defs inExpr)  t _GammaPath@(_Gamma, pathConstr) = do
        newGamma <- constrainRecursiveDefs tyMap _Gamma defs
        (inTy, constr) <- self (Map.union newGamma _Gamma, pathConstr) inExpr
        unifyConstr <- unifyTypes t inTy
        return $ constr /\ unifyConstr
    constrainExpr_ (Can.LetDestruct pat letExp inExp) t _GammaPath@(_Gamma, pathConstr) = do
        --TODO need to generalize?
        let lit = canPatToLit pat
        --Can't have a recursive let on a pattern-match, since won't be behind a lambda
        --TODO is this right?
        (letType, letConstr) <- self _GammaPath letExp
        --Safety constraint: must match whatever we are binding
        tellSafety [Can.CaseBranch pat inExp] pathConstr (letType << lit) region PatError.BadCase [pat]
        (envExt) <- envAfterDefMatch tyMap letType pat
        --TODO extend path cond
        (bodyType, bodyConstr) <- self (Map.union envExt _Gamma, pathConstr) inExp
        --Whole expression has type of body
        unifyTypes bodyType t
        return $ letConstr /\ bodyConstr
    constrainExpr_ (Can.Accessor expr) t _GammaPath = return $ deepUnifyTop t
    constrainExpr_ (Can.Access expr1 expr2) t _GammaPath = return $ deepUnifyTop t
    constrainExpr_ (Can.Update expr1 expr2 expr3) t _GammaPath = return $ deepUnifyTop t
    constrainExpr_ (Can.Record expr) t _GammaPath = return $ deepUnifyTop t
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
    constrainExpr_ (Can.Negate expr) t _GammaPath = return (deepUnifyTop t) --TODO something smart for negation
    constrainExpr_ e@(Can.VarKernel expr1 expr2) t _GammaPath = do
        logIO ("TOP for varKernel " ++ show e ++ ", var " ++ show t)
        return (deepUnifyTop t)
    constrainExpr_ (Can.VarForeign expr1 expr2 expr3) t _GammaPath = return (deepUnifyTop t)
    constrainExpr_ (Can.VarDebug expr1 expr2 expr3) t _GammaPath = return (deepUnifyTop t)
    constrainExpr_ Can.Unit (_:@v) _GammaPath = return $ litUnit ==== v
    constrainExpr_ (Can.Shader expr1 expr2 expr3) t _GammaPath = return (deepUnifyTop t )
    -- constrainExpr_ e t _ = error $ "Impossible type-expr combo " ++ (show e) ++ "  at type " ++ (show t)

defName :: Can.Def -> String
defName (Can.Def (A.At _ n) _ _ ) = N.toString n
defName (Can.TypedDef (A.At _ n) _ _ _ _ ) = N.toString n

constrainDef :: (ConstrainM m) => Map.Map R.Region Can.Type -> (Gamma, Constraint) -> Can.Def ->   m Gamma
constrainDef tyMap _GammaPath def = do
    liftIO $ putStrLn $ "Constraining definition " ++ defName def
    let unrolled =  unrollDef unrollLevel def
    logIO $ if (show unrolled /= show def) then  ("Unrolled def " ++ show def ++ "       into       " ++ show unrolled) else ""
    constrainDefUnrolled tyMap _GammaPath unrolled


unrollDef :: Int -> Can.Def -> Can.Def
unrollDef 0 d = d
unrollDef n1 d = unrollDef (n1-1) (unrollOneLevel d)

unrollOneLevel :: Can.Def -> Can.Def
unrollOneLevel (Can.Def x args body@(A.At region _)) =
  let bodyLam r = A.At r $ Can.Lambda args body
  in (Can.Def x args (esub bodyLam x body))
unrollOneLevel (Can.TypedDef x pats patTypes body@(A.At region _) retType) =
  let bodyLam r = A.At r $ Can.Lambda (map fst patTypes) body
  in (Can.TypedDef x pats patTypes (esub bodyLam x body) retType)

esub :: (R.Region -> Can.Expr) -> A.Located N.Name -> Can.Expr -> Can.Expr
esub body var@(A.At _ x) (A.At region (Can.VarLocal y)) | y == x  = body region
esub body var@(A.At _ x) etop@(A.At region e) = A.At region $ esub_  e
  where
    esub_ :: Can.Expr_ -> Can.Expr_
    esub_ e =
        let self = esub body var
        in case e of
            (Can.List es) -> Can.List (map self es)
            (Can.Negate e) -> Can.Negate $ self e
            (Can.Binop e1 e2 e3 e4 e5 e6) -> Can.Binop e1 e2 e3 e4 (self e5) (self e6)
            (Can.Lambda args e2) ->
                if elem x (concatMap patternFreeVars args)
                    then e
                    else Can.Lambda args (self e2)
            (Can.Call e1 e2) -> Can.Call (self e1) (map self e2)
            (Can.If e1 e2) -> Can.If (map (bimap self self) e1) (self e2)
            (Can.Let d e2) -> Can.Let (dsub body var d) (if x `elem` (defBoundVars d) then e2 else self e2)
            (Can.LetRec ds e2) -> if x `elem` (concatMap defBoundVars ds) then e else Can.LetRec (map (dsub body var) ds) (self e2)
            (Can.LetDestruct e1 e2 e3) ->
                Can.LetDestruct e1 (self e2) $ if x `elem` (patternFreeVars e1) then e3 else (self e3)
            (Can.Case e1 branches) -> Can.Case (self e1) $ (flip map) branches $ \br@(Can.CaseBranch pat rhs) ->
                (if x `elem` (patternFreeVars pat) then br else Can.CaseBranch pat (self rhs))
            (Can.Access e1 e2) -> Can.Access (self e1) e2
            (Can.Update e1 e2 e3) -> Can.Update e1 (self e2) e3
            (Can.Record e) -> Can.Record $ Map.map self e
            (Can.Tuple e1 e2 e3) -> Can.Tuple (self e1) (self e2) (self <$> e3)
            _ -> e

dsub :: (R.Region -> Can.Expr) -> A.Located N.Name -> Can.Def -> Can.Def
dsub e var (Can.Def d1 d2 d3) = Can.Def d1 d2 (esub e var d3)
dsub e var (Can.TypedDef d1 d2 d3 d4 d5) = Can.TypedDef d1 d2 d3 (esub e var d4) d5

--Takes place in the IO monad, not our ConstrainM
--Because we want to generate a separate set of safety constraints for this definition
--TODO check all this
constrainDefUnrolled :: (ConstrainM m) => Map.Map R.Region Can.Type -> (Gamma, Constraint) -> Can.Def ->   m Gamma
constrainDefUnrolled tyMap _GammaPath@(_Gamma, pathConstr) def = do
    theRef <- State.get
    (x, defType, defConstr, theSafety) <- case def of
        --Get the type of the body, and add it into the environment as a monoscheme
        --To start our argument-processing loop
        (Can.Def (A.At wholeRegion x) funArgs body) -> do
            wholeType <-
                    case Map.lookup wholeRegion tyMap of
                        Nothing -> error $ "constrainDef: Can't find region for " ++ show x ++ " region " ++ (show wholeRegion) ++ " in type map " ++ (show tyMap)
                        Just s-> addEffectVars s
            --We run in a separaelm te instance, so we get different safety constraints
            --TODO need this?
            (exprConstr, safety) <- unpackEither $ liftIO $ runCMIO theRef $ constrainDef_ x funArgs body wholeType (Map.insert x (monoschemeVar wholeType) _Gamma)
            return (x, wholeType, exprConstr, safety)
        (Can.TypedDef (A.At wholeRegion x) _ patTypes body retTipe) -> do
            retTyEff <- addEffectVars retTipe
            argTyEffs <- mapM (addEffectVars . snd) patTypes
            freshVars <- forM patTypes $ \ _ -> SetVar <$> freshVar --These are the type annots for each function, i.e. unused
            let wholeType = foldr (\ (arg, var) ret -> (Fun arg ret) :@ var) retTyEff (zip argTyEffs freshVars)
            -- forM (zip argTyEffs freshVars)  $ \(argTyEff, freshVarEff) -> unifyTypes argTyEff freshVarEff
            -- let wholeType = 
            --         case Map.lookup wholeRegion tyMap of
            --             Nothing -> error $ "constrainDef typed: Can't find region " ++ (show wholeRegion) ++ " in type map " ++ (show tyMap)
            --             Just s-> s 
            --We run in a separate instance, so we get different safety constraints
            --TODO need this?
            (exprConstr, safety)  <- unpackEither $ liftIO $ runCMIO theRef $ constrainDef_ x (map fst patTypes) body wholeType (Map.insert x (monoschemeVar wholeType) _Gamma)
            return (x, wholeType, exprConstr , safety)
    --We check that each safety constraint in this definition is compatible with the other constraints
    let safetyList = unSafety theSafety
    logIO $  "Solving constraints for definition " ++ N.toString  x
    logIO $ N.toString  x ++ ": Got regular constraints " ++ (show $ defConstr)
    logIO $ N.toString  x ++ ": Got safety constraints " ++ (show $ getSafetyConstrs theSafety)
    logIO $ N.toString  x ++ ": Got unopt type " ++ (show $ defType)
    -- (optimizedDefType , optimizedDefConstr, optimizedDefSafety) <- optimizeConstr defType (defConstr )
    logIO $ "Getting optimized constraints for scheme"
    (optType , optConstrList, optSafety) <- optimizeConstr True defType defConstr theSafety
    logIO $ N.toString  x ++ ": Got optimized safety constraints " ++ (show $ getSafetyConstrs optSafety)
    logIO $ N.toString  x ++ ": Got optimized regular constraints " ++ (show $ optConstrList)
    logIO $ N.toString  x ++ ": Got optimized type " ++ (show $ optType)
    -- --We do one last optimization pass where we don't care about the type
    -- --This is the constraint we pass to the SMT solver, but NOT the one we generalize for our type scheme
    -- (_ , toSolveConstr, toSolveSafety) <- optimizeConstr False (Unit :@ Top) optConstr optSafety
    -- mTestSoln <- solveConstraint optimizedDefConstr
    -- case mTestSoln of
    --     Right () -> return ()
    --     Left _ -> error $ "ERROR: Constraint unsatisfiable without safety\n  " ++ show optimizedDefConstr
    --We only both passing to Z3 when we actually have some safety constraints to check
    case ( unSafety optSafety) of
        [] -> logIO "Skipping because no safety constraints" 
        _ -> do 
            --Optimize away the constraints that don't have to do with the safety constraints
            let theSafetyList = getSafetyConstrs optSafety
            let theSafetyConstr = CAnd theSafetyList
            let safetyFreeVars =  constrFreeVars theSafetyConstr
            --A fake type containiing all free variables in the safety constraints
            --This makes sure we don't get rid of relevant variables for the safety
            let fakeType = foldr (\var ty -> (Fun (Unit :@ (SetVar var)) ty) :@ Top) (Unit :@ Top) safetyFreeVars 
            (_, relevantConstraints, _) <- optimizeConstr_ True fakeType optConstrList (unSafety optSafety) 
            let relevantConstraint = CAnd relevantConstraints
            mConstraintSoln <- solveConstraint ( theSafetyConstr /\ relevantConstraint)
            case mConstraintSoln of
                Right () -> return ()
                Left _ -> do
                    -- error "Pattern match failure"
                    logIO $ "Pattern match failure!\nSafety:  " ++ show theSafetyList 
                    failures <- forM (unSafety optSafety) $ \(safetyConstr, (region, context, pats)) -> do
                        soln <- solveConstraint (safetyConstr /\ relevantConstraint )
                        case soln of
                            Right _ -> do
                                logIO $ "Success"
                                return $ Nothing
                            Left _ -> do
                                logIO  $ "SAFETY:  failed with " ++ show safetyConstr -- ++ "\ndefConstr: " ++ show defConstr
                                return $ Just (region, context, pats) --TODO get unmatched patterns
                    case Maybe.catMaybes failures of
                        [] -> error "Incompatible safety constraint set"
                        l -> forM_ l $ \(region, context, pats) ->
                            throwError $ PatError.Incomplete region context (map PatError.simplify pats )
                    return ()
            --Iterate through each safety constraint, and see which one is satisfiable

    --Now that we have types and constraints for the body, we generalize them over all free variables
    --not  occurring in Gamma, and return an environment mapping the def name to this scheme
    scheme <- generalize (fst _GammaPath) optType (CAnd optConstrList) optSafety
    logIO $ "Generalized type for " ++ N.toString x ++ " is " ++ (show scheme)
    return $ Map.singleton x scheme
    where
        constrainDef_ x  (argPat : argList) body ((Fun dom cod) :@ vFun) _Gamma = do
            --Add the argument pattern vars to the dictionary, then check the rest at the codomain type
            (envExt) <- (envAfterDefMatch tyMap dom argPat)
            retConstr <- constrainDef_ x argList body cod (Map.union envExt  _Gamma)
            return (retConstr)
        constrainDef_ x  [] body exprType _Gamma = do
            logIO $ "DEF START CONSTRAIN for " ++ N.toString x ++ "  with Gamma  " ++ show _Gamma
            (bodyType, bodyConstr) <- constrainExpr tyMap (_Gamma, pathConstr) body
            logIO $ "CONSTRAINDEF: Unifying inferred type " ++ show bodyType ++ "  with annotation type " ++ show exprType
            unifyTypes bodyType exprType
            --Now that we have the type and constraints for our definition body
            --We can generalize it into a type scheme and return a new environment 
            --TODO run constraint solver at this point
            return bodyConstr
        constrainDef_ x argList body t _Gamma = error ("Got bad type " ++ show t ++ " for def num args " ++ show (length argList) )

--Convert to our pattern format
--Used to generate safety constraints for pattern matches   
canPatToLit ::  Can.Pattern -> LitPattern
canPatToLit  (A.At info pat) =
    case pat of
        Can.PAnything -> Top
        (Can.PVar x) -> Top
        (Can.PRecord p) -> Top
        (Can.PAlias p1 p2) -> Top
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

patternFreeVars ::  Can.Pattern -> [N.Name]
patternFreeVars  (A.At info pat) =
    case pat of
        (Can.PVar x) -> [x]
        (Can.PRecord p) -> p
        (Can.PAlias p1 p2) -> p2 : patternFreeVars p1
        (Can.PTuple p1 p2 (Just p3)) -> (patternFreeVars p1) ++ (patternFreeVars p2) ++ (patternFreeVars p3)
        (Can.PTuple p1 p2 Nothing) -> (patternFreeVars p1) ++ (patternFreeVars p2)
        (Can.PList plist) -> (concatMap patternFreeVars plist)
        (Can.PCons p1 p2) -> (patternFreeVars p1) ++ (patternFreeVars p2)
        Can.PCtor { Can._p_name = ctorName, Can._p_args = ctorArgs } ->  concatMap (patternFreeVars . Can._arg) ctorArgs
        _ -> []

defBoundVars :: Can.Def -> [N.Name]
defBoundVars (Can.Def (A.At _ x) argPats _) = x:(concatMap patternFreeVars argPats)
defBoundVars (Can.TypedDef (A.At _ x) _ argPatTypes _ _) =
    let
        argPats = map fst argPatTypes
    in x:(concatMap patternFreeVars argPats)

unionTypePairs ::  Int -> Can.Pattern -> [(Int, (N.Name,Can.Union) )]
unionTypePairs depth (A.At info pat) =
    case pat of
        Can.PCtor { Can._p_type = typeName, Can._p_union = u, Can._p_args = ctorArgs } ->
            (depth, (typeName, u)) : concatMap (unionTypePairs (depth + 1)) ( map Can._arg ctorArgs)
        (Can.PVar x) -> []
        (Can.PRecord p) -> []
        (Can.PAlias p1 p2) -> unionTypePairs (depth+1) p1
        (Can.PTuple p1 p2 (Just p3)) -> (unionTypePairs (depth + 1) p1) ++ (unionTypePairs (depth + 1) p2) ++ (unionTypePairs (depth + 1) p3)
        (Can.PTuple p1 p2 Nothing) -> (unionTypePairs (depth + 1) p1) ++ (unionTypePairs (depth + 1) p2)
        (Can.PList plist) -> (concatMap (unionTypePairs (depth + 1)) plist)
        (Can.PCons p1 p2) -> (unionTypePairs (depth + 1) p1) ++ (unionTypePairs (depth + 1) p2)
        _ -> []

tsub :: Map.Map N.Name Can.Type -> Can.Type -> Can.Type
tsub varMap t =
    let
        self = tsub varMap
    in case t of
        (Can.TLambda t1 t2) -> Can.TLambda (self t1) (self t2)
        (Can.TVar n) -> case Map.lookup n varMap of
            Just tnew -> tnew
            _ -> t
        (Can.TType t1 t2 t3) -> Can.TType t1 t2 (map self t3)
        (Can.TRecord t1 t2) -> t --TODO  
        Can.TUnit -> t
        (Can.TTuple t1 t2 t3) -> Can.TTuple (self t1) (self t2) (self <$> t3)
        (Can.TAlias t1 t2 t3 t4) -> t --TODO  

topForType :: Int ->  Map.Map N.Name Can.Union -> Can.Type -> LitPattern
topForType fuel unionMap tipe@(Can.TType _ name _) = case Map.lookup name unionMap of
    Nothing ->  Top
    Just u -> unionToLitPattern fuel unionMap u tipe
topForType _ _ t =   Top

unionToLitPattern :: Int -> Map.Map N.Name Can.Union -> Can.Union -> Can.Type -> LitPattern
unionToLitPattern fuel unionMap u (Can.TType _ _ targs )  | fuel >= 0 =
    let
        typeMap :: Map.Map N.Name Can.Type
        typeMap = Map.fromList $ zip (Can._u_vars u) targs
        componentForCtor (Can.Ctor n _ _ argTypes ) = Ctor (N.toString n) (map (topForType (fuel - 1) unionMap) $ map (tsub typeMap) argTypes)
    in unions $ map componentForCtor (Can._u_alts u)
unionToLitPattern _ _ _ _ = Top


-- getFullProjections :: (ConstrainM m) => String -> Arity -> LitPattern -> m (Constraint, [LitPattern])
-- getFullProjections name arity pat = do
--     projName <- freshName "projVar"
--     let argnums = [1 .. getArity arity]
--     projPatterns <- forM argnums $ \i -> SetVar <$> varNamed (projName ++ "_" ++ show i)
--     let partMatchingCtor = pat `intersect` (Ctor name (replicate (getArity arity) Top))
--     let varsMatchProj = (Ctor name projPatterns) ==== partMatchingCtor
--     let emptyIff = CAnd $ map (\proj -> (proj ==== Bottom) <==> (partMatchingCtor ==== Bottom)) projPatterns
--     return (varsMatchProj /\ emptyIff, projPatterns)



envAfterCaseMatch :: (ConstrainM m) => Map.Map R.Region Can.Type -> TypeEffect  -> Can.Pattern -> m (Gamma, Constraint)
envAfterCaseMatch tyMap topType pat = do
    (env, patType, shapeLit) <- envAfterMatchHelper tyMap pat
    --Constrain that everything in the discrinimee's effect matching the pattern
    --Is contained in the pattern that contains the projection variables 
    return (env, (shapeLit `intersect` topType) << patType)

envAfterDefMatch :: (ConstrainM m) => Map.Map R.Region Can.Type -> TypeEffect  -> Can.Pattern -> m (Gamma)
envAfterDefMatch tyMap topType pat = do
    (env, patType, shapeLit) <- envAfterMatchHelper tyMap pat
    --Constrain the type of the defined pattern is the same as the type put in the environment
    unifyTypes patType topType
    return env

--Generate fresh type-effect variables for each pattern variable
--And combine the patterns into the LitPattern and Env for the entire matched pattern
--envAfterMatch takes this and emits the constraint that it contains at least the portion of the input matching the pattern
--This is a conservative approximation  
--Also returns the "shape" of the pattern
--That we can use to get the matched parts out of the discriminee
envAfterMatchHelper :: (ConstrainM m) => Map.Map R.Region Can.Type   -> Can.Pattern -> m (Gamma, TypeEffect, LitPattern)
envAfterMatchHelper tyMap  (A.At region pat)  = do

    let ctorProjectionEnvs ourType nameString ctorArgPats = do
            let arity = Arity (length ctorArgPats)
            (subDicts, subEffects, subShapes) <- unzip3 <$> mapM  ((envAfterMatchHelper tyMap) )  ctorArgPats
            unifyEffects (toLit ourType) $ Ctor nameString (map toLit subEffects)
            return $ (Map.unions subDicts, ourType, Ctor nameString subShapes) --TODO put back 
    ourType <-
        case Map.lookup region tyMap of
            Nothing -> error $ "envAfterMatch: Can't find region " ++ (show region) ++ " in type map " ++ (show tyMap)
            Just s-> addEffectVars s
    case pat of
        --Variable: we return the input type, with its 
        Can.PVar x -> 
            return $ (Map.singleton x (monoschemeVar ourType), ourType, Top)
        Can.PCtor { Can._p_name = ctorName, Can._p_args = ctorArgs } ->
            ctorProjectionEnvs ourType  (N.toString ctorName) $ map Can._arg ctorArgs
        (Can.PTuple p1 p2 (Just p3)) -> ctorProjectionEnvs ourType ctorTriple [p1, p2, p3]
        (Can.PTuple p1 p2 Nothing) -> ctorProjectionEnvs ourType ctorPair [p1, p2]
        --For lists, we get split into head and tail and handle recursively
        (Can.PList (p1 : pList)) -> ctorProjectionEnvs ourType  ctorCons [p1, A.At region (Can.PList pList)]
        (Can.PCons p1 p2) -> ctorProjectionEnvs ourType ctorCons [p1, p2]
        (Can.PList []) -> ctorProjectionEnvs ourType ctorNull []
        _ -> do
            doDeepUnifyTop ourType
            return $ (Map.empty, ourType, Top)






--Helpers for generating literal patterns from builtin data types
ctorLam = "PatMatchLambda"
litLambda = Ctor ctorLam []
ctorUnit = "PatMatchUnit"
litUnit = Ctor ctorUnit []

ctorPair = "PatMatchPair"
litPair l1 l2 = Ctor ctorPair  [(toLit l1), (toLit l2)]
ctorTriple = "PatMatchTriple"
litTriple l1 l2 l3 = Ctor ctorTriple  (map toLit [l1, l2, l3])

litList l = case l of
    [] -> litNull
    (h : t) -> litCons h (litList t)
ctorNull = "PatMatchNull"
ctorCons = "PatMatchCons"
litNull = Ctor ctorNull []
litCons h t = Ctor ctorCons [toLit h, toLit t]

ctorTrue = "PatMatchTrue"
litTrue = Ctor ctorTrue []
ctorFalse = "PatMatchFalse"
litFalse = Ctor ctorFalse []
ctorChar c = "PatMatchCHAR_" ++ unpack c
litChar c = Ctor (ctorChar c) []
ctorString s = "PatMatchSTRING_" ++ (filter isAlphaNum $ show s)
litString s = Ctor (ctorString s) []

-- ctorZero = "PatMatchZERO"
-- ctorPos = "PatMatchPOSITIVE"
-- ctorNeg = "PatMatchNEGATIVE"
-- ctorSucc = "PatMatchSUCC"
litInt i = Ctor ("PatMatch_" ++ show i) []
-- litInt i = case i of
--     0 -> Ctor ctorZero []
--     _ | i < 0 -> Ctor ctorPos [litNat (-i)]
--     _ | i > 0 -> Ctor ctorNeg [litNat i]

-- litNat n = case n of
--     0 -> Ctor ctorZero []
--     i | i > 0 -> Ctor ctorSucc [litNat (i-1)]

constrainRecursiveDefs :: (ConstrainM m) => Map.Map R.Region Can.Type -> Gamma -> [Can.Def] -> m Gamma
constrainRecursiveDefs tyMap _Gamma defs = do
    let ourType region =
            case Map.lookup region tyMap of
                Nothing -> error $ "constrainRecursiveDefs: Can't find region " ++ (show region) ++ " in type map " ++ (show tyMap)
                Just s-> addEffectVars s
    defTypes <- forM defs $ \ def ->
        case def of
            (Can.Def (A.At region name) def2 def3) -> do
                ourActualType <- (ourType region)
                return (name, monoschemeVar ourActualType)
            (Can.TypedDef (A.At wholeRegion name) _ patTypes body retTipe) -> do
                retTyEff <- addEffectVars retTipe
                argTyEffs <- mapM addEffectVars (map snd patTypes)
                freshVars <- forM patTypes $ \ _ -> SetVar <$> freshVar
                let wholeType = foldr (\ (arg, var) ret -> (Fun arg ret) :@ var) retTyEff (zip argTyEffs freshVars)
                return  (name, monoschemeVar wholeType)

    let exprs =
            map (\ def -> case def of
                Can.Def (A.At region _) _ body -> (A.At region body)
                Can.TypedDef (A.At region _) _ _ body _ -> (A.At region body))
    let _Gamma = Map.fromList defTypes
    constrs <- forM defs $ constrainDef tyMap (_Gamma, CTrue)
    return _Gamma

patternMatchAnalysis :: Can.Module -> Result.Result i w Reporting.Error.Error ()
patternMatchAnalysis modul = do
    let
        eitherResult =  unsafePerformIO $ do
            theRef <- newIORef 0
            runCMIO theRef $ do
                tyMapRaw <- liftIO $ readIORef Type.globalTypeMap
                tyMap <- liftIO $ mapM Type.storedToCanType tyMapRaw
                helper tyMap (Can._decls modul) Map.empty
    case eitherResult of
        Left patError -> Result.throw $ Reporting.Error.Pattern [patError]
        Right _ -> return ()
  where
    -- helper ::   _ -> _ -> _ -> ConstrainMonad   _ 
    helper tyMap (Can.Declare def decls)  _Gamma = do
        envExt <- constrainDef tyMap (_Gamma, CTrue) def
        helper tyMap decls (Map.union envExt _Gamma)
    helper tyMap (Can.DeclareRec defs decls)  _Gamma = do
        newGamma <- constrainRecursiveDefs tyMap _Gamma defs
        helper tyMap decls newGamma
    helper theTyMap Can.SaveTheEnvironment  _Gamma =
        return ()