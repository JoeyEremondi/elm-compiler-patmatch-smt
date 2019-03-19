{-# LANGUAGE ScopedTypeVariables #-}
module Type.PatternMatch where

import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

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
    
data TypeEffect = WithEffect TypeEffect_ Variable 

data LitPattern = 
    SetVar Variable
    | Ctor String [LitPattern]
    | Proj Int LitPattern 

data Constraint =
    CAnd [Constraint]
    | COr [Constraint]
    | CImplies Constraint Constraint
    | CSubset LitPattern LitPattern
    | CTrue

p1 === p2 = 
    CSubset p1 p2 /\ CSubset p2 p1

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
    return $ innerType `WithEffect` constraintVar

unifyTypes :: TypeEffect -> TypeEffect -> IO ()
unifyTypes (WithEffect _ v1) (WithEffect _ v2) =
    UF.union v1 v2 ()
    --TODO do we need to unify sub-vars?




constrainExpr :: Map.Map R.Region TypeEffect -> Can.Expr ->  Gamma -> IO Constraint 
constrainExpr tyMap (A.At region expr) _Gamma = 
    case Map.lookup region tyMap of
        Just ty -> constrainExpr_ expr ty _Gamma
        Nothing -> error "Region not in type map"
  where
    self = constrainExpr tyMap
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
    constrainExpr_ (Can.Binop expr1 expr2 expr3 expr4 expr5 expr6) t _Gamma = _ 
    constrainExpr_ (Can.Case expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.Lambda expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.Call expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.If expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.Let expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.LetRec expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.LetDestruct expr1 expr2 expr3) t _Gamma = _ 
    constrainExpr_ (Can.Accessor expr) t _Gamma = _ 
    constrainExpr_ (Can.Access expr1 expr2) t _Gamma = _ 
    constrainExpr_ (Can.Update expr1 expr2 expr3) t _Gamma = _ 
    constrainExpr_ (Can.Record expr) t _Gamma = _ 
    constrainExpr_ (Can.Tuple expr1 expr2 expr3) t _Gamma = _ 
    constrainExpr_ (Can.List expr) t _Gamma = _ --TODO fold over list type 
    constrainExpr_ (Can.Chr expr) t _Gamma = return CTrue
    constrainExpr_ (Can.Str expr) t _Gamma = return CTrue
    constrainExpr_ (Can.Int expr) t _Gamma =  return CTrue
    constrainExpr_ (Can.Float expr) t _Gamma = return CTrue
    constrainExpr_ (Can.Negate expr) t _Gamma = return CTrue --TODO ints?
    constrainExpr_ (Can.VarKernel expr1 expr2) t _Gamma = return CTrue 
    constrainExpr_ (Can.VarForeign expr1 expr2 expr3) t _Gamma = return CTrue
    constrainExpr_ (Can.VarDebug expr1 expr2 expr3) t _Gamma = return CTrue
    constrainExpr_ Can.Unit t _Gamma = return CTrue 
    constrainExpr_ (Can.Shader expr1 expr2 expr3) t _Gamma = return CTrue     