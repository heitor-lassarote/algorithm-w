module Language.Dummy.TypeInference
  ( TI, runTI
  , TIError (..), prError
  , TIState (..)
  , typeInference
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, runState, get, put)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

import Language.Dummy.AST (Exp (..), Lit (..), Scheme (..), Type (..), prType)

type Subst = Map Text Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Types a where
  -- | Determines all the free type variables of a type.
  ftv :: a -> Set Text
  -- | Applies substitutions over the type variables of a given type.
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar n)     = Set.singleton n
  ftv TInt         = Set.empty
  ftv TBool        = Set.empty
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2

  apply s t@(TVar n)   = fromMaybe t $ Map.lookup n s
  apply _ TInt         = TInt
  apply _ TBool        = TBool
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)

instance Types Scheme where
  ftv (Scheme vars t) = ftv t Set.\\ Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
  ftv = foldr Set.union Set.empty . map ftv
  apply s = map (apply s)

newtype TypeEnv = TypeEnv (Map Text Scheme)

-- | Removes the binding for the given term variable from the environment.
remove :: TypeEnv -> Text -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv $ Map.elems env
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

-- | Abstracts a type over all type variables which are free in the type, but
-- not in the given type environment.
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars :: [Text]
    vars = Set.toList $ ftv t Set.\\ ftv env

data TIError
  = TypesDoNotUnify Type Type
  | OccursCheckFail Text Type
  | UnboundVariable Text
  deriving stock (Eq, Show)

prError :: TIError -> Doc TIError
prError (TypesDoNotUnify t1 t2) =
  "Types do not unify:" <+> PP.unAnnotate (prType t1) <+> "vs." <+> PP.unAnnotate (prType t2)
prError (OccursCheckFail u t) =
  "Occurs check fails:" <+> PP.pretty u <+> "vs." <+> PP.unAnnotate (prType t)
prError (UnboundVariable n) =
  "Unbound variable:" <+> PP.pretty n

newtype TIState = TIState
  { supply :: Int
  }

-- | A monad for generating fresh variable names for the type inference and
-- passing dynamically scoped environments.
type TI a = ExceptT TIError (State TIState) a

runTI :: TI a -> (Either TIError a, TIState)
runTI = flip runState initTIState . runExceptT
  where
    initTIState = TIState
      { supply = 0
      }

newTyVar :: Text -> TI Type
newTyVar prefix = do
  s <- get
  put s{supply = supply s + 1}
  pure $ TVar $ prefix <> Text.pack (show s.supply)

-- | Replaces all bound type variables in a type scheme with fresh type
-- variables.
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- traverse (const $ newTyVar "a") vars
  let s = Map.fromList $ zip vars nvars
  pure $ apply s t

-- | Produces a substitution which is the most general unifier (MGU).
unify :: Type -> Type -> TI Subst
unify (TFun l r) (TFun l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  pure $ s1 `composeSubst` s2
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
unify TInt TInt = pure nullSubst
unify TBool TBool = pure nullSubst
unify t1 t2 = throwError $ TypesDoNotUnify t1 t2

-- | Attempts to bind a type variable to a type, returning the new binding as a
-- substitution. Note that this function avoids binding a variable to itself and
-- performs the occurs check.
--
-- Avoid binding a variable with itself: This will avoid creating excessive
-- substitutions. Substituting something by itself is a redundancy.
--
-- Occurs check: As an example, suppose that the type inference finds a
-- constraint `t ~ [t]`. We'd need to construct a type `[[t]]`, which leads to
-- `[[[t]]]`, and so on, or an infinite type. This could cause an infinite loop
-- in the type inference, and so this is forbidden.
varBind :: Text -> Type -> TI Subst
varBind u t
  | t == TVar u          = pure nullSubst
  | u `Set.member` ftv t = throwError $ OccursCheckFail u t
  | otherwise            = pure $ Map.singleton u t

tiLit :: TypeEnv -> Lit -> TI (Subst, Type)
tiLit _ (LInt _) = pure (nullSubst, TInt)
tiLit _ (LBool _) = pure (nullSubst, TBool)

ti :: TypeEnv -> Exp -> TI (Subst, Type)
-- x : σ ∈ Γ   τ = inst(σ)
-- -----------------------
--     Γ ⊢ x : τ, ∅
ti (TypeEnv env) (EVar n) = case Map.lookup n env of
  Nothing    -> throwError $ UnboundVariable n
  Just sigma -> (nullSubst, ) <$> instantiate sigma
-- ----------------   ---------------   -----------------------------
-- Γ ⊢ false : Bool   Γ ⊢ true : Bool   Γ ⊢ ...,-2,-1,0,1,2,... : Int
ti env (ELit l) = tiLit env l
-- τ = newvar    Γ, x : τ ⊢ e : τ', S
-- ----------------------------------
--      Γ ⊢ λx. e : Sτ → τ', S
ti env (EAbs n e) = do
  tv <- newTyVar "a"
  let
    TypeEnv env' = remove env n
    env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
  (s1, t1) <- ti env'' e
  pure (s1, TFun (apply s1 tv) t1)
-- Γ ⊢ e₀ ∶ τ₀, S₀   S₀Γ ⊢ e₁ ∶ τ₁, S₁   τ' = newvar   S₂ = mgu(S₁τ₀, τ₁ → τ')
-- ---------------------------------------------------------------------------
--                        Γ ⊢ e₀ e₁ ∶ S₂τ', S₂S₁S₀
ti env (EApp e1 e2) = do
  tv <- newTyVar "a"
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- unify (apply s2 t1) (TFun t2 tv)
  pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
-- Γ ⊢ e₀ : τ,S₀   S₀Γ, x : generalize(S₀Γ)(τ) ⊢ e₁ : τ', S₁
-- ---------------------------------------------------------
--              Γ ⊢ let x = e₀ in e₁ : τ', S₁S₀
ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let
    t' = generalize (apply s1 env) t1
    TypeEnv env' = remove env x
    env'' = TypeEnv $ Map.insert x t' env'
  (s2, t2) <- ti (apply s1 env'') e2
  pure (s1 `composeSubst` s2, t2)

typeInference :: Map Text Scheme -> Exp -> TI Type
typeInference env e = uncurry apply <$> ti (TypeEnv env) e
