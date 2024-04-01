module Language.Dummy.AlgorithmJ
  ( TI, runTI
  , TIError (..), prError
  , TIState (..)
  , typeInference
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Text qualified as Text

import Language.Dummy.AST (Exp (..), Lit (..), Type (..))
import Language.Dummy.Error (TIError (..), prError)

data TVar s
  = Unbound Text
  | Bound (JType s)

type JTVar s = STRef s (TVar s)

-- | The intermediate AST for a type expression.
data JType s
  -- | A type variable. E.g.: `x : a`.
  = JTVar (JTVar s)
  -- | The type of some integer literal. E.g.: `42 : Int`.
  | JTInt
  -- | The type of some boolean literal. E.g.: `False : Bool`.
  | JTBool
  -- | The type of a function. E.g.: `(λ x → x) : a → a`.
  | JTFun (JType s) (JType s)

writeJTVar :: JTVar s -> TVar s -> TI s ()
writeJTVar ref var = lift $ lift $ writeSTRef ref var

readJTVar :: JTVar s -> TI s (TVar s)
readJTVar ref = lift $ lift $ readSTRef ref

newJTVar :: TVar s -> TI s (JTVar s)
newJTVar tvar = lift $ lift $ newSTRef tvar

-- | Represents the scheme of some type signature. E.g.: `∀ a b. (a → b) -> a → b`.
data JScheme s = JScheme
  [Text]  -- ^ Type variable binders. E.g.: `∀ a b.`.
  (JType s)  -- ^ Type expression. E.g.: `(a → b) → a → b`.

eqTVar :: JTVar s -> JTVar s -> TI s Bool
eqTVar t1 t2 = do
  t1' <- readJTVar t1
  t2' <- readJTVar t2
  case (t1', t2') of
    (Unbound n1  , Unbound n2  ) -> pure (n1 == n2)
    (Unbound _   , Bound   _   ) -> pure False
    (Bound   _   , Unbound _   ) -> pure False
    (Bound   t1'', Bound   t2'') -> eqJType t1'' t2''

eqJType :: JType s -> JType s -> TI s Bool
eqJType = \cases
  (JTVar t1)    (JTVar t2)    -> eqTVar t1 t2
  JTInt         JTInt         -> pure True
  JTBool        JTBool        -> pure True
  (JTFun l1 r1) (JTFun l2 r2) -> (&&) <$> eqJType l1 l2 <*> eqJType r1 r2
  _             _             -> pure False

encode :: Type -> TI s (JType s)
encode (TVar n)   = JTVar <$> newJTVar (Unbound n)
encode TInt       = pure JTInt
encode TBool      = pure JTBool
encode (TFun l r) = JTFun <$> encode l <*> encode r

decode :: JType s -> TI s Type
decode (JTVar t)   =
  readJTVar t >>= \case
    Unbound n  -> pure $ TVar n
    Bound   t' -> decode t'
decode JTInt       = pure TInt
decode JTBool      = pure TBool
decode (JTFun l r) = TFun <$> decode l <*> decode r

type Subst s = Map Text (JType s)

class Types s a where
  -- | Determines all the free type variables of a type.
  ftv :: a -> TI s (Set Text)
  -- | Applies substitutions over the type variables of a given type.
  apply :: Subst s -> a -> TI s a

instance Types s (JType s) where
  ftv (JTVar t)     =
    readJTVar t >>= \case
      Unbound n  -> pure $ Set.singleton n
      Bound   t' -> ftv t'
  ftv JTInt         = pure Set.empty
  ftv JTBool        = pure Set.empty
  ftv (JTFun t1 t2) = Set.union <$> ftv t1 <*> ftv t2

  apply s t@(JTVar t')   =
    readJTVar t' >>= \case
      Unbound n   -> pure $ fromMaybe t $ Map.lookup n s
      Bound   t'' -> apply s t''
  apply _ JTInt         = pure JTInt
  apply _ JTBool        = pure JTBool
  apply s (JTFun t1 t2) = JTFun <$> apply s t1 <*> apply s t2

instance Types s (JScheme s) where
  ftv (JScheme vars t) = (Set.\\) <$> ftv t <*> pure (Set.fromList vars)
  apply s (JScheme vars t) = JScheme vars <$> apply (foldr Map.delete s vars) t

instance Types s a => Types s [a] where
  ftv = fmap (foldr Set.union Set.empty) . traverse ftv
  apply s = traverse (apply s)

newtype TypeEnv s = TypeEnv (Map Text (JScheme s))

-- | Removes the binding for the given term variable from the environment.
remove :: TypeEnv s -> Text -> TypeEnv s
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

instance Types s (TypeEnv s) where
  ftv (TypeEnv env) = ftv $ Map.elems env
  apply s (TypeEnv env) = TypeEnv <$> Map.traverseWithKey (\_ -> apply s) env

-- | Abstracts a type over all type variables which are free in the type, but
-- not in the given type environment.
generalize :: forall s. TypeEnv s -> JType s -> TI s (JScheme s)
generalize env t = JScheme <$> vars <*> pure t
  where
    vars :: TI s [Text]
    vars = Set.toList <$> ((Set.\\) <$> ftv t <*> ftv env)

newtype TIState = TIState
  { supply :: Int
  }

-- | A monad for generating fresh variable names for the type inference and
-- passing dynamically scoped environments.
type TI s a = ExceptT TIError (StateT TIState (ST s)) a

runTI :: forall a. (forall s. TI s a) -> (Either TIError a, TIState)
runTI action = runST $ flip runStateT initTIState $ runExceptT action
  where
    initTIState = TIState
      { supply = 0
      }

newTyVar :: Text -> TI s (JType s)
newTyVar prefix = do
  s <- get
  put s{supply = supply s + 1}
  JTVar <$> newJTVar (Unbound $ prefix <> Text.pack (show s.supply))

-- | Replaces all bound type variables in a type scheme with fresh type
-- variables.
instantiate :: JScheme s -> TI s (JType s)
instantiate (JScheme vars t) = do
  nvars <- traverse (const $ newTyVar "a") vars
  let s = Map.fromList $ zip vars nvars
  apply s t

-- | Produces a substitution which is the most general unifier (MGU).
unify :: JType s -> JType s -> TI s ()
unify (JTFun l r) (JTFun l' r') = do
  unify l l'
  unify r r'
unify (JTVar u) t = varBind u t
unify t (JTVar u) = varBind u t
unify JTInt JTInt = pure ()
unify JTBool JTBool = pure ()
unify t1 t2 = do
  t1' <- decode t1
  t2' <- decode t2
  throwError $ TypesDoNotUnify t1' t2'

-- | Attempts to bind a type variable to a type, mutating the new binding as a
-- bound type variable. Note that this function avoids binding a variable to
-- itself and performs the occurs check.
--
-- Avoid binding a variable with itself: This will avoid creating excessive
-- substitutions. Substituting something by itself is a redundancy.
--
-- Occurs check: As an example, suppose that the type inference finds a
-- constraint `t ~ [t]`. We'd need to construct a type `[[t]]`, which leads to
-- `[[[t]]]`, and so on, or an infinite type. This could cause an infinite loop
-- in the type inference, and so this is forbidden.
varBind :: JTVar s -> JType s -> TI s ()
varBind u t =
  readJTVar u >>= \case
    Unbound n ->
      eqJType (JTVar u) t >>= \case
        False -> do
          ftvOfT <- ftv t
          if n `Set.member` ftvOfT
          then do
            t' <- decode t
            throwError $ OccursCheckFail n t'
          else writeJTVar u $ Bound t
        True -> pure ()
    Bound t' -> unify t' t

tiLit :: TypeEnv s -> Lit -> TI s (JType s)
tiLit _ (LInt _) = pure JTInt
tiLit _ (LBool _) = pure JTBool

ti :: TypeEnv s -> Exp -> TI s (JType s)
-- x : σ ∈ Γ   τ = inst(σ)
-- -----------------------
--     Γ ⊢ x : τ, ∅
ti (TypeEnv env) (EVar n) = case Map.lookup n env of
  Nothing    -> throwError $ UnboundVariable n
  Just sigma -> instantiate sigma
-- ----------------   ---------------   -----------------------------
-- Γ ⊢ false : Bool   Γ ⊢ true : Bool   Γ ⊢ ...,-2,-1,0,1,2,... : Int
ti env (ELit l) = tiLit env l
-- τ = newvar    Γ, x : τ ⊢ e : τ'
-- -------------------------------
--       Γ ⊢ λx. e : τ → τ'
ti env (EAbs n e) = do
  tv <- newTyVar "a"
  let
    TypeEnv env' = remove env n
    env'' = TypeEnv (env' `Map.union` Map.singleton n (JScheme [] tv))
  t1 <- ti env'' e
  pure $ JTFun tv t1
-- Γ ⊢ e₀ ∶ τ₀   Γ ⊢ e₁ ∶ τ₁   τ' = newvar   unify(τ₀, τ₁ → τ')
-- _-----------------------------------------------------------
--                        Γ ⊢ e₀ e₁ ∶ τ'
ti env (EApp e1 e2) = do
  tv <- newTyVar "a"
  t1 <- ti env e1
  t2 <- ti env e2
  unify t1 (JTFun t2 tv)
  pure tv
-- Γ ⊢ e₀ : τ   Γ, x : generalize(Γ)(τ) ⊢ e₁ : τ'
-- _---------------------------------------------
--            Γ ⊢ let x = e₀ in e₁ : τ'
ti env (ELet x e1 e2) = do
  t1 <- ti env e1
  t' <- generalize env t1
  let
    TypeEnv env' = remove env x
    env'' = TypeEnv $ Map.insert x t' env'
  t2 <- ti env'' e2
  pure t2

typeInference :: Exp -> TI s Type
typeInference e = decode =<< ti (TypeEnv mempty) e
