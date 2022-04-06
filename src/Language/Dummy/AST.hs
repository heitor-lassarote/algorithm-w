module Language.Dummy.AST
  ( Exp (..), Lit (..), Type (..), Scheme (..)
  , prExp, prLit, prType, prScheme
  ) where

import Data.Text (Text)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

-- | The AST for a term expression.
data Exp
  -- | A variable. E.g.: `x` or `foobar`.
  = EVar Text
  -- | A literal value. E.g.: `42` or `True`.
  | ELit Lit
  -- | A function application. E.g.: `f x`.
  | EApp Exp Exp
  -- | A function abstraction. E.g.: `λ x → x`.
  | EAbs Text Exp
  -- | A let binding. E.g.: `let id = λ x → x in id 42`.
  | ELet Text Exp Exp
  deriving stock (Eq, Ord, Show)

-- | The AST for some literal constant.
data Lit
  -- | An integer. E.g.: `42` or `-3`.
  = LInt Integer
  -- | A boolean. Either `False` or `True`.
  | LBool Bool
  deriving stock (Eq, Ord, Show)

-- | The AST for a type expression.
data Type
  -- | A type variable. E.g.: `x : a`.
  = TVar Text
  -- | The type of some integer literal. E.g.: `42 : Int`.
  | TInt
  -- | The type of some boolean literal. E.g.: `False : Bool`.
  | TBool
  -- | The type of a function. E.g.: `(λ x → x) : a → a`.
  | TFun Type Type
  deriving stock (Eq, Ord, Show)

-- | Represents the scheme of some type signature. E.g.: `∀ a b. (a → b) -> a → b`.
data Scheme = Scheme
  [Text]  -- ^ Type variable binders. E.g.: `∀ a b.`.
  Type  -- ^ Type expression. E.g.: `(a → b) → a → b`.

prExp :: Exp -> Doc Exp
prExp (EVar name) = PP.pretty name
prExp (ELit lit) = PP.unAnnotate $ prLit lit
prExp (ELet x b body) =
  "let" <+> PP.pretty x <+> "=" <+> prExp b <+> "in" <+> PP.nest 2 (prExp body)
prExp (EApp e1 e2) = prAbsLeftExp e1 <+> prParenExp e2
prExp (EAbs n e) = "λ" <+> PP.pretty n <+> "→" <+> prExp e

prAbsLeftExp :: Exp -> Doc Exp
prAbsLeftExp t = case t of
  ELet _ _ _ -> PP.parens $ prExp t
  EAbs _ _   -> PP.parens $ prExp t
  _          -> prExp t

prParenExp :: Exp -> Doc Exp
prParenExp t = case t of
  ELet _ _ _ -> PP.parens $ prExp t
  EApp _ _   -> PP.parens $ prExp t
  EAbs _ _   -> PP.parens $ prExp t
  _          -> prExp t

prLit :: Lit -> Doc Lit
prLit (LInt i) = PP.pretty i
prLit (LBool b) = PP.pretty @Text $ if b then "True" else "False"

prType :: Type -> Doc Type
prType (TVar n) = PP.pretty n
prType TInt = "Int"
prType TBool = "Bool"
prType (TFun t s) = prParenType t <+> "→" <+> prType s

prParenType :: Type -> Doc Type
prParenType t = case t of
  TFun _ _ -> PP.parens $ prType t
  _        -> prType t

prScheme :: Scheme -> Doc Scheme
prScheme (Scheme vars t) =
  "∀" <+> PP.hcat (PP.punctuate PP.space (map PP.pretty vars))
  <> "." <+> PP.unAnnotate (prType t)
