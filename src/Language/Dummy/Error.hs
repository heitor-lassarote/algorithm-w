module Language.Dummy.Error (TIError (..), prError) where

import Data.Text (Text)
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

import Language.Dummy.AST (Type (..), prType)

data TIError
  = TypesDoNotUnify Type Type
  | OccursCheckFail Text Type
  | UnboundVariable Text
  deriving stock (Eq, Show)

prError :: TIError -> Doc TIError
prError (TypesDoNotUnify t1 t2) =
  "Types do not unify:" <+> PP.unAnnotate (prType t1)
  <+> "vs." <+> PP.unAnnotate (prType t2)
prError (OccursCheckFail u t) =
  "Occurs check fails:" <+> PP.pretty u <+> "vs." <+> PP.unAnnotate (prType t)
prError (UnboundVariable n) =
  "Unbound variable:" <+> PP.pretty n
