module Main (main) where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import Prettyprinter.Render.Text (putDoc)

import Language.Dummy.AST (Exp (..), Lit (..), prExp, prType)
import Language.Dummy.TypeInference (runTI, typeInference, prError)

exps :: [Exp]
exps =
  [ ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
  , ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
  , ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
  , ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
  , ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
  , EAbs "m" (ELet "y" (EVar "m")
    (ELet "x" (EApp (EVar "y") (ELit (LBool True)))
    (EVar "x")))
  ]

test :: Exp -> IO ()
test e = do
  let (res, _) = runTI (typeInference mempty e)
  case res of
    Left err -> do
      Text.putStr "Error: "
      putDoc $ prError err
    Right t  -> do
      putDoc $ prExp e
      Text.putStr " : "
      putDoc $ prType t
  Text.putStrLn ""

main :: IO ()
main = traverse_ test exps
