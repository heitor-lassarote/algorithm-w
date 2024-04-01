module Main (main) where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import Prettyprinter.Render.Text (putDoc)

import Language.Dummy.AlgorithmJ qualified as J
import Language.Dummy.AlgorithmW qualified as W
import Language.Dummy.AST (Exp (..), Lit (..), prExp, prType)
import Language.Dummy.Error (prError)

exps :: [Exp]
exps =
  [ -- let id = \x -> x in id
    ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
  , -- let id = \x -> x in id id
    ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
  , -- let id = \x -> let y = x in y in id id
    ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
  , -- let id = \x -> let y = x in y in id id 2
    ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
  , -- let id = \x -> x x in id
    ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
  , -- \m -> let y = m in let x = y True in x
    EAbs "m" (ELet "y" (EVar "m")
    (ELet "x" (EApp (EVar "y") (ELit (LBool True)))
    (EVar "x")))
  ]

testJ :: Exp -> IO ()
testJ e = do
  let (res, _) = J.runTI (J.typeInference e)
  case res of
    Left err -> do
      Text.putStr "Error: "
      putDoc $ prError err
    Right t  -> do
      putDoc $ prExp e
      Text.putStr " : "
      putDoc $ prType t
  Text.putStrLn ""

testW :: Exp -> IO ()
testW e = do
  let (res, _) = W.runTI (W.typeInference e)
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
main = do
  Text.putStrLn "Running algorithm J"
  traverse_ testJ exps
  Text.putStrLn "Running algorithm W"
  traverse_ testW exps
