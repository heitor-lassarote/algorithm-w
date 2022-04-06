module Main (main) where

import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)

import Test.HUnit (Test (TestList), assertEqual, assertFailure, runTestTTAndExit, test, (~:))

import Language.Dummy.AST (Exp (..), Lit (..), Type (..), prExp, prType)
import Language.Dummy.TypeInference (TIError (..), prError, runTI, typeInference)

data TestResult
  = ExpectSuccess Exp Type
  | ExpectFailure Exp TIError

pr :: Doc a -> String
pr = renderString . layoutPretty defaultLayoutOptions

exps :: [TestResult]
exps =
  [ ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
    `ExpectSuccess`
    TFun (TVar "a1") (TVar "a1")
  , ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
    `ExpectSuccess`
    TFun (TVar "a3") (TVar "a3")
  , ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
    `ExpectSuccess`
    TFun (TVar "a3") (TVar "a3")
  , ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
    `ExpectSuccess`
    TInt
  , ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
    `ExpectFailure`
    OccursCheckFail "a0" (TFun (TVar "a0") (TVar "a1"))
  , EAbs "m" (ELet "y" (EVar "m")
    (ELet "x" (EApp (EVar "y") (ELit (LBool True)))
    (EVar "x")))
    `ExpectSuccess`
    TFun (TFun TBool (TVar "a1")) (TVar "a1")
  ]

testInference :: TestResult -> Test
testInference (ExpectSuccess e t) = pr (prExp e) ~: test $ do
  let (res, _) = runTI (typeInference mempty e)
  case res of
    Left err ->
      assertFailure $
        "Expected a type '" <> pr (prType t) <> "', but got an error: " <> pr (prError err)
    Right t' ->
      assertEqual "Types are equal" t t'
testInference (ExpectFailure e err) = pr (prExp e) ~: test $ do
  let (res, _) = runTI (typeInference mempty e)
  case res of
    Left err' ->
      assertEqual "Errors are equal" err err'
    Right t ->
      assertFailure $
        "Expected an error '" <> pr (prError err) <> "', but got a type: " <> pr (prType t)

main :: IO ()
main = runTestTTAndExit $ TestList $ map testInference exps
