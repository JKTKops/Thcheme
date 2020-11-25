module EvaluationTest (evaluationTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Data.Either
import Data.Array
import qualified Data.HashMap.Strict as Map

import Types
import Parsers
import Parsers.Internal
import Bootstrap
import Evaluation

evaluationTests :: TestTree
evaluationTests = testGroup "Evaluation" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ evalTests
    , applyTests
    ]

evalTests = testGroup "eval" $ map mkEvalTest
    [ EvalTB
        { testName = "Simple string"
        , input = "\"test string\""
        , expected = Right $ String "test string"
        }
    , EvalTB
        { testName = "Simple char"
        , input = "#\\t"
        , expected = Right $ Char 't'
        }
    , EvalTB
        { testName = "Simple number"
        , input = "105"
        , expected = Right $ Number 105
        }
    , EvalTB
        { testName = "Simple bool (true)"
        , input = "#t"
        , expected = Right $ Bool True
        }
    , EvalTB
        { testName = "Simple bool (false)"
        , input = "#f"
        , expected = Right $ Bool False
        }
    , EvalTB
        { testName = "Simple vector"
        , input = "#(1 2 \"test\")"
        , expected = Right $ Vector (listArray (0, 2)
            [Number 1, Number 2, String "test"])
        }
    , EvalTB
        { testName = "Unbound symbol"
        , input = "x"
        , expected = Left $ UnboundVar "[Get] unbound symbol" "x"
        }
    , EvalTB
        { testName = "Bound symbol" -- primEnv doesn't have consts yet so making one
        , input = "(define my-var 5)\nmy-var"
        , expected = Right $ Number 5
        }
    , EvalTB
        { testName = "Call quit"
        , input = "(quit)"
        , expected = Left Quit
        }
    , EvalTB
        { testName = "Quote symbol"
        , input = "'x"
        , expected = Right $ Atom "x"
        }
    , EvalTB
        { testName = "Quote list"
        , input = "'(+ 1 2)"
        , expected = Right $ List [Atom "+", Number 1, Number 2]
        }
    , EvalTB
        { testName = "if with no alt (pred true)"
        , input = "(define p #t)\n(if p \"success\")"
        , expected = Right $ String "success"
        }
    , EvalTB
        { testName = "if with no alt (pred false)"
        , input = "(if #f \"success\")"
        , expected = Right $ List []
        }
    , EvalTB
        { testName = "if (pred true)"
        , input = "(define p #t)\n(if p 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTB
        { testName = "if (pred false)"
        , input = "(define p #f)\n(if p #\\x #\\y)"
        , expected = Right $ Char 'y'
        }
    , EvalTB
        { testName = "if with multiple alts (pred true)"
        , input = "(if #t 1 (+ 1 2) \"fail\")"
        , expected = Right $ Number 1
        }
    , EvalTB
        { testName = "if with multiple alts (pred false)"
        , input = "(if #f 0 (define x 5) x)"
        , expected = Right $ Number 5
        }
    , EvalTB
        { testName = "0 is falsy"
        , input = "(if 0 1 0)"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "() is falsy"
        , input = "(if () 1 0)"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "\"\" is falsy"
        , input = "(if \"\" 1 0)"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "non-falsy is truthy"
        , input = "(if '(1) 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTB
        { testName = "Simple set! on bound var"
        , input = "(define x 5)\n(set! x 0)"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "Simple set! on unbound var"
        , input = "(set! x 0)"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "Complex set!"
        , input = "(define x 5)\n(set! x (+ 1 2))\nx"
        , expected = Right $ Number 3
        }
    , EvalTB
        { testName = "set! is scope independent"
        , input = "(define x 0)\n(define (test n) (set! x n))\n(test 5)\nx"
        , expected = Right $ Number 5
        }
    , EvalTB
        { testName = "Define symbol evaluates to new val"
        , input = "(define x 5)"
        , expected = Right $ Number 5
        }
    , EvalTB
        { testName = "Define symbol properly defines symbol"
        , input = "(define x 8)\nx"
        , expected = Right $ Number 8
        }
    , EvalTB
        { testName = "Define func no varargs evaluation"
        , input = "(define (test x) ())"
        , expected = Right $ Func
            { params  = ["x"]
            , vararg  = Nothing
            , body    = [List []]
            , closure = undefined
            , name    = Just "test"
            }
        }
    , EvalTB
        { testName = "Define func no varargs call"
        , input = "(define (test x) (list x))\n(test 5)"
        , expected = Right $ List [Number 5]
        }
    , EvalTB
        { testName = "Define func varargs evaluation"
        , input = "(define (test x . y) (x y))"
        , expected = Right $ Func
            { params  = ["x"]
            , vararg  = Just "y"
            , body    = [List [Atom "x", Atom "y"]]
            , closure = undefined
            , name = Just "test"
            }
        }
    , EvalTB
        { testName = "Define func varargs call"
        , input = "(define (test . xs) (cons 0 xs))\n(test 1 2)"
        , expected = Right $ List [Number 0, Number 1, Number 2]
        }
    , EvalTB
        { testName = "Define defines in lexical scope"
        , input = "(define (test n) (define + n))\n(test 0)\n+"
        , expected = Right $ Primitive 2 undefined "+"
        }
    , EvalTB
        { testName = "Lambda normal form no varargs"
        , input = "(lambda (x) (+ 1 x))"
        , expected = Right $ Func
            { params  = ["x"]
            , vararg  = Nothing
            , body    = [List [Atom "+", Number 1, Atom "x"]]
            , closure = undefined -- we only check == so this should never be evaluated
            , name    = Nothing
            }
        }
    , EvalTB
        { testName = "Lambda normal form varargs"
        , input = "(lambda (x y . zs) (null? zs))"
        , expected = Right $ Func
            { params  = ["x", "y"]
            , vararg  = Just "zs"
            , body    = [List [Atom "null?", Atom "zs"]]
            , closure = undefined
            , name    = Nothing
            }
        }
    , EvalTB
        { testName = "Lambda short form"
        , input = "(lambda xs (cons 0 xs))"
        , expected = Right $ Func
            { params  = []
            , vararg  = Just "xs"
            , body    = [List [Atom "cons", Number 0, Atom "xs"]]
            , closure = undefined
            , name    = Nothing
            }
        }
    , EvalTB
        { testName = "Lambda variable capture"
        , input    = "(((lambda (x) (lambda (y) (+ x y))) 1) 2)"
        , expected = Right $ Number 3
        }
    , EvalTB
        { testName = "captured variables are independent across captures"
        , input    = unlines [ "(define (cadd x) (lambda (y) (+ x y)))"
                             , "(define add1 (cadd 1))"
                             , "(define add3 (cadd 3))"
                             , "(list (add1 5) (add3 5))"
                             ]
        , expected = Right $ List [Number 6, Number 8]
        }
    , EvalTB
        { testName = "captured global variables mutate"
        , input    = unlines [ "(define x 5)"
                             , "(define (addx y) (+ x y))"
                             , "(define z1 (addx 1))"
                             , "(set! x 3)"
                             , "(define z2 (addx 1))"
                             , "(list z1 z2)"
                             ]
        , expected = Right $ List [Number 6, Number 4]
        }
    , EvalTB
        { testName = "captured local variables mutate"
        , input    = unlines [
                concat [ "(define (test x)"
                       , " (let ((go (lambda (y) x))"
                       , "       (r nil))"
                       , "  (set! r (cons (go nil) r))"
                       , "  (set! x (+ x 1))"
                       , "  (cons (go nil) r)))"
                       ]
                , "(test 0)"
                ]
        , expected = Right $ List [Number 1, Number 0]
        }
    , EvalTB
        { testName = "Begin opens a new scope"
        , input = "(begin (define x 0))\nx"
        , expected = Left $ UnboundVar "[Get] unbound symbol" "x"
        }
    , EvalTB
        { testName = "Begin evaluates to last"
        , input = "(begin (define x 0) x (+ x 1))"
        , expected = Right $ Number 1
        }
    , EvalTB
        { testName = "Empty primitive call fails"
        , input = "(null?)"
        , expected = Left $ NumArgs 1 []
        }
    , EvalTB
        { testName = "Empty func call fails"
        , input = "((lambda (x y) (+ x y)))"
        , expected = Left $ NumArgs 2 []
        }
    , EvalTB
        { testName = "Eval x fails in primEnv"
        , input = "x"
        , expected = Left $ UnboundVar "[Get] unbound symbol" "x"
        }
    , EvalTB
        { testName = "Define rejects empty-bodied functions"
        , input = "(define (test))"
        , expected = Left $ Default "Attempt to define function with no body"
        }
    , EvalTB
        { testName = "Lambda creation rejects empty-bodied functions"
        , input = "(lambda (x y))"
        , expected = Left $ Default "Attempt to define function with no body"
        }
    , EvalTB
        { testName = "dot unquote is unquote-splicing"
        , input    = "(define lst '(1 2))\n`(0 . ,lst)"
        , expected = Right $ List $ map Number [0..2]
        }
    , EvalTB
        { testName = "unquote in weird places is untouched"
        , input    = "`(0 unquote 1 2)"
        , expected = Right $ List [Number 0, Atom "unquote", Number 1, Number 2]
        }
    ]

data EvalTB = EvalTB
                 { testName :: String
                 , input :: String
                 , expected :: Either LispErr LispVal
                 }

mkEvalTest :: EvalTB -> TestTree
mkEvalTest tb = let exprs = lines $ input tb
                 in testCaseSteps (testName tb) $ \step -> do
    primEnv <- primitiveBindings
    evaluations <- fmap (map fst) . forM exprs $ \input -> do
        step $ "input: " ++ input
        step "Parsing input"
        let pExpr = readExpr input
        isRight pExpr @? "Parse failed on input: " ++ input
        let Right expr = pExpr

        step "Evaluating input"
        evaluateExpr primEnv Map.empty expr

    step "Verifying evaluation"
    let evaluation = last evaluations
    evaluation @?= expected tb

applyTests :: TestTree
applyTests = testGroup "Apply" $ map mkApplyTest
    [ ApplyTB
        { testNameA = "Fully apply to primitive"
        , funcIn = "+"
        , args = [Number 1, Number 2]
        , expectedA = Right $ Number 3
        }
    , ApplyTB
        { testNameA = "Empty apply primitive"
        , funcIn = "null?"
        , args = []
        , expectedA = Left $ NumArgs 1 []
        }
    , ApplyTB
        { testNameA = "Under apply primitive"
        , funcIn = "eq?"
        , args = [Number 1]
        , expectedA = Left $ NumArgs 2 [Number 1]
        }
    , ApplyTB
        { testNameA = "Fully apply IOPrimitive"
        , funcIn = "read-contents"
        , args = [String "test/test-text"]
        , expectedA = Right $ String "Some random text"
        }
    , ApplyTB
        { testNameA = "Empty apply IOPrimitive"
        , funcIn = "write-port"
        , args = []
        , expectedA = Left $ NumArgs 2 []
        }
    , ApplyTB
        { testNameA = "Under apply IOPrimitive"
        , funcIn = "write-port"
        , args = [Number 0]
        , expectedA = Left $ NumArgs 2 [Number 0]
        }
    , ApplyTB
        { testNameA = "Fully apply func"
        , funcIn = "(lambda (x y) (+ x y))"
        , args = [Number 2, Number 3]
        , expectedA = Right $ Number 5
        }
    , ApplyTB
        { testNameA = "Empty apply func"
        , funcIn = "(define (add x y) (+ x y))"
        , args = []
        , expectedA = Left $ NumArgs 2 []
        }
    , ApplyTB
        { testNameA = "Under apply func"
        , funcIn = "(define (test x y) y)"
        , args = [String ""]
        , expectedA = Left $ NumArgs 2 [String ""]
        }
    , ApplyTB
        { testNameA = "Min apply vararg func"
        , funcIn = "(define (test x y . z) (cons y z))"
        , args = [Number 0, Number 1]
        , expectedA = Right $ List [Number 1]
        }
    , ApplyTB
        { testNameA = "Overapply vararg func"
        , funcIn = "(define (test x . zs) (cons x zs))"
        , args = [Number 0, Number 1, Number 2]
        , expectedA = Right $ List [Number 0, Number 1, Number 2]
        }
    ]

-- Precondition on apply: args are fully evaluated
data ApplyTB = ApplyTB
             { testNameA :: String
             , funcIn :: String
             , args :: [LispVal]
             , expectedA :: Either LispErr LispVal
             }

mkApplyTest :: ApplyTB -> TestTree
mkApplyTest tb = testCase (testNameA tb) $ do
    let Right funcP = parse parseExpr "" $ funcIn tb
    primEnv <- primitiveBindings
    (Right func) <- fst <$> evaluateExpr primEnv Map.empty funcP
    res <- runTest primEnv Map.empty $ apply func (args tb)
    fst res @?= expectedA tb
