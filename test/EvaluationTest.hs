module EvaluationTest (evaluationTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Data.Either

import LispVal
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
        { testName = "Simple set! on bound var"
        , input = "(define x 5)\n(set! x 0)"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "Simple set! on unbound var"
        , input = "(set! x 0)"
        , expected = Left $ UnboundVar "[Set] unbound symbol" "x"
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
        { testName = "Begin does not open new scope"
        , input = "(begin (define x 0))\nx"
        , expected = Right $ Number 0
        }
    , EvalTB
        { testName = "Begin evaluates to last"
        , input = "(begin (define x 0) x (+ x 1))"
        , expected = Right $ Number 1
        }
    , EvalTB
        { testName = "Primitive evaluates to itself"
        , input = "(+)"
        , expected = Right $ Primitive undefined undefined "+"
        }
    , EvalTB
        { testName = "IOPrimitive evaluates to itself"
        , input = "(write)"
        , expected = Right $ IOPrimitive undefined undefined "write"
        }
    , EvalTB
        { testName = "Func evaluates to itself"
        , input = "((lambda (x y) (+ x y)))"
        , expected = Right $ Func
            { params  = ["x", "y"]
            , vararg  = Nothing
            , body    = [List [Atom "+", Atom "x", Atom "y"]]
            , closure = undefined
            , name = Nothing
            }
        }
    , EvalTB
        { testName = "Eval x fails in primEnv"
        , input = "x"
        , expected = Left $ UnboundVar "[Get] unbound symbol" "x"
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
    evaluations <- runExceptT . forM exprs $ \input -> do
        liftIO $ step "Parsing input"
        let pExpr = readExpr $ input
        liftIO $ isRight pExpr @? "Parse failed on input: " ++ input
        let Right expr = pExpr

        lift $ step "Evaluating input"
        eval primEnv expr

    step "Verifying evaluation"
    let evaluation = last <$> evaluations
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
        , funcIn = "+"
        , args = []
        , expectedA = Right $ Primitive 2 undefined "+"
        }
    , ApplyTB
        { testNameA = "Partial apply primitive"
        , funcIn = "+"
        , args = [Number 1]
        , expectedA = Right $ Func
            { params  = ["a"]
            , vararg  = Nothing
            , body    = [List [Atom "+", Atom "z", Atom "a"]]
            , closure = undefined
            , name    = Just "+"
            }
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
        , expectedA = Right $ IOPrimitive 2 undefined "write-port"
        }
    , ApplyTB
        { testNameA = "Partial apply primitive"
        , funcIn = "write-port"
        , args = [Number 0]
        , expectedA = Right $ Func
            { params  = ["a"]
            , vararg  = Nothing
            , body    = [List [Atom "write-port", Atom "z", Atom "a"]]
            , closure = undefined
            , name    = Just "write-port"
            }
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
        , expectedA = Right $ Func
            { params  = ["x", "y"]
            , vararg  = Nothing
            , body    = [List [Atom "+", Atom "x", Atom "y"]]
            , closure = undefined
            , name    = Just "add"
            }
        }
    , ApplyTB
        { testNameA = "Partial apply func"
        , funcIn = "(define (test x y) y)"
        , args = [String ""]
        , expectedA = Right $ Func
            { params  = ["y"]
            , vararg  = Nothing
            , body    = [List [Atom "y"]]
            , closure = undefined
            , name    = Just "test"
            }
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
    (Right func) <- runExceptT $ eval primEnv funcP
    res <- runExceptT $ apply func (args tb)
    res @?= expectedA tb
