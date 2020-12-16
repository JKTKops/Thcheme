module EvaluationTest
  ( evaluationTests

  , EvalTest (..), mkEvalTest, (?=)
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Control.Monad.Loops (allM)
import Data.Either
import Data.Array

import Val
import Parsers
import Parsers.Internal
import Bootstrap
import Evaluation
import EvaluationMonad (EM, unsafeEMtoIO)
import Options (noOpts)
import Primitives.Comparison (equalSSH)
import Primitives.WriteLib (showErrIO, writeSharedSH)

mkExpectedVal :: EM Val -> IO (Either a Val)
mkExpectedVal em = Right <$> unsafeEMtoIO em
-- strictly speaking; we don't need 'unsafeEMtoIO' for this, but it
-- conveniently handles getting a null environment and lint option
-- to run the action with.

mkExpectedErr :: EM LispErr -> IO (Either LispErr b)
mkExpectedErr em = Left <$> unsafeEMtoIO em
-- on the other hand, here, we do need 'unsafeEMtoIO'.

evaluationTests :: TestTree
evaluationTests = testGroup "Evaluation" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ evalTests
    , applyTests
    ]

evalTests = testGroup "eval" $ map mkEvalTest
    [ EvalTest
        { testName = "Simple string"
        , input = "\"test string\""
        , expected = Right $ String "test string"
        }
    , EvalTest
        { testName = "Simple char"
        , input = "#\\t"
        , expected = Right $ Char 't'
        }
    , EvalTest
        { testName = "Simple number"
        , input = "105"
        , expected = Right $ Number 105
        }
    , EvalTest
        { testName = "Simple bool (true)"
        , input = "#t"
        , expected = Right $ Bool True
        }
    , EvalTest
        { testName = "Simple bool (false)"
        , input = "#f"
        , expected = Right $ Bool False
        }
    , EvalTest
        { testName = "Simple vector"
        , input = "#(1 2 \"test\")"
        , expected = Right $ Vector (listArray (0, 2)
            [Number 1, Number 2, String "test"])
        }
    , EvalTest
        { testName = "Unbound symbol"
        , input = "x"
        , expected = Left $ UnboundVar "[Get] unbound symbol" "x"
        }
    , EvalTest
        { testName = "Bound symbol" -- primEnv doesn't have consts yet so making one
        , input = "(define my-var 5)\nmy-var"
        , expected = Right $ Number 5
        }
    , EvalTest
        { testName = "Call quit"
        , input = "(quit)"
        , expected = Left Quit
        }
    , EvalTest
        { testName = "Quote symbol"
        , input = "'x"
        , expected = Right $ Atom "x"
        }
    , EvalTest
        { testName = "Quote list"
        , input = "'(+ 1 2)"
        , expected = Right $ makeImmutableList [Atom "+", Number 1, Number 2]
        }
    , EvalTest
        { testName = "if with no alt (pred true)"
        , input = "(define p #t)\n(if p \"success\")"
        , expected = Right $ String "success"
        }
    , EvalTest
        { testName = "if with no alt (pred false)"
        , input = "(if #f \"success\")"
        , expected = Right Nil
        }
    , EvalTest
        { testName = "if (pred true)"
        , input = "(define p #t)\n(if p 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "if (pred false)"
        , input = "(define p #f)\n(if p #\\x #\\y)"
        , expected = Right $ Char 'y'
        }
    , EvalTest
        { testName = "if with multiple alts (pred true)"
        , input = "(if #t 1 (+ 1 2) \"fail\")"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "if with multiple alts (pred false)"
        , input = "(if #f 0 (define x 5) x)"
        , expected = Right $ Number 5
        }
    , EvalTest
        { testName = "#f is falsy"
        , input    = "(if #f 1 0)"
        , expected = Right $ Number 0
        }
    , EvalTest
        { testName = "0 is truthy"
        , input = "(if 0 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "() is truthy"
        , input = "(if () 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "\"\" is truthy"
        , input = "(if \"\" 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "non-falsy is truthy"
        , input = "(if '(1) 1 0)"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "Simple set! on bound var"
        , input = "(define x 5)\n(set! x 0)"
        , expected = Right $ Number 0
        }
    , EvalTest
        { testName = "Simple set! on unbound var"
        , input = "(set! x 0)"
        , expected = Right $ Number 0
        }
    , EvalTest
        { testName = "Complex set!"
        , input = "(define x 5)\n(set! x (+ 1 2))\nx"
        , expected = Right $ Number 3
        }
    , EvalTest
        { testName = "set! is scope independent"
        , input = "(define x 0)\n(define (test n) (set! x n))\n(test 5)\nx"
        , expected = Right $ Number 5
        }
    , EvalTest
        { testName = "Define symbol evaluates to new val"
        , input = "(define x 5)"
        , expected = Right $ Number 5
        }
    , EvalTest
        { testName = "Define symbol properly defines symbol"
        , input = "(define x 8)\nx"
        , expected = Right $ Number 8
        }
    , EvalTest
        { testName = "Define func no varargs evaluation"
        , input = "(define (test x) ())"
        , expected = Right $ Closure
            { params = ["x"]
            , vararg = Nothing
            , body   = [Nil]
            , cloEnv = undefined
            , name   = Just "test"
            }
        }
    , ImpureEvalTest
        { testName = "Define func no varargs call"
        , input = "(define (test x) (list x))\n(test 5)"
        , impureExpected = mkExpectedVal (makeMutableList [Number 5])
        }
    , EvalTest
        { testName = "Define func varargs evaluation"
        , input = "(define (test x . y) (x y))"
        , expected = Right $ Closure
            { params = ["x"]
            , vararg = Just "y"
            , body   = [makeImmutableList [Atom "x", Atom "y"]]
            , cloEnv = undefined
            , name   = Just "test"
            }
        }
    , ImpureEvalTest
        { testName = "Define func varargs call"
        , input = "(define (test . xs) (cons 0 xs))\n(test 1 2)"
        , impureExpected = mkExpectedVal $ 
            makeMutableList [Number 0, Number 1, Number 2]
        }
    , EvalTest
        { testName = "Define defines in lexical scope"
        , input = "(define (test n) (define + n))\n(test 0)\n+"
        , expected = Right $ Primitive 2 undefined "+"
        }
    , EvalTest
        { testName = "Lambda normal form no varargs"
        , input = "(lambda (x) (+ 1 x))"
        , expected = Right $ Closure
            { params = ["x"]
            , vararg = Nothing
            , body   = [makeImmutableList [Atom "+", Number 1, Atom "x"]]
            , cloEnv = undefined -- we only check == so this should never be evaluated
            , name   = Nothing
            }
        }
    , EvalTest
        { testName = "Lambda normal form varargs"
        , input = "(lambda (x y . zs) (null? zs))"
        , expected = Right $ Closure
            { params = ["x", "y"]
            , vararg = Just "zs"
            , body   = [makeImmutableList [Atom "null?", Atom "zs"]]
            , cloEnv = undefined
            , name   = Nothing
            }
        }
    , EvalTest
        { testName = "Lambda short form"
        , input = "(lambda xs (cons 0 xs))"
        , expected = Right $ Closure
            { params = []
            , vararg = Just "xs"
            , body   = [makeImmutableList [Atom "cons", Number 0, Atom "xs"]]
            , cloEnv = undefined
            , name   = Nothing
            }
        }
    , EvalTest
        { testName = "Lambda variable capture"
        , input    = "(((lambda (x) (lambda (y) (+ x y))) 1) 2)"
        , expected = Right $ Number 3
        }
    , EvalTest
        { testName = "define'd lambda captures itself"
        , input    = unlines
                -- we can't test this with just the letrec itself because
                -- we need to check that the lambda actually captured
                -- itself. To do that, we need to execute the lambda _outside_
                -- of the scope in which the closure is constructed.
                [ "(define test (letrec ((foo (lambda () foo))) foo))"
                , "(test)"
                ]
        , expected = Right $ Closure []
                                  Nothing
                                  [Atom "foo"]
                                  undefined
                                  (Just "foo")
        }
    , ImpureEvalTest
        { testName = "captured variables are independent across captures"
        , input    = unlines [ "(define (cadd x) (lambda (y) (+ x y)))"
                             , "(define add1 (cadd 1))"
                             , "(define add3 (cadd 3))"
                             , "(list (add1 5) (add3 5))"
                             ]
        , impureExpected = mkExpectedVal $
            makeMutableList [Number 6, Number 8]
        }
    , ImpureEvalTest
        { testName = "captured global variables mutate"
        , input    = unlines [ "(define x 5)"
                             , "(define (addx y) (+ x y))"
                             , "(define z1 (addx 1))"
                             , "(set! x 3)"
                             , "(define z2 (addx 1))"
                             , "(list z1 z2)"
                             ]
        , impureExpected = mkExpectedVal $
            makeMutableList [Number 6, Number 4]
        }
    , ImpureEvalTest
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
        , impureExpected = mkExpectedVal $
            makeMutableList [Number 1, Number 0]
        }
    , EvalTest
        { testName = "Begin does not open a new scope"
        , input = "(begin (define x 0))\nx"
        , expected = Right $ Number 0
        }
    , EvalTest
        { testName = "Begin evaluates to last"
        , input = "(begin (define x 0) x (+ x 1))"
        , expected = Right $ Number 1
        }
    , EvalTest
        { testName = "Empty primitive call fails"
        , input = "(null?)"
        , expected = Left $ NumArgs 1 []
        }
    , EvalTest
        { testName = "Empty func call fails"
        , input = "((lambda (x y) (+ x y)))"
        , expected = Left $ NumArgs 2 []
        }
    , EvalTest
        { testName = "Eval x fails in primEnv"
        , input = "x"
        , expected = Left $ UnboundVar "[Get] unbound symbol" "x"
        }
    , EvalTest
        { testName = "Define rejects empty-bodied functions"
        , input = "(define (test))"
        , expected = Left EmptyBody
        }
    , EvalTest
        { testName = "Lambda creation rejects empty-bodied functions"
        , input = "(lambda (x y))"
        , expected = Left EmptyBody
        }
    , ImpureEvalTest
        { testName = "dot unquote is unquote-splicing"
        , input    = "(define lst '(1 2))\n`(0 . ,lst)"
        , impureExpected = mkExpectedVal $
            makeMutableList $ map Number [0..2]
        }
    , ImpureEvalTest
        { testName = "unquote in weird places is untouched"
        , input    = "`(0 unquote 1 2)"
        , impureExpected = mkExpectedVal $
            makeMutableList [Number 0, Atom "unquote", Number 1, Number 2]
        }
    , EvalTest
        { testName = "'eval' primitive evaluates datum"
        , input    = "(eval '(+ 2 3))"
        , expected = Right $ Number 5
        }
    ]

data EvalTest 
  = ImpureEvalTest 
    { testName       :: String
    , input          :: String
    , impureExpected :: IO (Either LispErr Val)
    }
  | EvalTest
    { testName :: String
    , input    :: String
    , expected :: Either LispErr Val
    }

-- | Re-implementation of HUnit.assertEqual
-- to support 'Val's.
assertEqualIOWith
  :: HasCallStack
  => (a -> a -> IO Bool) -- ^ equality test
  -> (a -> IO String)    -- ^ show values
  -> a                   -- ^ expected
  -> a                   -- ^ actual
  -> Assertion
assertEqualIOWith test display expected actual = do
    b <- test expected actual
    unless b $ mkMsg >>= assertFailure
  where
    mkMsg = do
      dispE <- display expected
      dispA <- display actual
      return $ "expected: " ++ dispE ++ "\n but got: " ++ dispA

infix 1 ?=

(?=) :: Either LispErr Val -> Either LispErr Val -> Assertion
(?=) = assertEqualIOWith outputEquals
                         showOutput

outputEquals :: Either LispErr Val -> Either LispErr Val -> IO Bool
outputEquals e1@Left{} e2@Left{} = pure $ e1 == e2
outputEquals (Right e) (Right a) = valsSameShape e a
outputEquals _ _ = pure False

-- | This is a wrapper on equalSSH that overwrites the behavior on primitives
-- and closures. Note that if a value /contains/ a primitive or closure,
-- then the sense of equalSSH will be used to compare those internal values.
-- However this is plenty sufficient for testing, where we (usually) just
-- want to check that we evaluated to the primitive or closure with the right
-- name.
valsSameShape :: Val -> Val -> IO Bool
valsSameShape (Primitive _ _ n) (Primitive _ _ n') = return $ n == n'
valsSameShape (Closure p v b _ n) (Closure p' v' b' _ n') =
  if p == p' && v == v' && n == n'
    then allM id $ zipWith valsSameShape b b'
    else pure False
valsSameShape x y = equalSSH x y

showOutput :: Either LispErr Val -> IO String
showOutput (Left e)  = showErrIO e
showOutput (Right v) = writeSharedSH v

buildExpected :: EvalTest -> IO (Either LispErr Val)
buildExpected iet@ImpureEvalTest{} = impureExpected iet
buildExpected et@EvalTest{} = pure $ expected et

mkEvalTest :: EvalTest -> TestTree
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
        evaluateExpr primEnv noOpts expr

    step "Verifying evaluation"
    let evaluation = last evaluations
    expected <- buildExpected tb
    evaluation ?= expected

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
        , expectedA = Right $ makeImmutableList [Number 1]
        }
    , ApplyTB
        { testNameA = "Overapply vararg func"
        , funcIn = "(define (test x . zs) (cons x zs))"
        , args = [Number 0, Number 1, Number 2]
        , expectedA = Right $ makeImmutableList [Number 0, Number 1, Number 2]
        }
    ]

-- Precondition on apply: args are fully evaluated scheme forms
data ApplyTB = ApplyTB
             { testNameA :: String
             , funcIn :: String
             , args :: [Val]
             , expectedA :: Either LispErr Val
             }

mkApplyTest :: ApplyTB -> TestTree
mkApplyTest tb = testCase (testNameA tb) $ do
    let Right funcP = parse parseExpr "" $ funcIn tb
    primEnv <- primitiveBindings
    (Right func) <- fst <$> evaluateExpr primEnv noOpts funcP
    res <- runTest primEnv noOpts $ apply func (args tb)
    fst res ?= expectedA tb
