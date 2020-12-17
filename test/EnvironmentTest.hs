module EnvironmentTest (environmentTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Either
import Control.Monad.Trans.Except (runExceptT)

import LispValTest ()
import EvaluationTest ((?=))

import Val
import Bootstrap
import Environment

environmentTests :: TestTree
environmentTests = testGroup "Environment" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testIsBound
    , testGetVar
    , testSetVar
    , testDefineVar
    ]

testIsBound :: TestTree
testIsBound = testGroup "isBound"
    [ testCase "Var is bound" $ do
        primEnv <- primitiveBindings
        isBound primEnv "+" @? "+ is not bound"
    , testCase "Var is not bound" $ do
        primEnv <- primitiveBindings
        not <$> isBound primEnv "dumbname" @? "'dumbname' is bound"
    ]

testGetVar :: TestTree
testGetVar = testGroup "getVar"
    [ testCase "Get +" $ do
        primEnv <- primitiveBindings
        getRes <- runExceptT $ getVar primEnv "+"
        isRight getRes @? "Didn't find var +"
        let Right (Primitive _ _ name) = getRes
        name @?= "+"
    , testCase "Get nonexistant var" $ do
        primEnv <- primitiveBindings
        getRes <- runExceptT $ getVar primEnv "dne"
        isLeft getRes @? "Found 'dne' in primEnv"
        let Left e = getRes
        e @?= UnboundVar "[Get]" "dne"
    , testCase "Get from null env" $ do
        nullEnv <- nullEnv
        getRes <- runExceptT $ getVar nullEnv "fail"
        isLeft getRes @? "Found 'fail' in nullEnv"
        let Left f = getRes
        f @?= UnboundVar "[Get]" "fail"
    ]

testSetVar :: TestTree
testSetVar = testGroup "setVar"
    [ testCase "Set +" $ do
        primEnv <- primitiveBindings
        getRes <- runExceptT $ setVar primEnv "+" $ Number 0
        isRight getRes @? "Failed to set + in primEnv"
        let Right v = getRes
        v @?= Number 0
        toCheck <- runExceptT $ getVar primEnv "+"
        isRight toCheck @? "Couldn't get + after setting +"
        let Right v = toCheck
        v @?= Number 0
    , testCase "Set x in primEnv" $ do
        primEnv <- primitiveBindings
        getRes <- runExceptT $ setVar primEnv "x" $ Char 'a'
        isLeft getRes @? "Succeeded setting x in primEnv"
        let Left e = getRes
        e @?= UnboundVar "[Set]" "x"
    , testCase "Set x in nullEnv" $ do
        nullEnv <- nullEnv
        res <- runExceptT $ setVar nullEnv "x" $ String "test"
        isLeft res @? "Succeeded setting x in nullEnv"
        let Left e = res
        e @?= UnboundVar "[Set]" "x"
    ]

testDefineVar :: TestTree
testDefineVar = testGroup "defineVar"
    [ testCase "Define x 5" $ do
        primEnv <- primitiveBindings
        res <- runExceptT $ defineVar primEnv "x" $ Number 5
        isRight res @? "Failed to define x in primEnv"
        let Right v = res
        v @?= Number 5
        check <- runExceptT $ getVar primEnv "x"
        isRight check @? "Failed to get x after define"
        let Right v = check
        v @?= Number 5
    , testCase "Define + 5" $ do
        primEnv <- primitiveBindings
        res <- runExceptT $ defineVar primEnv "+" $ Char '+'
        isRight res @? "Failed to define '+' in primEnv"
        let Right v = res
        v @?= Char '+'
        check <- runExceptT $ getVar primEnv "+"
        isRight check @? "Failed to get + after define"
        let Right v = check
        v @?= Char '+'
    ]

testBindVar :: TestTree
testBindVar = testGroup "bindVar"
    [ testCaseSteps "bindVar in nullEnv" $ \step -> do
        step "Bind x to 5"
        env <- nullEnv
        env <- bindVar env "x" $ Number 5

        step "Check env size"
        map <- readIORef env
        Map.size map @?= 1

        step "Verify binding"
        check <- runExceptT $ getVar env "x"
        isRight check @? "Failed to get x after bind"
        let Right v = check
        v @?= Number 5
    , testCaseSteps "bindVar in primEnv" $ \step -> do
        step "Bind list to '(1 2)"
        env <- primitiveBindings
        oldSize <- do
            map <- readIORef env
            return $ Map.size map
        env <- bindVar env "list" $ makeImmutableList [Number 1, Number 2]

        step "Check env size"
        map <- readIORef env
        Map.size map @?= oldSize

        step "Verify binding"
        check <- runExceptT $ getVar env "list"
        isRight check @? "Failed to get list after bind"
        check ?= Right (makeImmutableList [Number 1, Number 2])
    ]
