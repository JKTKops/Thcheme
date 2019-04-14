module BootstrapTest (bootstrapTests) where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit

import Data.IORef
import qualified Data.HashMap.Strict as Map

import Bootstrap
import Primitives

bootstrapTests :: TestTree
bootstrapTests = testGroup "Bootstrap" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testNullEnvEmpty
    , testPrimitiveBindingsSize
    , testPrimitiveEnvSameKeys
    ]

-- UNIT TESTS 
testNullEnvEmpty = testCase "Null environment is empty" $
    do env <- nullEnv >>= readIORef
       Map.null env @? "Null Environment is not empty"

testPrimitiveBindingsSize = testCase "Prim. env has correct size" $
    do env <- primitiveBindings >>= readIORef
       Map.size env @?= length primitives

testPrimitiveEnvSameKeys = testCase "Prim. env has same keys as prim. list" $
    do let keys = map fst $ Map.toList primitives
       env <- primitiveBindings >>= readIORef
       mapM_ (\key -> Map.member key env @? "Prim. env does not contain " ++ key) keys
