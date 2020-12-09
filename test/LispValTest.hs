{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module LispValTest (lispValTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic as QC
import Test.SmallCheck.Series

import Data.List (isPrefixOf)
import Data.IORef
import Data.Array
import qualified Data.HashMap.Strict as Map
import Control.Monad (liftM2, (<=<))
import Control.Monad.Trans.Except (runExceptT)
import System.IO (stdout)

import Types
import Primitives

instance Monad m => Serial m LispVal where
    series = cons1 Atom
                \/ cons1 IList
                \/ cons2 IDottedList
                \/ cons1 Number
                \/ cons1 String
                \/ cons1 Char
                \/ cons1 Bool
                -- \/ cons3 Primitive
                -- \/ cons3 IOPrimitive
                -- Primitive function types left off for now
                -- \/ cons5 Func
                -- Env is probably difficult/impossible to give Serial instance
                -- \/ cons1 Port
                -- Handle is probably difficult/impossible to give Serial instance

instance Monad m => Serial m LispErr where
    series = cons2 NumArgs
                \/ cons2 TypeMismatch
                -- Parser ParseError left off for now
                \/ cons2 BadSpecialForm
                \/ cons2 NotFunction
                \/ cons2 UnboundVar
                \/ cons1 EvaluateDuringInit
                \/ cons1 Default
                \/ cons0 Quit

instance Arbitrary LispVal where
    arbitrary = sized lispval'
      where lispval' 0 = oneof simpleCons
            lispval' n = oneof $ simpleCons ++
                            [ do num <- choose (0, n)
                                 IList <$> vectorOf num subval
                            , do num <- choose (0, n)
                                 liftM2 IDottedList (vectorOf num subval) subval
                            ]
              where subval = lispval' $ n `div` 2
            simpleCons = [ Atom <$> arbitrary
                         , Number <$> arbitrary
                         , String <$> arbitrary
                         , Char <$> arbitrary
                         , Bool <$> arbitrary
                         ]

instance Arbitrary LispErr where
    arbitrary = oneof [ do n <- choose (0, 3)
                           liftM2 NumArgs (return $ toInteger n) (vectorOf n arbitrary)
                      , liftM2 TypeMismatch arbitrary arbitrary
                      , liftM2 BadSpecialForm arbitrary arbitrary
                      , liftM2 NotFunction arbitrary arbitrary
                      , liftM2 UnboundVar arbitrary arbitrary
                      , EvaluateDuringInit <$> arbitrary
                      , Default <$> arbitrary
                      , return Quit
                      ]

lispValTests :: TestTree
lispValTests = testGroup "LispVal" [unitTests, propTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testShowPort
    , testShowPrimitives
    , testShowFunctions
    ]

propTests :: TestTree
propTests = testGroup "Property tests" [qcTests, scTests]

qcTests :: TestTree
qcTests = testGroup "(QuickCheck)"
    [ prop_TrapErrorOutputsRight
    , prop_TrapErrorIdemp
    , prop_TerminationErrors
    , prop_ShowVal
    ]

scTests :: TestTree
scTests = testGroup "(SmallCheck)"
    [ prop_ShowErrPrefix
    ]

-- UNIT TESTS
testShowPort = testCase "Ports show correctly" $
    show (Port stdout) @?= "<port>"

testShowPrimitives = testCase "Primitives show correctly" $
    mapM_
        (\(key, func) -> let pType = case func of
                                 Primitive {} -> "function"
                                 PMacro {}     -> "macro"
                         in show func @?= "#<" ++ pType ++ " " ++ key ++ ">")
        -- Test only some of the primitives list as eventually it will be quite large
        . take 50 $ Map.toList primitives

testShowFunctions = testCase "Functions show correctly" $
    do emptyEnv <- newIORef Map.empty
       show (Func [] Nothing [] emptyEnv (Just "testFunc")) @?= "(testFunc () ...)"
       show (Func ["x"] Nothing [] emptyEnv (Just "testFunc")) @?=
            "(testFunc (x) ...)"
       show (Func [] Nothing [] emptyEnv Nothing) @?= "(lambda () ...)"
       show (Func ["x"] Nothing [] emptyEnv Nothing) @?= "(lambda (x) ...)"
       show (Func [] (Just "xs") [] emptyEnv (Just "testFunc")) @?=
            "(testFunc ( . xs) ...)"
       show (Func ["x"] (Just "xs") [] emptyEnv (Just "testFunc")) @?=
            "(testFunc (x . xs) ...)"
       show (Func ["x"] (Just "xs") [] emptyEnv Nothing) @?= "(lambda (x . xs) ...)"
       show (Func ["x", "y", "z"] (Just "others") [] emptyEnv Nothing) @?=
            "(lambda (x y z . others) ...)"

-- PROPERTY TESTS
prop_TrapErrorOutputsRight = QC.testProperty "Trap error evaluates to Right" $
    \input -> case trapError input of
        Left _  -> False
        Right _ -> True

prop_TrapErrorIdemp = QC.testProperty "trapError . trapError == trapError" $
    \input -> (extractValue . trapError . trapError) input == extractValue (trapError input)

prop_TerminationErrors = QC.testProperty "Only Quit is a termination error" $
    withMaxSuccess 50 $
        \input -> isTerminationError input == case input of
            (Left Quit) -> True
            _           -> False

prop_ShowVal = QC.testProperty "Showing a LispVal produces correct string" $
    withMaxSuccess 500 $
        \input -> case input of
            Atom _        -> testShowAtom input
            Number _      -> testShowNumber input
            String _      -> testShowString input
            Char _        -> testShowChar input
            Bool _        -> testShowBool input
            IList _        -> testShowList input
            IDottedList {} -> testShowDottedList input
            Vector _      -> testShowVector input
            _             -> True
  where testShowAtom atomVal = show atomVal == let Atom s = atomVal in s
        testShowNumber nVal = show nVal == let Number n = nVal in show n
        testShowString sVal = show sVal == let String s = sVal in show s
        testShowChar charVal = show charVal == let Char c = charVal in case c of
            ' '  -> "#\\space"
            '\t' -> "#\\tab"
            '\n' -> "#\\newline"
            '\r' -> "#\\carriage-return"
            _    -> "#\\" ++ [c]
        testShowBool boolVal = show boolVal == let Bool b = boolVal in
            if b
            then "#t"
            else "#f"
        testShowList listVal = show listVal == let IList ls = listVal in
            "(" ++ unwords (map show ls) ++ ")"
        testShowDottedList dListVal = show dListVal == let IDottedList ls l = dListVal in
            "(" ++ unwords (map show ls) ++ " . " ++ show l ++ ")"
        testShowVector vecVal = show vecVal == let Vector arr = vecVal in
            "#(" ++ unwords (map show $ elems arr) ++ ")"

prop_ShowErrPrefix = SC.testProperty "Showing LispErr is prefixed with 'Error:'" $
    changeDepth (const 3) $
        \input -> "Error: " `isPrefixOf` show (input :: LispErr)
