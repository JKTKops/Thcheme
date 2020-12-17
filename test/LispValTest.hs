{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module LispValTest (lispValTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series

import Data.List (isPrefixOf)
import Data.IORef
import Data.Array
import qualified Data.HashMap.Strict as Map
import Control.Monad (liftM2, (>=>))
import Control.Monad.Trans (lift)
import System.IO (stdout)

import Val
import EvaluationMonad (unsafeEMtoIO)
import System.IO.Unsafe (unsafePerformIO) -- be careful! for arbitrary instance
import Types (extractValue) -- these functions can probably be removed entirely.
import Primitives

instance Serial IO Val where
    series = cons1 Atom
                \/ (series >>= lift . unsafeEMtoIO . makeMutableList)
                \/ (makeImmutableList <$> series)
                \/ cons1 Number
                \/ cons1 String
                \/ cons1 Char
                \/ cons1 Bool
                \/ cons0 Undefined
                -- vector?
                -- \/ cons3 Primitive
                -- Primitive function types left off for now
                -- \/ cons5 Closure
                -- Env is probably difficult/impossible to give Serial instance
                -- \/ cons1 Port
                -- Handle is probably difficult/impossible to give Serial instance

instance Serial IO LispErr where
    series = cons2 NumArgs
                \/ cons2 TypeMismatch
                -- Parser ParseError left off for now
                \/ cons1 BadSpecialForm
                \/ cons2 NotFunction
                \/ cons2 UnboundVar
                \/ cons1 EvaluateDuringInit
                \/ cons1 Default
                \/ cons0 Quit

instance Arbitrary Val where
    arbitrary = sized lispval'
      where lispval' 0 = oneof simpleCons
            lispval' n = oneof $ simpleCons ++
                            [ do num <- choose (0, n)
                                 unsafePerformIO 
                                    . unsafeEMtoIO 
                                    . makeMutableList 
                                  <$> vectorOf num subval
            -- TODO: there's quite a conundrum here. I'm not _super_
            -- concerned about being able to test immutable data. After
            -- all, all the implementations should mirror the mutable versions
            -- exactly anyway. However there's always the chance that I do
            -- something really dumb.
            -- However, it's an invariant that once we start making immutable
            -- data, everything that that data contains must also be immutable.
            -- How to encode into the Arbitrary instance? Perhaps we should
            -- choose mutable/immutable with probability favoring mutable
            -- and then we need a helper that only generates immutable data.
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
                           liftM2 NumArgs (return n) (vectorOf n arbitrary)
                      , liftM2 TypeMismatch arbitrary arbitrary
                      , BadSpecialForm <$> arbitrary
                      , liftM2 NotFunction arbitrary arbitrary
                      , liftM2 UnboundVar arbitrary arbitrary
                      , EvaluateDuringInit <$> arbitrary
                      , Default <$> arbitrary
                      , return Quit
                      ]

lispValTests :: TestTree
lispValTests = testGroup "Val" [unitTests, propTests]

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
    [ prop_TerminationErrors
    , prop_ShowVal
    ]

scTests :: TestTree
scTests = testGroup "(SmallCheck)"
    [ prop_ShowErrPrefix
    ]

-- UNIT TESTS
testShowPort = testCase "Ports show correctly" $
    show (Port stdout) @?= "#<port>"

testShowPrimitives = testCase "Primitives show correctly" $
    mapM_
        (\(key, func) -> let pType = case func of
                                 Primitive {} -> "function"
                                 PrimMacro {} -> "macro"
                         in show func @?= "#<" ++ pType ++ " " ++ key ++ ">")
        -- Test only some of the primitives list as eventually it will be quite large
        . take 50 $ Map.toList primitives

testShowFunctions :: TestTree
testShowFunctions = testCase "Functions show correctly" $
    do emptyEnv <- newIORef Map.empty
       show (Closure [] Nothing [] emptyEnv (Just "testFunc")) @?= "(testFunc () ...)"
       show (Closure ["x"] Nothing [] emptyEnv (Just "testFunc")) @?=
            "(testFunc (x) ...)"
       show (Closure [] Nothing [] emptyEnv Nothing) @?= "(lambda () ...)"
       show (Closure ["x"] Nothing [] emptyEnv Nothing) @?= "(lambda (x) ...)"
       show (Closure [] (Just "xs") [] emptyEnv (Just "testFunc")) @?=
            "(testFunc ( . xs) ...)"
       show (Closure ["x"] (Just "xs") [] emptyEnv (Just "testFunc")) @?=
            "(testFunc (x . xs) ...)"
       show (Closure ["x"] (Just "xs") [] emptyEnv Nothing) @?= "(lambda (x . xs) ...)"
       show (Closure ["x", "y", "z"] (Just "others") [] emptyEnv Nothing) @?=
            "(lambda (x y z . others) ...)"

-- PROPERTY TESTS
prop_TerminationErrors :: TestTree
prop_TerminationErrors = QC.testProperty "Only Quit is a termination error" $
    withMaxSuccess 50 $
        \input -> isTerminationError input == case input of
            Quit -> True
            _    -> False

prop_ShowVal :: TestTree
prop_ShowVal = QC.testProperty "Showing a Val produces correct string" $
    \input -> QC.ioProperty $
        (==) <$> unsafeEMtoIO (referenceShowV input) <*> showValIO input
            --Atom _        -> testShowAtom input
            --Number _      -> testShowNumber input
            --String _      -> testShowString input
            --Char _        -> testShowChar input
            --Bool _        -> testShowBool input
            --IList _        -> testShowList input
            --IDottedList {} -> testShowDottedList input
            --Nil -> ("()" == ) <$> showValIO input
            --Vector _      -> testShowVector input
            --_             -> True
  where referenceShowV = freezeList >=> referenceShow
      
        referenceShow (FNotList (Atom s)) = pure s
        referenceShow (FNotList (Number n)) = pure $ show n
        referenceShow (FNotList (String s)) = pure $ show s
        referenceShow (FNotList (Char c)) = pure $ case c of
            ' '  -> "#\\space"
            '\t' -> "#\\tab"
            '\n' -> "#\\newline"
            '\r' -> "#\\carriage-return"
            _    -> "#\\" ++ [c]
        referenceShow (FNotList (Bool b)) = pure $
            if b then "#t" else "#f"
        referenceShow (FNotList (Vector arr)) =
            (\es -> "#(" ++ unwords es ++ ")") 
            <$> mapM referenceShowV (elems arr)

        referenceShow (FList vs) = (\es -> "(" ++ unwords es ++ ")") 
            <$> mapM referenceShowV vs
        referenceShow (FDottedList vs v) =
            (\es e -> "(" ++ unwords es ++ ". " ++ e ++ ")")
            <$> mapM referenceShowV vs <*> referenceShowV v
        --    "(" ++ unwords (map show vs) ++ " . " ++ show v ++ ")"
      
        testShowAtom atomVal = show atomVal == let Atom s = atomVal in s
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
        --testShowList listVal = show listVal == let IList ls = listVal in
        --    "(" ++ unwords (map show ls) ++ ")"
        --testShowDottedList dListVal = show dListVal == let IDottedList ls l = dListVal in
        --    "(" ++ unwords (map show ls) ++ " . " ++ show l ++ ")"
        testShowVector vecVal = show vecVal == let Vector arr = vecVal in
            "#(" ++ unwords (map show $ elems arr) ++ ")"

prop_ShowErrPrefix :: TestTree
prop_ShowErrPrefix = SC.testProperty "Showing LispErr is prefixed with 'Error:'" $
    changeDepth (const 3) $
        \input -> "Error: " `isPrefixOf` show (input :: LispErr)
