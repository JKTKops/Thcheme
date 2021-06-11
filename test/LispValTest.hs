{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module LispValTest (lispValTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series

import Data.Char (isAscii)
import Data.Complex (Complex(..))
import Data.List (isPrefixOf)
import Data.IORef (newIORef)
import Data.Ratio (numerator, denominator)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import Control.Monad (liftM2, (>=>))
import Control.Monad.Trans (lift)
import System.IO (stdout)

import Val
import EvaluationMonad (unsafeEMtoIO, liftIO)
import System.IO.Unsafe (unsafePerformIO) -- be careful! for arbitrary instance
import Types (extractValue) -- these functions can probably be removed entirely.
import Primitives
import Primitives.WriteLib (writeSharedSH, ushowString)
import Types.Port (pStdout)

instance Serial IO Val where
    series = cons1 (Symbol . pack)
                \/ (series >>= lift . unsafeEMtoIO . makeMutableList)
                \/ (makeImmutableList <$> series)
                \/ cons1 Number
                \/ cons1 (IString . pack)
                \/ (series >>= lift . fmap String . newIORef . pack)
                \/ cons1 Char
                \/ cons1 Bool
                \/ cons0 Undefined
                \/ cons0 Nil
                \/ cons0 EOF
                \/ (series >>= lift . fmap Vector . V.thaw . V.fromList)
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
                \/ cons2 (\str name -> UnboundVar str (pack name))
                \/ cons1 (EvaluateDuringInit . pack)
                \/ cons1 Default
                \/ cons0 Quit

instance Monad m => Serial m Arity where
    series = cons1 Exactly
          \/ cons1 AtLeast
          \/ cons2 Between

instance Monad m => Serial m Number where
    series = cons1 Real \/ cons1 Complex

instance Monad m => Serial m RealNumber where
    series = cons1 Bignum \/ cons1 Ratnum \/ cons1 Flonum

instance Arbitrary Val where
    arbitrary = sized lispval'
      where lispval' 0 = oneof simpleCons
            lispval' n = oneof $ simpleCons ++
                            [ do num <- choose (0, n)
                                 unsafePerformIO 
                                    . unsafeEMtoIO 
                                    . makeMutableList 
                                  <$> vectorOf num subval
                            , do num <- choose (0, n)
                                 unsafePerformIO
                                   . fmap Vector . V.thaw . V.fromList
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
            simpleCons = [ Symbol . pack <$> arbitrary
                         , Number <$> arbitrary
                         , IString . pack <$> arbitrary
                         , Char <$> arbitrary
                         , Bool <$> arbitrary
                         ]

instance Arbitrary LispErr where
    arbitrary = oneof [ do n <- choose (0, 3)
                           NumArgs <$> arbitrary <*> vectorOf n arbitrary
                      , liftM2 TypeMismatch arbitrary arbitrary
                      , BadSpecialForm <$> arbitrary
                      , liftM2 NotFunction arbitrary arbitrary
                      , liftM2 UnboundVar arbitrary (pack <$> arbitrary)
                      , EvaluateDuringInit . pack <$> arbitrary
                      , Default <$> arbitrary
                      , return Quit
                      ]

instance Arbitrary Arity where
    arbitrary = oneof 
      [ Exactly . QC.getPositive <$> arbitrary
      , AtLeast . QC.getPositive <$> arbitrary
      , mkBetween <$> arbitrary <*> arbitrary
      ]
      where
        mkBetween lo hi = Between (QC.getPositive lo) (QC.getPositive hi)

instance Arbitrary Number where
    arbitrary = oneof [Real <$> arbitrary, Complex <$> arbitrary]

instance Arbitrary RealNumber where
    arbitrary = oneof
      [ Bignum <$> arbitrary
      , Ratnum <$> arbitrary
      , Flonum <$> arbitrary
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
testShowPort :: TestTree
testShowPort = testCase "Ports show correctly" $
    (show :: Val -> String) (Port pStdout) @?= "#<port>"

testShowPrimitives :: TestTree
testShowPrimitives = testCase "Primitives show correctly" $
    mapM_
        (\(key, func) -> 
            let str = case func of
                  Primitive arity _ _ -> "#<procedure "
                                           ++ symbolAsString key ++ " "
                                           ++ displayArity arity
                                           ++ ">"
                  PrimMacro {} -> "#<macro " ++ symbolAsString key ++ ">"

            in show func @?= str)
        -- Test only some of the primitives list as eventually it will be quite large
        . take 50 $ Map.toList primitives
  where                  
    displayArity (Exactly n) = "(" ++ simple n ++ ")"
    displayArity (Between lo hi) = "(" ++ between lo hi ++ ")"
    displayArity (AtLeast 0) = "_"
    displayArity (AtLeast n) = "(" ++ simple n ++ " . _)"

    simple 0 = ""
    simple 1 = "_"
    simple n = "_ " ++ simple (n - 1)

    between 0 0  = ""
    between 0 1  = "[_]"
    between 0 hi = "[_] " ++ between 0 (hi - 1)
    between n hi = "_ " ++ between (n-1) (hi - 1)

testShowFunctions :: TestTree
testShowFunctions = testCase "Functions show correctly" $
    do let emptyEnv = []
       show (Closure [] Nothing [] emptyEnv (Just "testFunc"))
         @?= "#<procedure testFunc ()>"
       show (Closure ["x"] Nothing [] emptyEnv (Just "testFunc"))
         @?= "#<procedure testFunc (x)>"
       show (Closure [] Nothing [] emptyEnv Nothing)
         @?= "#<procedure lambda ()>"
       show (Closure ["x"] Nothing [] emptyEnv Nothing)
         @?= "#<procedure lambda (x)>"
       show (Closure [] (Just "xs") [] emptyEnv (Just "testFunc"))
         @?= "#<procedure testFunc xs>"
       show (Closure ["x"] (Just "xs") [] emptyEnv (Just "testFunc"))
         @?= "#<procedure testFunc (x . xs)>"
       show (Closure ["x"] (Just "xs") [] emptyEnv Nothing)
         @?= "#<procedure lambda (x . xs)>"
       show (Closure ["x", "y", "z"] (Just "others") [] emptyEnv Nothing)
         @?= "#<procedure lambda (x y z . others)>"

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
        (==) <$> unsafeEMtoIO (referenceShowV input) <*> writeSharedSH input
  where referenceShowV = freezeList >=> referenceShow
        -- im not sure that this is right if s has non-ascii characters
        -- TODO [r7rs]: it's explicitly wrong, but so is 'write' atm.
        referenceShow (FNotList (Symbol s)) = pure $ unpack s
        referenceShow (FNotList EOF)        = pure   "#<eof>"
        referenceShow (FNotList (Number n)) = pure $ showNumber n
        referenceShow (FNotList (IString s)) = pure $ ushowString (unpack s) ""
        referenceShow (FNotList (Char c)) = pure $ case c of
            ' '  -> "#\\space"
            '\t' -> "#\\tab"
            '\n' -> "#\\newline"
            '\r' -> "#\\carriage-return"
            _    -> "#\\" ++ [c]
        referenceShow (FNotList (Bool b)) = pure $
            if b then "#t" else "#f"
        referenceShow (FNotList (Vector v)) =
            (\es -> "#(" ++ unwords es ++ ")")
            <$> (liftIO (V.freeze v) >>= mapM referenceShowV . V.toList)

        referenceShow (FList vs) = (\es -> "(" ++ unwords es ++ ")") 
            <$> mapM referenceShowV vs
        referenceShow (FDottedList vs v) =
            (\es e -> "(" ++ unwords es ++ ". " ++ e ++ ")")
            <$> mapM referenceShowV vs <*> referenceShowV v
        --    "(" ++ unwords (map show vs) ++ " . " ++ show v ++ ")"

        showNumber (Real (Bignum i)) = show i
        showNumber (Real (Ratnum r))
          | denominator r == 1 = show $ numerator r
          | otherwise = show (numerator r)
                      ++ "/"
                      ++ show (denominator r)
        showNumber (Real (Flonum f))
          | isInfinite f = sign : "inf.0"
          | isNaN f      = "+nan.0"
          | otherwise    = show f
          where sign = if f < 0 then '-' else '+'
        showNumber (Complex (r :+ i))
          | isExactZero $ Real i = showNumber (Real r)
          | isExactZero $ Real r = showNumber (Real i) ++ "i"

          -- minus sign will be given since i is negative
          | i < 0 || isNegativeZero i = showNumber (Real r)
                                      ++ showNumber (Real i)
                                      ++ "i"
          | otherwise = showNumber (Real r)
                      ++ (if isInfinite i || isNaN i then "" else "+")
                      ++ showNumber (Real i)
                      ++ "i"

prop_ShowErrPrefix :: TestTree
prop_ShowErrPrefix = SC.testProperty "Showing LispErr is prefixed with 'Error:'" $
    changeDepth (const 3) $
        \input -> "Error: " `isPrefixOf` show (input :: LispErr)
