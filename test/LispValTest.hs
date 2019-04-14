{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module LispValTest (lispValTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Arbitrary
import Test.SmallCheck.Series

import Control.Monad (liftM2)

import LispVal

instance Monad m => Serial m LispVal where
    series = cons1 Atom
                \/ cons1 List
                \/ cons2 DottedList
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
                \/ cons1 Default
                \/ cons0 Quit

instance Arbitrary LispVal where
    arbitrary = sized lispval'
      where lispval' 0 = oneof simpleCons
            lispval' n = oneof $ simpleCons ++
                            [ do num <- choose (0, n)
                                 List <$> vectorOf num subval
                            , do num <- choose (0, n)
                                 liftM2 DottedList (vectorOf num subval) subval
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
                      , Default <$> arbitrary
                      , return Quit
                      ]

lispValTests :: TestTree
lispValTests = testGroup "LispVal" [propTests]

propTests :: TestTree
propTests = testGroup "Property tests"
    [ prop_TrapErrorOutputsRight
    ]


-- PROPERTY TESTS
prop_TrapErrorOutputsRight = QC.testProperty "Trap error evaluates to Right" $
    \input -> case trapError input of
        Left {}  -> False
        Right {} -> True


