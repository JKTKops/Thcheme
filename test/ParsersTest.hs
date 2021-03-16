{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module ParsersTest (parsersTests) where

import Prelude hiding (getChar)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Complex (Complex(..))
import Data.Maybe
import Data.Either
import qualified Data.Vector as V

import Control.Arrow (first)

import Parsers
import Val
import LispValTest ()
import EvaluationTest ((?=))

pattern IList xs <- (unrollList -> Just xs)
  where IList xs = makeImmutableList xs

pattern IDottedList xs x <- (unrollDList -> Just (xs, x))
  where IDottedList xs x = makeImproperImmutableList xs x

parsersTests :: TestTree
parsersTests = testGroup "Parser tests" [unitTests, propTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ stringParserTests
    , atomParserTests
    , numberParserTests
    , charParserTests
    , listParserTests
    , dotListParserTests
    , vectorParserTests
    , endToEndTests
    ]

propTests :: TestTree
propTests = testGroup "Property tests" [qcTests]

qcTests :: TestTree
qcTests = testGroup "(QuickCheck)"
    [ prop_QuoteParser
    , prop_QuasiquoteParser
    , prop_UnquoteParser
    , prop_UnquoteSplicingParser
    ]

-- END TO END TESTS
endToEndTests = testGroup "End to End" $ map testParseExpr
    [ mkE2Etest
        { testName = "#d Number"
        , input = "12"
        , expectedContents = Just $ Number 12
        }
    , mkE2Etest
        { testName = "#x, negative number"
        , input = "#x-ef"
        , expectedContents = Just $ Number (-239)
        }
    , mkE2Etest
        { testName = "#o, number with +"
        , input = "#o+55"
        , expectedContents = Just $ Number 45
        }
    , mkE2Etest
        { testName = "Char"
        , input = "#\\f"
        , expectedContents = Just $ Char 'f'
        }
    , mkE2Etest
        { testName = "Regular Symbol"
        , input = "symbol"
        , expectedContents = Just $ Symbol "symbol"
        }
    , mkE2Etest
        { testName = "#t"
        , input = "#t"
        , expectedContents = Just $ Bool True
        }
    , mkE2Etest
        { testName = "#f"
        , input = "#f"
        , expectedContents = Just $ Bool False
        }
    , mkE2Etest
        { testName = "Simple string"
        , input = "\"string\""
        , expectedContents = Just $ IString "string"
        }
    , mkE2Etest
        { testName = "Quoted atom"
        , input = "'$name"
        , expectedContents = Just $ IList [Symbol "quote", Symbol "$name"]
        }
    , mkE2Etest
        { testName = "Quoted char"
        , input = "'#\\*"
        , expectedContents = Just $ IList [Symbol "quote", Char '*']
        }
    , mkE2Etest
        { testName = "Immediately nested quote forms"
        , input = "`,4"
        , expectedContents = Just $ IList [Symbol "quasiquote", IList
                                          [Symbol "unquote", Number 4]]
        }
    , mkE2Etest
        { testName = "Quasiquoted mess"
        , input = "`(,(foo) ,@(bar) x 1)"
        , expectedContents = Just $
            IList [Symbol "quasiquote", IList
                  [ IList [Symbol "unquote", IList [Symbol "foo"]]
                  , IList [Symbol "unquote-splicing", IList [Symbol "bar"]]
                  , Symbol "x"
                  , Number 1
                  ]]
        }
    , mkE2Etest
        { testName = "Nested List"
        , input = "(f \"x\" #\\5 (+ 3))"
        , expectedContents = Just $
            IList [Symbol "f", IString "x", Char '5', IList
                  [Symbol "+", Number 3]
            ]
        }
    , mkE2Etest
        { testName = "List with []"
        , input = "[+ 1 2]"
        , expectedContents = Just $
            IList [Symbol "+", Number 1, Number 2]
        }
    , mkE2Etest
        { testName = "List with {}"
        , input = "{test-func #\\a 0}"
        , expectedContents = Just $
            IList [Symbol "test-func", Char 'a', Number 0]
        }
    , mkE2Etest
        { testName = "Regular dotted List"
        , input = "(f 5 #\\$ . (1 2 \"test\"))"
        , expectedContents = Just $
            IList [Symbol "f", Number 5, Char '$'
                  , Number 1, Number 2, IString "test"]
        }
    , mkE2Etest
        { testName = "Dotted with nil"
        , input = "(+ 0 . ())"
        , expectedContents = Just $ IList [Symbol "+", Number 0]
        }
    , mkE2Etest
        { testName = "Irregular dotted List"
        , input = "(f 0 . m)"
        , expectedContents = Just $ IDottedList [Symbol "f", Number 0] (Symbol "m")
        }
    , mkE2Etest
        { testName = "Degenerate dotted List"
        , input = "(. xs)"
        , expectedContents = Just $ Symbol "xs"
        }
    , mkE2Etest
        { testName = "Empty vector"
        , input = "#()"
        , expectedContents = Just $ IVector V.empty
        }
    , mkE2Etest
        { testName = "regular vector"
        , input = "#(\"test\" 0 atom)"
        , expectedContents = Just $ IVector $ V.fromList
            [IString "test", Number 0, Symbol "atom"]
        }
    , mkE2Etest
        { testName = "Irregular spacing"
        , input = "(f 5 #\\$(1 2 #x45) #b10(test))"
        , expectedContents = Just $
            IList
                [Symbol "f", Number 5, Char '$', IList
                    [Number 1, Number 2, Number 69]
                , Number 2, IList
                    [Symbol "test"]
                ]
        }
    , mkE2Etest
        { testName = "Invalid #d number and invalid atom"
        , input = "(+ 1 5b)"
        }
    , mkE2Etest
        { testName = "Invalid symbol"
        , input = "symb`ol"
        }
    , mkE2Etest
        { testName = "Too few parens"
        , input = "(test-func 1 (cons (car xs) (cdr xs))"
        }
    , mkE2Etest
        { testName = "Too many parens"
        , input = "(test-func \"t\" car xs))"
        }
    , mkE2Etest
        { testName = "Extra input after list expr"
        , input = "(test-func + 0) randomwords"
        }
    , mkE2Etest
        { testName = "Extra input after normal expr"
        , input = "#\\c (test-func 0 1)"
        }
    ]

mkE2Etest :: TestBuilder Val
mkE2Etest = TB End2End "" "" Nothing

testParseExpr :: TestBuilder Val -> TestTree
testParseExpr testBuilder =
    testCaseSteps (testName testBuilder) $ \step -> do
        step "Parsing"
        let pInput = input testBuilder
            parse = run pInput

        if isJust $ expectedContents testBuilder
        then do
            step "Verifying success"
            isRight parse @? "Parse failed on: " ++ pInput

            let val = fromRight undefined parse

            step "Verifying parse"
            -- TODO: we should also verify that the result is
            -- appropriately mutable/immutable.
            Right val ?= Right (fromJust $ expectedContents testBuilder)
        else do
            step "Verifying failure"
            isLeft parse @? "Parse succeeded on: " ++ pInput

-- UNIT TESTS
stringParserTests = testGroup "Parsing Strings" $ map testStringParser
    [ mkStringTest
        { testName = "Simple string"
        , input = "\"simple test string\""
        , expectedContents = Just "simple test string"
        }
    , mkStringTest
        { testName = "Missing start quote"
        , input = "simple test string\""
        }
    , mkStringTest
        { testName = "Missing end quote"
        , input = "\"simple test string"
        }
    , mkStringTest
        { testName = "Unquoted"
        , input = "simple test string"
        }
    , mkStringTest
        { testName = "String with \\"
        , input = "\"test\\\\string\""
        , expectedContents = Just "test\\string"
        }
    , mkStringTest
        { testName = "String with \""
        , input =  "\"test \\\"string in a string\\\" string\""
        , expectedContents = Just "test \"string in a string\" string"
        }
    , mkStringTest
        { testName = "String with newline"
        , input = "\"test string\\nwith two lines\""
        , expectedContents = Just "test string\nwith two lines"
        }
    , mkStringTest
        { testName = "String with carriage return"
        , input = "\"test string \\r with carriage ret\""
        , expectedContents = Just "test string \r with carriage ret"
        }
    , mkStringTest
        { testName = "String with tab"
        , input = "\"test string\\t with tab\""
        , expectedContents = Just "test string\t with tab"
        }
    ]

atomParserTests :: TestTree
atomParserTests = testGroup "Parsing Symbols" $ map testSymbolParser
    [ let symbol = "atom" in mkSymbolTest
        { testName = "Simple atom"
        , input = symbol
        , expectedContents = Just symbol
        }
    , let symbol = "#name" in mkSymbolTest
        { testName = "Symbol with #"
        , input = symbol
        }
    , let symbol = "!@#$%&*-_=+|:\\/?<>~" in mkSymbolTest
        { testName = "Symbol with all symbols"
        , input = symbol
        , expectedContents = Just symbol
        }
    , let symbol = "t1" in mkSymbolTest
        { testName = "Symbol with digit"
        , input = symbol
        , expectedContents = Just symbol
        }
    , mkSymbolTest
        { testName = "Symbol starts with digit"
        , input = "5a"
        }
    , mkSymbolTest
        { testName = "Symbol with brackets"
        , input = "na[]me"
        }
    , mkSymbolTest
        { testName = "Symbol with braces"
        , input = "na{}me"
        }
    , mkSymbolTest
        { testName = "Symbol with invalid symbols"
        , input = "AlmostValidSymbol`name"
        }
    , let symbol = "Complex#Valid15Symbol\\Name8?" in mkSymbolTest
        { testName = "Complex valid atom"
        , input = symbol
        , expectedContents = Just symbol
        }
    , mkSymbolTest
        { testName = "vertical bar symbol"
        , input = "|H\\x65;llo|"
        , expectedContents = Just "Hello"
        }
    ]

numberParserTests :: TestTree
numberParserTests = testGroup "Parsing Numbers" $ map testNumberParser
    [ mkNumTest
        { testName = "25"
        , input = "25"
        , expectedContents = Just 25
        }
   , mkNumTest
        { testName = "-2"
        , input = "-2"
        , expectedContents = Just (-2)
        }
    , mkNumTest
        { testName = "+15"
        , input = "+15"
        , expectedContents = Just 15
        }
    , mkNumTest
        { testName = "#d80915"
        , input = "#d80915"
        , expectedContents = Just 80915
        }
    , mkNumTest
        { testName = "#d-100"
        , input = "#d-100"
        , expectedContents = Just (-100)
        }
    , mkNumTest
        { testName = "#d+06"
        , input = "#d+06"
        , expectedContents = Just 6
        }
    , mkNumTest
        { testName = "#b1010"
        , input = "#b1010"
        , expectedContents = Just 10
        }
    , mkNumTest
        { testName = "#b-11101"
        , input = "#b-11101"
        , expectedContents = Just (-29)
        }
    , mkNumTest
        { testName = "#b+1011"
        , input = "#b+1011"
        , expectedContents = Just 11
        }
    , mkNumTest
        { testName = "#o517"
        , input = "#o517"
        , expectedContents = Just 335
        }
    , mkNumTest
        { testName = "#o-24"
        , input = "#o-24"
        , expectedContents = Just (-20)
        }
    , mkNumTest
        { testName = "#o+36"
        , input = "#o+36"
        , expectedContents = Just 30
        }
    , mkNumTest
        { testName = "#xf4"
        , input = "#xf4"
        , expectedContents = Just 244
        }
    , mkNumTest
        { testName = "#x-cb8"
        , input = "#x-cb8"
        , expectedContents = Just (-3256)
        }
    , mkNumTest
        { testName = "#x+aa1"
        , input = "#x+aa1"
        , expectedContents = Just 2721
        }
    , mkNumTest
        { testName = "0.5"
        , input = "0.5"
        , expectedContents = Just (Real (Flonum 0.5))
        }
    , mkNumTest
        { testName = "#e0.5"
        , input = "#e0.5"
        , expectedContents = Just (Real (Ratnum 0.5))
        }
    , mkNumTest
        { testName = "infinity"
        , input = "+inf.0"
        , expectedContents = Just (Real (Flonum (1/0)))
        }
    , mkNumTest
        { testName = "rational"
        , input = "3/5"
        , expectedContents = Just (Real (Ratnum (3/5)))
        }
    , mkNumTest
        { testName = "rectangular complex"
        , input = "1+1i"
        , expectedContents = Just (Complex (1 :+ 1))
        }
    , mkNumTest
        { testName = "rectangular with scientific notation"
        , input = "2e+5+3i"
        , expectedContents = Just (Complex (2e+5 :+ 3))
        }
    , mkNumTest
        -- this is _almost_ a peculiar identifier, but the dot subsequent
        -- cannot be a number.
        { testName = "werid rectangular 0"
        , input = "-.0i"
        , expectedContents = Just (Complex (0 :+ (-0.0)))
        }
    , mkNumTest
        { testName = "15a3"
        , input = "15a3"
        }
    , mkNumTest
        { testName = "#b1201"
        , input = "#b1201"
        }
    , mkNumTest
        { testName = "#bnx"
        , input = "#bnx"
        }
    , mkNumTest
        { testName = "#o358"
        , input = "#o358"
        }
    , mkNumTest
        { testName = "#oa"
        , input = "#oa"
        }
    , mkNumTest
        { testName = "#x54mk"
        , input = "#x54mk"
        }
    , mkNumTest
        { testName = "Empty string"
        , input = ""
        }
    ]

charParserTests :: TestTree
charParserTests = testGroup "Parsing Chars" $ map testCharParser
    [ mkCharTest
        { testName = "Basic single character"
        , input = "#\\k"
        , expectedContents = Just 'k'
        }
    , mkCharTest
        { testName = "Basic single character"
        , input = "#\\$"
        , expectedContents = Just '$'
        }
    , mkCharTest
        { testName = "Basic single character"
        , input = "#\\`"
        , expectedContents = Just '`'
        }
    , mkCharTest
        { testName = "Too many characters"
        , input = "#\\ab"
        }
    , mkCharTest
        { testName = "No \\"
        , input = "#8"
        }
    , mkCharTest
        { testName = "Space"
        , input = "#\\space"
        , expectedContents = Just ' '
        }
    , mkCharTest
        { testName = "Newline"
        , input = "#\\newline"
        , expectedContents = Just '\n'
        }
    , mkCharTest
        { testName = "Carriage return"
        , input = "#\\return"
        , expectedContents = Just '\r'
        }
    , mkCharTest
        { testName = "Tab"
        , input = "#\\tab"
        , expectedContents = Just '\t'
        }
    , mkCharTest
        { testName = "escaped unicode"
        , input = "#\\x3bb"
        , expectedContents = Just 'λ'
        }
    ]

listParserTests :: TestTree
listParserTests = testGroup "Parsing Lists"
    [ testCase "\"(5 a #\\a)\"" $ do
        let p = run "(5 a #\\a)"
        isRight p @? "Parse failed."
        let val = unrollList $ fromRight undefined p
        isJust val @? "Parse did not return a list."
        let correct = case fromJust val of
                [Number 5, Symbol "a", Char 'a'] -> True
                _ -> False
        correct @? "Contents of the list are wrong: " ++ show (fromJust val)
    ]

dotListParserTests :: TestTree
dotListParserTests = testGroup "Parsing Dotted Lists"
    [ testCase "\"(5 a . #\\a)\"" $ do
        let p = run "(5 a . #\\a)"
        isRight p @? "Parse failed."
        let Right val = p
            mpair = unrollDList val
        isJust mpair @? "Parse did not return a dotted list."
        let correct = case fromJust mpair of
              ([Number 5, Symbol "a"], Char 'a') -> True
              _ -> False
        correct @? "Contents of the dotted list are wrong: " ++ show val
    ]

vectorParserTests :: TestTree
vectorParserTests = testGroup "Parsing vectors"
    [ testCase "\"#(1 #\\x ())\"" $ do
        let p = run "#(1 #\\x ())"
        val <- verify p
        fromJust val @?= V.fromList [Number 1, Char 'x', Nil]
    , testCase "\"#()\"" $ do
        let p = run "#()"
        val <- verify p
        fromJust val @?= V.empty
    ]
  where verify p = do
            isRight p @? "Parse failed."
            let val = case fromRight undefined p of
                    IVector v -> Just v
                    _ -> Nothing
            isJust val @? "Parse did not return an IVector."
            return val

-- PROPERTY TESTS
prop_QuoteParser = testProperty "tick always results in quote" $
    withMaxSuccess 50 $ \input ->
        let result = run ('\'' : show (input :: Val))
        in isRight result ==> case result of
            Right (IPair (Symbol "quote") _) -> True
            _ -> False

prop_QuasiquoteParser = testProperty "backtick always results in quasiquote" $
    withMaxSuccess 50 $ \input ->
        let result = run ('`' : show (input :: Val))
        in isRight result ==> case result of
            Right (IPair (Symbol "quasiquote") _)  -> True
            _ -> False

prop_UnquoteParser = testProperty "comma always results in unquote" $
    withMaxSuccess 50 $ \input ->
        let result = run (',' : clean (show (input :: Val)))
        in isRight result ==> case result of
            Right (IPair (Symbol "unquote") _)  -> True
            _ -> False
  -- if we don't do this, a LispString/Symbol that starts with '@' will turn it into
  -- an unquote splicing, which happened in a test at least once.
  where clean = dropWhile (== '@')

prop_UnquoteSplicingParser = testProperty "comma@ always results in unquote-splicing" $
    withMaxSuccess 50 $ \input ->
        let result = run (",@" ++ show (input :: Val))
        in isRight result ==> case result of
            Right (IPair (Symbol "unquote-splicing") _)  -> True
            _ -> False

run :: String -> Either LispErr Val
run = labeledReadExpr ""

parseSucceeds :: String -> Bool
parseSucceeds s = isRight $ run s

testStringParser :: TestBuilder String -> TestTree
testStringParser tb = testParser tb getString

testSymbolParser :: TestBuilder String -> TestTree
testSymbolParser tb = testParser tb getSymbol

testNumberParser :: TestBuilder Number -> TestTree
testNumberParser tb = testParser tb getNumber

testCharParser :: TestBuilder Char -> TestTree
testCharParser tb = testParser tb getChar

data TestType = TString | TSymbol | TNumber | TChar | End2End
instance Show TestType where
    show TString = "String"
    show TSymbol = "Symbol"
    show TNumber = "Number"
    show TChar   = "Char"
    show End2End = "End to End"

data TestBuilder a = TB { testType :: TestType
                        , testName :: String
                        , input :: String
                        , expectedContents :: Maybe a
                        }

mkStringTest, mkSymbolTest, mkNumTest, mkCharTest :: TestBuilder a
mkStringTest = TB TString "String Parser Test" "" Nothing
mkSymbolTest = TB TSymbol "Symbol Parser Test" "" Nothing
mkNumTest = TB TNumber "Number Parser Test" "" Nothing
mkCharTest = TB TChar "Char Parser Test" "" Nothing

testParser :: (Eq a, Show a)
           => TestBuilder a
           -> (Val -> Maybe a)
           -> TestTree
testParser testBuilder decons =
    testCaseSteps (testName testBuilder) $ \step -> do
        step "Parsing"
        let parse = run $ input testBuilder

        if isJust $ expectedContents testBuilder
        then do
            step "Verifying success"
            isRight parse @? "Parse failed on: " ++ input testBuilder

            step "Verifying type"
            let val = (decons . fromRight undefined) parse

            isJust val @? "Parse did not produce a "
                          ++ show (testType testBuilder) ++ "."

            step "Verifying contents"
            fromJust val @?= fromJust (expectedContents testBuilder)
        else do
            step "Verifying failure"
            isLeft parse @? "Parse succeeded on: " ++ input testBuilder

getString :: Val -> Maybe String
getString (IString s) = Just s
getString _          = Nothing

getSymbol :: Val -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _        = Nothing

getNumber :: Val -> Maybe Number
getNumber (Number n) = Just n
getNumber _ = Nothing

getChar :: Val -> Maybe Char
getChar (Char c) = Just c
getChar _        = Nothing

unrollList :: Val -> Maybe [Val]
unrollList (IPair c d) =
    (c:) <$> unrollList d
unrollList Nil = Just []
unrollList _ = Nothing

unrollDList :: Val -> Maybe ([Val], Val)
unrollDList (IPair c d) =
    first (c:) <$> unrollDList d
unrollDList Nil = Nothing -- not dotted
unrollDList obj = Just ([], obj)
