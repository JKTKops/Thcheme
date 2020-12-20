{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module ParsersTest (parsersTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Either
import qualified Data.Vector as V

import Control.Arrow (first)

import Parsers
import Parsers.Internal
import Val
import LispValTest ()
import EvaluationTest ((?=))

pattern IList xs <- (fromList -> Just xs)
  where IList xs = makeImmutableList xs

pattern IDottedList xs x <- (fromDList -> Just (xs, x))
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
    [ prop_CorrectSymbols
    , prop_QuoteParser
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
        { testName = "#h, negative number"
        , input = "-#hef"
        , expectedContents = Just $ Number (-239)
        }
    , mkE2Etest
        { testName = "#o, number with +"
        , input = "+#o55"
        , expectedContents = Just $ Number 45
        }
    , mkE2Etest
        { testName = "Char"
        , input = "#\\f"
        , expectedContents = Just $ Char 'f'
        }
    , mkE2Etest
        { testName = "Atom that looks like a Char"
        , input = "#\\ff"
        , expectedContents = Just $ Atom "#\\ff"
        }
    , mkE2Etest
        { testName = "Regular Atom"
        , input = "symbol"
        , expectedContents = Just $ Atom "symbol"
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
        , expectedContents = Just $ IList [Atom "quote", Atom "$name"]
        }
    , mkE2Etest
        { testName = "Quoted char"
        , input = "'#\\*"
        , expectedContents = Just $ IList [Atom "quote", Char '*']
        }
    , mkE2Etest
        { testName = "Immediately nested quote forms"
        , input = "`,4"
        , expectedContents = Just $ IList [Atom "quasiquote", IList
                                          [Atom "unquote", Number 4]]
        }
    , mkE2Etest
        { testName = "Quasiquoted mess"
        , input = "`(,(foo) ,@(bar) x 1)"
        , expectedContents = Just $
            IList [Atom "quasiquote", IList
                  [ IList [Atom "unquote", IList [Atom "foo"]]
                  , IList [Atom "unquote-splicing", IList [Atom "bar"]]
                  , Atom "x"
                  , Number 1
                  ]]
        }
    , mkE2Etest
        { testName = "Nested List"
        , input = "(f \"x\" #\\5 (+ 3))"
        , expectedContents = Just $
            IList [Atom "f", IString "x", Char '5', IList
                  [Atom "+", Number 3]
            ]
        }
    , mkE2Etest
        { testName = "List with []"
        , input = "[+ 1 2]"
        , expectedContents = Just $
            IList [Atom "+", Number 1, Number 2]
        }
    , mkE2Etest
        { testName = "List with {}"
        , input = "{test-func #\\a 0}"
        , expectedContents = Just $
            IList [Atom "test-func", Char 'a', Number 0]
        }
    , mkE2Etest
        { testName = "Regular dotted List"
        , input = "(f 5 #\\$ . (1 2 \"test\"))"
        , expectedContents = Just $
            IList [Atom "f", Number 5, Char '$'
                  , Number 1, Number 2, IString "test"]
        }
    , mkE2Etest
        { testName = "Dotted with nil"
        , input = "(+ 0 . ())"
        , expectedContents = Just $ IList [Atom "+", Number 0]
        }
    , mkE2Etest
        { testName = "Irregular dotted List"
        , input = "(f 0 . m)"
        , expectedContents = Just $ IDottedList [Atom "f", Number 0] (Atom "m")
        }
    , mkE2Etest
        { testName = "Degenerate dotted List"
        , input = "(. xs)"
        , expectedContents = Just $ Atom "xs"
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
            [IString "test", Number 0, Atom "atom"]
        }
    , mkE2Etest
        { testName = "Irregular spacing"
        , input = "(f 5 #\\$(1 2 #h45) #b10(test))"
        , expectedContents = Just $
            IList
                [Atom "f", Number 5, Char '$', IList
                    [Number 1, Number 2, Number 69]
                , Number 2, IList
                    [Atom "test"]
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
            parse = run (do v <- parseExpr; eof; return v) pInput

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
atomParserTests = testGroup "Parsing Atoms" $ map testAtomParser
    [ let symbol = "atom" in mkAtomTest
        { testName = "Simple atom"
        , input = symbol
        , expectedContents = Just symbol
        }
    , let symbol = "#name" in mkAtomTest
        { testName = "Atom with #"
        , input = symbol
        , expectedContents = Just symbol
        }
    , let symbol = "!@#$%&*-_=+|:\\/?<>~" in mkAtomTest
        { testName = "Atom with all symbols"
        , input = symbol
        , expectedContents = Just symbol
        }
    , let symbol = "t1" in mkAtomTest
        { testName = "Atom with digit"
        , input = symbol
        , expectedContents = Just symbol
        }
    , mkAtomTest
        { testName = "Atom starts with digit"
        , input = "5a"
        }
    , mkAtomTest
        { testName = "Atom with brackets"
        , input = "na[]me"
        }
    , mkAtomTest
        { testName = "Atom with braces"
        , input = "na{}me"
        }
    , mkAtomTest
        { testName = "Atom with invalid symbols"
        , input = "AlmostValidAtom`name"
        }
    , let symbol = "Complex#Valid15Atom\\Name8?" in mkAtomTest
        { testName = "Complex valid atom"
        , input = symbol
        , expectedContents = Just symbol
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
        { testName = "-#d100"
        , input = "-#d100"
        , expectedContents = Just (-100)
        }
    , mkNumTest
        { testName = "+#d06"
        , input = "+#d06"
        , expectedContents = Just 6
        }
    , mkNumTest
        { testName = "#b1010"
        , input = "#b1010"
        , expectedContents = Just 10
        }
    , mkNumTest
        { testName = "-#b11101"
        , input = "-#b11101"
        , expectedContents = Just (-29)
        }
    , mkNumTest
        { testName = "+#b1011"
        , input = "+#b1011"
        , expectedContents = Just 11
        }
    , mkNumTest
        { testName = "#o517"
        , input = "#o517"
        , expectedContents = Just 335
        }
    , mkNumTest
        { testName = "-#o24"
        , input = "-#o24"
        , expectedContents = Just (-20)
        }
    , mkNumTest
        { testName = "+#o36"
        , input = "+#o36"
        , expectedContents = Just 30
        }
    , mkNumTest
        { testName = "#hf4"
        , input = "#hf4"
        , expectedContents = Just 244
        }
    , mkNumTest
        { testName = "-#hcb8"
        , input = "-#hcb8"
        , expectedContents = Just (-3256)
        }
    , mkNumTest
        { testName = "+#haa1"
        , input = "+#haa1"
        , expectedContents = Just 2721
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
        { testName = "#h54mk"
        , input = "#h54mk"
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
        { testName = "No #"
        , input = "\\."
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
        , input = "#\\carriage-return"
        , expectedContents = Just '\r'
        }
    , mkCharTest
        { testName = "Tab"
        , input = "#\\tab"
        , expectedContents = Just '\t'
        }
    ]

listParserTests :: TestTree
listParserTests = testGroup "Parsing Lists"
    [ testCase "\"5 #a #\\a\"" $ do
        let p = parse parseList "" "5 #a #\\a"
        isRight p @? "Parse failed."
        let val = fromList $ fromRight undefined p
        isJust val @? "Parse did not return a list."
        let correct = case fromJust val of
                [Number 5, Atom "#a", Char 'a'] -> True
                _ -> False
        correct @? "Contents of the list are wrong: " ++ show (fromJust val)
    ]

dotListParserTests :: TestTree
dotListParserTests = testGroup "Parsing Dotted Lists"
    [ testCase "\"5 #a . #\\a\"" $ do
        let p = parse parseDottedList "" "5 #a . #\\a"
        isRight p @? "Parse failed."
        let Right val = p
            mpair = fromDList val
        isJust mpair @? "Parse did not return a dotted list."
        let correct = case fromJust mpair of
              ([Number 5, Atom "#a"], Char 'a') -> True
              _ -> False
        correct @? "Contents of the dotted list are wrong: " ++ show val
    ]

vectorParserTests :: TestTree
vectorParserTests = testGroup "Parsing vectors"
    [ testCase "\"#(1 #\\x ())\"" $ do
        let p = run parseVector "#(1 #\\x ())"
        val <- verify p
        fromJust val @?= V.fromList [Number 1, Char 'x', Nil]
    , testCase "\"#()\"" $ do
        let p = run parseVector "#()"
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
prop_CorrectSymbols = testProperty "Recognize correct set of symbols" $
    \input ->
        parseSucceeds symbol [input]
        == (input `elem` "!@#$%^&*-_=+|:\\/?<>~")

prop_QuoteParser = testProperty "tick always results in quote" $
    withMaxSuccess 50 $ \input ->
        let result = parse parseQuoted "" ('\'' : show (input :: Val))
        in isRight result ==> case result of
            Right (IPair (Atom "quote") _) -> True
            _ -> False

prop_QuasiquoteParser = testProperty "backtick always results in quasiquote" $
    withMaxSuccess 50 $ \input ->
        let result = parse parseQuoted "" ('`' : show (input :: Val))
        in isRight result ==> case result of
            Right (IPair (Atom "quasiquote") _)  -> True
            _ -> False

prop_UnquoteParser = testProperty "comma always results in unquote" $
    withMaxSuccess 50 $ \input ->
        let result = parse parseQuoted "" (',' : clean (show (input :: Val)))
        in isRight result ==> case result of
            Right (IPair (Atom "unquote") _)  -> True
            _ -> False
  -- if we don't do this, a LispString/Atom that starts with '@' will turn it into
  -- an unquote splicing, which happened in a test at least once.
  where clean = dropWhile (== '@')

prop_UnquoteSplicingParser = testProperty "comma@ always results in unquote-splicing" $
    withMaxSuccess 50 $ \input ->
        let result = parse parseQuoted "" (",@" ++ show (input :: Val))
        in isRight result ==> case result of
            Right (IPair (Atom "unquote-splicing") _)  -> True
            _ -> False

-- HELPER
run :: Parser a -> String -> Either ParseError a
run p = parse p ""

parseSucceeds :: Parser a -> String -> Bool
parseSucceeds p s = isRight $ parse p "" s

testStringParser :: TestBuilder String -> TestTree
testStringParser tb = testParser tb fromString

testAtomParser :: TestBuilder String -> TestTree
testAtomParser tb = testParser tb fromAtom

testNumberParser :: TestBuilder Integer -> TestTree
testNumberParser tb = testParser tb fromNumber

testCharParser :: TestBuilder Char -> TestTree
testCharParser tb = testParser tb fromChar

data TestType = TString | TAtom | TNumber | TChar | End2End
instance Show TestType where
    show TString = "String"
    show TAtom   = "Atom"
    show TNumber = "Number"
    show TChar   = "Char"
    show End2End = "End to End"

data TestBuilder a = TB { testType :: TestType
                        , testName :: String
                        , input :: String
                        , expectedContents :: Maybe a
                        }

mkStringTest, mkAtomTest, mkNumTest, mkCharTest :: TestBuilder a
mkStringTest = TB TString "String Parser Test" "" Nothing
mkAtomTest = TB TAtom "Atom Parser Test" "" Nothing
mkNumTest = TB TNumber "Number Parser Test" "" Nothing
mkCharTest = TB TChar "Char Parser Test" "" Nothing

testParser :: (Eq a, Show a)
           => TestBuilder a
           -> (Val -> Maybe a)
           -> TestTree
testParser testBuilder decons = let
    parser = case testType testBuilder of
        TString -> parseString
        TAtom   -> parseAtom
        TNumber -> parseNumber
        TChar   -> parseChar

    in testCaseSteps (testName testBuilder) $ \step -> do
        step "Parsing"
        let parse = run (do{ v <- parser; eof; return v}) $ input testBuilder

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

fromString :: Val -> Maybe String
fromString (IString s) = Just s
fromString _          = Nothing

fromAtom :: Val -> Maybe String
fromAtom (Atom s) = Just s
fromAtom _        = Nothing

fromNumber :: Val -> Maybe Integer
fromNumber (Number n) = Just n
fromNumber _          = Nothing

fromChar :: Val -> Maybe Char
fromChar (Char c) = Just c
fromChar _        = Nothing

fromList :: Val -> Maybe [Val]
fromList (IPair c d) =
    (c:) <$> fromList d
fromList Nil = Just []
fromList _ = Nothing

fromDList :: Val -> Maybe ([Val], Val)
fromDList (IPair c d) =
    first (c:) <$> fromDList d
fromDList Nil = Nothing -- not dotted
fromDList obj = Just ([], obj)
