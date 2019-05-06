module ParsersTest (parsersTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Either
import Data.Array

import Parsers
import Parsers.Internal
import Types
import LispValTest

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
    , prop_StringEscapes
    , prop_QuoteParser
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
        , expectedContents = Just $ String "string"
        }
    , mkE2Etest
        { testName = "Quoted atom"
        , input = "'$name"
        , expectedContents = Just $ List [Atom "quote", Atom "$name"]
        }
    , mkE2Etest
        { testName = "Quoted char"
        , input = "'#\\*"
        , expectedContents = Just $ List [Atom "quote", Char '*']
        }
    , mkE2Etest
        { testName = "Nested list"
        , input = "(f \"x\" #\\5 (+ 3))"
        , expectedContents = Just $
            List [Atom "f", String "x", Char '5', List
                [Atom "+", Number 3]
            ]
        }
    , mkE2Etest
        { testName = "List with []"
        , input = "[+ 1 2]"
        , expectedContents = Just $
            List [Atom "+", Number 1, Number 2]
        }
    , mkE2Etest
        { testName = "List with {}"
        , input = "{test-func #\\a 0}"
        , expectedContents = Just $
            List [Atom "test-func", Char 'a', Number 0]
        }
    , mkE2Etest
        { testName = "Regular dotted list"
        , input = "(f 5 #\\$ . (1 2 \"test\"))"
        , expectedContents = Just $
            List [Atom "f", Number 5, Char '$'
                 , Number 1, Number 2, String "test"]
        }
    , mkE2Etest
        { testName = "Dotted with nil"
        , input = "(+ 0 . ())"
        , expectedContents = Just $ List [Atom "+", Number 0]
        }
    , mkE2Etest
        { testName = "Irregular dotted list"
        , input = "(f 0 . m)"
        , expectedContents = Just $ DottedList [Atom "f", Number 0] (Atom "m")
        }
    , mkE2Etest
        { testName = "Degenerate dotted list"
        , input = "(. xs)"
        , expectedContents = Just $ Atom "xs"
        }
    , mkE2Etest
        { testName = "Empty vector"
        , input = "#()"
        , expectedContents = Just $ Vector (listArray (0, -1) [])
        }
    , mkE2Etest
        { testName = "regular vector"
        , input = "#(\"test\" 0 atom)"
        , expectedContents = Just $ Vector (listArray (0, 2)
            [String "test", Number 0, Atom "atom"])
        }
    , mkE2Etest
        { testName = "Irregular spacing"
        , input = "(f 5 #\\$(1 2 #h45) #b10(test))"
        , expectedContents = Just $
            List
                [Atom "f", Number 5, Char '$', List
                    [Number 1, Number 2, Number 69]
                , Number 2, List
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

mkE2Etest :: TestBuilder LispVal
mkE2Etest = TB End2End "" "" Nothing

testParseExpr :: TestBuilder LispVal -> TestTree
testParseExpr testBuilder =
    testCaseSteps (testName testBuilder) $ \step -> do
        step "Parsing"
        let pInput = input testBuilder
            parse = run (do{ v <- parseExpr; eof; return v}) $ pInput

        if isJust $ expectedContents testBuilder
        then do
            step "Verifying success"
            isRight parse @? "Parse failed on: " ++ pInput

            let val = fromRight undefined parse

            step "Verifying parse"
            val @?= fromJust (expectedContents testBuilder)
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
        { testName = "#d80915"
        , input = "#d80915"
        , expectedContents = Just 80915
        }
    , mkNumTest
        { testName = "-2"
        , input = "-2"
        , expectedContents = Just (-2)
        }
    , mkNumTest
        { testName = "-#d100"
        , input = "-#d100"
        , expectedContents = Just (-100)
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

listParserTests = testGroup "Parsing Lists"
    [ testCase "\"5 #a #\\a\"" $ do
        let p = parse parseList "" "5 #a #\\a"
        isRight p @? "Parse failed."
        let val = fromList $ fromRight undefined p
        isJust val @? "Parse did not return a List."
        let correct = case fromJust val of
                [Number 5, Atom "#a", Char 'a'] -> True
                _ -> False
        correct @? "Contents of the list are wrong: " ++ show (fromJust val)
    ]

dotListParserTests = testGroup "Parsing Dotted Lists"
    [ testCase "\"5 #a . #\\a\"" $ do
        let p = parse parseDottedList "" "5 #a . #\\a"
        isRight p @? "Parse failed."
        let val = case fromRight undefined p of
                DottedList ls l -> Just (ls, l)
                _ -> Nothing
        isJust val @? "Parse did not return a Dotted List."
        let correct = case fromJust val of
                ([Number 5, Atom "#a"], Char 'a') -> True
                _ -> False
        correct @? "Contents of the dotted list are wrong: " ++ show (fromJust val)
    ]

vectorParserTests = testGroup "Parsing vectors"
    [ testCase "\"#(1 #\\x ())\"" $ do
        let p = run parseVector "#(1 #\\x ())"
        isRight p @? "Parse failed."
        let val = case fromRight undefined p of
                Vector arr -> Just arr
                _ -> Nothing
        isJust val @? "Parse did not return a Vector."
        fromJust val @?= listArray (0, 2) [Number 1, Char 'x', List []]
    , testCase "\"#()\"" $ do
        let p = run parseVector "#()"
        isRight p @? "Parse failed."
        let val = case fromRight undefined p of
                Vector arr -> Just arr
                _ -> Nothing
        isJust val @? "Parse did not return a Vector."
        fromJust val @?= listArray (0, -1) []
    ]

-- PROPERTY TESTS
prop_CorrectSymbols = testProperty "Recognize correct set of symbols" $
    \input ->
        parseSucceeds symbol [input]
        == (input `elem` "!@#$%^&*-_=+|:\\/?<>~")

prop_StringEscapes = testProperty "Strings parse correct escape sequences" $
    \input ->
        parseSucceeds parseString ("\"\\" ++ [input] ++ "\"")
        == (input `elem` "\\\"nrt")

prop_QuoteParser = testProperty "parseQuoted always results in quote" $
    withMaxSuccess 50 $ \input ->
        let result = parse parseQuoted "" ('\'' : show (input :: LispVal))
        in isRight result ==> case result of
            Right (List (Atom "quote" : _)) -> True
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
           -> (LispVal -> Maybe a)
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

fromString :: LispVal -> Maybe String
fromString (String s) = Just s
fromString _          = Nothing

fromAtom :: LispVal -> Maybe String
fromAtom (Atom s) = Just s
fromAtom _        = Nothing

fromNumber :: LispVal -> Maybe Integer
fromNumber (Number n) = Just n
fromNumber _          = Nothing

fromChar :: LispVal -> Maybe Char
fromChar (Char c) = Just c
fromChar _        = Nothing

fromList :: LispVal -> Maybe [LispVal]
fromList (List ls) = Just ls
fromList _            = Nothing
