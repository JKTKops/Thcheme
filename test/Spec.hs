import Test.Tasty

import BootstrapTest
import LispValTest
import EnvironmentTest
import ParsersTest
import EvaluationTest

main :: IO ()
main = putStrLn "%expect 7" >> defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ bootstrapTests
    , lispValTests
    , environmentTests
    , parsersTests
    , evaluationTests
    ]
