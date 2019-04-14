import Test.Tasty

import BootstrapTest
import LispValTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ bootstrapTests
    , lispValTests
    ]
