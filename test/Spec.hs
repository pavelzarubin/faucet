module Main where

import Spec.Trace
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "faucet tests" [testsSuc, testsFail]
