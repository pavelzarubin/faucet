module Main where

import Spec.Trace (grabForceTests, testsFail1, testsFail2, testsFail3, testsSuc)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "faucet tests"
      [ testsSuc,
        testsFail1,
        testsFail2,
        grabForceTests,
        testsFail3
      ]
