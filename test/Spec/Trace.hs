{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Faucet
import Ledger.Ada
import Plutus.Contract.Test
import Plutus.Trace
import Test.Tasty

--------------------------------

testsSuc :: TestTree
testsSuc =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfSuc)
    "succesfull fauceting"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-10_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 1_000_000)
        .&&. walletFundsChange (knownWallet 3) (lovelaceValueOf 2_000_000)
        .&&. walletFundsChange (knownWallet 4) (lovelaceValueOf 3_000_000)
    )
    myTraceSuc

emuConfSuc :: EmulatorConfig
emuConfSuc =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000),
               (knownWallet 3, lovelaceValueOf 10_000_000),
               (knownWallet 4, lovelaceValueOf 10_000_000)
             ]
       )

myTraceSuc :: EmulatorTrace ()
myTraceSuc = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints

  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        newDat = FaucetDatum 123 456
      }
  void $ waitNSlots 1
  callEndpoint @"grab" h2 $ FaucetParams 1
  void $ waitNSlots 1
  callEndpoint @"grab" h3 $ FaucetParams 123
  void $ waitNSlots 1
  callEndpoint @"grab" h4 $ FaucetParams 456
  s <- waitNSlots 1
  Extras.logInfo $ "reached: " ++ show s

runMyTraceSuc :: IO ()
runMyTraceSuc = runEmulatorTraceIO' def emuConfSuc myTraceSuc

------------------------------------------------------------------------------------

myTraceFail :: EmulatorTrace ()
myTraceFail = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        newDat = FaucetDatum 123 456
      }
  void $ waitNSlots 1
  callEndpoint @"grab" h2 $ FaucetParams 1
  s <- waitNSlots 1
  Extras.logInfo $ "reached: " ++ show s

emuConfFail :: EmulatorConfig
emuConfFail =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 5_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

runMyTraceFail :: IO ()
runMyTraceFail = runEmulatorTraceIO' def emuConfFail myTraceFail

testsFail :: TestTree
testsFail =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfFail)
    "failed fauceting"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    myTraceFail
