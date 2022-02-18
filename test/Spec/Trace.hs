{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace (grabForceTests, testsFail1, testsFail2, testsSuc) where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Faucet
import Ledger.Ada
import Plutus.Contract.Test
import Plutus.Trace
import Spec.GrabForce
import Test.Tasty

--------------------------------
-- Tests with succesfull fauceting.
-- Тесты с успешной раздачей.
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
  Extras.logInfo $ "START TRACE"
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  Extras.logInfo $ "FIRST WALLET START FAUCET SCRIPT"
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        newDat = FaucetDatum 123 456
      }
  void $ waitNSlots 1
  Extras.logInfo $ "SECOND WALLET GRAB 1 ADA"
  callEndpoint @"grab" h2 $ FaucetParams 1
  void $ waitNSlots 1
  Extras.logInfo $ "SECOND WALLET GRAB 2 ADA"
  callEndpoint @"grab" h3 $ FaucetParams 123
  void $ waitNSlots 1
  Extras.logInfo $ "SECOND WALLET GRAB 3 ADA"
  callEndpoint @"grab" h4 $ FaucetParams 456
  void $ waitNSlots 1
  Extras.logInfo $ "END TRACE"

runMyTraceSuc :: IO ()
runMyTraceSuc = runEmulatorTraceIO' def emuConfSuc myTraceSuc

------------------------------------------------------------------------------------
-- Tests with failed fauceting. No ada on the script
-- Тесты с неудачной раздачей. Нет ada на скрипте
myTraceFail1 :: EmulatorTrace ()
myTraceFail1 = do
  Extras.logInfo $ "START TRACE"
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  Extras.logInfo $ "FIRST WALLET TRY START FAUCET SCRIPT, BUT CANT CAUSE HAS NOT 10 ADA"
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        newDat = FaucetDatum 123 456
      }
  void $ waitNSlots 1
  Extras.logInfo $ "SECOND WALLET TRY GRAB ADA, BUT SCRIPT HAS NOT ADA"
  callEndpoint @"grab" h2 $ FaucetParams 1
  void $ waitNSlots 1
  Extras.logInfo $ "END TRACE"

emuConfFail1 :: EmulatorConfig
emuConfFail1 =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 5_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

runMyTraceFail1 :: IO ()
runMyTraceFail1 = runEmulatorTraceIO' def emuConfFail1 myTraceFail1

testsFail1 :: TestTree
testsFail1 =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfFail1)
    "failed fauceting, because it is not possible to pass the script the right amount of ADA"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf 0)
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    myTraceFail1

------------------------------------------------------------------------------
-- Tests with failed fauceting. No ada on the script for paying to user
-- Тесты с неудачной раздачей. Нет достаточного количества ada на скрипте, чтобы отдать пользователю.
myTraceFail2 :: EmulatorTrace ()
myTraceFail2 = do
  Extras.logInfo $ "START TRACE"
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  Extras.logInfo $ "FIRST WALLET TRY FAUCET SCRIPT"
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 2_000_000,
        newDat = FaucetDatum 123 456
      }
  void $ waitNSlots 1
  Extras.logInfo $ "SECOND WALLET TRY GRAB ADA, BUT SCRIPT HAS NOT 3 ADA"
  callEndpoint @"grab" h2 $ FaucetParams 456
  void $ waitNSlots 1
  Extras.logInfo $ "END TRACE"

emuConfFail2 :: EmulatorConfig
emuConfFail2 =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 10_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

runMyTraceFail2 :: IO ()
runMyTraceFail2 = runEmulatorTraceIO' def emuConfFail2 myTraceFail2

testsFail2 :: TestTree
testsFail2 =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfFail2)
    "failed fauceting, because the script does not have the right amount of ADA "
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-2_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    myTraceFail2

-------------------------------------------------------

-- Тест с неудачной раздачей. Валидатор блокирует попытку украсть все с баланса скрипта.
-- A test with an unsuccessful fauceting. Validator blocks attempt to steal everything from script balance.
grabForceTrace :: EmulatorTrace ()
grabForceTrace = do
  Extras.logInfo $ "START TRACE"
  h1 <- activateContractWallet (knownWallet 1) grabForceEndpoints
  h2 <- activateContractWallet (knownWallet 2) grabForceEndpoints
  Extras.logInfo $ "FIRST WALLET TRY FAUCET SCRIPT"
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        newDat = FaucetDatum 123 456
      }
  void $ waitNSlots 1
  Extras.logInfo $ "SECOND WALLET TRY GRAB ADA, BUT VALIDATOR BLOCK WRONG ACTION"
  callEndpoint @"grabForce" h2 $ ()
  void $ waitNSlots 1
  Extras.logInfo $ "END TRACE"

runGrabForceTrace :: IO ()
runGrabForceTrace = runEmulatorTraceIO' def def grabForceTrace

grabForceTests :: TestTree
grabForceTests =
  checkPredicateOptions
    defaultCheckOptions
    "failed fauceting, because validator block bad action"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-10_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    grabForceTrace
