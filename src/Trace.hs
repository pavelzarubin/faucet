{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor (void)
import Faucet
import Plutus.Trace
import Wallet.Emulator.Wallet

testReach2Ada :: IO ()
testReach2Ada = runEmulatorTraceIO traceReach2Ada

traceReach2Ada :: EmulatorTrace ()
traceReach2Ada = do
  h1 <- activateContractWallet (knownWallet 1) startEndpoint
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        keyOne = 123,
        keyTwo = 456
      }
  void $ waitNSlots 1
  callEndpoint @"grab" h2 $ FaucetParams 123
  s <- void $ waitNSlots 2
  Extras.logInfo $ "reached " ++ show s

traceReach3Ada :: EmulatorTrace ()
traceReach3Ada = do
  h1 <- activateContractWallet (knownWallet 1) startEndpoint
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        keyOne = 123,
        keyTwo = 456
      }
  void $ waitNSlots 1
  callEndpoint @"grab" h2 $ FaucetParams 456
  s <- void $ waitNSlots 2
  Extras.logInfo $ "reached " ++ show s

traceReach1Ada :: EmulatorTrace ()
traceReach1Ada = do
  h1 <- activateContractWallet (knownWallet 1) startEndpoint
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        keyOne = 123,
        keyTwo = 456
      }
  void $ waitNSlots 1
  callEndpoint @"grab" h2 $ FaucetParams 1
  s <- void $ waitNSlots 2
  Extras.logInfo $ "reached " ++ show s

traceReach1AdaWithTwoSchemas :: EmulatorTrace ()
traceReach1AdaWithTwoSchemas = do
  h1 <- activateContractWallet (knownWallet 1) startEndpoint
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  callEndpoint @"start" h1 $
    StartParams
      { newAmount = 10_000_000,
        keyOne = 123,
        keyTwo = 456
      }
  void $ waitNSlots 1
  callEndpoint @"grab" h2 $ FaucetParams 1
  void $ waitNSlots 2
  h3 <- activateContractWallet (knownWallet 1) grabEndpoint
  callEndpoint @"grab" h3 $ FaucetParams 1
  s <- void $ waitNSlots 2
  Extras.logInfo $ "reached " ++ show s

testTraceReach1AdaWithTwoSchemas :: IO ()
testTraceReach1AdaWithTwoSchemas = runEmulatorTraceIO traceReach1AdaWithTwoSchemas
