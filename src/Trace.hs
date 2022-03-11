{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor (void)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Faucet
import Ledger.Value hiding (tokenName)
import Plutus.Contract.Test
import Plutus.Trace

testReach2Ada :: IO ()
testReach2Ada = runEmulatorTraceIO traceReach2Ada

traceReach2Ada :: EmulatorTrace ()
traceReach2Ada = do
  let sp =
        StartParams
          { newAmount = 10_000_000,
            keyOne = 123,
            keyTwo = 456,
            tokenName = "AAA"
          }
  h1 <- activateContractWallet (knownWallet 1) $ runFaucet sp
  void $ waitNSlots 2
  nft <- getNFT h1
  void $ waitNSlots 2

  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  callEndpoint @"grab" h2 $ FaucetParams 123 nft
  void $ waitNSlots 2

  Extras.logInfo @String "END OF TRACE"
  where
    getNFT :: ContractHandle (Last AssetClass) StartSchema Text -> EmulatorTrace AssetClass
    getNFT h = do
      l <- observableState h
      case l of
        Last Nothing -> throwError $ GenericError "TOKEN NOT CREATED"
        Last (Just nft) -> Extras.logInfo @String (show nft) >> return nft
