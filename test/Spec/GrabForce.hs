{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.GrabForce (grabForceEndpoints) where

import Control.Monad (void)
import Data.Text (Text)
import Faucet (FaucetParams (..), FaucetRedeemer (..), Fauceting, faucetAddress, faucetValidator, findFaucet)
import Ledger
import Ledger.Constraints as Constraints
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import Prelude (Semigroup (..), String)

-- Новое действие, которое якобы забирает все ADA с скрипта. Используется для тестирования валидатора.
-- A new action that supposedly takes all ADA from the script. Used to test the validator.
grabForce :: FaucetParams -> Contract w s Text ()
grabForce FaucetParams {..} = do
  pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
  utxos <- utxosAt (faucetAddress fpToken)
  let listOfOrefAndCh = findFaucet utxos
  case listOfOrefAndCh of
    [(oref, _)] -> do
      logInfo @String $ "Faucet founded"
      let lookups =
            Constraints.unspentOutputs utxos
              <> Constraints.otherScript (faucetValidator fpToken)
          tx = Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (Key1 1 pkh)
      ledgerTx <- submitTxConstraintsWith @Fauceting lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "collected gifts"
    _ -> logInfo @String $ "no faucets"

type GrabForceSchema = Endpoint "grabForce" FaucetParams

grabForceEndpoints :: Contract () GrabForceSchema Text ()
grabForceEndpoints = awaitPromise (endpoint @"grabForce" grabForce) >> grabForceEndpoints
