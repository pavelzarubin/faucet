{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module PAB (runSimulator) where

import Control.Monad (void)
import Control.Monad.Freer (interpret)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Default (def)
import qualified Data.OpenApi as OpenApi
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Faucet
import GHC.Generics (Generic)
import Ledger.Value (TokenName)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Wallet.Emulator.Wallet (knownWallet)

data FaucetContracts = Init StartParams | Grab FaucetParams
  deriving stock (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema, ToJSON, FromJSON)

--deriving anyclass (OpenApi.ToSchema)

instance Pretty FaucetContracts where
  pretty = viaShow

instance Builtin.HasDefinitions FaucetContracts where
  getDefinitions = []
  getSchema = \case
    Init _ -> Builtin.endpointsToSchemas @StartSchema
    Grab _ -> Builtin.endpointsToSchemas @GrabSchema
  getContract = \case
    Init sp -> SomeBuiltin $ startFaucet @() sp
    Grab fp -> SomeBuiltin $ grabWithError @() fp

handlers :: SimulatorEffectHandlers (Builtin FaucetContracts)
handlers =
  Simulator.mkSimulatorHandlers def def $
    interpret (contractHandler Builtin.handleBuiltin)

runSimulator :: IO ()
runSimulator = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin FaucetContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        startParams =
          StartParams
            { newAmount = 10_000_000,
              newDat = FaucetDatum 123 456
            }
    void $ Simulator.activateContract wallet1 $ Init startParams

    Simulator.waitNSlots 1

    void $ Simulator.activateContract wallet2 $ Grab (FaucetParams 123)

    void $ Simulator.waitNSlots 1

    Simulator.logString @(Builtin FaucetContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin FaucetContracts) b

    shutdown
