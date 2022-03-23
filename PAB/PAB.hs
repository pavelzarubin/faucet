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

module PAB where

import Control.Monad (void)
import Control.Monad.Freer (interpret)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON)
import Data.Default (def)
import Data.Monoid (Last (..))
import qualified Data.OpenApi as OpenApi
import Faucet
import GHC.Generics (Generic)
import Ledger.Value (TokenName)
import Plutus.Contract (Empty)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Events.Contract
import Plutus.PAB.Run (runWith)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Prettyprinter (Pretty (..), viaShow)
import Wallet.Emulator.Wallet (knownWallet)

data FaucetContracts = Grab FaucetParams | Init StartParams
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (OpenApi.ToSchema, ToJSON, FromJSON)

instance Pretty FaucetContracts where
  pretty = viaShow

instance Builtin.HasDefinitions FaucetContracts where
  getDefinitions = []
  getSchema = \case
    Init _ -> Builtin.endpointsToSchemas @StartSchema
    Grab _ -> Builtin.endpointsToSchemas @GrabSchema
  getContract = \case
    Init sp -> SomeBuiltin $ runFaucet sp
    Grab fp -> SomeBuiltin $ grab @() @GrabSchema fp

handlers :: SimulatorEffectHandlers (Builtin FaucetContracts)
handlers =
  Simulator.mkSimulatorHandlers def $
    interpret (contractHandler Builtin.handleBuiltin)

runSimulator :: IO ()
runSimulator = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin FaucetContracts) "Starting plutus-starter PAB webserver on port 9080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        startParams =
          StartParams
            { newAmount = 100_000_000,
              keyOne = 123,
              keyTwo = 456,
              tokenName = "AAA"
            }
    startId <- Simulator.activateContract wallet1 $ Init startParams

    nft <- waitForLast startId

    liftIO $ writeFile "start.cid" $ show $ unContractInstanceId startId

    Simulator.waitNSlots 2

    giverId <- Simulator.activateContract wallet2 $ Grab (FaucetParams 123 nft)
    liftIO $ writeFile "giver.cid" $ show $ unContractInstanceId giverId
    void $ Simulator.waitNSlots 2

    void $ liftIO getLine
    void $ Simulator.waitNSlots 2

    Simulator.logString @(Builtin FaucetContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin FaucetContracts) b
    shutdown

startPAB = do
  runWith (Builtin.handleBuiltin @FaucetContracts)

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
  flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just x)) -> Just x
    _ -> Nothing
