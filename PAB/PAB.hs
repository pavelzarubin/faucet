{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module PAB where

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
