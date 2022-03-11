{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module FaucetClient where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Data.UUID
import Faucet
import GHC.Generics
import Ledger.Value hiding (tokenName)
import Network.HTTP.Req
import PAB
import Plutus.PAB.Webserver.Types
import Text.Printf
import Wallet.Emulator.Wallet
import Wallet.Types

data ActivateContractParams t = ActivateContractParams
  { contents :: t,
    tag :: String
  }
  deriving (FromJSON, ToJSON, Generic)

startTest :: StartParams -> Wallet -> IO ()
startTest sp wallet = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson $ ContractActivationArgs {caID = ActivateContractParams sp "Init", caWallet = Just wallet})
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "sale started" ++ (show response)
        else "error starting"

grabTest :: FaucetParams -> Wallet -> IO ()
grabTest fp wallet = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson $ ContractActivationArgs {caID = ActivateContractParams fp "Grab", caWallet = Just wallet})
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "funds grabbed" ++ (show response)
        else "error starting"

startSimulatorTest :: IO ()
startSimulatorTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson testStartSimulatorInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "sale started" ++ (show response)
        else "error starting"

testStartSimulatorInstance :: ContractActivationArgs (ActivateContractParams StartParams)
testStartSimulatorInstance = ContractActivationArgs {caID = ActivateContractParams (StartParams 100_000_000 123 456 "AAA") "Init", caWallet = Just (knownWallet 3)}

grabSimulatorTest :: IO ()
grabSimulatorTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson testGrabSimulatorInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "funds grabbed" ++ (show response)
        else "error starting"

testGrabSimulatorInstance :: ContractActivationArgs (ActivateContractParams FaucetParams)
testGrabSimulatorInstance = ContractActivationArgs {caID = ActivateContractParams (FaucetParams 456 nftTest) "Grab", caWallet = Just (knownWallet 4)}

nftTest :: AssetClass
nftTest = AssetClass ("9441d44110f119f51a39adf898fe6436d20c15a71b96aaac242a5731", "AAA")
