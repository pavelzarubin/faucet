{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module FaucetClient where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Faucet
import GHC.Generics
import Network.HTTP.Req
import PAB
import Plutus.PAB.Webserver.Types
import Wallet.Emulator.Wallet (knownWallet)

data ActivateContractParams t = ActivateContractParams
  { contents :: t,
    tag :: String
  }
  deriving (FromJSON, ToJSON, Generic)

startTest :: IO ()
startTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson testStartInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "sale started" ++ (show response)
        else "error starting"

testStartInstance :: ContractActivationArgs (ActivateContractParams StartParams)
testStartInstance = ContractActivationArgs {caID = ActivateContractParams (StartParams 100_000_000 123 456) "Init", caWallet = Just (knownWallet 1)}

grabTest :: IO ()
grabTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "127.0.0.1" /: "api" /: "contract" /: "activate")
      (ReqBodyJson testGrabInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "sale started" ++ (show response)
        else "error starting"

testGrabInstance :: ContractActivationArgs (ActivateContractParams FaucetParams)
testGrabInstance = ContractActivationArgs {caID = ActivateContractParams (FaucetParams 456) "Grab", caWallet = Just (knownWallet 2)}
