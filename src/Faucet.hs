{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}


module Faucet where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text,pack)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude
 
import qualified Plutus.Contracts.Currency as PCC
--import Playground.Contract

data FaucetDatum = FaucetDatum
                    { fstApi :: !Integer
                    , sndApi :: !Integer
                    } deriving (Show, ToJSON, FromJSON,Generic, ToSchema)


PlutusTx.unstableMakeIsData ''FaucetDatum


data FaucetRedeemer =  Use | Update deriving (Show, ToJSON, FromJSON,Generic, ToSchema)


PlutusTx.unstableMakeIsData ''FaucetRedeemer

{-mkFaucetValidator :: FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
mkFaucetValidator dat red ctx = traceIfFalse "expected exactly one script input" hasOneScriptInput 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasOneScriptInput :: Bool
        hasOneScriptInput = let
                                xs = filter (isJust . Ledger.toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
                            in length xs == 1 -}


mkFaucetValidator :: FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
mkFaucetValidator dat red ctx = case (red,dat) of 
                                    (Update, _) -> True
                                    (Use  , _) -> True

                                    
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasOneScriptInput :: Bool
        hasOneScriptInput = let
                                xs = filter (isJust . Ledger.toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
                            in length xs == 1 

data Fauceting
instance Scripts.ValidatorTypes Fauceting where
    type instance DatumType Fauceting = FaucetDatum
    type instance RedeemerType Fauceting = FaucetRedeemer

typedValidator :: Scripts.TypedValidator Fauceting
typedValidator = Scripts.mkTypedValidator @Fauceting
    $$(PlutusTx.compile [|| mkFaucetValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
        where
            wrap = Scripts.wrapValidator @FaucetDatum @FaucetRedeemer



faucetValidator :: Validator
faucetValidator = Scripts.validatorScript  typedValidator

faucetAddress :: Address
faucetAddress = scriptAddress faucetValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator


findFaucet k utxos  = Map.toList $ Map.filter f utxos
  where
    f = \ch -> case _ciTxOutDatum ch of
              Left _ -> False
              Right (Datum d) -> case (PlutusTx.fromBuiltinData d :: Maybe FaucetDatum) of
                                    Nothing -> False
                                    Just (FaucetDatum k1 k2)-> k1 == k || k2 == k
findRightFaucet utxos  = f <$> Map.toList utxos
  where f (oref,ch) = case _ciTxOutDatum ch of
              Left _ -> Nothing
              Right (Datum d) -> PlutusTx.fromBuiltinData d :: Maybe FaucetDatum


newtype FaucetParams = FaucetParams {fpApiKey :: Integer} deriving (Generic, ToJSON, FromJSON, ToSchema)

grab :: FaucetParams -> Contract w s Text () 
grab fp = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- utxosAt faucetAddress
  let listOfOrefAndCh =  findFaucet (fpApiKey fp) utxos
      listOfDatums = findRightFaucet utxos
  case listOfOrefAndCh of
    [(oref, ch)] -> do
                logInfo @String $ "Right api key founded"
                let lookups = Constraints.unspentOutputs utxos <>
                              Constraints.otherScript (faucetValidator)
                    tx = Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData Use
                ledgerTx <- submitTxConstraintsWith @Fauceting lookups tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "collected gifts"
    something ->  logInfo @String $ "no faucets "  ++ (show listOfDatums)


type FaucetSchema = Endpoint "grab" FaucetParams .\/ Endpoint "update" UpdateParams .\/ Endpoint "start" ()


endpoints :: Contract () FaucetSchema Text ()
endpoints = awaitPromise (grab' `select` start' `select` update') >> endpoints
  where
    grab' = endpoint @"grab" grab
    update' = endpoint @"update" updateFaucet
    start' = endpoint @"start" $ const startFaucet






startFaucet :: Contract w s Text ()
startFaucet = do

  logInfo @String $ "started faucet" 
  logInfo @String $ "script address: " ++ show faucetAddress


data UpdateParams = UpdateParams { newAmount :: !Integer, newDat :: FaucetDatum} deriving (Generic, ToJSON, FromJSON, ToSchema)

updateFaucet :: UpdateParams -> Contract w s Text ()
updateFaucet (UpdateParams amount newKeys) = do
  m <- utxosAt faucetAddress
  let c = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData newKeys) $ Ada.lovelaceValueOf amount
  case Map.toList m of
    [] -> do
      ledgerTx <- submitTxConstraints typedValidator c
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "set new keys to " ++ (show $ fstApi newKeys) ++ " and " ++ (show $ sndApi newKeys)
      logInfo @String $ "script address: " ++ show faucetAddress
    _ -> logError @String "faucet already started"
      
    

