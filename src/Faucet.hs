{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Faucet where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema)
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Semigroup (..), Show (..), String)

-- Датум, храним в нем оба два ключа апи
-- Datum, contains api keys
data FaucetDatum = FaucetDatum
  { fstApi :: !Integer,
    sndApi :: !Integer
  }
  deriving (Show, ToJSON, FromJSON, Generic, ToSchema)

PlutusTx.unstableMakeIsData ''FaucetDatum

-- Редимер, отражает варианты выплат
-- Redeemer, contains payment options
data FaucetRedeemer
  = Key1
      { key :: !Integer,
        phk :: !PubKeyHash
      }
  | Key2
      { key :: !Integer,
        phk :: !PubKeyHash
      }
  | AnotherKey
      { phk :: !PubKeyHash
      }
  deriving (Show, ToJSON, FromJSON, Generic)

PlutusTx.unstableMakeIsData ''FaucetRedeemer

-- Валидатор. Проверяет что всего может быть два входа и число ADA на выходе, оставшихся на скрипте, в соответвии с ключами.
-- Validator сhecks that there can be a total of two inputs and the number of ADA on the output left on the script, according to the keys.
mkFaucetValidator :: FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
mkFaucetValidator dat red ctx =
  traceIfFalse "num of more than 2" twoInputs
    && case red of
      (Key1 key1 phk1) ->
        traceIfFalse "api key not equal to first api key" (key1 == fstApi dat)
          && traceIfFalse "wrong lovelace paid" (oneOfOutputsMustWith phk1 2_000_000 inputFunds)
      (Key2 key2 phk2) ->
        traceIfFalse "api key not equal to second api key" (key2 == sndApi dat)
          && traceIfFalse "wrong lovelace paid" (oneOfOutputsMustWith phk2 3_000_000 inputFunds)
      (AnotherKey phk3) -> traceIfFalse "wrong lovelace paid" (oneOfOutputsMustWith phk3 1_000_000 inputFunds)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputsTr :: [TxInInfo]
    inputsTr = txInfoInputs info

    outputsTr :: [TxOut]
    outputsTr = txInfoOutputs info

    twoInputs :: Bool
    twoInputs = length inputsTr <= 2

    inputFunds :: Maybe Integer
    inputFunds = do
      txin <- findOwnInput ctx
      return $ getLovelace . fromValue . txOutValue . txInInfoResolved $ txin

    oneOfOutputsMustWith :: PubKeyHash -> Integer -> Maybe Integer -> Bool
    oneOfOutputsMustWith p i (Just m) = any f outputsTr && m >= i
      where
        f x = (getLovelace . fromValue . txOutValue $ x) == (m - i) && (toPubKeyHash . txOutAddress $ x) /= (Just p)
    oneOfOutputsMustWith _ _ Nothing = False

------------------------------------------------------------------
data Fauceting

instance Scripts.ValidatorTypes Fauceting where
  type DatumType Fauceting = FaucetDatum
  type RedeemerType Fauceting = FaucetRedeemer

typedValidator :: Scripts.TypedValidator Fauceting
typedValidator =
  Scripts.mkTypedValidator @Fauceting
    $$(PlutusTx.compile [||mkFaucetValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @FaucetDatum @FaucetRedeemer

faucetValidator :: Validator
faucetValidator = Scripts.validatorScript typedValidator

faucetAddress :: Address
faucetAddress = scriptAddress faucetValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

------------------------------------------------------------------

-- Функция фильтрации UTxO. Ищет только те, которые соответсвуют нашему датуму
-- Filter function. The function searches for UTXOs that match the datum
findFaucet :: (Map.Map TxOutRef ChainIndexTxOut) -> [(TxOutRef, ChainIndexTxOut)]
findFaucet utxos = Map.toList $ Map.filter f utxos
  where
    f = \ch -> case _ciTxOutDatum ch of
      Left _ -> False
      Right (Datum d) -> case (PlutusTx.fromBuiltinData d :: Maybe FaucetDatum) of
        Nothing -> False
        Just _ -> True

-- Еще одна функция фильтрации. Использовалась для дополнительного логгирования. Можно убрать.
-- One more filter function. Was used for an additional log. Can be removed.
findRightFaucet :: (Map.Map TxOutRef ChainIndexTxOut) -> [Maybe FaucetDatum]
findRightFaucet utxos = f <$> Map.toList utxos
  where
    f (_, ch) = case _ciTxOutDatum ch of
      Left _ -> Nothing
      Right (Datum d) -> PlutusTx.fromBuiltinData d :: Maybe FaucetDatum

-- Функция для опредленеия редимера.
-- A function for determining the redeemer.
selectRedeemer :: PubKeyHash -> Integer -> ChainIndexTxOut -> Maybe (FaucetRedeemer, FaucetDatum)
selectRedeemer pkh k ch = case _ciTxOutDatum ch of
  Left _ -> Nothing
  Right (Datum d) -> case (PlutusTx.fromBuiltinData d :: Maybe FaucetDatum) of
    Nothing -> Nothing
    Just (FaucetDatum k1 k2) ->
      if k1 == k
        then Just (Key1 k pkh, (FaucetDatum k1 k2))
        else
          if k2 == k
            then Just (Key2 k pkh, (FaucetDatum k1 k2))
            else Just (AnotherKey pkh, (FaucetDatum k1 k2))

-- Апи ключ, который используем когда хотим получить Ada
-- The api key we use when we want to get Ada
newtype FaucetParams = FaucetParams {fpApiKey :: Integer} deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Функция grab. Получение Ada от скрипта
-- Grab function. Getting Ada from a script
grab :: FaucetParams -> Contract w s Text ()
grab fp = do
  pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
  utxos <- utxosAt faucetAddress -- Получение неизрасходованных выходов транзакции по адресу. Get the unspent transaction outputs at an address.
  let listOfOrefAndCh = findFaucet utxos -- Фильтрация. Filtration
      listOfDatums = findRightFaucet utxos
  case listOfOrefAndCh of
    [(oref, ch)] -> do
      logInfo @String $ "Faucet founded"
      case selectRedeemer pkh (fpApiKey fp) ch of -- Проверка api ключа. Cheking api key
        Nothing -> logError @String "Bad datum"
        Just (red, dat) -> do
          let lookups =
                Constraints.unspentOutputs utxos
                  <> Constraints.otherScript (faucetValidator)
              -- Ограничение, в котором говорим что скрипт нам должен выплатить все что у него есть
              -- A constraints saying that the script must pay us everything it has
              tx = Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData red
              tx' = case red of
                Key1 _ _ -> returnMerg ch dat 2_000_000
                Key2 _ _ -> returnMerg ch dat 3_000_000
                AnotherKey _ -> returnMerg ch dat 1_000_000
          ledgerTx <- submitTxConstraintsWith @Fauceting lookups (tx <> tx')
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ "collected gifts using " ++ show red
    _ -> logInfo @String $ "no faucets " ++ (show listOfDatums)
  where
    -- Получение, числа Ada на скрипте
    -- Getting, Ada number on the script
    amount a = getLovelace $ fromValue $ _ciTxOutValue a
    -- Вычисление сдачи, в соответсвии с апи ключом
    -- Calculation of change, according to the api key
    returnMerg b dat n = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData dat) $ Ada.lovelaceValueOf (amount b - n)

{-grabForce :: Contract w s Text ()
grabForce = do
  pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
  utxos <- utxosAt faucetAddress
  let listOfOrefAndCh = findFaucet utxos
  case listOfOrefAndCh of
    [(oref, _)] -> do
      logInfo @String $ "Faucet founded"
      let lookups =
            Constraints.unspentOutputs utxos
              <> Constraints.otherScript (faucetValidator)
          -- Ограничение, в котором говорим что скрипт нам должен выплатить все что у него есть
          -- A constraints saying that the script must pay us everything it has
          tx = Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (Key1 1 pkh)
      {-tx' = case red of
        Key1 -> returnMerg ch dat 2_000_000
        Key2 -> returnMerg ch dat 3_000_000
        AnotherKey -> returnMerg ch dat 1_000_000 -}
      ledgerTx <- submitTxConstraintsWith @Fauceting lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "collected gifts"
    _ -> logInfo @String $ "no faucets" -}

grabWithError :: FaucetParams -> Contract w s Text ()
grabWithError fp = handleError (\e -> logError @String $ "Catching error: " ++ show e) (grab fp)

-- Схема
-- Schema
type FaucetSchema = Endpoint "grab" FaucetParams .\/ Endpoint "start" StartParams -- .\/ Endpoint "grabForce" ()

endpoints :: Contract () FaucetSchema Text ()
endpoints = awaitPromise (grab' `select` start') >> endpoints
  where
    grab' = endpoint @"grab" grabWithError
    start' = endpoint @"start" startFaucet

-- grabForce' = endpoint @"grabForce" $ const grabForce

-- Создание контракта раздачи
-- Creating Faucet's contract
createFaucetContract :: Contract w s Text ()
createFaucetContract = do
  logInfo @String $ "started faucet"
  logInfo @String $ "script address: " ++ show faucetAddress

-- Изменение параметров. Добавление Ada и ключей.
-- Changing Parameters. Adding Ada and keys.
data StartParams = StartParams {newAmount :: !Integer, newDat :: FaucetDatum} deriving (Generic, ToJSON, FromJSON, ToSchema)

updateFaucet :: StartParams -> Contract w s Text ()
updateFaucet (StartParams amount newKeys) = do
  m <- utxosAt faucetAddress
  let c = Constraints.mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData newKeys) $ Ada.lovelaceValueOf amount
  case Map.toList m of
    [] -> do
      ledgerTx <- submitTxConstraints typedValidator c
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "set new keys to " ++ (show $ fstApi newKeys) ++ " and " ++ (show $ sndApi newKeys)
      logInfo @String $ "script address: " ++ show faucetAddress
    _ -> logError @String "faucet already started"

-- Старт раздающего
-- Faucet start
startFaucet :: StartParams -> Contract w s Text ()
startFaucet up = handleError (\e -> logError @String $ "Catching error: " ++ show e) (createFaucetContract >> updateFaucet up)
