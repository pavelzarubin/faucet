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

--import Playground.Contract

-- Датум, храним в нем оба два ключа апи
-- Datum, contains api keys
data FaucetDatum = FaucetDatum
  { fstApi :: !Integer,
    sndApi :: !Integer
  }
  deriving (Show, ToJSON, FromJSON, Generic, ToSchema)

PlutusTx.unstableMakeIsData ''FaucetDatum

-- Редимер, отражает варианты выплат (пока дорабатываю валидатор)
-- Redeemer, contains payment options (validator in works)
data FaucetRedeemer = Key1 | Key2 | AnotherKey deriving (Show, ToJSON, FromJSON, Generic, ToSchema)

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

-- Валидатор, пока принимает все. (в разработке)
-- Validator, always accept all. (in works)
mkFaucetValidator :: FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
mkFaucetValidator _ _ _ = True

{-} traceIfFalse "has more the one script input" hasOneScriptInput
    && traceIfFalse "has no money" outputValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasOneScriptInput :: Bool
    hasOneScriptInput =
      let xs = filter (isJust . Ledger.toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
       in length xs == 1

    outputValue :: Bool
    outputValue = any (== 1_000_000) $ (getLovelace . fromValue . txOutValue) <$> txInfoOutputs info
-}
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
selectRedeemer :: Integer -> ChainIndexTxOut -> Maybe (FaucetRedeemer, FaucetDatum)
selectRedeemer k ch = case _ciTxOutDatum ch of
  Left _ -> Nothing
  Right (Datum d) -> case (PlutusTx.fromBuiltinData d :: Maybe FaucetDatum) of
    Nothing -> Nothing
    Just (FaucetDatum k1 k2) ->
      if k1 == k
        then Just (Key1, (FaucetDatum k1 k2))
        else
          if k2 == k
            then Just (Key2, (FaucetDatum k1 k2))
            else Just (AnotherKey, (FaucetDatum k1 k2))

-- Апи ключ, который используем когда хотим получить Ada
-- The api key we use when we want to get Ada
newtype FaucetParams = FaucetParams {fpApiKey :: Integer} deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Функция grab. Получение Ada от скрипта
-- Grab function. Getting Ada from a script
grab :: FaucetParams -> Contract w s Text ()
grab fp = do
  utxos <- utxosAt faucetAddress -- Получение неизрасходованных выходов транзакции по адресу. Get the unspent transaction outputs at an address.
  let listOfOrefAndCh = findFaucet utxos -- Фильтрация. Filtration
      listOfDatums = findRightFaucet utxos
  case listOfOrefAndCh of
    [(oref, ch)] -> do
      logInfo @String $ "Faucet founded"
      case selectRedeemer (fpApiKey fp) ch of -- Проверка api ключа. Cheking api key
        Nothing -> logError @String "Bad datum"
        Just (red, dat) -> do
          let lookups =
                Constraints.unspentOutputs utxos
                  <> Constraints.otherScript (faucetValidator)
              -- Ограничение, в котором говорим что скрипт нам должен выплатить все что у него есть
              -- A constraints saying that the script must pay us everything it has
              tx = Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData red
              tx' = case red of
                Key1 -> returnMerg ch dat 2_000_000
                Key2 -> returnMerg ch dat 3_000_000
                AnotherKey -> returnMerg ch dat 1_000_000
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

grabWithError :: FaucetParams -> Contract w s Text ()
grabWithError fp = handleError (\_ -> logError @String "Script has not money") (grab fp)

-- Схема
-- Schema
type FaucetSchema = Endpoint "grab" FaucetParams .\/ Endpoint "start" StartParams

endpoints :: Contract () FaucetSchema Text ()
endpoints = awaitPromise (grab' `select` start') >> endpoints
  where
    grab' = endpoint @"grab" grabWithError
    start' = endpoint @"start" startFaucet

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
startFaucet up = handleError (\_ -> logError @String "User has not money for script") (createFaucetContract >> updateFaucet up)
