{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Monoid (Last (..))
import qualified Data.OpenApi as OpenApi
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value hiding (currencySymbol, tokenName)
import Playground.Contract (ToSchema)
import Plutus.Contract as Contract
import Plutus.Contracts.Currency (CurrencyError, OneShotCurrency, currencySymbol, mintContract)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Eq, Ord, Semigroup (..), Show (..), String, last)

-- Датум, храним в нем оба два ключа апи
-- Datum, contains api keys
data FaucetDatum = FaucetDatum
  { fstApi :: !Integer,
    sndApi :: !Integer,
    usersDict :: [(PubKeyHash, POSIXTime)]
  }
  deriving (Show, ToJSON, FromJSON, Prelude.Eq, Generic, ToSchema, OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''FaucetDatum

instance PlutusTx.Prelude.Eq FaucetDatum where
  (FaucetDatum k1 k2 d1) == (FaucetDatum a1 a2 d2) = (k1 == a1) && (k2 == a2) && (d1 == d2)

oneHour :: POSIXTime
oneHour = 3_600_000

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

getLovelaceFromValueOf :: Value -> Integer
getLovelaceFromValueOf = getLovelace . fromValue

PlutusTx.unstableMakeIsData ''FaucetRedeemer

-- Валидатор. Проверяет что всего может быть два входа и число ADA на выходе, оставшихся на скрипте, в соответвии с ключами.
-- Validator сhecks that there can be a total of two inputs and the number of ADA on the output left on the script, according to the keys.
mkFaucetValidator :: AssetClass -> FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
mkFaucetValidator nft dat red ctx =
  traceIfFalse "bad output datum" correctOutputDatum
    && traceIfFalse "bad time for validation" (validTime userTime)
    && traceIfFalse "no token on script input" inputHasToken
    && traceIfFalse "no token on script output" outputHasToken
    && case red of
      (Key1 key1 _) ->
        traceIfFalse "api key not equal to first api key" (key1 == fstApi dat)
          && traceIfFalse "wrong lovelace paid" (outputFunds 2_000_000)
      (Key2 key2 _) ->
        traceIfFalse "api key not equal to second api key" (key2 == sndApi dat)
          && traceIfFalse "wrong lovelace paid" (outputFunds 3_000_000)
      (AnotherKey _) -> traceIfFalse "wrong lovelace paid" (outputFunds 1_000_000)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "own input not found"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected only one output"

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) nft == 1

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) nft == 1

    inputFunds :: Integer
    inputFunds =
      getLovelaceFromValueOf . txOutValue $ ownInput

    outputFunds :: Integer -> Bool
    outputFunds i = inputFunds >= i && (getLovelaceFromValueOf . txOutValue $ ownOutput) == (inputFunds - i)

    userTime :: Maybe POSIXTime
    userTime = mkLookup (phk red) (usersDict outputDatum)

    validTime :: Maybe POSIXTime -> Bool
    validTime (Just userTime') = contains (txInfoValidRange info) $ from userTime'
    validTime _ = False

    outputDatum :: FaucetDatum
    outputDatum =
      case txOutDatumHash ownOutput of
        Nothing -> traceError "wrong output type"
        Just h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just dat' -> dat'
            Nothing -> traceError "error decoding data"

    correctOutputDatum :: Bool
    correctOutputDatum = correctDatums (phk red) dat outputDatum

------------------------------------------------------------------
{-# INLINEABLE mkLookup #-}
mkLookup :: PlutusTx.Prelude.Eq t => t -> [(t, a)] -> Maybe a
mkLookup a ((i, x) : xs) = if a == i then Just x else mkLookup a xs
mkLookup _ [] = Nothing

{-# INLINEABLE mkFindKey #-}
mkFindKey :: [(a, POSIXTime)] -> POSIXTime -> Bool
mkFindKey ((_, t') : s) t = t' <= t && (mkFindKey s t')
mkFindKey [] _ = True

{-# INLINEABLE correctDatums #-}
correctDatums :: PubKeyHash -> FaucetDatum -> FaucetDatum -> Bool
correctDatums pkh (FaucetDatum a1 a2 d1) (FaucetDatum b1 b2 d2) = (a1 == b1) && (a2 == b2) && checkDate timeInOldDatum timeInNewDatum
  where
    timeInOldDatum = mkLookup pkh d1
    timeInNewDatum = mkLookup pkh d2
    checkDate (Just n1) (Just n2) = n1 + oneHour < n2 && mkFindKey d1 n2
    checkDate Nothing (Just n2) = mkFindKey d1 n2
    checkDate _ _ = False

data Fauceting

instance Scripts.ValidatorTypes Fauceting where
  type DatumType Fauceting = FaucetDatum
  type RedeemerType Fauceting = FaucetRedeemer

typedValidator :: AssetClass -> Scripts.TypedValidator Fauceting
typedValidator nft =
  Scripts.mkTypedValidator @Fauceting
    ( $$(PlutusTx.compile [||mkFaucetValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode nft
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @FaucetDatum @FaucetRedeemer

faucetValidator :: AssetClass -> Validator
faucetValidator = Scripts.validatorScript . typedValidator

faucetAddress :: AssetClass -> Address
faucetAddress = scriptAddress . faucetValidator

valHash :: AssetClass -> ValidatorHash
valHash = Scripts.validatorHash . typedValidator

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
    Just (FaucetDatum k1 k2 d1) ->
      if k1 == k
        then Just (Key1 k pkh, (FaucetDatum k1 k2 d1))
        else
          if k2 == k
            then Just (Key2 k pkh, (FaucetDatum k1 k2 d1))
            else Just (AnotherKey pkh, (FaucetDatum k1 k2 d1))

checkTime :: PubKeyHash -> POSIXTime -> FaucetDatum -> Bool
checkTime pkh now (FaucetDatum _ _ dict) = case Map.lookup pkh (Map.fromList dict) of
  Nothing -> True
  Just inter -> inter + oneHour < now

updateDat :: PubKeyHash -> POSIXTime -> FaucetDatum -> FaucetDatum
updateDat pkh now (FaucetDatum k1 k2 dict) = FaucetDatum k1 k2 dict2
  where
    dict2 = Map.toList $ Map.insert pkh (now + oneHour) $ Map.fromList dict

-- Апи ключ, который используем когда хотим получить Ada
-- The api key we use when we want to get Ada
data FaucetParams = FaucetParams
  { fpApiKey :: Integer,
    fpToken :: AssetClass
  }
  deriving (Generic, Prelude.Eq, ToJSON, FromJSON, ToSchema, Show, OpenApi.ToSchema, Prelude.Ord)

-- Функция grab. Получение Ada от скрипта
-- Grab function. Getting Ada from a script
grab :: FaucetParams -> Contract w s Text ()
grab FaucetParams {..} = do
  pkh <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
  now <- currentTime
  utxos <- utxosAt (faucetAddress fpToken) -- Получение неизрасходованных выходов транзакции по адресу. Get the unspent transaction outputs at an address.
  let listOfOrefAndCh = findFaucet utxos -- Фильтрация. Filtration
      listOfDatums = findRightFaucet utxos
  if not $ null listOfOrefAndCh
    then do
      let (oref, ch) = Prelude.last listOfOrefAndCh
      logInfo @String $ "Faucet founded"
      case selectRedeemer pkh fpApiKey ch of -- Проверка api ключа. Cheking api key
        Nothing -> logError @String "Bad datum"
        Just (red, dat) ->
          if checkTime pkh now dat
            then do
              let lookups =
                    Constraints.unspentOutputs utxos
                      <> Constraints.otherScript (faucetValidator fpToken)
                  -- Ограничение, в котором говорим что скрипт нам должен выплатить все что у него есть
                  -- A constraints saying that the script must pay us everything it has
                  tx =
                    (Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData red)
                      <> (Constraints.mustValidateIn (from now))
                  newdat = updateDat pkh now dat
                  tx' = case red of
                    Key1 _ _ -> returnMerg ch newdat 2_000_000
                    Key2 _ _ -> returnMerg ch newdat 3_000_000
                    AnotherKey _ -> returnMerg ch newdat 1_000_000
              ledgerTx <- submitTxConstraintsWith @Fauceting lookups (tx <> tx')
              void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
              logInfo @String $ "collected gifts using " ++ show red
            else logError @String $ "too early"
    else logInfo @String $ "no faucets " ++ (show listOfDatums)
  where
    -- Получение, числа Ada на скрипте
    -- Getting, Ada number on the script
    amount a = getLovelaceFromValueOf $ _ciTxOutValue a

    token = assetClassValue fpToken 1
    -- Вычисление сдачи, в соответсвии с апи ключом
    -- Calculation of change, according to the api key
    returnMerg b dat n = Constraints.mustPayToOtherScript (valHash fpToken) (Datum $ PlutusTx.toBuiltinData dat) $ (Ada.lovelaceValueOf (amount b - n)) <> token

type GrabSchema = Endpoint "grab" FaucetParams

data StartParams = StartParams
  { newAmount :: !Integer,
    keyOne :: !Integer,
    keyTwo :: !Integer,
    tokenName :: !TokenName
  }
  deriving (Generic, Prelude.Eq, ToJSON, FromJSON, ToSchema, Show, OpenApi.ToSchema, Prelude.Ord)

-- Старт раздающего
-- Faucet start
startFaucet :: StartParams -> Contract w s Text AssetClass
startFaucet StartParams {..} = do
  let dat = FaucetDatum keyOne keyTwo []
  pkh <- Contract.ownPaymentPubKeyHash
  osc <- mapError (pack . show) (mintContract pkh [(tokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
  let nft = AssetClass (currencySymbol osc, tokenName)
      c = Constraints.mustPayToTheScript dat $ (Ada.lovelaceValueOf newAmount) <> (assetClassValue nft 1)
  ledgerTx <- submitTxConstraints (typedValidator nft) c
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "set new keys to " ++ (show keyOne) ++ " and " ++ (show keyTwo)
  logInfo @String $ "script address: " ++ show (faucetAddress nft)
  logInfo @String $ "token parameter: " ++ show nft
  logInfo @String $ "started faucet"
  return nft

type StartSchema = Endpoint "start" StartParams

startEndpoint :: Contract () StartSchema Text AssetClass
startEndpoint = awaitPromise start' >> startEndpoint
  where
    start' = endpoint @"start" startFaucet

grabEndpoint :: Contract () GrabSchema Text ()
grabEndpoint = awaitPromise grab' >> grabEndpoint
  where
    grab' = endpoint @"grab" grab

runFaucet :: StartParams -> Contract (Last AssetClass) StartSchema Text ()
runFaucet sp = do
  nft <- startFaucet sp
  tell $ Last $ Just nft
  return ()
