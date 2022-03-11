{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Faucet
import Ledger.Ada
import Ledger.Value hiding (tokenName)
import Plutus.Contract.Test
import Plutus.Trace
import Spec.GrabForce
import Test.Tasty

--------------------------------
-- Tests with succesfull fauceting.
-- Тесты с успешной раздачей.
testsSuc :: TestTree
testsSuc =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfSuc)
    "succesfull fauceting"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-10_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 1_000_000)
        .&&. walletFundsChange (knownWallet 3) (lovelaceValueOf 2_000_000)
        .&&. walletFundsChange (knownWallet 4) (lovelaceValueOf 3_000_000)
    )
    myTraceSuc

emuConfSuc :: EmulatorConfig
emuConfSuc =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000),
               (knownWallet 3, lovelaceValueOf 10_000_000),
               (knownWallet 4, lovelaceValueOf 10_000_000)
             ]
       )

myTraceSuc :: EmulatorTrace ()
myTraceSuc = do
  Extras.logInfo @String "START TRACE"
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  h3 <- activateContractWallet (knownWallet 3) grabEndpoint
  h4 <- activateContractWallet (knownWallet 4) grabEndpoint
  Extras.logInfo @String "FIRST WALLET START FAUCET SCRIPT"
  let sp =
        StartParams
          { newAmount = 10_000_000,
            keyOne = 123,
            keyTwo = 456,
            tokenName = "AAA"
          }
  h1 <- activateContractWallet (knownWallet 1) $ runFaucet sp
  void $ waitNSlots 2
  nft <- getNFT h1
  void $ waitNSlots 2

  Extras.logInfo @String "SECOND WALLET GRAB 1 ADA"
  callEndpoint @"grab" h2 $ FaucetParams 1 nft
  void $ waitNSlots 2
  Extras.logInfo @String "THIRD WALLET GRAB 2 ADA"
  callEndpoint @"grab" h3 $ FaucetParams 123 nft
  void $ waitNSlots 2
  Extras.logInfo @String "FOURTH WALLET GRAB 3 ADA"
  callEndpoint @"grab" h4 $ FaucetParams 456 nft
  void $ waitNSlots 2
  Extras.logInfo @String "END TRACE"

------------------------------------------------------------------------------------
-- Tests with failed fauceting. No ada on the script
-- Тесты с неудачной раздачей. Нет ada на скрипте
myTraceFail1 :: EmulatorTrace ()
myTraceFail1 = do
  Extras.logInfo @String "START TRACE"
  --h1 <- activateContractWallet (knownWallet 1) startEndpoint
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  Extras.logInfo @String "FIRST WALLET TRY START FAUCET SCRIPT, BUT CANT CAUSE HAS NOT 10 ADA"
  let sp =
        StartParams
          { newAmount = 10_000_000,
            keyOne = 123,
            keyTwo = 456,
            tokenName = "AAA"
          }
  h1 <- activateContractWallet (knownWallet 1) $ runFaucet sp
  void $ waitNSlots 2
  nft <- getNFT h1
  void $ waitNSlots 2
  Extras.logInfo @String "SECOND WALLET TRY GRAB ADA, BUT SCRIPT HAS NOT ADA"
  callEndpoint @"grab" h2 $ FaucetParams 1 nft
  void $ waitNSlots 1
  Extras.logInfo @String "END TRACE"

emuConfFail1 :: EmulatorConfig
emuConfFail1 =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 5_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

testsFail1 :: TestTree
testsFail1 =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfFail1)
    "failed fauceting, because it is not possible to pass the script the right amount of ADA"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf 0) <> (assetClassValue nft 1))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    myTraceFail1
  where
    nft = AssetClass ("b57aef00c85d3bde449a3b12551cbd195bcf1e30a11ff1d15d28a30a", "AAA")

------------------------------------------------------------------------------
-- Tests with failed fauceting. No ada on the script for paying to user
-- Тесты с неудачной раздачей. Нет достаточного количества ada на скрипте, чтобы отдать пользователю.
myTraceFail2 :: EmulatorTrace ()
myTraceFail2 = do
  Extras.logInfo @String "START TRACE"
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  let sp =
        StartParams
          { newAmount = 2_000_000,
            keyOne = 123,
            keyTwo = 456,
            tokenName = "AAA"
          }
  h1 <- activateContractWallet (knownWallet 1) $ runFaucet sp
  void $ waitNSlots 2
  nft <- getNFT h1
  void $ waitNSlots 2
  Extras.logInfo @String "SECOND WALLET TRY GRAB ADA, BUT SCRIPT HAS NOT 3 ADA"
  callEndpoint @"grab" h2 $ FaucetParams 456 nft
  void $ waitNSlots 1
  Extras.logInfo @String "END TRACE"

emuConfFail2 :: EmulatorConfig
emuConfFail2 =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 10_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

testsFail2 :: TestTree
testsFail2 =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfFail2)
    "failed fauceting, because the script does not have the right amount of ADA "
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-2_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    myTraceFail2

-------------------------------------------------------

-- Тест с неудачной раздачей. Валидатор блокирует попытку украсть все с баланса скрипта.
-- A test with an unsuccessful fauceting. Validator blocks attempt to steal everything from script balance.
grabForceTrace :: EmulatorTrace ()
grabForceTrace = do
  Extras.logInfo @String "START TRACE"
  h2 <- activateContractWallet (knownWallet 2) grabForceEndpoints
  Extras.logInfo @String "FIRST WALLET TRY FAUCET SCRIPT"
  let sp =
        StartParams
          { newAmount = 10_000_000,
            keyOne = 123,
            keyTwo = 456,
            tokenName = "AAA"
          }
  h1 <- activateContractWallet (knownWallet 1) $ runFaucet sp
  void $ waitNSlots 2
  nft <- getNFT h1
  void $ waitNSlots 2
  Extras.logInfo @String "SECOND WALLET TRY GRAB ADA, BUT VALIDATOR BLOCK WRONG ACTION"
  callEndpoint @"grabForce" h2 $ FaucetParams 1 nft
  void $ waitNSlots 1
  Extras.logInfo @String "END TRACE"

grabForceTests :: TestTree
grabForceTests =
  checkPredicateOptions
    defaultCheckOptions
    "failed fauceting, because validator block bad action"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-10_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
    )
    grabForceTrace

------------------------------------------------------------------------------
-- Tests with failed fauceting. User can't pick up ADA because he has to wait an hour
-- Тесты с неудачной раздачей. Пользователь не может забрать ADA, так как должен подождатть час.
myTraceFail3 :: EmulatorTrace ()
myTraceFail3 = do
  Extras.logInfo @String "START TRACE"
  h2 <- activateContractWallet (knownWallet 2) grabEndpoint
  Extras.logInfo @String "FIRST WALLET START FAUCET SCRIPT"
  let sp =
        StartParams
          { newAmount = 10_000_000,
            keyOne = 123,
            keyTwo = 456,
            tokenName = "AAA"
          }
  h1 <- activateContractWallet (knownWallet 1) $ runFaucet sp
  void $ waitNSlots 2
  nft <- getNFT h1
  void $ waitNSlots 2
  Extras.logInfo @String "SECOND WALLET GRAB ADA"
  callEndpoint @"grab" h2 $ FaucetParams 123 nft
  void $ waitNSlots 2
  Extras.logInfo @String "SECOND WALLET  TRY GRAB ADA, BUT CANT BECAUSE NEED WAIT 1 HOUR"
  callEndpoint @"grab" h2 $ FaucetParams 123 nft
  void $ waitNSlots 2
  Extras.logInfo @String "END TRACE"

testsFail3 :: TestTree
testsFail3 =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfSuc)
    "Failed fauceting. User can't pick up ADA because he has to wait an hour"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-10_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 2_000_000)
    )
    myTraceFail3

-------------------------------------------------------

getNFT :: ContractHandle (Last AssetClass) StartSchema Text -> EmulatorTrace AssetClass
getNFT h = do
  l <- observableState h
  case l of
    Last Nothing -> throwError $ GenericError "TOKEN NOT CREATED"
    Last (Just nft) -> Extras.logInfo @String (show nft) >> return nft
