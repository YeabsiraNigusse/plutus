{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Homework
    ( stakeValidator'
    , saveStakeValidator'
    ) where

import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V2.Ledger.Api (Address, BuiltinData, PubKeyHash,
                                       ScriptContext (scriptContextPurpose, scriptContextTxInfo), StakeValidator,StakingCredential,
                                       mkStakeValidatorScript,ScriptPurpose (Certifying, Rewarding),
                                       TxInfo (txInfoOutputs, txInfoWdrl),TxOut (txOutAddress, txOutValue), adaSymbol, adaToken)
import           Plutus.V2.Ledger.Contexts (txSignedBy)                                       
import qualified PlutusTx
import qualified PlutusTx.AssocMap      as PlutusTx
import           PlutusTx.Prelude     (Bool (..), ($), (.),Maybe (Just, Nothing),traceIfFalse,
                                      otherwise, traceError,foldl,Eq ((==)),AdditiveSemigroup ((+)),
                                      MultiplicativeSemigroup ((*)),
                                         Ord ((>=)),(&&))
import           Prelude              (IO, String, undefined,Integer)
import           Utilities            (wrapStakeValidator)

-- | A staking validator with two parameters, a pubkey hash and an address. The validator
--   should work as follows:
--   1.) The given pubkey hash needs to sign all transactions involving this validator.
--   2.) The given address needs to receive at least half of all withdrawn rewards.
{-# INLINABLE mkStakeValidator' #-}
mkStakeValidator' :: PubKeyHash -> Address -> () -> ScriptContext -> Bool
mkStakeValidator' _pkh _addr () _ctx = case scriptContextPurpose _ctx of
    Certifying _   -> True
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred &&
                      traceIfFalse "not signed by the right user" signed
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    signed :: Bool
    signed = txSignedBy info _pkh

    amount :: StakingCredential -> Integer
    amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of
        Just amt -> amt
        Nothing  -> traceError "withdrawal not found"

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == _addr = n + valueOf (txOutValue o) adaSymbol adaToken
            | otherwise              = n

{-# INLINABLE mkWrappedStakeValidator' #-}
mkWrappedStakeValidator' :: PubKeyHash -> Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator' pkh = wrapStakeValidator . mkStakeValidator' pkh

stakeValidator' :: PubKeyHash -> Address -> StakeValidator
stakeValidator' pkh addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveStakeValidator' :: String -> String -> IO ()
saveStakeValidator' _pkh _bech32 = undefined
