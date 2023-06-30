{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       TxInfo (txInfoValidRange),
                                       mkValidatorScript,from,to)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..),traceIfFalse, ($), (&&), (||), (+))
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Utilities            (Network, posixTimeFromIso8601,
                                            printDataToJSON,
                                            validatorAddressBech32,
                                            wrapValidator, writeValidatorToFile)


---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the  hasdeadline passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = (traceIfFalse "either brnrficiary1 has not signed or the current slot is after the deadline" (signedByBeneficiary1 && beforedeadline))||
                                (traceIfFalse "beneficiary2 has not signed or the deadline is not reached" (signedByBeneficiary2 && afterdeadline))
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    signedByBeneficiary1 :: Bool
    signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

    signedByBeneficiary2 :: Bool
    signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat

    beforedeadline :: Bool
    beforedeadline = contains (to $ deadline _dat) (txInfoValidRange info)

    afterdeadline :: Bool
    afterdeadline = contains (from $ (deadline _dat + 1)) (txInfoValidRange info)

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
