{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy,
                                       ScriptContext (scriptContextTxInfo), TokenName (TokenName), TxOutRef,
                                       mkMintingPolicyScript,TxInfo (txInfoInputs,txInfoMint),TxInInfo (txInInfoOutRef),)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (.), any, Eq ((==)),traceIfFalse, (&&))
import           Utilities            (wrapPolicy)
  

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.

mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy _oref () _ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                 traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == _oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == (TokenName "") && amt == 1 -- TokenName ""
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
