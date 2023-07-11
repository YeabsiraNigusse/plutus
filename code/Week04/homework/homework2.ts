import {
    Data,
    Lucid,
    Blockfrost,
    getAddressDetails,
    SpendingValidator,
    TxHash,
    Datum,
    UTxO,
    Address,
    AddressDetails,
}from "https://deno.land/x/lucid@0.9.1/mod.ts"
// create a seed.ts file with your seed
import { secretSeed } from "../lecture/seed.ts"

// set blockfrost endpoint
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "previewRPhikzGxsRPLAQxHlw7dLeObVD2GDtnn"
  ),
  "Preview"
);

lucid.selectWalletFromSeed(secretSeed, { accountIndex: 0 });
const addr: Address = await lucid.wallet.address();