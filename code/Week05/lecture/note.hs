module Main where

-- creating a value


-- assetClassValue :: AssetClass -> Integer -> Value

-- assetClass :: CurrencySymbol -> TokenName -> AssetClass

-- AssetClass = (CurrencySymbol, TokenName)


-- let w = assetClassValue (assetClass "a205ff" "token1") 1000
-- let x = assetClassValue (assetClass "bb" "token2") 3000

-- sum opration

-- since value is an instance of monoid typeclass we can concatnet two values

-- w <> x  return value like ->  Value (Map [(a205ff,Map [("token1",3000)]),(bb,Map [("x",2000)])])

-- to get the value of a spacific assetClass

-- let ada = assetClass "a205ff" "token1"
-- let ada2 = assetClass "bb" "token2"


--assetClassValueOf :: Value -> assetClass -> Integer
-- example 
-- let val = w <> x
-- assetClassValue val ada  return 1000
-- assetClassValue val ada2  return 3000

-- substruction opration

-- import PlutusTx.Monoid
-- Prelude Plutus.V1.Ledger.Value PlutusTx.Monoid Free> gsub val v
-- Value (Map [(a205ff,Map [("token1",3000)])])