
module State where

import           Control.Monad.State (State, get, put, runState)

---------------------------------------------------------------------------------------------------
--------------------------------- HELPER FUNCTIONS/TYPES ------------------------------------------

-- Mock UTxO type
data UTxO = UTxO { owner :: String , value :: Integer }
    deriving (Show, Eq)

-- Mock blockchain type
newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

-- Initial blockchain state
initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]

---------------------------------------------------------------------------------------------------
------------------------------------ WITHOUT STATE MONAD ------------------------------------------

sendValue :: String -> Integer -> String -> Mock -> (Bool, Mock)
sendValue from amount to mockS =
    let senderUtxos = filter ((== from) . owner) (utxos mockS) -- list of sender utxo
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)-- list of utxo without sender utxos
        totalSenderFunds = sum (map value senderUtxos) -- adding all sender utxo
        receiverUtxo = UTxO to amount -- creating utxo for reciver with specified amount
        senderChange = UTxO from (totalSenderFunds - amount) -- getting the amout left for sender utxo by substructing the amount sended for the reciver
    in if totalSenderFunds >= amount -- checking if the sender has the specified amount
        then (True, Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos)--updating the blockchain
        else (False, mockS) -- else return to previous state


multipleTx :: (Bool, Mock)
multipleTx =
    let (isOk,  mockS1) = sendValue "Alice" 100 "Bob"   initialMockS -- with out state monad
        (isOk2, mockS2) = sendValue "Alice" 300 "Bob"   mockS1 -- creating transaction with sequence of different result of the blockchain
        (isOk3, mockS3) = sendValue "Bob"   200 "Rick"  mockS2
    in (isOk && isOk2 && isOk3, mockS3)

---------------------------------------------------------------------------------------------------
-------------------------------------- WITH STATE MONAD -------------------------------------------

-- newtype State s a = State { runState :: s -> (a, s) }

sendValue' :: String -> Integer -> String -> State Mock Bool
sendValue' from amount to = do
    mockS <- get
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    if totalSenderFunds >= amount
        then do
            put $ Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos
            return True --handling the state of the Mock behind the scene
        else return False

multipleTx' :: (Bool, Mock)
multipleTx' = runState (do -- by using runState we avoid the worry of chaining the state of the Mock
    isOk  <- sendValue' "Alice" 100 "Bob"
    isOk2 <- sendValue' "Alice" 300 "Bob"
    isOk3 <- sendValue' "Bob"   200 "Rick"
    return (isOk && isOk2 && isOk3))  -- only caring about the return result
    initialMockS


type Run a = State Mock a
