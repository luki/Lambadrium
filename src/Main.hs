module Main where

import Chrono.TimeStamp

--type Height = Int
--type Nonce = Int
--type PrevHash = String
--
--data Block = Block Height PrevHash

data Block = Block
  { height :: Int
  , timestamp :: IO TimeStamp
  , nonce :: Int
  , prevHash :: String
  }

-- Creates the 'Genesis' block
initBlock :: Block
initBlock = Block
  { height = 0
  , timestamp = getCurrentTimeNanoseconds
  , nonce = 0 -- Needs to be calculated
  , prevHash = "0"
  }

newBlock :: Block -> Block
newBlock (Block{height = h}) = Block
  { height = h + 1
  , timestamp = getCurrentTimeNanoseconds
  , nonce = 0       -- Needs to be calculated
  , prevHash = "1"  -- Needs to be calculated
  }

data Blockchain = Blockchain { blocks :: [Block] }

initBlockchain :: Blockchain
initBlockchain = Blockchain { blocks = [initBlock] }

addBlock :: Blockchain -> Blockchain
addBlock (Blockchain {blocks = b}) = Blockchain { blocks = (b ++ [last b]) }

main :: IO ()
main = do
  let d = addBlock $ initBlockchain

  print "Hello"
