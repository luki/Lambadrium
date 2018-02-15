module Main where

import Data.Time.Clock.POSIX

-- QUESTION: What is strict v.s. lazy??

data Block = Block
    { height    :: Int
    , timestamp :: IO POSIXTime
    , nonce     :: Int
    , prevHash  :: String
    }

-- Creates the 'Genesis' block
initBlock :: Block
initBlock = Block
    { height    = 0
    , timestamp = getPOSIXTime
    , nonce     = 0               -- Needs to be calculated
    , prevHash  = "0"
    }

newBlock :: Block -> Block
newBlock (Block{height = h}) = Block
    { height    = h + 1
    , timestamp = getPOSIXTime
    , nonce     =  3              -- Needs to be calculated
    , prevHash  = "1"             -- Needs to be calculated
    }

data Blockchain = Blockchain
    { blocks :: [Block]
    }

initBlockchain :: Blockchain
initBlockchain = Blockchain
    { blocks = [initBlock]
    }

addBlock :: Blockchain -> Blockchain
addBlock (Blockchain {blocks = b}) = Blockchain
  { blocks = (b ++ [last b])
  }

main :: IO ()
main = do
  print . floor =<< getPOSIXTime
