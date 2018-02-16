module Main where

import Data.Time.Clock.POSIX
import Data.Time.Format

import Data.Char                (ord)
import Data.ByteString          (unpack, pack)
import Crypto.Hash.SHA256       as SHA256
import Text.Printf              (printf)

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

-- | TODO
-- 1. Hash them to a SHA256 string
-- 2. Calculate a nonce
hashBlock :: Block -> IO String
hashBlock (Block{height = h, timestamp = t, nonce = n, prevHash = p}) = do
    time <- t
    let ts = formatTime defaultTimeLocale "%s" (posixSecondsToUTCTime time)

    return $ hashStr $ show h ++ show n ++ p ++ ts

hashStr :: String -> String
hashStr s = concatMap (printf "%02x") $ -- To Hex Str
            unpack $
            hash $
            pack $
            map (fromIntegral.ord) s

data Blockchain = Blockchain { blocks :: [Block] }

initBlockchain :: Blockchain
initBlockchain = Blockchain
    { blocks = [initBlock]
    }

addBlock :: Blockchain -> Blockchain
addBlock (Blockchain {blocks = b}) = Blockchain { blocks = (b ++ [last b]) }

main :: IO ()
main = do
  let b = initBlock
  str <- hashBlock b
  print str

  print "It runs."
