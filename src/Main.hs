module Main where

import Data.Time.Clock.POSIX
import Data.Time.Format

import Data.Char                (ord)
import Data.ByteString          (unpack, pack)
import Crypto.Hash.SHA256
import Text.Printf              (printf)

-- QUESTION: What is strict v.s. lazy??

strToIOStr :: String -> IO String
strToIOStr str = do return str

data Block = Block
    { height    :: Int
    , timestamp :: IO POSIXTime
    , nonce     :: Int
    , prevHash  :: IO String
    }

-- Creates the 'Genesis' block
initBlock :: Block
initBlock = Block
    { height    = 0
    , timestamp = getPOSIXTime
    , nonce     = 0               -- Needs to be calculated
    , prevHash  = strToIOStr "0"
    }

-- Creates a new block with a prev. block
newBlock :: Block -> Block
newBlock (b@Block{height = h}) = Block
    { height    = h + 1
    , timestamp = getPOSIXTime
    , nonce     =  3              -- Needs to be calculated
    , prevHash  = hashBlock b
    }

-- | TODO
-- 1. Calculate a nonce
hashBlock :: Block -> IO String
hashBlock (Block{height = h, timestamp = t, nonce = n, prevHash = p}) = do
    prevHash <- p
    time <- t
    let ts = formatTime defaultTimeLocale "%s" (posixSecondsToUTCTime time)
    return $ hashStr $ show h ++ show n ++ prevHash ++ ts

-- A function to hash a String (with SHA256)
hashStr :: String -> String
hashStr s =
    concatMap (printf "%02x") $ -- To Hex
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
addBlock (Blockchain {blocks = b}) = Blockchain { blocks = b ++ new }
  where new = [newBlock $ last b]

main :: IO ()
main = do
    let b = initBlock
    str <- hashBlock b
    print str

    print "It runs."
