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
    , timestamp :: POSIXTime
    , nonce     :: Int
    , prevHash  :: String
    }

-- Creates the 'Genesis' block
initBlock :: IO Block
initBlock = do
    t <- getPOSIXTime
    return $ Block
        { height    = 0
        , timestamp = t
        , nonce     = 0               -- Needs to be calculated
        , prevHash  = "0"
        }

-- Creates a new block with a prev. block
newBlock :: Block -> POSIXTime -> Block
newBlock (b@Block{height = h}) time = Block
    { height    = h + 1
    , timestamp = time
    , nonce     =  3              -- Needs to be calculated
    , prevHash  = hashBlock b
    }

-- | TODO
-- 1. Calculate a nonce
hashBlock :: Block -> String
hashBlock (Block{height = h, timestamp = t, nonce = n, prevHash = p}) =
    let ts = formatTime defaultTimeLocale "%s" (posixSecondsToUTCTime t)
     in hashStr $ show h ++ show n ++ p ++ ts

-- A function to hash a String (with SHA256)
hashStr :: String -> String
hashStr s =
    concatMap (printf "%02x") $ -- To Hex
    unpack $
    hash $
    pack $
    map (fromIntegral.ord) s

data Blockchain = Blockchain { blocks :: [Block] }

initBlockchain :: IO Blockchain
initBlockchain = do
    i <- initBlock
    return $ Blockchain { blocks = [i] }

addBlock :: Blockchain -> IO Blockchain
addBlock (Blockchain {blocks = b}) = new >>= \n -> return $ Blockchain { blocks = n : b }
   where
       new = do
           let l = last b
           stamp <- getPOSIXTime
           return $ newBlock l stamp

main :: IO ()
main = do
    chain <- initBlockchain
    newChain <- addBlock chain
    putStrLn . hashBlock . head . blocks $ newChain
    putStrLn "Runs :D"
