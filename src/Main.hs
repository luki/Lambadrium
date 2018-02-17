module Main where

import Data.Time.Clock.POSIX
import Data.Time.Format

import Data.Char                (ord)
import Data.ByteString          (unpack, pack)
import Crypto.Hash.SHA256
import Text.Printf              (printf)

-- strToIOStr :: String -> IO String
-- strToIOStr str = do return str

takeNElements :: (Show a) => [a] -> Int -> [a]
takeNElements [] _      = []
takeNElements _ 0       = []
takeNElements (x:xs) n  = x : (takeNElements xs $ n-1)

repeatElem :: (Show a) => a -> Int -> [a]
repeatElem _ 0  = []
repeatElem a n  = a : (repeatElem a $ n-1)

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

calculateNonce :: Block -> Int -> Block
calculateNonce (b@Block{height = h, timestamp = t, nonce = n, prevHash = p}) d
    | firstOfHash == difRequirement = b
    | otherwise = calculateNonce newBlock d
  where
      difRequirement = repeatElem '0' d
      firstOfHash    = hashBlock b `takeNElements` d
      newBlock       = Block { height = h, timestamp = t, nonce = n + 1, prevHash = p }

data Blockchain = Blockchain
    { blocks     :: [Block]
    , difficulty :: Int
    }

initBlockchain :: IO Blockchain
initBlockchain = do
    i <- initBlock
    return $ Blockchain
        { blocks = [i]
        , difficulty = 0
        }

addBlock :: Blockchain -> IO Blockchain
addBlock (Blockchain {blocks = b, difficulty = d}) =
    new >>= \n -> return $ Blockchain
        { blocks = n : b
        , difficulty = d
        }
   where
       new = do
           let l = last b
           stamp <- getPOSIXTime
           return $ newBlock l stamp

main :: IO ()
main = do
    time <- getPOSIXTime
    genesis <- initBlock
    let b = newBlock genesis time
    let final_b = calculateNonce b 4
    
    putStrLn $ hashBlock b
    putStrLn $ hashBlock final_b


    -- chain <- initBlockchain
    -- newChain <- addBlock chain
    -- putStrLn . hashBlock . head . blocks $ newChain
    putStrLn "Runs :D"
