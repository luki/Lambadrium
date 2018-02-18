module Models.Block where

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format      (defaultTimeLocale, formatTime)
import Helpers

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

hashBlock :: Block -> String
hashBlock (Block{height = h, timestamp = t, nonce = n, prevHash = p}) =
    let ts = formatTime defaultTimeLocale "%s" (posixSecondsToUTCTime t)
      in hashStr $ show h ++ show n ++ p ++ ts

-- | This function takes in a block and a difficulty
-- It then return the same block with a nonce
-- That satisfies the given difficulty
mineBlock :: Block -> Int -> Block
mineBlock (b@Block{height = h, timestamp = t, nonce = n, prevHash = p}) d
    | firstOfHash == difRequirement = b
    | otherwise = mineBlock newBlock d
  where
      difRequirement = repeatElem '0' d
      firstOfHash    = take d $ hashBlock b
      newBlock       = Block { height = h, timestamp = t, nonce = n + 1, prevHash = p }
