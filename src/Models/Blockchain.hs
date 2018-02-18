module Models.Blockchain where

import Models.Block
import Data.Time.Clock.POSIX (getPOSIXTime)

data Blockchain = Blockchain
    { blocks     :: [Block]
    , difficulty :: Int
    }

initBlockchain :: Int -> IO Blockchain
initBlockchain d = do
    i <- initBlock
    return $ Blockchain
        { blocks = [mineBlock i d]
        , difficulty = d
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
           return $ mineBlock (newBlock l stamp) d

verifyBlocks :: [Block] -> Bool
verifyBlocks (x:xs@((Block{prevHash = p}):_))
    | hashBlock currentHead /= p = False
    | otherwise = verifyBlocks currentTail
  where currentHead   = x
        currentTail   = xs
verifyBlocks (x:[]) = True

verifyBlockchain :: Blockchain -> Bool
verifyBlockchain (Blockchain{blocks = b}) = verifyBlocks b
