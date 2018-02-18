module Main where

import Models.Block
import Models.Blockchain

import Data.Time.Clock.POSIX
import Data.Time.Format

main :: IO ()
main = do
    chain <- initBlockchain 1
    putStrLn . hashBlock . head . blocks $ chain
    newChain <- addBlock chain

    -- print $ verifyBlockchain newChain

    putStrLn . hashBlock . head . blocks $ newChain
    putStrLn "Runs :D"
