module Helpers where

  import Data.ByteString          (unpack, pack)
  import Text.Printf              (printf)
  import Data.Char                (ord)
  import Crypto.Hash.SHA256
  import Text.Printf              (printf)

  repeatElem :: (Show a) => a -> Int -> [a]
  repeatElem _ 0  = []
  repeatElem a n  = a : (repeatElem a $ n-1)

  -- A function to hash a String (with SHA256)
  hashStr :: String -> String
  hashStr s =
      concatMap (printf "%02x") $ -- To Hex
      unpack $
      hash $
      pack $
      map (fromIntegral.ord) s
