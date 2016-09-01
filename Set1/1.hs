{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString.Char8 
import qualified Data.ByteString
import Data.ByteString.Base16
import Data.ByteString.Base64
import Data.String

hexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

main::IO()
main = do
     if ((rawTo64 $ hexToRaw hexStr) == pack expected)
          then do print $ rawTo64 $ hexToRaw hexStr
     else do print $ "Error"

--Convert from hex to raw bytes
hexToRaw :: ByteString -> ByteString
hexToRaw str = fst $ Data.ByteString.Base16.decode str

--Convert raw bytes to base64
rawTo64 :: ByteString -> ByteString
rawTo64 str = Data.ByteString.Base64.encode str