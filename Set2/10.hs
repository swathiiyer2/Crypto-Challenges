{-# LANGUAGE OverloadedStrings #-}
import Crypto.Cipher
import Data.Bits (xor)
import Data.List 
import Data.List.Split
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Internal as I
import Data.ByteString.Base16 (decode)

Right key = makeKey (B.pack "YELLOW SUBMARINE")
iv = B.pack "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

main :: IO()
main = do
         file <- BS.readFile "10.txt"
         let cipherChunks = chunk16 $ B64.decodeLenient file 
         print $ concat $ map (\x -> B.unpack x) (decrypt (init cipherChunks) [xorStr [B.unpack $ (aesDecrypt $ B.pack $ head $ cipherChunks)] iv])

--Initialize cipher context from key
aes128 :: AES128
aes128 = cipherInit key

--Decrypt using AES-128 in ECB mode
aesDecrypt :: B.ByteString -> B.ByteString
aesDecrypt contents = ecbDecrypt aes128 contents

--XOR two ByteStrings
xorStr :: [String] -> B.ByteString -> B.ByteString 
xorStr cText = BS.pack . BS.zipWith xor buffer1
      where buffer1 = B.pack $ head cText

--Split a bytestring into 16-byte chunks
chunk16 :: B.ByteString -> [[Char]]
chunk16 bstr = chunksOf 16 (B.zipWith (\x y -> x) bstr bstr)

--CBC Decryption
decrypt :: [String] -> [B.ByteString]-> [B.ByteString]
decrypt cText pText 
     |null cText = pText
     |otherwise = decrypt (tail cText) (pText ++ [xorStr (cText) $ (aesDecrypt $ B.pack $ head $ tail cText)])
