{-# LANGUAGE OverloadedStrings #-}
module 5 where
import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import Data.ByteString.Base16 (encode)

buffer = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"

main :: IO()
main = print $ encrypt buffer "ICE"

--XORs buffer against the repeating list [I, C, E] and hex encodes it
encrypt :: I.ByteString -> [Char] -> I.ByteString
encrypt b key = encode $ B.pack $ B.zipWith xor b $ B.pack $ map (\x -> I.c2w x)(take (B.length b) $ cycle key)