{-# LANGUAGE OverloadedStrings #-}
import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import Data.ByteString.Base16 (encode)

buffer = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"

main :: IO()
main = print $ encrypt buffer

--XORs buffer against the repeating list [I, C, E] and hex encodes it
encrypt :: I.ByteString -> I.ByteString
encrypt b = encode $ B.pack $ B.zipWith xor b $ B.pack $ (take 100 $ cycle [I.c2w 'I', I.c2w 'C', I.c2w 'E'])