{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 
import qualified Data.ByteString as B
import Data.ByteString.Base16 (decode)
import Data.Bits (xor)

buffer1 = fst $ decode "1c0111001f010100061a024b53535009181c"
buffer2 = fst $ decode "686974207468652062756c6c277320657965"

main:: IO()
main = print $ xorStr buffer2

xorStr :: ByteString -> ByteString
xorStr = B.pack . B.zipWith xor buffer1