{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.ByteString.Char8 
import qualified Data.ByteString as B
import Data.ByteString.Base16 (decode)
import Data.Bits (xor)
import Data.List

--From https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
frequencies :: Map.Map Char Double
frequencies = Map.fromList ([('a', 8.12), ('b', 1.49), ('c', 2.71), ('d', 4.32), ('e', 12.02), ('f', 2.30), ('g', 2.03), 
                         ('h', 5.92), ('i', 7.31), ('j', 0.10), ('k', 0.69), ('l', 3.98), ('m', 2.61), ('n', 6.95), ('o', 7.68), 
                         ('p', 1.82), ('q', 0.11), ('r', 6.02), ('s', 6.28),('t', 9.10), ('u', 2.88), ('v', 1.11),('w', 2.09), 
                         ('x', 0.17), ('y', 2.11),('z', 0.07), (' ', 0.0)])

buffer = fst $ decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

main::IO()
main = print $ getKey 

--Gets the score given a xor'd string
getScore:: [Char] -> Maybe Double
getScore [] = Nothing
getScore xs = fmap sum $ sequence (Data.List.map (\x -> Map.lookup x frequencies) xs)

--Returns the key with the highest score
getKey :: Map.Map Double Char -> Char
getKey m = snd $ Map.findMax m

--Maps each character to its score 
scoreMap :: Map.Map Char Double
scoreMap = Map.mapKeys (getScore xorString buffer) frequencies

--Returns the xor'd string against a character
xorString :: Char -> String 
xorString c = B.pack $ Data.List.map (xor fst $ decode c) 

