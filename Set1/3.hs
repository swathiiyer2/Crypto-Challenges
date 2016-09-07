{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as I
import Data.ByteString.Base16 (decode)
import Data.Bits (xor)
import Data.Char
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as C 
import qualified Data.List as L
import Data.Tuple

--letters = Map.fromList ([('a', 0.082), ('b', 0.015), ('c', 0.028), ('d', 0.043), ('e', 0.127), ('f', 0.022), ('g', 0.020), 
--                             ('h', 0.061), ('i', 0.070), ('j', 0.002), ('k', 0.008), ('l', 0.040), ('m', 0.024), ('n', 0.067), ('o', 0.075), 
--                             ('p', 0.019), ('q', 0.001), ('r', 0.060), ('s', 0.063),('t', 0.091), ('u', 0.028), ('v', 0.010),('w', 0.023), 
--                             ('x', 0.001), ('y', 0.020),('z', 0.001), (' ', 0.200)])

buffer1 = fst $ decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

main :: IO()
main = print $ getKey

--Sets value of all letters to 1 
letters :: Map.Map Char Double
letters = Map.fromList $ zip ['a'..'z'] (repeat 1)

--XORs the buffer against a character k
xorStr :: Char -> String
xorStr k = map I.w2c $ B.zipWith xor buffer1 $ B.pack $ replicate 100 (I.c2w k)

--Gets the score given a xor'd string based on number of letters 
getScore:: String -> Double
getScore xs = sum $ map getScoreHelper xs

--Looks up the value of the character in the frequency map. If not a letter, not in table, value is 0.
getScoreHelper:: Char -> Double
getScoreHelper x = case Map.lookup (toLower x) letters of 
    Just frequency  -> frequency
    Nothing         -> 0

--Maps each character to its score 
scoreMap :: Map.Map Char Double
scoreMap = Map.fromSet  (\k -> getScore $ xorStr k) (Map.keysSet letters)

----Returns the key in scoreMap with the highest score
--Converts to list, swaps elems, converts back to map, finds value of max key
getKey :: Char
getKey = snd $ Map.findMax $ Map.fromList (map (\x -> swap x) (Map.toList scoreMap))






