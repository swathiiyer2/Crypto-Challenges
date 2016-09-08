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

buffer1 = fst $ decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
buffer2 = fst $ decode "HUIfTQsPAh9PE048GmllH0kcDk4TA"

main :: IO()
main = do
     print $ xorStr 'T'
     print $ getKey

--Sets value of all letters to 1 
letters :: Map.Map Char Double
letters = Map.fromList $ zip ['a'..'z'] (repeat 1)

--XORs the buffer against a character k
xorStr :: Char -> String
xorStr k = map I.w2c $ B.zipWith xor buffer2 $ B.pack $ replicate 100 (I.c2w k)

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






