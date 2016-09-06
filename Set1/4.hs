{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import System.IO  
import Data.Tuple
import Control.Monad
import Data.Bits (xor)
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Data.ByteString.Base16 (decode)
import qualified Data.ByteString.Internal as I

main :: IO()
main = do  
        contents <- readFile "4.txt"
        let linesOfFile = lines contents
        print $ getKey linesOfFile
        print $ (xorStr (fst $ decode $ B.pack $ map I.c2w ("7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f")) ('5'))

getTuples :: [String] -> [(String, Char)]
getTuples list = concat $ map (\x -> zip (replicate 100 (x))  [(chr 0) ..(chr 255)]) list

--Letter frequencies
letters :: Map.Map Char Double
letters = Map.fromList ([('a', 0.082), ('b', 0.015), ('c', 0.028), ('d', 0.043), ('e', 0.127), ('f', 0.022), ('g', 0.020), 
                             ('h', 0.061), ('i', 0.070), ('j', 0.002), ('k', 0.008), ('l', 0.040), ('m', 0.024), ('n', 0.067), ('o', 0.075), 
                             ('p', 0.019), ('q', 0.001), ('r', 0.060), ('s', 0.063),('t', 0.091), ('u', 0.028), ('v', 0.010),('w', 0.023), 
                             ('x', 0.001), ('y', 0.020),('z', 0.001), (' ', 0.200)])

--Maps each (buffer,char) tuple to its score 
scoreMap :: [String] -> Map.Map (String, Char) Double
scoreMap x = Map.fromSet (\x -> getScore x) (S.fromList $ getTuples x)

--XORs the buffer against a character k and a decoded string
xorStr :: I.ByteString -> Char -> String
xorStr buffer k = map I.w2c $ B.zipWith xor buffer $ B.pack $ replicate 100 (I.c2w k)

--Gets the score given a xor'd string based on number of letters 
getScore :: (String, Char) -> Double
getScore xs = sum $ map (\x-> getScoreHelper x) (xorStr (fst $ decode $ B.pack $ map I.c2w (fst $ xs)) (snd xs))

--Looks up the value of the character in the frequency map. If not a letter, not in table, value is 0.
getScoreHelper:: Char -> Double
getScoreHelper x = case Map.lookup (toLower x) letters of 
    Just frequency  -> frequency
    Nothing         -> 0

----Returns the key in scoreMap with the highest score
--Converts to list, swaps elems, converts back to map, finds value of max key
getKey :: [String] -> (Double, (String, Char))
getKey x = Map.findMax $ Map.fromList (map (\x -> swap x) (Map.toList $ scoreMap x))



