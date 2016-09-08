{-# LANGUAGE OverloadedStrings #-}
import GHC.Int 
import Data.Bits
import Data.Char
import Data.Tuple
import Data.Binary 
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.BitString as BiS
import qualified Data.ByteString.Base64 as D
import qualified Data.ByteString.Internal as I

main :: IO()
main = do 
         contents <- B.readFile "6.txt"
         let bytes = D.decodeLenient contents
         --print $ keysizeMap bytes 
         --print $ keySize bytes
         --print $ cipherBlocks (keySize bytes) (bytes)
         --print $ transposeBlocks (keySize bytes) (cipherBlocks (keySize bytes) (bytes))
         --print $ scoreMap $ head $ transposeBlocks (keySize bytes) (cipherBlocks (keySize bytes) (bytes))
         --print $ scoreMap $ last $ transposeBlocks (keySize bytes) (cipherBlocks (keySize bytes) (bytes))
         --print $ singleXOR $ transposeBlocks (keySize bytes) (cipherBlocks (keySize bytes) (bytes))
         --print $ xorStr (head $ transposeBlocks (keySize bytes) (cipherBlocks (keySize bytes) (bytes))) ('a')
         
         print $ bytes 
         print $ keysizeMap bytes 
         print $ keySize bytes
         print $ cipherBlocks (29) (bytes)
         print $ transposeBlocks (29) (cipherBlocks (29) (bytes))
         print $ scoreMap $ head $ transposeBlocks (29) (cipherBlocks (29) (bytes))
         print $ scoreMap $ last $ transposeBlocks (29) (cipherBlocks (29) (bytes))

         print $ singleXOR $ transposeBlocks (29) (cipherBlocks (29) (bytes))
         print $ encrypt bytes (singleXOR $ transposeBlocks (29) (cipherBlocks (29) (bytes)))

--Finds the hamming distance given two bitStrings
hamming :: BiS.BitString -> BiS.BitString -> GHC.Int.Int64 -> GHC.Int.Int64
hamming x y count
     | (BiS.null x) = count + BiS.length y
     | (BiS.null y) = count + BiS.length x
     | equalBit x y = hamming (BiS.drop 1 x) (BiS.drop 1 y) count
     | otherwise = hamming (BiS.drop 1 x) (BiS.drop 1 y) (count + 1)

--Checks if one bit is equal to another
equalBit :: BiS.BitString -> BiS.BitString -> Bool
equalBit x y = (BiS.take 1 x == BiS.take 1 y)

--Maps keysizes 2-40 to the edit distances
keysizeMap :: I.ByteString -> Map.Map Int Double
keysizeMap str = Map.fromList $ zip [2..40] (hammingsList str) 

--Creates a list of edit distances for each keysize
hammingsList :: I.ByteString -> [Double]
hammingsList str = map (\x -> (fromIntegral (keyHamming x str) :: Double) / (fromIntegral (x) :: Double)) [2..40] 

--Finds the edit distance for one keysize
keyHamming :: Int -> I.ByteString -> Int64
keyHamming x str = hamming (BiS.bitString $ (B.take x str)) (BiS.bitString (B.take x $ B.drop x str)) 0

--Get keysize from map with smallest edit distance
keySize :: I.ByteString -> Int
keySize str = snd $ Map.findMin $ Map.fromList (map (\x -> swap x) (Map.toList $ keysizeMap str))

--Split ciphertext into blocks of keysize length
cipherBlocks :: Int -> I.ByteString -> [I.ByteString]
cipherBlocks size content  
     |B.length content == 0 = []
     |otherwise = [(B.take size content)] ++ (cipherBlocks size (B.drop size content))

--Creates a list of blocks made from each index
transposeBlocks :: Int -> [I.ByteString] -> [I.ByteString]
transposeBlocks size block = map (\y -> B.pack y) (map (\x -> blockIndex x block) [1..size])

--Creates a block for a single index
blockIndex :: Int -> [I.ByteString] -> [Word8]
blockIndex i block = map (\x -> B.last $ B.take i x) block

--Single-char XOR for each block
singleXOR :: [I.ByteString] -> [Char]
singleXOR blocks = map (\x -> getKey x) blocks

----Returns the key in scoreMap with the highest score
--Converts to list, swaps elems, converts back to map, finds value of max key
getKey :: I.ByteString -> Char
getKey buffer = snd $ Map.findMax $ Map.fromList (map (\x -> swap x) (Map.toList $ scoreMap buffer))

--Maps each character to its score 
scoreMap :: I.ByteString -> Map.Map Char Double
scoreMap buffer = Map.fromSet  (\k -> getScore $ xorStr buffer k) (Map.keysSet letters)

--Sets value of all letters to 1 
letters :: Map.Map Char Double
--letters = Map.fromList $ zip [(chr 0) ..(chr 255)](repeat 1)
letters = Map.fromList ([('a', 0.082), ('b', 0.015), ('c', 0.028), ('d', 0.043), ('e', 0.127), ('f', 0.022), ('g', 0.020), 
                             ('h', 0.061), ('i', 0.070), ('j', 0.002), ('k', 0.008), ('l', 0.040), ('m', 0.024), ('n', 0.067), ('o', 0.075), 
                             ('p', 0.019), ('q', 0.001), ('r', 0.060), ('s', 0.063),('t', 0.091), ('u', 0.028), ('v', 0.010),('w', 0.023), 
                             ('x', 0.001), ('y', 0.020),('z', 0.001), (' ', 0.200)])

--XORs the buffer against a character k
xorStr :: I.ByteString -> Char -> String
xorStr buffer k = map I.w2c $ B.zipWith xor buffer $ B.pack $ replicate 100 (I.c2w k)

--Gets the score given a xor'd string based on number of letters 
getScore:: String -> Double
getScore xs = sum $ map getScoreHelper xs

--Looks up the value of the character in the frequency map. If not a letter, not in table, value is 0.
getScoreHelper:: Char -> Double
getScoreHelper x = case Map.lookup (toLower x) letters of 
    Just frequency  -> frequency
    Nothing         -> 0

--XORs buffer against the key 
encrypt :: I.ByteString -> [Char] -> I.ByteString
encrypt b key = B.pack $ B.zipWith xor b $ B.pack $ map (\x -> I.c2w x)(take (B.length b) $ cycle key)





