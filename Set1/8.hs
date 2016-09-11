import Data.List 
import Data.List.Split
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Base16 as H

main :: IO()
main = do
         file <- readFile "8.txt"
         let cipherText = map (\x -> fst $ H.decode $ B.pack x) (lines file)
         let cipherChunks = map (\x -> chunk16 x) cipherText 
         print $ getAESString $ checkDuplicates cipherText cipherChunks

--Split a bytestring into 16-byte chunks
chunk16 :: B.ByteString -> [[Char]]
chunk16 bstr = chunksOf 16 (B.zipWith (\x y -> x) bstr bstr) 

{-Checks if any 16-byte chunks are repeated for each string, deletes any duplicates, and maps 
 the length of the shortened list to the corresponding hex-encoded string-}
checkDuplicates:: [B.ByteString] -> [[String]] -> [(Int, B.ByteString)]
checkDuplicates hexStrings chunks = zipWith (\x y -> (length $ nub y, H.encode x)) hexStrings chunks 

{-Gets the hex-encoded string that has been AES encrypted from the list of tuples. Only the string
AES encrypted in ECB mode will have been shortened during check for duplicates-}
getAESString :: [(Int, B.ByteString)] -> Maybe B.ByteString
getAESString tuples = lookup (minimum $ map (\x -> fst $ x) tuples) tuples