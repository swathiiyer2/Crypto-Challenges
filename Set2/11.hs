import Data.List 
import Data.List.Split
import Crypto.Data.PKCS7
import Crypto.Cipher
import System.Random
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as H

keylen = 16
iv = do
    iv' <- randomKey
    let iv'' = makeIV iv' :: Maybe (IV AES128)
    case iv'' of Just x  -> return x
                 Nothing -> return nullIV

main :: IO()
main = do
    input <- getLine
    let input' = BS.pack input
    print $ input'
    let k = BS.pack $ concat $ replicate 48 ['0']
    print $ k
    let encryptionMode = randomRIO(0,1) :: IO Int 
    let cText = [aesEncrypt (unsafePerformIO encryptionMode) (k)]
    let cChunks = map (\x -> chunk16 x) cText 
    print $ (unsafePerformIO encryptionMode)
    print $ ecbEncrypt (unsafePerformIO aes128) (padBytes $ unsafePerformIO $ appendBytes k)
    if isCBC $ checkDuplicates cText cChunks 
        then do print $ "CBC"
    else do print $ "ECB"


--Random bytestring for encryption
randomKey :: IO (BS.ByteString)
randomKey = do
    g <- newStdGen
    let key = BS.pack $ take keylen (randoms g :: [Char]) 
    return key

---Initialize cipher context from key
aes128 :: IO AES128
aes128 = do
    key <- randomKey
    let key' = makeKey key
    case key' of Right key -> return $ cipherInit key
                 otherwise -> error "Context Error"

--Get random number of bytes to append
numAppend :: IO Int
numAppend = randomRIO (5, 10) 

--Append random bytes
appendBytes :: BS.ByteString -> IO BS.ByteString
appendBytes pText = do
     h <- newStdGen
     let numBytes = (unsafePerformIO numAppend)
     let prefix = BS.pack $ take (numBytes) (randoms h :: [Char]) 
     let suffix = BS.pack $ take (numBytes) (randoms h :: [Char]) 
     return $ BS.append (BS.append prefix pText) suffix 

--Decrypt using AES-128 
aesEncrypt :: Int -> BS.ByteString -> BS.ByteString
aesEncrypt x contents 
       |x == 0 = ecbEncrypt (unsafePerformIO aes128) (padBytes $ unsafePerformIO $ appendBytes contents)
       |otherwise = cbcEncrypt (unsafePerformIO aes128) (unsafePerformIO iv) (padBytes $ unsafePerformIO $ appendBytes contents)

--Split a bytestring into 16-byte chunks
chunk16 :: BS.ByteString -> [[Char]]
chunk16 bstr = chunksOf 16 (BS.zipWith (\x y -> x) bstr bstr) 

{-Checks if any 16-byte chunks are repeated for each string, deletes any duplicates, and maps 
 the length of the shortened list to the corresponding hex-encoded string-}
checkDuplicates:: [BS.ByteString] -> [[String]] -> [(Int, BS.ByteString)]
checkDuplicates hexStrings chunks = zipWith (\x y -> (length $ nub y, x)) hexStrings chunks 

{-Gets the hex-encoded string that has been AES encrypted from the list of tuples. Only the string
AES encrypted in ECB mode will have been shortened during check for duplicates-}
isCBC :: [(Int, BS.ByteString)] -> Bool
isCBC tuples = lookup (minimum $ map (\x -> fst $ x) tuples) tuples == lookup (maximum $ map (\x -> fst $ x) tuples) tuples