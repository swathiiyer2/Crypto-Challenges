import Data.List 
import Data.List.Split
import Crypto.Data.PKCS7
import Crypto.Cipher
import System.Random
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS

keylen = 16
iv = do
    iv' <- randomKey
    let iv'' = makeIV iv' :: Maybe (IV AES128)
    case iv'' of Just x  -> return x
                 Nothing -> return nullIV

main :: IO()
main = do
    let input = BS.pack $ concat $ replicate 48 ['A'] 
    encryptionMode <- randomRIO(0,1) :: IO Int 
    let cText = [aesEncrypt (encryptionMode) (input)]
    let cChunks = concat $ map (\x-> chunk16 x) cText 

    print $ head cText

    if encryptionMode == 0
        then do print $ "Encrypted With: EBC"
    else do print $ "Encrypted With: CBC"

    if isCBC cChunks
        then do print $ "Oracle Says: CBC"
    else do print $ "Oracle Says: ECB"

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
     numBytes <- numAppend
     let prefix = BS.pack $ take (numBytes) (randoms h :: [Char]) 
     let suffix = BS.pack $ take (numBytes) (randoms h :: [Char]) 
     return $ BS.append (BS.append prefix pText) suffix 

--Encrypt using AES-128 
aesEncrypt :: Int -> BS.ByteString -> BS.ByteString
aesEncrypt x contents 
       |x == 0 = ecbEncrypt (unsafePerformIO aes128) (padBytes $ unsafePerformIO $ appendBytes contents)
       |otherwise = cbcEncrypt (unsafePerformIO aes128) (unsafePerformIO iv) (padBytes $ unsafePerformIO $ appendBytes contents)

--Split a bytestring into 16-byte chunks
chunk16 :: BS.ByteString -> [String]
chunk16 bstr = chunksOf 16 (BS.zipWith (\x y -> x) bstr bstr) 

{-Gets thestring that has been AES encrypted. Only the string AES encrypted in ECB mode will
have been shortened during check for duplicates-}
isCBC :: [String] -> Bool
isCBC chunks = length (chunks) == length (nub $ chunks)