import Data.List 
import Data.List.Split
import System.Random
import Crypto.Cipher
import Crypto.Data.PKCS7
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64

keylen = 16
strAppend = B64.decodeLenient $ BS.pack ("Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")
myStr = BS.pack $ concat $ replicate (3 * keylen) ['A']

main :: IO()
main = do
    let cText = [aesEncrypt (myStr)]
    let cChunks = concat $ map (\x-> chunk16 x) cText
    print $ getBlockSize (BS.pack "") strAppend

    if isNotECB cChunks
        then do print $ "Not using ECB"
    else do print $ "Oracle Says: Yep, we're using ECB"

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

--Encrypt using AES-128 in ECB mode
aesEncrypt :: BS.ByteString -> BS.ByteString
aesEncrypt contents  = ecbEncrypt (unsafePerformIO aes128) (padBytes contents)

{-With padding, all cipherText in same block should have the same length.
Function finds the blocksize by adding bytes until the cipherText length increases -}

getBlockSize :: BS.ByteString -> BS.ByteString -> Int 
getBlockSize check unknown 
     |differentLength check unknown = BS.length (aesEncrypt (BS.append check unknown)) - (BS.length (aesEncrypt (BS.append (BS.pack "") unknown)))
     |otherwise = getBlockSize (BS.append check (BS.pack "A")) unknown 

--Function compares lengths of original and version with appended bytes
differentLength :: BS.ByteString -> BS.ByteString -> Bool
differentLength check unknown = (BS.length (aesEncrypt (BS.append check unknown))) /= (BS.length (aesEncrypt (BS.append (BS.pack "") unknown)))

--Split a bytestring into chunks
chunk16 :: BS.ByteString -> [String]
chunk16 bstr = chunksOf (getBlockSize (BS.pack "") strAppend) (BS.zipWith (\x y -> x) bstr bstr) 

{-Gets thestring that has been AES encrypted and returns true if it detects CBC Mode. Only the string 
AES encrypted in ECB mode will have been shortened during check for duplicates-}
isNotECB :: [String] -> Bool
isNotECB chunks = length (chunks) == length (nub $ chunks)




