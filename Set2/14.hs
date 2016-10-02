import Data.List
import Data.List.Split
import Crypto.Data.PKCS7
import Crypto.Cipher
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import System.IO.Unsafe

blocksize = 16
pText = B64.decodeLenient $ BS.pack ("Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")

main :: IO()
main = do
     --print $ pText  
     rcount <- randomRIO(0,15) :: IO Int
     print $ rcount 
     --print $ BS.length pText

     rbytes <- randomBytes rcount
     let rKey = randomBytes blocksize
     aes <- aes128 rKey
     let strToDecrypt = BS.append rbytes pText 
     --print $ strToDecrypt  

     let repeatedCharTest = BS.append strToDecrypt (BS.pack $ replicate (blocksize * 2) 'A') 
     --print $ repeatedCharTest
     --print $ aesEncrypt repeatedCharTest aes
     --print $ chunk16 $ aesEncrypt repeatedCharTest aes

     let bytesAdded = decrypt repeatedCharTest aes 0
     --print $ decrypt repeatedCharTest aes 0

     let len = prefixLen (BS.length pText) (bytesAdded) 0
     print $ len 

     print $ BS.drop len strToDecrypt


{-Gets thestring that has been AES encrypted and returns true if it detects CBC Mode. Only the string 
AES encrypted in ECB mode will have been shortened during check for duplicates-}
isNotECB :: [String] -> Bool
isNotECB chunks = length (chunks) == length (nub $ chunks)

{-Random bytestring-}
randomBytes :: Int -> IO BS.ByteString
randomBytes len = do
    g <- newStdGen
    let rbytes = BS.pack $ take len (randoms g :: [Char]) 
    return rbytes

---Initialize cipher context from key
aes128 :: IO BS.ByteString -> IO AES128
aes128 rKey = do
    key <- rKey
    let key' = makeKey key
    case key' of Right key -> return $ cipherInit key
                 otherwise -> error "Context Error"

--Encrypt using AES-128 in ECB mode
aesEncrypt :: BS.ByteString -> AES128 -> BS.ByteString
aesEncrypt contents aes  = ecbEncrypt aes (padBytes contents)

--Split a bytestring into chunks
chunk16 :: BS.ByteString -> [String]
chunk16 bstr = chunksOf blocksize (BS.zipWith (\x y -> x) bstr bstr)

--Add bytes until there is are repeated blocks of cipherText
decrypt :: BS.ByteString -> AES128 -> Int -> Int 
decrypt testStr aes count  
     |isNotECB (chunk16 $ aesEncrypt testStr aes) == False = count
     |otherwise = decrypt (BS.append testStr (BS.pack "A")) aes (count + 1)

prefixLen :: Int -> Int -> Int -> Int
prefixLen textLen bytesAdded prefLen
     |(textLen + bytesAdded + prefLen) `mod` blocksize == 0 = prefLen
     |otherwise = prefixLen textLen bytesAdded (prefLen + 1)

