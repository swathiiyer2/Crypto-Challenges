import Data.Char
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
    aes <- aes128
    let cText = [aesEncrypt (myStr) aes strAppend]
    let cChunks = concat $ map (\x-> chunk16 x aes) cText
    let blockSize = getBlockSize (BS.pack "") aes 
    print $ blockSize
    if isNotECB cChunks
        then do print $ "Not using ECB"
    else do print $ "Oracle Says: Yep, we're using ECB"

    let inputBlock = BS.pack $ concat $ replicate (blockSize -1) ['A']
    let allBStrings = decrypt inputBlock aes strAppend []    
    let decrypted = BS.pack $ concat $ map (\x -> getLastByte x) allBStrings
    if decrypted == strAppend
        then do print $ decrypted
    else do print $ "Error"

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

--Encrypt using AES-128 in ECB mode
aesEncrypt :: BS.ByteString -> AES128 -> BS.ByteString -> BS.ByteString
aesEncrypt contents aes append = ecbEncrypt aes (padBytes $ BS.append contents append)

{-With padding, all cipherText in same block should have the same length.
Function finds the blocksize by adding bytes until the cipherText length increases -}

getBlockSize :: BS.ByteString -> AES128 -> Int 
getBlockSize check aes 
     |differentLength check aes = BS.length (aesEncrypt check aes strAppend) - (BS.length (aesEncrypt (BS.pack "") aes strAppend))
     |otherwise = getBlockSize (BS.append check (BS.pack "A")) aes

--Function compares lengths of original and version with appended bytes
differentLength :: BS.ByteString -> AES128 -> Bool
differentLength check aes = (BS.length (aesEncrypt check aes strAppend)) /= (BS.length (aesEncrypt (BS.pack "") aes strAppend))

--Split a bytestring into chunks
chunk16 :: BS.ByteString -> AES128 -> [String]
chunk16 bstr aes = chunksOf (getBlockSize (BS.pack "") aes) (BS.zipWith (\x y -> x) bstr bstr) 

{-Gets thestring that has been AES encrypted and returns true if it detects CBC Mode. Only the string 
AES encrypted in ECB mode will have been shortened during check for duplicates-}
isNotECB :: [String] -> Bool
isNotECB chunks = length (chunks) == length (nub $ chunks)

{-Make a dictionary of every possible last byte by feeding different strings to the oracle, and
 remembering the first block of each invocation.-}
dictionary :: BS.ByteString -> AES128 -> [(BS.ByteString, BS.ByteString)]
dictionary input aes= map (\x -> (BS.pack $ head $ chunk16 (aesEncrypt x aes strAppend) aes, x)) $ zipWith (\x y -> (BS.append x y)) (replicate 256 input) (map (\x->BS.pack [x]) [chr(0) .. chr(255)])

{-Match the output of the one-byte-short input to one of the entries in your dictionary. Gives the first 
byte of unknown-string.-}
getNextByte :: BS.ByteString -> AES128 -> BS.ByteString -> Maybe BS.ByteString
getNextByte inputBlock aes append = lookup (BS.pack $ head $ chunk16 (aesEncrypt inputBlock aes append) aes) (dictionary inputBlock aes)

--Gets next byte for all bytes in the unknown string
decrypt :: BS.ByteString -> AES128 -> BS.ByteString -> [Maybe BS.ByteString] -> [Maybe BS.ByteString]
decrypt inputBlock aes append list 
    |BS.null append = list
    |otherwise = decrypt inputBlock aes (BS.tail append) (list ++ [getNextByte inputBlock aes append])

--Gives only the byte contained in the unknown string
getLastByte :: Maybe BS.ByteString -> String
getLastByte bstring = do
     case bstring of Just x  -> return (BS.last x)
                     Nothing -> return ' '