import Text.Regex
import Data.List
import Crypto.Cipher
import System.Random
import Crypto.Data.PKCS7
import Data.String.Utils
import qualified Data.ByteString.Char8 as BS
stringForParse = "foo=bar&baz=qux&zap=zazzle"

keylen = 16

main :: IO()
main = do
          let fooProfile = profileFor "foo@bar.com"
          print $ encodeProfile fooProfile
          
          aes <- aes128
          let encryptedProfile = aesEncrypt (padBytes $ BS.pack $ encodeProfile fooProfile) aes 
          print $ encryptedProfile

          let decryptedProfile = aesDecrypt encryptedProfile aes
          print $ decryptedProfile

          print $ adminProfile
          
          let encryptAdmin = aesEncrypt (padBytes $ BS.pack $ head adminProfile) aes 
          print $ encryptAdmin

          let decryptAdmin = aesDecrypt encryptAdmin aes
          print $ decryptAdmin


--Parses and encoded profile
kvparse :: String -> [[(String, String)]]
kvparse str = do
     let eqs = map (\x -> splitRegex (mkRegex "=") x) $ splitRegex (mkRegex "&") str
     return $ map (\x -> (head x, last x)) eqs 

--Creates a profile for a user-inputed email
profileFor :: String -> [(String, String)]
profileFor email = [("email", handleMetas(email)), ("uid", "10"), ("role", "user")]

--Encodes a profile with metachars
encodeProfile :: [(String, String)] -> String
encodeProfile profile = join "&" $ map (\x -> fst x ++ "=" ++ snd x) profile 

--Deletes all metachars from a string
handleMetas :: String -> String
handleMetas str = deleteChar '&' (deleteChar '=' str)

--Deletes all instances of a char in a word
deleteChar :: Char -> String -> String 
deleteChar char str  
     |delete char str == str = str
     |otherwise = deleteChar char (delete char str)

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
aesEncrypt :: BS.ByteString -> AES128 -> BS.ByteString
aesEncrypt contents aes = ecbEncrypt aes contents 

--Decrypts the user profile and parses it 
aesDecrypt :: BS.ByteString -> AES128 -> [(String, String)]
aesDecrypt encoded aes = do
     case (unpadBytes $ ecbDecrypt aes encoded) of Just x  -> concat $ kvparse $ BS.unpack x
                                                   Nothing -> concat $ kvparse $ BS.unpack $ ecbDecrypt aes encoded

--Creates an encoded foobar profile and replaces "user" with "admin"
adminProfile:: [String]
adminProfile = do
    let base = encodeProfile $ profileFor "foo@bar.com"
    let admin = replace "user" "admin" base
    return $ admin
