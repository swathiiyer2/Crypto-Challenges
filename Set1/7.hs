import Crypto.Cipher
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Base64 as D

Right key = makeKey (B.pack "YELLOW SUBMARINE")

main :: IO()
main = do
         file <- B.readFile "7.txt"
         let decodedFile = D.decodeLenient file
         putStrLn $ B.unpack $ aesDecrypt decodedFile 

--Initialize cipher context from key
aes128 :: AES128
aes128 = cipherInit key

--Decrypt using AES-128 in ECB mode
aesDecrypt :: B.ByteString -> B.ByteString
aesDecrypt contents = ecbDecrypt aes128 contents