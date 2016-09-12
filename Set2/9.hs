import Data.Char 
import Crypto.Data.PKCS7
import qualified Data.ByteString.Char8 as B 

block = B.pack $ "YELLOW SUBMARINE"
expected = B.pack $ "YELLOW SUBMARINE\x04\x04\x04\x04"
blockSize = 20

main :: IO()
main = do
    if expected == padBytesN (padSize block blockSize) block
        then do print $ padBytesN (padSize block blockSize) block
    else do print $ "Error"

--The number of bytes that need to be added
padSize :: B.ByteString -> Int -> Int
padSize str len = len - (B.length str)