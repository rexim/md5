module MD5 (md5String, md5File) where

import Data.Word
import Data.Bits
import Data.Char
import Data.List (genericLength)
import qualified Data.ByteString.Lazy as BS

skip _ [] = []
skip n (x:xs) = x:(skip n $ drop n xs)

type State = (Word32, Word32, Word32, Word32)
type AuxFunction = Word32 -> Word32 -> Word32 -> Word32

auxF :: AuxFunction
auxF x y z = x .&. y .|. (complement x) .&. z

auxG :: AuxFunction
auxG x y z = x .&. z .|. y .&. (complement z)

auxH :: AuxFunction
auxH x y z = x `xor` y `xor` z

auxI :: AuxFunction
auxI x y z = y `xor` (x .|. complement z)

initialState :: State
initialState = (0x67452301,
                0xefcdab89,
                0x98badcfe,
                0x10325476)

oneRound :: State -> [(Word32, Int, Word32)] -> AuxFunction -> State
oneRound state [] _ = state
oneRound (a, b, c, d) ((x, s, t):rest) f = oneRound (d, a', b, c) rest f
  where a' = b + ((a + (f b c d) + x + t) `rotateL` s)

transform :: State -> [Word32] -> State
transform (a, b, c, d) blocks = (a + aa, b + bb, c + cc, d + dd)
  where round1           = oneRound (a, b, c, d) data1 auxF
        round2           = oneRound round1       data2 auxG
        round3           = oneRound round2       data3 auxH
        (aa, bb, cc, dd) = oneRound round3       data4 auxI
        
        data1 = zip3 (x 0 1) (s [7, 12, 17, 22]) (t  1 16)
        data2 = zip3 (x 1 5) (s [5,  9, 14, 20]) (t 17 32)
        data3 = zip3 (x 5 3) (s [4, 11, 16, 23]) (t 33 48)
        data4 = zip3 (x 0 7) (s [6, 10, 15, 21]) (t 49 64)

        x start step = take 16 $ skip (step - 1) (drop start blocks ++ cycle blocks)
        s xs = take 16 $ cycle xs
        t start end = map (\x -> truncate (4294967296 * (abs $ sin x))) [start..end]

padding :: [Word8] -> [Word8]
padding xs = xs ++ (take n pad) ++ count
  where pad = 0x80:replicate 63 0

        n = if a > 0 then a else 64 + a
        a = 56 - (fromIntegral $ byteLen `mod` 64)

        byteLen = genericLength xs :: Word64
        bitLen  = byteLen * 8

        count = encode $ map fromIntegral [bitLen .&. 0xffffffff,
                                           bitLen `shiftR` 32 .&. 0xffffffff]

md5String :: String -> String
md5String xs = md5 $ map (fromIntegral.ord) xs

md5 :: [Word8] -> String
md5 xs = let iter :: State -> [Word8] -> State
             iter state [] = state
             iter state ms = iter state' restMs
               where state' = transform state (decode currentMs)
                     (currentMs, restMs) = splitAt 64 ms
         in getDigest $ iter initialState message
           where message = padding xs

md5File :: FilePath -> IO String
md5File filename = do bs <- BS.readFile filename
                      return $ md5 (BS.unpack bs)

encode :: [Word32] -> [Word8]
encode xs = concatMap f xs
  where f :: Word32 -> [Word8]
        f x = map fromIntegral [x .&. 0xff,
                                x `shiftR` 8 .&. 0xff,
                                x `shiftR` 16 .&. 0xff,
                                x `shiftR` 24 .&. 0xff]

decode :: [Word8] -> [Word32]
decode [] = []
decode xs = (f a):(decode b)
  where f ys = foldr (\y x -> x * 256 + fromIntegral y) 0 ys
        (a, b) = splitAt 4 xs

getDigest :: State -> String
getDigest (a, b, c, d) = map intToDigit $ concatMap f (encode [a, b, c, d])
  where f :: Word8 -> [Int]
        f x = map fromIntegral [x `shiftR` 4 .&. 0xf,
                                x .&. 0xf]