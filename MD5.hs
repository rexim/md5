{-# LANGUAGE ExtendedDefaultRules #-}

module MD5 (md5String, md5File) where

import Data.Word
import Data.Bits
import Data.Char
import Data.List (genericLength)
import qualified Data.ByteString.Lazy as BS
import Data.List.Split (chunksOf)

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

oneRound :: [(Word32, Int, Word32)] -> State -> AuxFunction -> State
oneRound f = foldl $ \(x, s, t) (a, b, c, d) = (d, a', b, c)
  where a' = b + ((a + (f b c d) + x + t) `rotateL` s)

-- Implements https://tools.ietf.org/html/rfc1321
transform :: [Word32] -> State -> State
transform blocks (a, b, c, d) = (a + aa, b + bb, c + cc, d + dd) where 
  round1           = oneRound auxF (a, b, c, d) data1
  round2           = oneRound auxG round1 data2
  round3           = oneRound auxH round2 data3
  (aa, bb, cc, dd) = oneRound auxI round3 data4
  
  data1 = zip3 (x 0 1) (s [7, 12, 17, 22]) (t  1 16)
  data2 = zip3 (x 1 5) (s [5,  9, 14, 20]) (t 17 32)
  data3 = zip3 (x 5 3) (s [4, 11, 16, 23]) (t 33 48)
  data4 = zip3 (x 0 7) (s [6, 10, 15, 21]) (t 49 64)

  x start step = take 16 $ map head $ chunksOf (step - 1) $ drop start $ cycle blocks where
  s xs = take 16 $ cycle xs
  t start end = map (\x -> truncate $ 4294967296 * abs (sin x)) [start..end]

padding :: [Word8] -> [Word8]
padding xs = xs ++ (take n pad) ++ count where
  pad = 0x80:replicate 63 0

  n = if a > 0 then a else 64 + a
  a = 56 - (fromIntegral $ byteLen `mod` 64)

  byteLen = genericLength xs
  bitLen  = byteLen * 8

  count = encode $ map fromIntegral
    [ bitLen .&. 0xffffffff
    , bitLen `shiftR` 32 .&. 0xffffffff
    ]

md5String :: String -> String
md5String = md5 . map (fromIntegral . ord)

md5 :: [Word8] -> String
md5 = getDigest . foldr (transform . decode) initialState . reverse . chunksOf 64 . padding

md5File :: FilePath -> IO String
md5File =  fmap (md5 . BS.unpack) . BS.readFile

encode :: [Word32] -> [Word8]
encode = concatMap $ \x -> map fromIntegral
  [ x .&. 0xff
  , x `shiftR` 8 .&. 0xff
  , x `shiftR` 16 .&. 0xff
  , x `shiftR` 24 .&. 0xff
  ]

decode :: [Word8] -> [Word32]
decode = map (foldr (\y x -> x * 256 + fromIntegral y) 0) . chunksOf 4

getDigest :: State -> String
getDigest (a, b, c, d) = map (intToDigit . fromIntegral) $ encode [a, b, c, d] >>= \x ->
  [ x `shiftR` 4 .&. 0xf
  , x .&. 0xf
  ]
