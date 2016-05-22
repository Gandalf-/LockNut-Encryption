import Data.Char

-- max character supported
maxChr = 100000

-- takes the inverse of a list
inverseList :: (Num a) => [a] -> [a]
inverseList xs = map ((-1) *) xs

-- extended vigenere cipher, shifts are modulo uft-8 max char
cipher :: String -> [Int] -> String
cipher input key
  | length input > length key = zipWith shift inputNums $ cycle key
  | otherwise                 = zipWith shift key inputNums
  where
    inputNums = map ord input
    shift x y = chr $ mod (x + y) maxChr

-- splits a list into n sublists
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n). iterate (drop n)

-- encrypts the first chunk with the key. Subsequent chunks are encrypted using
-- the plain text version of the previous chunk.
waterfallEncrypt :: String -> String -> String
waterfallEncrypt input key = 
  encrypt $ splitEvery (length key) input
  where
    encrypt [x]  = cipher x $ map ord key
    encrypt xs   = (encrypt $ init xs) ++ cipher (last xs) (prevChunk xs)
    prevChunk xs = map ord (last $ init xs)

-- decrypts the first element of input with the key. Susequent elements are decrypted
-- with the previous element. Recursion stops when there is only on element left. 
waterfallDecrypt :: String -> String -> String
waterfallDecrypt input key =
  decrypt [] $ splitEvery (length key) (key ++ input)
  where
    decrypt out [x] = out
    decrypt out xs  = decrypt (out ++ cipher (nChunk xs) (cChunk xs)) (newInput xs)
    newInput xs     = (cipher (nChunk xs) (cChunk xs)) : (tail $ tail xs)
    cChunk xs       = inverseList $ map ord $ head xs
    nChunk xs       = head $ tail xs
