module HipBarcodes where

-- You may add, remove, or edit imports as you wish. Note the use of
-- qualified imports to avoid collisions between common names. For
-- example, Prelude and Data.Map and Graphics.Image all define `map`.

import           Code128
import           Data.Char
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.List
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import           System.Environment
import           System.Exit

--------------------------------------------------------------------------------
-- NOTE: Defining these at top-level rather than changing Code128.TheCodes
-- to include them, like the start and stop symbols.

bToC, cToB :: SymbolId
bToC = 99
cToB = 100

--------------------------------------------------------------------------------
-- 1. General Decoding

decode :: TheCodes -> BC -> Either Error String
decode theCodes (start_, data_, checksum_, stop_) =
  if start_ /= 104 && start_ /= 105
    then (Left $ "decode: bad start: " ++ (show start_))
  else if stop_ /= 106
    then (Left $ "decode: bad stop: " ++ (show stop_))
  else if (computeChecksum start_ data_) /= checksum_
    then (Left "")
  else (Right (fst theValues)) where
        theValues = foldl (\(y, i) x -> 
          if x == 99 && i == 104
            then (y, 105)
          else if x == 100 && i == 105
            then (y, 104)
          else if i == 104
            then case ((bDecodings theCodes) ! x) of 
              (Left ch) -> (y++[ch], i)
              _ -> (y, i)
          else case ((cDecodings theCodes) ! x) of 
              (Left (c1, c2)) -> (y++[c1, c2], i)
              _ -> (y, i)) ([], start_) data_

--------------------------------------------------------------------------------
-- 2. Optimal Encodings

-- -- TODO: This is a placeholder definition, to be replaced.
-- encode :: TheCodes -> String -> Either Error BC
-- encode theCodes str
--   | not $ all isPrintable str = Left "encode: unsupported characters"
--   | all isDigit str           = encodeC theCodes str
--   | otherwise                 = encodeB theCodes str

encode :: TheCodes -> String -> Either Error BC
encode theCodes str
  | not $ all isPrintable str = Left "encode: unsupported characters"
  | otherwise = (Right (startSymbol, allSymbols, (computeChecksum startSymbol allSymbols), (stop theCodes))) where
    separatedNums = separateNums str
    startSymbol = 
      if isDigit (head $ head separatedNums) && (length (head separatedNums) == 2 || length (head separatedNums) >= 4) && maxLen == 0 && mod (length (head separatedNums)) 2 == 0
        then (startC theCodes)
      else if isDigit (head $ head separatedNums) && length (head separatedNums) >= 4 && maxLen > 0
        then (startC theCodes)
      else (startB theCodes)
    allSymbols = fst $ foldl (\(y, i) x -> 
      if i == 0 && maxLen == 0 && isDigit (head x) && (length x == 2 || length x >= 4)
        then (y ++ (notLast (encodeInC x True i)), (i+1))
      else if i == 0 && isDigit (head x) && length x >= 4
        then (y ++ (encodeInC x False i), (i+1))
      else if i == maxLen && isDigit (head x) && length x >= 4
        then (y ++ (notLast (encodeInC x True i)), (i+1))
      else if i /= 0 && i /= maxLen && isDigit (head x) && length x >= 6
        then (y ++ (encodeInC x True i), (i+1))
      else if i == maxLen
        then (y ++ (encodeInB x), (i+1))
      else (y ++ (encodeInB x), (i+1))) ([], 0) separatedNums
    encodeInC str first i = 
      if mod (length str) 2 /= 0 && first
        then [((bEncodings theCodes) ! (head str)), 99] ++ (map (\x -> (cEncodings theCodes) ! x) (adjacentPairs (tail str))) ++ [100]
      else if mod (length str) 2 /= 0 && i /= 0
        then [99] ++ (map (\x -> (cEncodings theCodes) ! x) (adjacentPairs (notLast str))) ++ [100, ((bEncodings theCodes) ! (last str))]
      else if mod (length str) 2 /= 0
        then (map (\x -> (cEncodings theCodes) ! x) (adjacentPairs (notLast str))) ++ [100, ((bEncodings theCodes) ! (last str))]
      else if i /= 0
        then [99] ++ (map (\x -> (cEncodings theCodes) ! x) (adjacentPairs str)) ++ [100]
      else (map (\x -> (cEncodings theCodes) ! x) (adjacentPairs str)) ++ [100]
    encodeInB str = (map (\x -> (bEncodings theCodes) ! x) str)
    notLast str = reverse $ tail $ reverse str
    maxLen = (length separatedNums) - 1
separateNums :: String -> [String]
separateNums str =
  helper str [] [] where
    helper [] xs result = result++[xs]
    helper (c:cs) xs result = 
      if length xs == 0 
        then helper cs [c] result
      else if (isDigit c) == (isDigit (head xs))
        then helper cs (xs++[c]) result
      else helper cs [c] (result++[xs])

-- encodeB :: TheCodes -> String -> Either Error BC
-- encodeB theCodes str =
--   if foldr (\x y -> isPrintable x && y) True str
--     then (Right (startCode, allSymbols, (computeChecksum startCode allSymbols), (stop theCodes)))
--   else (Left "encodeB: unsupported characters")
--    where
--     startCode = (startB theCodes)
--     allSymbols = (map (\x -> (bEncodings theCodes) ! x) str)

--------------------------------------------------------------------------------
-- 3. Making Barcodes

makeBarcode :: FilePath -> Int -> Int -> BCString -> IO ()
makeBarcode filePath imageHeight moduleWidth (BCString symbols) = do
  let img = Image.makeImageR VU (imageHeight, imageWidth) makePixel
  Image.writeImage filePath img where
    imageWidth = (11*moduleWidth*((length symbols) - 1)) + (moduleWidth*13)
    allPixels = (expandSymbols 5 (BCString (symbols++[[True, True]])))
    makePixel = ((\(i, j)-> getPixel allPixels j)::(Int, Int) -> Pixel Y Double)

getPixel :: (Num e) => [Bool] -> Int -> Pixel Y e
getPixel pixels j = if pixels !! j then PixelY 0 else PixelY 255

expandSymbols :: Int -> BCString -> [Bool]
expandSymbols moduleWidth (BCString symbols) = 
  let combinedSymbols = foldl (\x y -> x ++ y) [] symbols in
    expand moduleWidth combinedSymbols where
      expand i [] = []
      expand i (x:xs) = (getList i x) ++ (expand i xs)
      getList i x = if i <= 0 then [] else [x] ++ (getList (i-1) x)

--------------------------------------------------------------------------------
-- 4. Scanning Barcodes

scanBarcode :: FilePath -> IO BCString
scanBarcode filePath = do
  img <- Image.readImageRGB VU filePath
  return (imageToBCString img)

imageToBCString :: (MArray arr cs e, ToY cs e) => Image arr cs e -> BCString
imageToBCString img =
  strToBCString $ imgToStr img

imgToStr :: (MArray arr cs e, ToY cs e) => Image arr cs e -> String
imgToStr img =
  takeEvery moduleWidth $ longStr where
    columns = Image.cols img
    longStr = foldl (\x y -> 
                              if isBlack (Image.index img (0, y)) 
                                then x ++ "1" 
                              else x ++ "0") [] [0..columns-1]
    moduleWidth = getModuleWidth longStr

strToBCString :: String -> BCString
strToBCString str = 
  (BCString (map reverse 
                        (reverse (fst (foldr (\x (lst, i) -> 
                          if i > 0 
                            then (updateString x lst i) 
                          else (addString x lst)) ([[]], 11) strWithoutLastTwo))))) where
    strToBC x = if x == '1' then True else False
    strWithoutLastTwo = reverse . tail . tail $ reverse str
    addString x lst = ((lst ++ [[(strToBC x)]]), 10)
    updateString x lst i = (((reverse . tail $ reverse lst) ++ [(last lst) ++ [(strToBC x)]]), i - 1)

isBlack :: (ToY cs e) => Pixel cs e -> Bool
isBlack pixel =
  let PixelY y = toPixelY pixel in y == 0

getModuleWidth :: String -> Int
getModuleWidth str =
  foldr min 16 (fst3 $ foldr (\x (y, i, prev) -> 
    if i == 0 
      then (y, (i + 1), x) 
    else if x /= prev 
      then (y++[i], 1, x) 
    else (y, (i + 1), x)) ([], 0, '2') str)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  fst $ foldl (\(y, i) x -> if i == 1 then ((y ++ [x]), n) else (y, i - 1)) ([], n) xs

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

--------------------------------------------------------------------------------
-- 5. Scanning Designed Barcodes

scanDesignedBarcode :: FilePath -> IO BCString
scanDesignedBarcode filePath =
  undefined

--------------------------------------------------------------------------------
-- Main

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> FilePath -> Int -> Int -> String
  -> IO ()
runEncoder f filePath height moduleWidth str = do
  theCodes <- loadTheCodes
  let result = bcToBCString theCodes <$> f theCodes str
  either (const (die "encoder failed")) printEncoding result
    where
      printEncoding bcString = do
        putStrLn $ "\nPayload:\n" ++ str
        putStrLn $ "\nEncoding:\n" ++ show bcString
        makeBarcode filePath height moduleWidth bcString

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f filePath = do
  theCodes <- loadTheCodes
  bcString <- scanBarcode filePath
  let bc = bcStringToBC theCodes bcString
  either (const (die "decoder failed")) printDecoding (f theCodes bc)
    where
      printDecoding str = do
        putStrLn $ "\nDecoding:\n" ++ str
        putStrLn ""

main :: IO ()
main =
  getArgs >>= processArgs
  where
    processArgs ["encode", filePath, imageHeight, moduleWidth, str] =
      HipBarcodes.runEncoder
        encode filePath (read imageHeight) (read moduleWidth) str
    processArgs ["decode", file] =
      HipBarcodes.runDecoder decode file
    processArgs _ =
      die $ "\nUsage:\n\n"
        ++ "  cabal run hip-barcodes -- encode imageFilePath imageHeight moduleWidth string\n"
        ++ "  cabal run hip-barcodes -- decode imageFilePath\n"