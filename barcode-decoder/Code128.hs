module Code128 where

import           Data.Char
import           Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           System.Environment
import           System.Exit


--------------------------------------------------------------------------------
-- Data Representation

data BCString =            -- "Barcode String"
  BCString [Symbol]        --   Start, data, checksum, and stop symbols.
                           --   The final bar ("11") is implicit.
                           --   No quiet zones.

type Symbol   = [Module]   -- Always length 11
type Module   = Bool       -- True/False means part of bar/space

type BC =                  -- "Barcode" (internal representation)
  ( SymbolId               --   Start symbol
  , [SymbolId]             --   Encoded data
  , SymbolId               --   Checksum
  , SymbolId               --   Stop symbol
  )                        --   Final bar is implicit

type SymbolId = Int
type BValue   = Either Char String
type CValue   = Either (Char, Char) String

data TheCodes =
  TheCodes
    { startB       :: SymbolId
    , startC       :: SymbolId
    , stop         :: SymbolId
    , idsToSymbols :: Map SymbolId Symbol
    , symbolsToIds :: Map Symbol SymbolId
    , bEncodings   :: Map Char SymbolId
    , cEncodings   :: Map (Char, Char) SymbolId
    , bDecodings   :: Map SymbolId BValue
    , cDecodings   :: Map SymbolId CValue
    } deriving (Show)

type Error = String


--------------------------------------------------------------------------------
-- 1. Data Loading

loadTheCodes :: IO TheCodes
loadTheCodes = do
  rows <- map (splitOn ',') <$> lines <$> readFile "code128.csv"
  return $ rowsToCodes $ dropFirstAndLast rows

rowsToCodes :: [[String]] -> TheCodes
rowsToCodes rows =

  -- Perfect opportunity for NamedFieldPuns. Try it!
  TheCodes
    { startB = 104, startC = 105, stop = 106
    , idsToSymbols = idsToSymbols
    , symbolsToIds = symbolsToIds
    , bEncodings = bEncodings
    , cEncodings = cEncodings
    , bDecodings = bDecodings
    , cDecodings = cDecodings
    }

  where
    (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
      foldr processRow
        (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
        rows

    processRow row accumulators =
      accumulators' where

        [_id, _pattern, _, _, _bValue, _cValue] =
          row

        (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
          accumulators

        accumulators' =
          (idsToSymbols', symbolsToIds', bEncodings', cEncodings', bDecodings', cDecodings')

        symbolId =
          undefined

        idsToSymbols' =
          undefined

        symbolsToIds' =
          undefined

        bEncodings' =
          undefined

        cEncodings' =
          undefined

        bDecodings' =
          undefined

        cDecodings' =
          undefined

splitOn :: Char -> String -> [String]
splitOn delim str =
  undefined

dropFirstAndLast :: [a] -> [a]
dropFirstAndLast xs =
  undefined

readSymbol :: String -> Symbol
readSymbol str =
  undefined

readBValue :: String -> BValue
readBValue str =
  undefined

readCValue :: String -> CValue
readCValue str =
  undefined


--------------------------------------------------------------------------------
-- 2. Basic Encodings

encodeB :: TheCodes -> String -> Either Error BC
encodeB theCodes str =
  undefined

encodeC :: TheCodes -> String -> Either Error BC
encodeC theCodes str =
  undefined

computeChecksum :: SymbolId -> [SymbolId] -> Int
computeChecksum symbol symbols =
  undefined

isPrintable :: Char -> Bool
isPrintable c =
  undefined

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs helmets =
  undefined

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe maybes =
  undefined


--------------------------------------------------------------------------------
-- 3. Basic Decodings

decodeB :: TheCodes -> BC -> Either Error String
decodeB theCodes (start_, data_, checksum_, stop_) =
  undefined

decodeC :: TheCodes -> BC -> Either Error String
decodeC theCodes (start_, data_, checksum_, stop_) =
  undefined


--------------------------------------------------------------------------------
-- 4. Barcode String Manipulation

finalBar     = "11"
symbolLength =  11

instance Show BCString where
  show :: BCString -> String
  show (BCString symbols) =
    undefined

instance Read BCString where
  readsPrec :: Int -> String -> [(BCString, String)]
  readsPrec _ str =
    case maybeReadBCString str of
      Just bcString -> [(bcString, "")]
      Nothing       -> []

maybeReadBCString :: String -> Maybe BCString
maybeReadBCString str =
  undefined


--------------------------------------------------------------------------------

run :: (TheCodes -> a -> Either Error b) -> a -> IO (Either Error b)
run f a = do
  theCodes <- loadTheCodes
  pure $ f theCodes a

runEncoderAndShow
  :: (TheCodes -> String -> Either Error BC) -> String
  -> IO ()
runEncoderAndShow f str = do
  theCodes <- loadTheCodes
  case f theCodes str of
    Left error -> die error
    Right bc   -> putStrLn $ show $ bcToBCString theCodes bc

bcToBCString :: TheCodes -> BC -> BCString
bcToBCString theCodes (start, data_, checksum, stop) =
  let symbolIds = [start] ++ data_ ++ [checksum, stop] in
  BCString $ map (\i -> (idsToSymbols theCodes) ! i) symbolIds

runDecoderAndShow
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoderAndShow f str = do
  theCodes <- loadTheCodes
  let barcode = bcStringToBC theCodes $ read str
  case f theCodes barcode of
    Left error -> die error
    Right str  -> putStrLn str

bcStringToBC :: TheCodes -> BCString -> BC
bcStringToBC theCodes (BCString symbols) =
  (start, data_, checksum, stop)
    where
      list     = map (\symbol -> (symbolsToIds theCodes) ! symbol) symbols
      start    = head list
      revRest  = reverse $ tail list
      stop     = head revRest
      checksum = head $ tail revRest
      data_    = reverse $ tail $ tail revRest

main = do
  theCodes <- loadTheCodes
  args <- getArgs
  case args of
    ["encodeB", str] -> runEncoderAndShow encodeB str
    ["decodeB", str] -> runDecoderAndShow decodeB str
    ["encodeC", str] -> runEncoderAndShow encodeC str
    ["decodeC", str] -> runDecoderAndShow decodeC str
    _                -> die "Usage: cabal run code128 -- {en,de}code{B,C} string"
                         -- "Usage: ./Code128 {en,de}code{B,C} string"
