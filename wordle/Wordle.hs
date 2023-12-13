module Main where

import System.IO
import System.Environment
import System.Exit
import System.Random
import Data.Char hiding (isNumber)
import Data.List
import System.Console.ANSI
import Data.Colour.SRGB (sRGB24)
import System.IO.NoBufferingWorkaround

maxGuesses = 6
wordLength = 5
wordListLength = 2315
invalidGameError = "Invalid game number\n"
firstGuessMade = False
usage = "Usage:\n\n  ./wordle                  Play random game\n  ./wordle gameNumber       Play specific game\n  ./wordle --how-to-play    Display instructions\n"

data Model = 
  Model {wordList :: [String], answer :: String, theBoard :: [String], currentGuess :: String, errorMessage :: String}

data Action =
  KeyPress Char

debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()

isNumber :: String -> Bool
isNumber s = and [all isDigit s, length s > 0]

removeLastLetter :: String -> String
removeLastLetter "" = ""
removeLastLetter (x:"") = ""
removeLastLetter (x:xs) = [x] ++ removeLastLetter xs

removeLastLetters :: String -> Int -> String
removeLastLetters s 0 = s
removeLastLetters s i = removeLastLetters (removeLastLetter s) (i - 1)

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  initGetCharNoBuffering
  hSetEcho stdin False

  args <- getArgs

  hFlush stdout

  file <- readFile "words.txt"
  let wordList = lines (file)
  randomNum <- (randomRIO (0, (length wordList) - 1) :: IO Int)
  let randomWord = wordList !! randomNum

  if length args == 0
      then return ()
  else if length args > 1
    then do {printPrompt usage; exitSuccess}
  else if head args == "--how-to-play"
    then do {printHowToPlay; exitSuccess}
  else if not (isNumber (head args))
    then do {printPrompt invalidGameError; exitSuccess}
  else return ()

  let wordNumber = if length args == 0
                    then 0
                  else if isNumber (head args)
                    then read (head args) :: Int
                  else 0
  
  if or [wordNumber > (length wordList) - 1, wordNumber < 0]
    then do {printPrompt invalidGameError; exitSuccess}
  else return ()

  let selectedWord = if length args == 0
                      then randomWord
                    else wordList !! wordNumber
  printPrompt "Guess the wordle!\n "
  view (Model wordList selectedWord [] "" "")
  model <- controller (Model wordList selectedWord [] "" "")
  return ()

--------------------------------------------------------------------------------
-- Controller

controller :: Model -> IO Model
controller (Model wordList answer board currentWord message) = do
  input <- getCharNoBuffering
  hFlush stdout

  let result = (update (KeyPress input) (Model wordList answer board currentWord ""))
  let newBoard = (theBoard result)
  clearScreen
  if (errorMessage result) == "Exit"
    then exitSuccess
  else if ((length newBoard) > 0 && (map toUpper (last newBoard)) == (map toUpper answer)) || length newBoard == 6
    then return ()
  else printPrompt (errorMessage result)
  
  if (length newBoard) > 0 && (map toUpper (last newBoard)) == (map toUpper answer)
    then do {(printEndgameMessage newBoard); view result; return result}
  else if length newBoard == 6
    then do {printPrompt ("Bummer, the answer was " ++ (map toUpper answer) ++ "\n"); view result; return result}
  else do {view result; controller result}

update :: Action -> Model -> Model
update (KeyPress key) (Model wordList answer board currentWord _) =
  let errorMessage = (verifyGuessIsValid wordList currentWord) in 
  if key == '\EOT'
    then (Model wordList answer board currentWord "Exit")
  else if key == '\r' && errorMessage == ""
    then (Model wordList answer (board++[currentWord]) "" "Next guess?\n")
  else if key == '\r'
    then (Model wordList answer board currentWord errorMessage)
  else if key == '\b'
    then (Model wordList answer board (removeLastLetter currentWord) "")
  else if isAlpha key && length currentWord < 5
    then (Model wordList answer board (currentWord++[key]) "")
  else (Model wordList answer board currentWord "")


verifyGuessIsValid :: [String] -> String -> String
verifyGuessIsValid wordList s =
  if length s < 5
    then "Not enough letters\n"
  else if not (elem s wordList)
    then "Not in word list\n"
  else ""

--------------------------------------------------------------------------------
-- View

printPrompt :: String -> IO ()
printPrompt str = do
  putStr $ id str

printHowToPlay :: IO ()
printHowToPlay = do
  printPrompt "\nHOW TO PLAY\n\nGuess the WORDLE in 6 tries.\n\nEach guess must be a valid 5 letter word. Hit the enter button to submit.\n\nExamples\n\n "
  printGuess "which" "weary"
  printPrompt "  The letter W is in the word and in the correct spot.\n "
  printGuess "which" "pills"
  printPrompt "  The letter I is in the word but in the wrong spot.\n "
  printGuess "which" "vague"
  printPrompt "  None of the letters are in the word in any spot.\n"


view :: Model -> IO ()
view (Model wordList answer board currentWord _) = do
  printPrompt "\n"
  setSGR [SetColor Background Dull White]
  printPrompt "                   "
  setSGR [Reset]
  printPrompt "\n"
  repeatPrintingLine maxGuesses answer board currentWord
  setSGR [SetColor Background Dull White]
  printPrompt "                   "
  setSGR [Reset]
  printPrompt "\n\n"
  printKeyboard answer board

lettersNotInWord :: String -> String -> String
lettersNotInWord answer "" = ""
lettersNotInWord answer (x:xs) = if elem x answer
                                    then lettersNotInWord answer xs
                                 else [x] ++ lettersNotInWord answer xs

lettersInRightPlace :: String -> String -> String
lettersInRightPlace answer "" = ""
lettersInRightPlace "" guess = ""
lettersInRightPlace (c:answer) (x:guess) = if x == c
                                              then [x] ++ lettersInRightPlace answer guess
                                           else lettersInRightPlace answer guess

printKeyboard :: String -> [String] -> IO ()
printKeyboard answer board = do
  let wrongLetters = map toUpper (foldr (++) "" (map (lettersNotInWord answer) board))
  let correctLetters = map toUpper (foldr (++) "" (map (lettersInRightPlace answer) board))
  printKey "QWERTYUIOP" wrongLetters correctLetters board
  printPrompt "\n"
  printKey "ASDFGHJKL" wrongLetters correctLetters board
  printPrompt "\n"
  printKey "ZXCVBNM" wrongLetters correctLetters board
  printPrompt "\n"

printKey :: String -> String -> String -> [String] -> IO ()
printKey "" _ _ _ = return ()
printKey (x:xs) wrongLetters correctLetters board = do
  let finalLetter = " " ++ [toUpper x] ++ " "
  if elem x wrongLetters
    then do {setSGR [SetRGBColor Background $ sRGB24 117 117 117]; printPrompt finalLetter; setSGR [Reset]}
  else if elem x correctLetters
    then do {setSGR [SetColor Background Dull Green]; printPrompt finalLetter; setSGR [Reset]}
  else if elem x (map toUpper (foldr (++) "" board))
    then do {setSGR [SetColor Background Dull Yellow]; printPrompt finalLetter; setSGR [Reset]}
  else printPrompt finalLetter
  printKey xs wrongLetters correctLetters board

repeatPrintingLine :: Int -> String -> [String] -> String -> IO ()
repeatPrintingLine i answer [] "" = do
  if i > 0
    then do {setSGR [SetColor Background Dull White]; printPrompt "  "; setSGR [Reset]; printPrompt "               "; setSGR [SetColor Background Dull White]; printPrompt "  "; setSGR [Reset]; printPrompt "\n"; repeatPrintingLine (i - 1) answer [] ""}
  else return ()
repeatPrintingLine i answer [] currentWord = do
  setSGR [SetColor Background Dull White]
  printPrompt "  "
  setSGR [Reset]
  printLetters currentWord 5
  setSGR [SetColor Background Dull White]
  printPrompt "  "
  setSGR [Reset]
  printPrompt "\n"
  repeatPrintingLine (i - 1) answer [] ""
repeatPrintingLine i answer (c:cs) currentWord = do
  setSGR [SetColor Background Dull White]
  printPrompt "  "
  setSGR [Reset]
  printGuess answer c
  setSGR [SetColor Background Dull White]
  printPrompt "  "
  setSGR [Reset]
  printPrompt "\n"
  repeatPrintingLine (i - 1) answer cs currentWord

printGuess :: String -> String -> IO ()
printGuess answer guess = do
  printLetter answer guess 0
  printLetter answer guess 1
  printLetter answer guess 2
  printLetter answer guess 3
  printLetter answer guess 4

printLetters :: String -> Int -> IO ()
printLetters "" 0 = do
  return ()
printLetters "" i = do
  printPrompt "   "
  printLetters "" (i - 1)
printLetters (x:xs) i = do
  printPrompt (" " ++ [toUpper x] ++ " " )
  printLetters xs (i - 1)

getGreenLetters :: String -> String -> Int -> [Int]
getGreenLetters [] _ i = []
getGreenLetters _ [] i = []
getGreenLetters (c:answer) (x:guess) i =
  if x == c
    then [i] ++ getGreenLetters answer guess (i+1)
  else getGreenLetters answer guess (i+1)

-- This function takes in an answer, the guess, and a specific location for a letter in the guess,
-- as well as a list of indices where green letters are in the guess. It then makes a list of indices
-- for letters in the answer that are the same as the indicated letter and don't correspond to a
-- green letter, then checks if the length of that list is longer than or equal to the list of 
-- the same letter in the guess up to and including the specific location of the letter itself,
-- not including any of the letters in that range that are green letters.
-- If true, then the letter is yellow.
getLetterColor :: String -> String -> Int -> [Int] -> String
getLetterColor answer guess i greens = 
  let isYellow = length (filter (\x -> not (elem x greens)) (elemIndices (guess !! i) answer)) >= length (filter (\x -> not (elem x greens)) (elemIndices (guess !! i) (removeLastLetters guess (4-i)))) && length (filter (\x -> not (elem x greens)) (elemIndices (guess !! i) answer)) > 0 in
  if elem i greens
    then "Green"
  else if isYellow
    then "Yellow"
  else "Black"


printLetter :: String -> String -> Int -> IO ()
printLetter answer guess i = do
  let greens = getGreenLetters answer guess 0
  let newLetter = " " ++ [toUpper (guess !! i)]++ " "
  let letterColor = getLetterColor answer guess i greens
  if letterColor == "Green"
    then do {setSGR [SetColor Background Dull Green]; printPrompt newLetter}
  else if letterColor == "Black"
    then do {setSGR [SetRGBColor Background $ sRGB24 117 117 117]; printPrompt newLetter}
  else do {setSGR [SetColor Background Dull Yellow]; printPrompt newLetter}
  setSGR [Reset]
  return ()

printEndgameMessage :: [String] -> IO ()
printEndgameMessage board = do
  let len = length board
  if len == 1
    then printPrompt "Genius!\n"
  else if len == 2
    then printPrompt "Magnificent!\n"
  else if len == 3
    then printPrompt "Impressive!\n"
  else if len == 4
    then printPrompt "Splendid!\n"
  else if len == 5
    then printPrompt "Great!\n"
  else if len == 6
    then printPrompt "Phew!\n"
  else return ()

mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f as = do
  case as of
    (c:cs) -> do {f c; mapIO_ f cs}
    [] -> return ()
