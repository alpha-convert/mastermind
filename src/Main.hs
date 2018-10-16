module Main where
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Lens
import Data.List
import Control.Monad.ST.Lazy
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.IO.Class
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Token
import Text.Parsec.String

data Color = Red
           | Green
           | Blue
           | Purple
           | Orange
           | Brown
           | Pink
           | Yellow
           deriving (Eq,Show)

allColors = [Red,Green,Blue,Purple,Orange,Brown,Pink,Yellow]

strToColor :: String -> Color
strToColor "Red" = Red
strToColor "Green" = Green
strToColor "Blue" = Blue
strToColor "Purple" = Purple
strToColor "Orange" = Orange
strToColor "Brown" = Brown
strToColor "Pink" = Pink
strToColor "Yellow" = Yellow

parseColString :: Parser String
parseColString = do
        cname <- many letter
        guard $ cname `elem` (show <$> allColors)
        return cname

parseColor :: Parser Color
parseColor = do
        cname <- parseColString
        return $ strToColor cname

parseColorCSV :: Parser [Color]
parseColorCSV = do 
        first <- parseColor
        rest <- (char ',' >> parseColorCSV) <|> (return [])
        return (first : rest)

parseGuess :: Parser (Row Color)
parseGuess = do
        colors <- parseColorCSV
        case (fromList colors) of
             (Just cs) -> return cs
             Nothing ->  fail "Guess must be 4 colors"


-- Corr: Right color, right space
-- RCWS: Right color, wrong space
-- WC: Wrong color
data Diff = Corr | RCWS | WC deriving (Eq,Show)
data Row a = Row a a a a deriving (Eq,Show)

fromList :: [a] -> Maybe (Row a)
fromList [a,b,c,d] = Just $ Row a b c d
fromList _ = Nothing

instance Functor Row where
        fmap f (Row a b c d) = Row (f a) (f b) (f c) (f d)

instance Foldable Row where
        foldr f z (Row a b c d) = f a (f b (f c (f d z)))

rowZip :: Row a -> Row b -> Row (a,b)
rowZip (Row x y z w) (Row a b c d) = Row (x,a) (y,b) (z,c) (w,d)


colorsInRow :: Row Color -> [Color]
colorsInRow (Row a b c d) = nub [a,b,c,d]

individualDiff :: [Color] -> (Color, Color) -> Diff
individualDiff colors (ccorr,cguess)
        | ccorr == cguess = Corr
        | inColors cguess = RCWS
        | otherwise = WC
        where
                inColors = flip elem colors

getDiff :: Row Color -> Row Color -> Row Diff
getDiff code guess = fmap (individualDiff colors) (rowZip code guess)
        where colors = colorsInRow code

isCorrect :: Row Diff -> Bool
isCorrect = foldr isCorr True
        where isCorr d b = (d == Corr) && b

data GameStateRecord = GS { code :: Row Color, priorGuesses :: [Row (Color, Diff)] } deriving (Show, Eq)

{-
A GameIOState is a monad in which I can perform IO and also manipulate GameStateRecords
-}
type GameIOState a = StateT GameStateRecord IO a


setCode :: Row Color -> GameIOState ()
setCode c = do
        put (GS c [])
        return ()

-- A code is valid if it has all unique entries.
isValidCode :: Row Color -> Bool
isValidCode code = (length clist) == (length $ nub clist)
        where
                clist = foldr (:) [] code

-- GameIOState wrapper for isValidCode
confirmValidCode :: GameIOState Bool
confirmValidCode = do
        (GS c _) <- get
        return $ isValidCode c

checkLatestGuess :: GameIOState Bool
checkLatestGuess = do
        GS _ gs <- get
        case gs of
             [] -> return False
             g:_ -> return $ isCorrect (fmap snd g)

-- Compute the difference between the right code and the guess,
-- then add this (guess,diff) to the list.
-- return true if the guess was correct.
makeGuess :: Row Color -> GameIOState Bool
makeGuess guess = do
        GS code gs <- get
        let diff = getDiff code guess
        put $ GS code $ (rowZip guess diff):gs
        checkLatestGuess

numGuesses :: GameIOState Int
numGuesses = do
        GS _ gs <- get
        return (length gs)


getStr :: GameIOState String
getStr = liftIO getLine

printStr :: String -> GameIOState ()
printStr = liftIO . putStrLn

-- Play a single step of the game:
-- Loop until the user inputs a valid guess string
-- update the state with the guess, returns true
-- if the guess was correct.
playStep :: GameIOState Bool
playStep = do
        printStr "Enter your guess:"
        rawGuess <- getStr
        case (runParser parseGuess () "" rawGuess) of
             Left err -> do
                     printStr "Not a valid guess. Try again."
                     playStep
             Right guess -> makeGuess guess

-- Prints the last diff
printDiff :: GameIOState ()
printDiff = do
        (GS _ gs) <- get
        let diffs = (fmap . fmap) snd gs
        case diffs of
             [] -> return ()
             (diff:_) -> printStr $ "Diff: " ++ (show diff)

doGuessing :: GameIOState ()
doGuessing = do
        won <- playStep
        if won
           then return ()
           else printDiff >> doGuessing

playRound :: GameIOState ()
playRound = do
        valid <- confirmValidCode
        guard valid
        doGuessing
        guesses <- show <$> numGuesses
        printStr $ "You won in " ++ guesses ++ " guesses!"


main :: IO ()
main = do
        let init = GS {code = (Row Red Blue Green Yellow), priorGuesses = []}
        _ <- runStateT playRound init
        return ()
