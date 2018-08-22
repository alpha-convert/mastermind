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

data Color = Red
           | Green
           | Blue
           | Purple
           | Orange
           | Brown
           | Pink
           | Yellow
           deriving (Eq,Show)
-- Corr: Right color, right space
-- RCWS: Right color, wrong space
-- WC: Wrong color
data Diff = Corr | RCWS | WC deriving (Eq,Show)
data Row a = Row a a a a deriving (Eq,Show)


instance Read Color where
        readsPrec _ "Red" = [(Red,"")]
        readsPrec _ "Green" = [(Green,"")]
        readsPrec _ "Blue" = [(Blue,"")]
        readsPrec _ "Purple" = [(Purple,"")]
        readsPrec _ "Orange" = [(Orange,"")]
        readsPrec _ "Brown" = [(Brown,"")]
        readsPrec _ "Pink" = [(Pink,"")]
        readsPrec _ "Yellow" = [(Yellow,"")]
        readsPrec _ _ = []

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

type GameIOState a = StateT GameStateRecord IO a


setCode :: Row Color -> GameIOState ()
setCode c = do
        put (GS c [])
        return ()

isValidCode :: Row Color -> Bool
isValidCode code = (length clist) == (length $ nub clist)
        where
                clist = foldr (:) [] code

confirmValidCode :: GameIOState Bool
confirmValidCode = do
        (GS c _) <- get
        return $ isValidCode c

makeGuess :: Row Color -> GameIOState ()
makeGuess guess = do
        GS code gs <- get
        let diff = getDiff code guess
        put $ GS code $ (rowZip guess diff):gs
        return ()

checkLatestGuess :: GameIOState Bool
checkLatestGuess = do
        GS _ gs <- get
        case gs of
             [] -> return False
             g:_ -> return $ isCorrect (fmap snd g)

numGuesses :: GameIOState Int
numGuesses = do
        GS _ gs <- get
        return (length gs)


getStr :: StateT GameStateRecord IO String
getStr = liftIO getLine

printStr :: String -> StateT GameStateRecord IO ()
printStr = liftIO . putStrLn

playStep :: GameIOState Bool
playStep = do
        printStr "Enter your guess:"
        rawGuess <- getStr
        makeGuess (Row Red Blue Green Brown)
        return (rawGuess == "asdf")

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
           else do
                   printDiff
                   doGuessing

playRound :: GameIOState ()
playRound = do
        setCode (Row Red Blue Green Yellow)
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
