module Game where

import Data.Array(Array, Ix, listArray, (!), (//), indices)
import Prelude

type Position = (Int, Int)
type Label = Char
type Board = Array Position Label

data Player = X | O | GameOver

getPlayerChar :: Player -> Char
getPlayerChar X = 'X'
getPlayerChar O = 'O'

getPlayerFromChar :: Char -> Player
getPlayerFromChar 'X' = X
getPlayerFromChar 'O' = O

changePlayer :: Player -> Player
changePlayer X = O
changePlayer O = X

makeList :: Char -> Int -> [Char]
makeList char 0 = [char]
makeList char num = char:makeList char (num-1)

startGame :: IO Game
startGame = return $ Game X 0 $ listArray ((0,0),(2,2)) $ makeList '_' 9

temp :: Ix i => (i,i) -> [b] -> Array i b
temp = listArray

data Game = Game {
    currentTurn :: Player,
    numberOfTurn :: Int,
    gameBoard :: Board
}

isGameOver :: Game -> Maybe Player
-- isGameOver _ = Nothing
isGameOver game =
  let (Game _ num board) = game
   in case hasLine game 0 of
        Just num -> Just $ getPlayerFromChar $ board ! (num, 0)
        Nothing -> case hasColumn game 0 of
          Just num' -> Just $ getPlayerFromChar $ board ! (0, num')
          Nothing -> if hasDiagonal game then Just $ getPlayerFromChar $ board ! (1, 1) else
              if num >= 9 then Just GameOver else Nothing

hasLine :: Game -> Int -> Maybe Int
hasLine _ 3 = Nothing
hasLine game num = let (Game _ _ board) = game in 
    if not (board ! (num,0) == '_') && board ! (num, 0) == board ! (num, 1) && board ! (num, 2) == board ! (num, 1)
        then Just num 
        else hasLine game (num + 1)

hasColumn :: Game -> Int -> Maybe Int
hasColumn _ 3 = Nothing
hasColumn game num = let (Game _ _ board) = game in 
    if not (board ! (0,num) == '_') && board ! (0, num) == board ! (1, num) && board ! (2, num) == board ! (1, num)
        then Just num 
        else hasColumn game (num + 1)


hasDiagonal :: Game -> Bool
hasDiagonal (Game _ _ board) =
  not (board ! (1, 1) == '_')
    && ((board ! (0, 0) == board ! (1, 1) && board ! (0, 0) == board ! (2, 2)) || (board ! (0, 2) == board ! (1, 1) && board ! (1, 1) == board ! (2, 0)))

getMatrixFromArray :: Board -> String
getMatrixFromArray arr = foldl (\a b -> a ++ " " ++ b) "" $ map (\a -> (arr ! a):"" ++ (if snd a==2 then "\n" else "")) $ indices arr



  



instance Show Game where
  show (Game _ num board) = getMatrixFromArray board
    