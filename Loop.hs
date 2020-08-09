module Loop where

import Data.Array ((!),(//))
import Data.Char(digitToInt, isDigit)
import Game
-- import Prelude (Bool, IO, Int, Maybe (Just, Nothing), String, getLine, length, maybe, otherwise, putStr, putStrLn, return, show, ($), (&&), (+), (++), (.), (<), (==), (>=), (>>), (>>=))
import Prelude

data Query = Quit | NewGame | Help | Play Position

play :: IO ()
play = greetings >> setup >>= gameLoop

greetings :: IO ()
greetings = putStrLn "Hi, Player!!!" >> showHelp

setup :: IO Game
setup = putStrLn "Let's start" >> startGame

showHelp :: IO ()
showHelp =
  putStrLn "How to use menu: "
    >> putStrLn "'Quit' - quit the game'"
    >> putStrLn "'New Game - start new game'"
    >> putStrLn "'0 1' - put your mark on the board, where 0 a is number of line from the up, 1 is number of column from the left, counting is from 0"
    >> putStrLn "How to play - read in the Internet)))"
    >> putStrLn "Good luck)"

gameLoop :: Game -> IO ()
gameLoop game = let (Game _ num _) = game in case isGameOver game of
  Just res -> showResult game >> showWinner res >> setup >>= gameLoop
  _ -> showGame game >> askForTurn game >>= reactOnTurn game


showWinner :: Player -> IO ()
showWinner res = case res of
  GameOver -> putStrLn "Draw!"
  player -> putStrLn $ "Winner is " ++ getPlayerChar player:"!\n"

showResult :: Game -> IO ()
showResult game = showGame game >> putStrLn "Game over"

showGame :: Game -> IO ()
showGame = putStrLn . show

askForTurn :: Game -> IO Query
askForTurn game = showAsk player >> getLine >>= maybe askAgain return . parseQuery game
  where
    askAgain = wrongPosition >> askForTurn game
    (Game player _ _) = game

parseQuery :: Game -> String -> Maybe Query
parseQuery _  "Quit" = Just Quit
parseQuery _  "New Game" = Just NewGame
parseQuery _  "Help" = Just Help
parseQuery game arr = case length arr of
    3 -> case b of
      ' ' ->
        if isDigit a && isDigit c && p a && p c && isFree game pos
          then Just $ Play pos
          else Nothing
      _ -> Nothing
      where
        a : b : c : [] = arr
        p x = digitToInt x >= 0 && digitToInt x < 3
        pos = (digitToInt a, digitToInt c)
    _ -> Nothing

showAsk :: Player -> IO ()
showAsk player = putStrLn ("Now is " ++ getPlayerChar player:"" ++ "'s turn") >> putStrLn "Your turn: "

wrongPosition :: IO ()
wrongPosition = putStrLn "Wrong input, try again" >> putStrLn "Use 'Help' to get instructions\n"

isFree :: Game -> Position -> Bool
isFree (Game _ _ board) pos = board ! pos == '_'

reactOnTurn :: Game -> Query -> IO ()
reactOnTurn game query = case query of
  Quit -> quit
  Help -> showHelp >> gameLoop game
  NewGame -> startGame >>= gameLoop
  Play pos -> gameLoop $ put pos game



put :: Position -> Game -> Game
put pos (Game player num board) = Game (changePlayer player) (num + 1) $ board // [(pos, getPlayerChar player)]

quit :: IO ()
quit = putStrLn "Bye" >> return ()