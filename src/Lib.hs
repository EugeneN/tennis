{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Args(..)
    , parseArgs
    , readInput
    , processInput
    , writeOutput
    ) where

import           System.Environment    (getArgs)    
import           System.FilePath.Posix (isValid, equalFilePath)    
import           Data.List             (foldl')
import qualified Data.Text.IO          as TIO
import qualified Data.Text             as T
import           Data.Text             (Text(..))


-- Business logic --------------------------------------------------------------

-- NOTE: set score order is ambiguous in the problem description, and is 
-- not defined in the cited Wikipedia article 
-- (https://en.wikipedia.org/wiki/Tennis_scoring_system).
-- 
-- Also, the score order is unclear from the provided output sample. For 
-- example, in line 14 of the input and output respectively, the server is A, and 
-- she wins the first and only game in the current set, but the output score is 
-- shown as if the server was B. Similar behaviour can be seen in the following
-- lines as well.
--
-- This program outputs set scores with A score first. It still does respect game 
-- server order and outputs game scores accordingly.

-- business data types
data Score = Score
  { sets        :: [SetScore]
  , currentSet  :: SetScore
  , currentGame :: Game
  }

data Game = Game GameScore Server
  deriving (Show)

data GameScore = Points Int Int | Deuce | AdvantageA | AdvantageB 
  deriving (Show)

data Server = ServerA | ServerB
  deriving (Show)

data SetScore = SetScore 
  { aWins :: Int -- number of games in set won by A
  , bWins :: Int -- number of games in set won by B
  }

-- initial values
newSet :: SetScore
newSet = SetScore 0 0

newGame :: Game
newGame = Game (Points 0 0) ServerA

initialScore :: Score
initialScore = Score [] newSet newGame


processInput :: Text -> [Score]
processInput xs = fmap (processMatch . T.strip) (T.lines xs)

processMatch :: Text -> Score
processMatch pointsText =
  foldl' go initialScore points

  where
    points = T.unpack pointsText

    go :: Score -> Char -> Score
    go (Score sets curSet g) point =
      -- game scoring logic
      case (g, point) of
        (Game Deuce s, 'A')                           -> continueCurGame (Game AdvantageA s)
        (Game Deuce s, 'B')                           -> continueCurGame (Game AdvantageB s)
        (Game AdvantageA s, 'A')                      -> aWinsGame s
        (Game AdvantageA s, 'B')                      -> continueCurGame (Game Deuce s)
        (Game AdvantageB s, 'B')                      -> bWinsGame s
        (Game AdvantageB s, 'A')                      -> continueCurGame (Game Deuce s)
        (Game (Points a b) s, 'A') | a == 3 && b == 0 -> aWinsGame s
        (Game (Points a b) s, 'B') | b == 3 && a == 0 -> bWinsGame s
        (Game (Points a b) s, 'A') | a == 2 && b == 3 -> continueCurGame (Game Deuce s)
        (Game (Points a b) s, 'B') | a == 3 && b == 2 -> continueCurGame (Game Deuce s)
        (Game (Points a b) s, 'A')                    -> continueCurGame (Game (Points (a+1) b) s)
        (Game (Points a b) s, 'B')                    -> continueCurGame (Game (Points a (b+1)) s)

        _ -> error $ "Unknown game condition: " <> show (g, point)

      where
        aWinsGame = gameWonBy incSetsWonByA (\(SetScore a b) -> a >= 6 && a - b >= 2)
        bWinsGame = gameWonBy incSetsWonByB (\(SetScore a b) -> b >= 6 && b - a >= 2)
            
        gameWonBy inc setWon server = 
          let curSet'  = inc curSet 
              newGame' = reverseGameServer newGame server -- keeping track of game servers
              continueCurSetScore = Score sets curSet' newGame'
              setWonScore = Score (sets <> [curSet']) newSet newGame'

          -- set scoring logic, when a game is won by either player check if
          -- a set is won as well, or if the current set continues
          in if setWon curSet' 
              then setWonScore
              else continueCurSetScore

        continueCurGame curGame = Score sets curSet curGame

        incSetsWonByA (SetScore a b) = SetScore (a + 1) b
        incSetsWonByB (SetScore a b) = SetScore a (b + 1)

        reverseGameServer (Game score _) server = case server of
          ServerA -> Game score ServerB
          ServerB -> Game score ServerA


-- Utilities -------------------------------------------------------------------

showScore :: Score -> Text
showScore (Score ss cs cg) = case ss of
  [] -> T.intercalate " " [showSet cs, showGame cg]
  xs -> T.intercalate " " [showSets xs, showSet cs, showGame cg]

showSets :: [SetScore] -> Text
showSets xs = T.intercalate " " (fmap showSet xs)

showSet :: SetScore -> Text
showSet (SetScore x y) = T.pack (show x) <> "-" <> T.pack (show y)

showGame :: Game -> Text
showGame g = case g of
  Game (Points 0 0) _ -> ""
  Game Deuce _        -> "40-40"
  Game AdvantageA _   -> "A-40"
  Game AdvantageB _   -> "40-A"
  Game (Points x y) s -> case s of
    ServerA -> showPointsScore x <> "-" <> showPointsScore y
    ServerB -> showPointsScore y <> "-" <> showPointsScore x

showPointsScore :: Int -> Text
showPointsScore x = case x of
  0 -> "0" -- should be "love" according to the Wikipedia article
  1 -> "15"
  2 -> "30"
  3 -> "40"
  4 -> "Game"
  _ -> error $ "Wrong score: " <> show x

-- Auxiliary logic -------------------------------------------------------------

data Args = Args 
  { inputFn  :: FilePath
  , outputFn :: FilePath
  }

filenamesAreValid :: FilePath -> FilePath -> Bool
filenamesAreValid x y = 
  isValid x && isValid y && not (equalFilePath x y)

parseArgs :: IO (Maybe Args)
parseArgs = do
  args <- getArgs

  pure $ case args of
    x:y:[] | filenamesAreValid x y -> Just $ Args x y
    _                              -> Nothing

readInput :: FilePath -> IO Text
readInput fn = TIO.readFile fn

writeOutput :: FilePath -> [Score] -> IO ()
writeOutput fn xs = case fn of
  "-" -> putStrLn . T.unpack $ data_
  _   -> TIO.writeFile fn data_
  where
    data_ = T.unlines $ fmap showScore xs

