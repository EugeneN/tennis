{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Args(..)
    , parseArgs
    , readInput
    , processInput
    , writeOutput
    , formatOutput
    ) where

import           System.Environment    (getArgs)    
import           System.FilePath.Posix (isValid, equalFilePath)    
import           Data.List             (foldl')
import qualified Data.Text.IO          as TIO
import qualified Data.Text             as T
import           Data.Text             (Text(..))


-- Business logic --------------------------------------------------------------

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
  { aWins :: Int -- number of games in a set won by A
  , bWins :: Int -- number of games in a set won by B
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
processMatch pointsText = foldl' scoreMatch initialScore points
  where
    points = T.unpack pointsText

    scoreMatch :: Score -> Char -> Score
    scoreMatch (Score sets curSet g) point =
      -- game scoring logic, tennis rules distilled into a declarative form
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
        continueCurGame curGame = Score sets curSet curGame

        aWinsGame = gameWon incSetsScoreWonByA isSetWonByA
        bWinsGame = gameWon incSetsScoreWonByB isSetWonByB
            
        gameWon incSetScore isSetWon server = 
          let curSet'  = incSetScore curSet 
              newGame' = reverseGameServer newGame server     -- keeping track of game servers

          in if isSetWon curSet' 
              then Score (sets <> [curSet']) newSet  newGame' -- the current set is won
              else Score sets                curSet' newGame' -- the current set continues

        incSetsScoreWonByA (SetScore a b) = SetScore (a + 1) b
        incSetsScoreWonByB (SetScore a b) = SetScore a (b + 1)

        isSetWonByA (SetScore a b) = a >= 6 && a - b >= 2
        isSetWonByB (SetScore a b) = b >= 6 && b - a >= 2

        reverseGameServer (Game score _) server = case server of
          ServerA -> Game score ServerB
          ServerB -> Game score ServerA


-- Utilities -------------------------------------------------------------------

showScore :: Score -> Text
showScore (Score ss cs cg@(Game _ server)) = case ss of
  [] -> T.intercalate " " [showSet server cs, showGame cg]
  xs -> T.intercalate " " [showSets server xs, showSet server cs, showGame cg]

showSets :: Server -> [SetScore] -> Text
showSets server xs = T.intercalate " " (fmap (showSet server) xs)

showSet :: Server -> SetScore -> Text
showSet server (SetScore x y) = case server of
  ServerA -> T.pack (show x) <> "-" <> T.pack (show y)
  ServerB -> T.pack (show y) <> "-" <> T.pack (show x)

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

formatOutput :: [Score] -> [Text]
formatOutput xs = fmap showScore xs

writeOutput :: FilePath -> [Score] -> IO ()
writeOutput fn xs = case fn of
  "-" -> putStrLn . T.unpack $ data_
  _   -> TIO.writeFile fn data_
  where
    data_ = T.unlines $ formatOutput xs

