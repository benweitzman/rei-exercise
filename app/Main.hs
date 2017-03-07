module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Robot

-- | The main event loop. Grab input from the user, parse it to an action
-- run the action, validate the result and repeat.
loop :: RobotState -> IO ()
loop initialState = do
  cmd <- T.stripEnd <$> T.getLine
  case parseCommand cmd of
    Nothing -> do
      putStrLn "Invalid command"
      loop initialState
    Just action -> case runRobot action initialState of
                     Left error -> putStrLn error >> loop initialState
                     Right newState -> printBins (currentBinState newState) >> loop newState


main :: IO ()
main = loop emptyState
