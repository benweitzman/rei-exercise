{-|

This module defines the different high level actions that the robot arm can
perform.

-}
module Robot.Actions
    ( resize
    , add
    , mv
    , remove
    , undo
    , redo
    , runRobot
    ) where

import Control.Monad.State

import Robot.Types

modifyIndex :: Int -> (a -> Either String a) -> [a] -> Either String [a]
modifyIndex _ _ [] = Left "Index out of bounds"
modifyIndex 0 f (x : xs) = do
  x' <- f x
  return $ x' : xs
modifyIndex n f (x : xs) = do
  rest <- modifyIndex (n - 1) f xs
  return (x : rest)

updateBin :: Int -> (Int -> Either String Int) -> RobotM ()
updateBin n f = do
  xs <- gets currentBinState
  case modifyIndex n f xs of
    Right xs' -> modify $ \s -> s{currentBinState = xs'}
    Left err -> lift $ Left err

-- | This function is used to wrap simple actions (i.e. those that don't
-- depend on the undo or replay state. Every simple action saves the
-- current state and the action being performed to the undo history.
-- It also sets the future state to empty, as performing a new action
-- creates a "branch in time"
wrapAction :: RobotM () -> RobotM ()
wrapAction run = do
  RobotState past current future <- get
  put $ RobotState ((current, run) : past) current []
  run


resize :: Int -> RobotM ()
resize n = wrapAction $ do
  current <- gets currentBinState
  let new = take n $ current ++ repeat 0
  modify $ \s -> s{currentBinState = new}


add :: Int -> RobotM ()
add n = wrapAction $ updateBin n (return . (+ 1))

safeRemove :: Int -> Either String Int
safeRemove 0 = Left "Can't remove from an empty bin"
safeRemove n = Right $ n - 1

mv :: Int -> Int -> RobotM ()
mv from to = wrapAction $ do
  updateBin from safeRemove
  updateBin to (return . succ)

remove :: Int -> RobotM ()
remove n = wrapAction $ updateBin n safeRemove

-- | Iteratively removes the most recent past state and sets it
-- as the current state, sending the corresponding action to the
-- future.
undo :: Int -> RobotM ()
undo 0 = return ()
undo n = do
  RobotState past current future <- get
  case past of
    [] -> lift $ Left "Can't undo that far"
    ((oldState, action) : furtherPast) -> do
      put $ RobotState furtherPast oldState (action : future)
      undo (n - 1)

-- | Iteratively takes an action off the future state and
-- runs it.
redo :: Int -> RobotM ()
redo 0 = return ()
redo n = do
  RobotState _ current future <- get
  case future of
    [] -> lift $ Left "Can't replay that far"
    (action : furtherFuture) -> do
      action
      modify $ \s@RobotState{oldStatesAndActions = past} -> s{oldStatesAndActions = (current, action) : past
                                                             ,futureActions=furtherFuture
                                                             }
      redo (n - 1)


runRobot :: RobotM () -> RobotState -> Either String RobotState
runRobot = execStateT
