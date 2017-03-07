module Robot.Types where

import Control.Monad.State

-- | The robot is simulated using the `StateT` monad transformer, which will
-- let us thread the state of the machine through our program while being able
-- to compose actions easily, as well as giving us an "escape hatch" via the
-- Either inside the transformer to let us catch any errors that may arise.
type RobotM a = StateT RobotState (Either String) a


 -- | The state of the robot has three parts:
-- 1: The current state of all the bins, represented as a list of ints,
--    where the nth in the list corresponds to the number of boxes in the nth bin
-- 2: The history, represented by a list of pairs of past states and actions run
--    from those states. This allows us to go back in time to previous states while
--    keeping track of the actions that may need to be replayed
-- 3: The future actions, that can possible be replayed if we have already undone
--    some actions.
data RobotState = RobotState
    { oldStatesAndActions :: [([Int], RobotM ())]
    , currentBinState :: [Int]
    , futureActions :: [RobotM ()]
    }


printBins :: [Int] -> IO ()
printBins bins = putStrLn . unlines $ showLine <$> zip [1..] bins
    where showLine (n, count) = show n ++ ": " ++ replicate count 'X'

-- | The initial state, no bins, no past, no future
emptyState :: RobotState
emptyState = RobotState [] [] []
