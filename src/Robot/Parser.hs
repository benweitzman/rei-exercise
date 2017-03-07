{-# LANGUAGE OverloadedStrings #-}
{-|

This module defines a parser for robot commands.

-}
module Robot.Parser where

import qualified Data.Attoparsec.Text as P

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Robot.Actions
import Robot.Types

parseCommand :: Text -> Maybe (RobotM ())
parseCommand cmd = case P.parseOnly parser cmd of
                     Left _ -> Nothing
                     Right x -> Just x
    where parser = P.choice $ P.try <$> [parseSize
                                        ,parseAdd
                                        ,parseMove
                                        ,parseRemove
                                        ,parseUndo
                                        ,parseReplay
                                        ]

          parseSize = do
            P.string "size "
            n <- P.decimal
            P.endOfInput
            return $ resize n

          parseAdd = do
            P.string "add "
            n <- P.decimal
            P.endOfInput
            return $ add (n - 1)

          parseMove = do
            P.string "mv "
            from <- P.decimal
            P.string " "
            to <- P.decimal
            P.endOfInput
            return $ mv (from - 1) (to - 1)

          parseRemove = do
            P.string "rm "
            n <- P.decimal
            P.endOfInput
            return $ remove (n - 1)

          parseUndo = do
            P.string "undo "
            n <- P.decimal
            P.endOfInput
            return $ undo n

          parseReplay = do
            P.string "replay "
            n <- P.decimal
            P.endOfInput
            return $ redo n
