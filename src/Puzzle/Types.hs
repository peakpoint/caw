{-# LANGUAGE Strict #-}

module Puzzle.Types
    ( Style (..)
    , SpecialStyle (..)
    , Dir (..), dirChar
    , Square (..)
    , GridIndex
    , ClueIndex
    , Clue (..)
    , ClueMap
    , Clues (..)
    , Metadata (..)
    , Grid
    , Puzzle (..)
    ) where

import Data.Array
import Data.Text
import Data.IntMap.Strict (IntMap)

data Style = Special | Normal deriving (Show, Eq)
data SpecialStyle = Shaded | Circle deriving (Show, Eq)

data Dir = Across | Down deriving (Show, Eq, Ord)

dirChar :: Dir -> Char
dirChar Across = 'A'
dirChar Down = 'D'

data Square =
      Rebus Text Style
    | Letter Char Style
    | Block
    deriving (Show)

type GridIndex = (Int, Int)

type ClueIndex = (Dir, Int)

data Clue = Clue
    { cDir :: Dir
    , cNum :: Int
    , cClue :: Text
    , cAnswer :: Text
    , cRefs :: [(Dir, Int)]
    } deriving (Show)

type ClueMap = IntMap Clue

data Clues = Clues
    { acrossClues :: ClueMap
    , downClues :: ClueMap
    } deriving (Show)

data Metadata = Metadata
    { title :: Maybe Text
    , author :: Maybe Text
    , editor :: Maybe Text
    , date :: Maybe Text
    , copyright :: Maybe Text
    } deriving (Show)

type Grid = Array GridIndex Square

data Puzzle = Puzzle
    { width :: Int
    , height :: Int
    , grid :: Grid
    , special :: Maybe SpecialStyle
    , clues :: Clues
    , metadata :: Metadata
    } deriving (Show)
