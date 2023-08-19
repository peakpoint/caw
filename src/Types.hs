{-# LANGUAGE StrictData #-}

module Types
    ( module Puzzle.Types
    , Bounds
    , Square (..)
    , Grid
    , SquareData (..)
    , GridData
    , Hist (..)
    , Field (..)
    , ClueID
    , ClueIDMap
    , AppState (..)
    , Name (..)
    , ArrowSettings (..)
    , TabSettings (..)
    , SpaceSettings (..)
    , TypingSettings (..)
    , NextWordSettings (..)
    , EndOfWordJump (..)
    , EndOfWordSettings (..)
    , StartOfWordSettings (..)
    , Settings (..)
    ) where

import Data.Array
import Puzzle.Types hiding (Square (..), Grid)
import Data.Text
import Data.Map.Strict (Map)

type Bounds = GridIndex

data Square = Letter Char | Rebus Text | Block
    deriving (Show, Eq)

type Grid = Array GridIndex Square

data SquareData = SquareData
    { _sqNum :: Maybe Int
    , _sqAcross :: Maybe Int
    , _sqDown :: Maybe Int
    , _style :: Maybe Style
    } deriving (Show)

type GridData = Array GridIndex SquareData

data ArrowSettings =
    ChangeDirAndMove | KeepDir | PauseFirst
    deriving (Eq, Show)

data TabSettings =
    TabSwitchDir | TabNextWord | TabNextIncompleteWord
    deriving (Eq, Show)

data SpaceSettings =
    ClearSquareAndMove | SpaceSwitchDir
    deriving (Eq, Show)

data TypingSettings =
    SkipOverFilledSquares | TypeOverFilledSquares
    deriving (Eq, Show)

data NextWordSettings =
    NextWord | NextIncompleteWord | RemainOnCurrentWord
    deriving (Eq, Show)

data EndOfWordJump =
    JumpToFirstBlank | DoNotJumpToFirstBlank
    deriving (Eq, Show)

data EndOfWordSettings =
    EndOfWord EndOfWordJump NextWordSettings
    deriving (Eq, Show)

data StartOfWordSettings = JumpToPreviousWord | RemainAtStart
    deriving (Eq, Show)

data Settings = Settings
    { _arrow :: ArrowSettings
    , _tab :: TabSettings
    , _space :: SpaceSettings
    , _typing :: TypingSettings
    , _endOfWord :: EndOfWordSettings
    , _startOfWord :: StartOfWordSettings
    } deriving (Eq, Show)

data Hist = Hist
    { _hSel :: GridIndex
    , _hDir :: Dir
    , _hSquare :: Square
    } deriving (Eq, Show)

data Field = Field
    { _playerGrid :: Grid
    , _selected :: GridIndex
    , _selectedDir :: Dir
    , _prev :: [Hist]
    , _next :: [Hist]
    } deriving (Show)

type ClueID = (Dir, Int)
type ClueIDMap = Map ClueID Clue

data AppState = AppState
    { _field :: Field
    , _gridData :: GridData
    , _bounds :: Bounds
    , _puzzle :: Puzzle
    , _clueIDs :: ClueIDMap
    , _settings :: Settings
    } deriving (Show)

data Name =
    ClueX Dir Int
    | CluesX Dir
    | SquareX GridIndex
    | GridX
    | MetaX
    deriving (Show, Eq, Ord)
