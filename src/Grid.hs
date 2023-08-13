{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grid where

import Data.Array (array, inRange, (!))
import qualified Data.Array as A
import Types
import qualified Puzzle.Types as P
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Movement

mkGridData :: P.Grid -> Bounds -> GridData
mkGridData g b@(x, y) = array bs $
        [let ind = (i, j) in
            (ind, SquareData
                (gridNums ! ind)
                (gridAcross ! ind)
                (gridDown ! ind)
                (case g ! ind of
                    P.Letter _ s -> Just s
                    P.Rebus _ s -> Just s
                    _ -> Nothing))
            | i <- [1..x], j <- [1..y]]
    where
        bs = ((1, 1), b)

        isEmpty i = not (inRange bs i) ||
            case g ! i of
                P.Block -> True
                _ -> False

        folder ::
            (Int, [(GridIndex, Maybe Int)]) ->
            GridIndex ->
            (Int, [(GridIndex, Maybe Int)])
        folder (count, l) i@(ix, iy) =
            case g ! i of
                P.Block -> (count, (i, Nothing) : l)
                _ ->
                    if (isEmpty (ix, iy - 1) && not (isEmpty (ix, iy + 1))) || isEmpty (ix - 1, iy)
                        then (count + 1, (i, Just count) : l)
                        else (count, (i, Nothing) : l)

        (_, gn) = foldl folder (1, [])
            [(i, j) | i <- [1..x], j <- [1..y]]

        gridNums = array bs gn

        folderNums ::
            (Maybe Int, [(GridIndex, Maybe Int)]) ->
            GridIndex ->
            (Maybe Int, [(GridIndex, Maybe Int)])
        folderNums (count, l) ind =
            if not $ inRange bs ind then (Nothing, l) else
            case g ! ind of
                P.Block -> (Nothing, (ind, Nothing) : l)
                _ ->
                    case count <|> gridNums ! ind of
                        Just c -> (Just c, (ind, Just c) : l)
                        Nothing -> (count, (ind, count) : l)

        (_, ga) = foldl folderNums (Nothing, [])
            [(i, j) | i <- [1..x], j <- [1..y+1]]

        (_, gd) = foldl folderNums (Nothing, [])
            [(i, j) | j <- [1..y], i <- [1..x+1]]

        gridAcross = array bs ga
        gridDown = array bs gd

clearGrid :: Grid -> Grid
clearGrid = fmap $ \case
    Block -> Block
    _ -> Letter ' '

toSquare :: P.Square -> Square
toSquare (P.Letter c _) = Letter c
toSquare (P.Rebus t _) = Rebus t
toSquare P.Block = Block

defaultSettings :: Settings
defaultSettings = Settings
    { _arrow = PauseFirst
    , _tab = TabNextIncompleteWord
    , _space = ClearSquareAndMove
    , _typing = SkipOverFilledSquares
    , _endOfWord = EndOfWord JumpToFirstBlank RemainOnCurrentWord
    , _startOfWord = RemainAtStart
    }

mkAppState :: Puzzle -> AppState
mkAppState p = AppState
    { _field = Field
        { _playerGrid = clearGrid g'
        , _selected = ind
        , _selectedDir = tryDir Across sd }
    , _history = []
    , _gridData = gd
    , _bounds = b
    , _puzzle = p
    , _settings = defaultSettings }
    where
        b = snd $ A.bounds $ grid p
        g = grid p
        g' = toSquare <$> g
        gd = mkGridData g b
        ind = fst $ fromJust $ find (not . isBlock . snd) $ A.assocs g'
        sd = gd ! ind
