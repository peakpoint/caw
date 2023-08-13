{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Movement
    (Move (..)
    , isBlock
    , tryDir, dirL'
    , oppositeDir
    , oppositeMove
    , setSquare
    , clueL, selL, dirL, clueMapL, moveL
    ) where

import Types
import Lens.Micro.TH
import Data.Maybe
import Control.Applicative
import Data.Array ((!), inRange)
import qualified Data.Array as A
import Data.Foldable
import Lens.Micro.GHC
import Data.Char

makeLenses ''SquareData
makeLenses ''Field
makeLenses ''AppState

isBlock :: Square -> Bool
isBlock Block = True
isBlock _ = False

data Move = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Show, Eq)

toOffset :: Move -> (Int, Int) -> (Int, Int)
toOffset MoveUp = _1 %~ subtract 1
toOffset MoveDown = _1 %~ (+ 1)
toOffset MoveLeft = _2 %~ subtract 1
toOffset MoveRight = _2 %~ (+ 1)

toDir :: Move -> Dir
toDir MoveUp = Down
toDir MoveDown = Down
toDir _ = Across

toMove :: Dir -> Move
toMove Across = MoveRight
toMove Down = MoveDown

oppositeDir :: Dir -> Dir
oppositeDir Across = Down
oppositeDir Down = Across

oppositeMove :: Move -> Move
oppositeMove MoveUp = MoveDown
oppositeMove MoveDown = MoveUp
oppositeMove MoveLeft = MoveRight
oppositeMove MoveRight = MoveLeft

dirL' :: Dir -> Lens' SquareData (Maybe Int)
dirL' d = case d of
    Across -> sqAcross
    Down -> sqDown

tryDir :: Dir -> SquareData -> Dir
tryDir d sd = fromMaybe Across $
    d <$ sd ^. dirL' d <|> d' <$ sd ^. dirL' d'
    where
        d' = oppositeDir d

move :: GridData -> Move -> Field -> Field
move gd m f = f
    & selected .~ sel'
    & selectedDir .~ d
    where
        g = f ^. playerGrid
        b = A.bounds g
        dxy = toOffset m

        dir' = toDir m -- new direction

        sel = f ^. selected
        sel' = if f ^. selectedDir /= dir' then sel else
            fromMaybe sel $
                find (not . isBlock . (g !)) $
                takeWhile (inRange b) $
                tail $
                iterate dxy sel

        d = tryDir dir' $ gd ! sel'

moveL :: Lens' AppState Move
moveL = lens
    (\st -> toMove $ st ^. field . selectedDir)
    (\st m ->
        st & field %~ move (st ^. gridData) m)

setSquare :: Char -> Field -> Field
setSquare c f =
    f & playerGrid . ix (f ^. selected) %~ \case
        Letter _ -> Letter (toUpper c)
        s -> s

clueMapL :: Dir -> Lens' Clues ClueMap
clueMapL Across = lens acrossClues (\cs m -> cs { acrossClues = m })
clueMapL Down = lens downClues (\cs m -> cs { downClues = m })

squareData :: GridIndex -> Traversal' AppState SquareData
squareData i = gridData . ix i

square :: GridIndex -> SimpleGetter AppState Square
square i = to $ \st -> (st ^. field . playerGrid) ! i

clueL :: Lens' AppState (Dir, Int)
clueL = lens
    (\st ->
        let d = st ^. field . selectedDir in
        (d, fromJust $ st ^?! squareData (st ^. field . selected) . dirL' d))
    (\st (d, i) ->
        case find (\(_, sd) -> sd ^. sqNum == Just i) $ A.assocs $ st ^. gridData of
            Just (ind, _) -> st
                & field . selected .~ ind
                & field . selectedDir .~ d
            Nothing -> st
        )

dirL :: Lens' AppState Dir
dirL = lens
    (^. field . selectedDir)
    (\st d -> st & field . selectedDir .~
        tryDir d (st ^?! squareData (st ^. field . selected)))

selL :: Lens' AppState GridIndex
selL = lens (^. field . selected)
    (\st i -> if
        | isBlock (st ^. square i) -> st
        | i == st ^. field . selected ->
            st & dirL %~ oppositeDir
        | otherwise -> st
            & field . selected .~ i
            & dirL %~ id)
