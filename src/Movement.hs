{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Movement
    (Move (..)
    , isBlock
    , getHist, prevField, nextField
    , tryDir, dirL'
    , oppositeDir
    , oppositeMove
    , setSquare
    , clueL, selL, dirL, clueMapL, moveL
    , clueHead
    , clueAssocs
    , clueSquaresL
    , prevClue, nextClue
    , prevUnfilledClue , nextUnfilledClue
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
import qualified Data.Text as T
import qualified Data.Map.Strict as M

makeLenses ''SquareData
makeLenses ''Hist
makeLenses ''Field
makeLenses ''AppState

isBlock :: Square -> Bool
isBlock Block = True
isBlock _ = False

getHist :: Field -> Hist
getHist f = Hist
    { _hSel = s
    , _hDir = f ^. selectedDir
    , _hSquare = (f ^. playerGrid) ! s
    }
    where
        s = f ^. selected

-- go backwards in history
prevField :: Field -> Field
prevField f = case f ^. prev of
    [] -> f
    (Hist s d sq : hs) -> f
        & prev .~ hs
        & next %~ (getHist f :)
        & selected .~ s
        & selectedDir .~ d
        & playerGrid . ix s .~ sq

-- go forwards in history
nextField :: Field -> Field
nextField f = case f ^. next of
    [] -> f
    (Hist s d sq : hs) -> f
        & next .~ hs
        & prev %~ (getHist f :)
        & selected .~ s
        & selectedDir .~ d
        & playerGrid . ix s .~ sq

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

setSquare :: Square -> Field -> Field
setSquare s f =
    f & playerGrid . ix (f ^. selected) .~ s

clueMapL :: Dir -> Lens' Clues ClueMap
clueMapL Across = lens acrossClues (\cs m -> cs { acrossClues = m })
clueMapL Down = lens downClues (\cs m -> cs { downClues = m })

squareData :: GridIndex -> Traversal' AppState SquareData
squareData i = gridData . ix i

square :: GridIndex -> SimpleGetter AppState Square
square i = to $ \st -> (st ^. field . playerGrid) ! i

clueL :: Lens' AppState ClueID
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

clueHead :: ClueID -> SimpleGetter AppState GridIndex
clueHead (_, n) = to $
    \st -> head
        [ i | (i, sd) <- A.assocs $ st ^. gridData
            , sd ^. sqNum == Just n ]

clueAssocs :: ClueID -> SimpleGetter AppState [GridIndex]
clueAssocs (d, n) = to $ \st ->
    [ i | (i, sd) <- A.assocs $ st ^. gridData
        , sd ^. dirL' d == Just n ]

clueSquaresL :: ClueID -> Lens' AppState [Square]
clueSquaresL c = lens
        (\st ->
            [st ^. square i | i <- st ^. clueAssocs c])
        (\st sqs -> st &
            field . playerGrid %~
                (A.// zip (st ^. clueAssocs c) sqs))

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

prevClue, nextClue :: ClueIDMap -> ClueID -> ClueID

prevClue cs cID =
    case M.lookupLT cID cs of
        Just (c, _) -> c
        Nothing -> fst $ M.findMax cs

nextClue cs cID =
    case M.lookupGT cID cs of
        Just (c, _) -> c
        Nothing -> fst $ M.findMin cs

isFilled :: Square -> Bool
isFilled (Letter c) = c /= ' '
isFilled (Rebus t) = not $ T.null t
isFilled Block = True

isClueFilled :: AppState -> ClueID -> Bool
isClueFilled st c = all isFilled $ st ^. clueSquaresL c

isGridFilled :: Grid -> Bool
isGridFilled = all isFilled . A.elems

stepUnfilledClue :: (ClueID -> ClueID) -> AppState -> ClueID -> ClueID
stepUnfilledClue f st c =
    if isGridFilled $ st ^. field . playerGrid
        then c'
        else fromMaybe c' $
            find (not . isClueFilled st) $
            take (M.size cs) $
            iterate f c'
    where
        cs = st ^. clueIDs
        c' = f c

prevUnfilledClue, nextUnfilledClue :: AppState -> ClueID -> ClueID
prevUnfilledClue st = stepUnfilledClue (prevClue $ st ^. clueIDs) st
nextUnfilledClue st = stepUnfilledClue (nextClue $ st ^. clueIDs) st
