{-# LANGUAGE TemplateHaskell #-}

module RenderGrid (renderGrid) where

import Lens.Micro.TH
import Types
import Brick hiding (Down)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Table
import Lens.Micro
import Data.Array ((!))
import Movement

makeLenses ''SquareData
makeLenses ''Field
makeLenses ''AppState

rightBlock, leftBlock, fullBlock :: Char
rightBlock = '\x2590'
leftBlock = '\x258C'
fullBlock = '\x2588'

subscript :: Char -> Char
subscript c =
   case c of
      '0' -> '\x2080'
      '1' -> '\x2081'
      '2' -> '\x2082'
      '3' -> '\x2083'
      '4' -> '\x2084'
      '5' -> '\x2085'
      '6' -> '\x2086'
      '7' -> '\x2087'
      '8' -> '\x2088'
      '9' -> '\x2089'
      _ -> c

data CellStyle = Selected | Highlighted deriving (Show, Eq)

applyCellStyle :: Maybe CellStyle -> Widget n -> Widget n
applyCellStyle sty w =
    case sty of
        Nothing -> w
        Just s ->
            case s of
            Highlighted ->
                withAttr (attrName "highlightedBlockSide") (str [rightBlock])
                <+> withAttr (attrName "highlighted") w
                <+> withAttr (attrName "highlightedBlockSide") (str [leftBlock])
            Selected ->
                withAttr (attrName "selectedBlockSide") (str [rightBlock])
                <+> withAttr (attrName "selected") w
                <+> withAttr (attrName "selectedBlockSide") (str [leftBlock])

cellWidget :: Square -> Maybe CellStyle -> Widget Name
cellWidget cell sty = case cell of
    Letter s -> center $
        sty `applyCellStyle` str [s]
    Rebus _ -> emptyWidget
    Block -> dim $ str
        [ rightBlock
        , fullBlock
        , leftBlock
        ]

dim :: Widget n -> Widget n
dim = withDefAttr (attrName "dim")

drawCell :: Bounds -> GridIndex -> Widget Name -> Widget Name -> Widget Name
drawCell (x, y) (i, j) label body =
    vLimit (if isBottom then 3 else 2) $
        hLimit (if isLeft then 5 else 4) $
            leftBorder <+> (topBorder <=> body <=> botBorder) <+> rightBorder
    where
        isBottom = i == x
        isRight = j == y
        isLeft = j == 1

        topBorder = label <+> dim hBorder
        botBorder = dim $ if isBottom then hBorder else emptyWidget

        leftBorder = dim $ if isLeft then vBox
            [ joinableBorder $ Edges False False False True
            , vBorder
            , joinableBorder $ Edges True (not isBottom) False False
            ] else emptyWidget

        rightBorder = dim $ vBox
            [ joinableBorder $ Edges False True True (not isRight)
            , vBorder
            , joinableBorder $ Edges False (not isBottom) False False ]

renderGrid :: AppState -> Widget Name
renderGrid st =
    renderTable $
    surroundingBorder False $
    columnBorders False $
    rowBorders False t
    where
        f = st ^. field
        sel = f ^. selected
        gd = st ^. gridData
        g = f ^. playerGrid

        b@(x, y) = st ^. bounds
        
        clueNumL = dirL' (f ^. selectedDir)
        clueNum = gd ! sel ^. clueNumL

        t = table [[
            let ind = (i, j)
                sty
                  | sel == ind = Just Selected
                  | gd ! ind ^. clueNumL == clueNum = Just Highlighted
                  | otherwise = Nothing in
            drawCell b ind
                (case (gd ! ind) ^. sqNum of
                    Just n ->
                        let num = str (subscript <$> show n) in
                        if Just n == clueNum
                            then withAttr (attrName "boldLabel") num
                            else dim num
                    Nothing -> emptyWidget) $
                clickable (SquareX (i, j)) $
                cellWidget (g ! ind) sty | j <- [1..y]] | i <- [1..x]]
