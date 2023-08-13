{-# LANGUAGE TemplateHaskell #-}

module RenderClue
    (drawClues) where

import Brick hiding (Down)
import Types
import Brick.Widgets.Table
import qualified Data.IntMap.Strict as IM
import Lens.Micro
import Lens.Micro.TH
import Movement

makeLenses ''SquareData
makeLenses ''AppState

drawDirClues :: Dir -> Maybe Int -> ClueMap -> Widget Name
drawDirClues d vis m =
    hLimit 40 $
    vLimit 20 $
    withVScrollBars OnRight $
    viewport (CluesX d) Vertical $
    renderTable $
    surroundingBorder False $
    columnBorders False $
    rowBorders False $
    table
    [ clickable (ClueX d i) <$>
        [ str $ dirChar (cDir c) : show (cNum c) ++ ". ",
            let t = hLimit 30 $ txtWrap (cClue c) in
            if vis == Just i
                then visible $ withAttr (attrName "clueHL") t
                else t ]
        | (i, c) <- IM.toAscList m ]

drawClues :: AppState -> Widget Name
drawClues st =
    padTop (Pad 1) (drawDirClues Across (sd ^. sqAcross) ac) <=>
    padTop (Pad 2) (drawDirClues Down (sd ^. sqDown) dc)
    where
        Clues ac dc = st ^. puzzle . to clues
        sd = st ^. gridData ^?! ix (st ^. selL)
