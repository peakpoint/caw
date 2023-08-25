{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RenderClue
    (drawClues) where

import Brick hiding (Down)
import Types
import Brick.Widgets.Table
import qualified Data.IntMap.Strict as IM
import Lens.Micro
import Lens.Micro.TH
import Movement
import Data.Text (Text)
import qualified Data.Text as T

makeLenses ''SquareData
makeLenses ''Field
makeLenses ''AppState

toText :: Square -> Text
toText Block = ""
toText (Letter c) = T.singleton $
    if c == ' ' then '.' else c
toText (Rebus t) = t

drawDirClues :: Dir -> Maybe Int -> ClueMap -> AppState -> Widget Name
drawDirClues d vis m st =
    hLimit 55 $
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
            let t = hLimit 50 $
                    txtWrap $ T.concat
                    [ cClue c
                    , " ["
                    , T.concat $ toText <$> st ^. clueSquaresL (d, i)
                    , "]"
                    ]
                in
            if vis == Just i
                then visible $ withAttr (attrName "clueHL") t
                else t ]
        | (i, c) <- IM.toAscList m ]

drawClues :: AppState -> Widget Name
drawClues st =
    padTop (Pad 1) (drawDirClues Across (sd ^. sqAcross) ac st) <=>
    padTop (Pad 2) (drawDirClues Down (sd ^. sqDown) dc st)
    where
        Clues ac dc = st ^. puzzle . to clues
        sd = st ^. gridData ^?! ix (st ^. selL)
