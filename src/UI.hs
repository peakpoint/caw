{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module UI
    ( app
    , mkAppState
    ) where

import Brick hiding (Down)
import Brick.Widgets.Center

import Control.Monad
import Control.Monad.Trans

import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)

import Graphics.Vty (withStyle)
import qualified Graphics.Vty as V

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Grid
import Movement
import RenderClue
import RenderGrid
import Types

makeLenses ''Settings
makeLenses ''AppState

data Event = Event

selectedCol, highlightedCol :: V.Color
selectedCol = V.brightMagenta
highlightedCol = V.blue

app :: App AppState Event Name
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = do
        vty <- getVtyHandle
        let output = V.outputIface vty
        when (V.supportsMode output V.Mouse) $ do
            liftIO $ V.setMode output V.Mouse True
    , appAttrMap = const $ attrMap V.defAttr
        [ (attrName "selected",
            V.black `on` selectedCol `withStyle` V.bold)
        , (attrName "highlighted",
            V.black `on` highlightedCol `withStyle` V.bold)
        , (attrName "selectedBlockSide", fg selectedCol)
        , (attrName "highlightedBlockSide", fg highlightedCol)
        , (attrName "dim", fg V.brightBlack)
        , (attrName "boldLabel", fg V.brightWhite `withStyle` V.bold)
        , (attrName "clueHL", V.brightWhite `on` V.brightBlack `withStyle` V.bold)
        ]
    }

drawUI :: AppState -> [Widget Name]
drawUI st =
    [ center $
        top <=> (g <+> drawClues st)
    ]
    where
        p = st ^. puzzle
        meta = metadata p
        top =
            clickable MetaX $
            -- cached MetaX $
            padBottom (Pad 1) $
            txt (fromMaybe "Untitled" $ title meta)
            <=> maybe emptyWidget txt (author meta)
            <=> maybe emptyWidget txt (editor meta)
            <=> maybe emptyWidget txt (copyright meta)

        g = cached GridX $
            padRight (Pad 10) $
            renderGrid st

acrossClueList, downClueList :: ViewportScroll Name
acrossClueList = viewportScroll (CluesX Across)
downClueList = viewportScroll (CluesX Down)

handleEvent :: BrickEvent Name Event -> EventM Name AppState ()
handleEvent (MouseDown name V.BLeft _ _) =
    case name of
        SquareX i -> do
            invalidateCacheEntry GridX
            selL .= i
        _ -> return ()
handleEvent (VtyEvent v) =
    case v of
        V.EvKey V.KBS [] -> do
            invalidateCacheEntry GridX
            field %= setSquare (Letter ' ')
            moveL %= oppositeMove
        V.EvKey (V.KChar ' ') [] -> do
            invalidateCacheEntry GridX
            s <- use $ settings . space
            case s of
                ClearSquareAndMove -> do
                    field %= setSquare (Letter ' ')
                    moveL %= id
                SpaceSwitchDir -> do
                    dirL %= oppositeDir
        V.EvKey (V.KChar '\t') [] -> do
            invalidateCacheEntry GridX
            cs <- use clueIDs
            t <- use $ settings . tab
            case t of
                TabSwitchDir ->
                    dirL %= oppositeDir
                TabNextWord ->
                    clueL %= nextClue cs
                TabNextIncompleteWord -> do
                    st <- get
                    clueL %= nextUnfilledClue st
        V.EvKey V.KBackTab _ -> do
            invalidateCacheEntry GridX
            cs <- use clueIDs
            t <- use $ settings . tab
            case t of
                TabSwitchDir ->
                    dirL %= oppositeDir
                TabNextWord ->
                    clueL %= prevClue cs
                TabNextIncompleteWord -> do
                    st <- get
                    clueL %= prevUnfilledClue st
        -- TODO: enter rebus
        V.EvKey V.KEsc _ ->
            halt
        V.EvKey (V.KChar 'q') [V.MCtrl] ->
            halt
        V.EvKey (V.KChar c) [] ->
            when (isAlpha c) $ do
                invalidateCacheEntry GridX
                field %= setSquare (Letter c)
                moveL %= id
        V.EvKey k [] ->
            when (k `elem` [V.KUp, V.KDown, V.KLeft, V.KRight]) $ do
                invalidateCacheEntry GridX
                moveL .= case k of
                    V.KUp -> MoveUp
                    V.KDown -> MoveDown
                    V.KLeft -> MoveLeft
                    _ -> MoveRight
        _ -> return ()
handleEvent (AppEvent _) = return ()
handleEvent _ = return ()
