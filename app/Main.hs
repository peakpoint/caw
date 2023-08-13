module Main (main) where

import UI
import System.Environment
import System.Exit

import qualified Data.Text.IO as T
import Puzzle.Parser (pPuzzleImplicit)
import Text.Megaparsec
import qualified Graphics.Vty as V
import Brick

main :: IO ()
main = do
    args <- getArgs

    print args

    st <- case args of
        [file] -> do
            puzzleText <- T.readFile file
            case parse pPuzzleImplicit file puzzleText of
                Left e -> do
                    putStr $ errorBundlePretty e
                    exitFailure
                Right p ->
                    return $ mkAppState p
        _ -> exitFailure
    
    finalState <- defaultMain app st

    return ()
