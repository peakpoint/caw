{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle.Parser
    ( pPuzzleImplicit
    )
    where

import Data.Void

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Puzzle.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Monad
import Data.Char
import GHC.Arr
import Data.List (partition, find)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)

type Parser = Parsec Void Text

data Section = SMetadata | SGrid | SClues deriving (Show, Eq)

data Header =
      HTitle
    | HAuthor
    | HEditor
    | HCopyright
    | HDate
    | HRebus
    | HSpecial
    deriving (Show, Eq)

type RebusMap = Map Char Text


data HeaderData = DText Header Text | DRebus RebusMap | DSpecial SpecialStyle deriving (Show, Eq)

lexeme :: Parser s -> Parser s
lexeme = L.lexeme hspace

lexeme' :: Parser s -> Parser s
lexeme' = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol hspace

pHeader :: Parser Header
pHeader = choice
    [ HTitle <$ "Title"
    , HAuthor <$ "Author"
    , HEditor <$ "Editor"
    , HCopyright <$ "Copyright"
    , HDate <$ "Date"
    , HRebus <$ "Rebus"
    , HSpecial <$ "Special"
    ]

isRebusChar :: Char -> Bool
isRebusChar c = not (isUpper c) && not (isSpace c)

pRebusData :: Parser (Char, Text)
pRebusData = lexeme $ do
    c <- satisfy isRebusChar <?> "non-uppercase and non-space character"
    void (char '=')
    text <- takeWhileP Nothing (not . isSpace)
    return (c, text)

eol' :: Parser ()
eol' = void eol <|> eof

pRebus :: Parser (Map Char Text)
pRebus = M.fromList <$> manyTill pRebusData eol'

isEOLChar :: Char -> Bool
isEOLChar c = c == '\n' || c == '\r'

-- takes everything until the EOL or EOF
toEOL :: Parser Text
toEOL = do
    t <- takeWhileP Nothing $ not . isEOLChar
    eol'
    return t

pHeaderData :: Header -> Parser HeaderData
pHeaderData HRebus = DRebus <$> pRebus
pHeaderData HSpecial = DSpecial <$> lexeme (choice
    [ Shaded <$ "shaded"
    , Circle <$ "circle"
    ])
pHeaderData h = DText h <$> toEOL

pSection1 :: Parser [HeaderData] --[(Header, HeaderData)]
pSection1 = lexeme' . many $ do
    h <- pHeader
    void $ string ": "
    pHeaderData h

pSection :: Parser Section
pSection = lexeme' $ do
    void $ string "## "
    choice
        [ SMetadata <$ string' "metadata"
        , SGrid <$ string' "grid"
        , SClues <$ string' "clues"
        ]

type PreGrid = Array GridIndex Char

-- toPreSquare :: Char -> PreSquare
-- toPreSquare c
--     | isLower c = Letter C
--     | isUpper c = 

pPreGrid :: Parser PreGrid
pPreGrid = lexeme' $ do
    grid <- some row
    let x = length grid
        y = length $ head grid
    return $
        array ((1, 1), (x, y)) $
        concat $
        zipWith (\r i ->
            zipWith (\c j -> ((i, j), c)) r [1..]
            ) grid [1..]
    where
        row :: Parser String
        row = do
            hspace
            t <- lexeme $ takeWhile1P Nothing $ not . isSpace
            void newline
            return $ T.unpack t

toSquare :: RebusMap -> Char -> Square
toSquare m c
    | isUpper c = Letter c Normal
    | c `elem` ("#_." :: String) = Block
    | otherwise =
        case M.lookup c m of
            Just t -> Rebus t $ if isLower c then Special else Normal
            Nothing -> if isLower c then Letter c Special else Block

toGrid :: PreGrid -> RebusMap -> Grid
toGrid arr m = toSquare m <$> arr

pDir :: Parser Dir
pDir = choice
    [ Across <$ char 'A'
    , Down <$ char 'D'
    ]

pClue :: Parser Clue
pClue = lexeme' $ do
    dir <- pDir

    (num :: Int) <- L.decimal
    void ". "

    (clue, answer) <- manyTill_ anySingle
        (string " ~ " >> toEOL)

    void . optional . try $ do
        dir' <- pDir
        when (dir /= dir') $ fail "Direction must match"

        (num' :: Int) <- L.decimal
        when (num /= num') $ fail "Number must match"

        void " ^"
        void toEOL

    return $ Clue
        { cDir = dir
        , cNum = num
        , cClue = T.pack clue
        , cAnswer = answer
        , cRefs = []
        }

pClues :: Parser Clues
pClues = do
    (as, ds) <- partition (\c -> cDir c == Across) <$> many pClue

    return $ Clues
        { acrossClues = toClueMap as
        , downClues = toClueMap ds
        }

    where
        toClueMap :: [Clue] -> ClueMap
        toClueMap l = IM.fromList $ map (\c -> (cNum c, c)) l

-- implicit order
pPuzzleImplicit :: Parser Puzzle
pPuzzleImplicit = liftM3 assemblePuzzle pSection1 pPreGrid pClues

assemblePuzzle :: [HeaderData] -> PreGrid -> Clues -> Puzzle
assemblePuzzle hd preGrid clues = Puzzle
    { width = width
    , height = height
    , grid = toGrid preGrid $ fromMaybe mempty rebus
    , special = special
    , clues = clues
    , metadata = Metadata
        { title = find' HTitle
        , author = find' HAuthor
        , editor = find' HEditor
        , date = find' HDate
        , copyright = find' HCopyright } }
    where
        (_, (width, height)) = bounds preGrid

        findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
        findMaybe f l = msum $ f <$> l

        rebus = findMaybe (\case
            DRebus r -> Just r
            _ -> Nothing) hd
        
        special = findMaybe (\case
            DSpecial s -> Just s
            _ -> Nothing) hd

        find' h = findMaybe (\case
                DText h' t -> if h == h' then Just t else Nothing
                _ -> Nothing) hd

