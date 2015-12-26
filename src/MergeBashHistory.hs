module MergeBashHistory
  ( Timestamp
  , Command
  , HmbhError
  , HistoryRecord
  , mergeRecords
  , parseFile
  , recordToText
  ) where

import Control.Applicative ((<|>))

import Data.EitherR (fmapL)
import Data.List.Ordered (merge, nub)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Attoparsec.Text


type Timestamp = Integer
type Command = Text

data HistoryLine = Header Timestamp
                 | Command Command
                 deriving (Show)

data HmbhError = ParseError String
               | EmptyFile
               | MissingHeader

instance Show HmbhError where
  show (ParseError e) = "Error: Parser Error '" ++ e ++ "'"
  show EmptyFile      = "Error: Merging with empty file"
  show MissingHeader  = "Error: Missing header"

data HistoryRecord = HistoryRecord { header   :: Timestamp
                                   , commands :: [Command]
                                   } deriving (Eq, Show)

instance Ord HistoryRecord where
  h1 `compare` h2 = header h1 `compare` header h2

recordToText :: HistoryRecord -> Text
recordToText (HistoryRecord t c) = Text.unlines $ Text.pack ('#' : show t) : c

emptyRecord :: Timestamp -> HistoryRecord
emptyRecord t = HistoryRecord { header = t, commands = [] }

appendCommand :: HistoryRecord -> Command -> HistoryRecord
appendCommand hr c = hr { commands = commands hr ++ [c] }

linesToRecords :: [HistoryLine] -> Either HmbhError [HistoryRecord]
linesToRecords []             = Left EmptyFile
linesToRecords (Command _: _) = Left MissingHeader
linesToRecords (Header t: as) = Right $ squash $ foldr f (emptyRecord t, []) as
  where f (Header  s) acc           = (emptyRecord s, squash acc)
        f (Command c) (current, ps) = (appendCommand current c, ps)
        squash (x, xs) = x : xs

historyHeader :: Parser HistoryLine
historyHeader =
  do _ <- char '#'
     t <- decimal
     endOfLine
     return $ Header t

historyCommand :: Parser HistoryLine
historyCommand =
  do c <- takeTill isEndOfLine
     endOfLine
     return $ Command c

historyLine :: Parser HistoryLine
historyLine = historyHeader <|> historyCommand

historyLines :: Parser [HistoryLine]
historyLines = many' historyLine

mergeRecords :: [HistoryRecord] -> [HistoryRecord] -> [HistoryRecord]
mergeRecords a b = nub $ merge a b

parseFile :: Text -> Either HmbhError [HistoryRecord]
parseFile t = fmapL ParseError (parseOnly historyLines t) >>= linesToRecords
