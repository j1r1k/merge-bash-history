import Control.Applicative ((<|>))

import Data.List.Ordered (merge, nub)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text.IO as TIO (readFile, putStr)

import Data.Attoparsec.Text

import qualified Options.Applicative as OP

type Timestamp = Integer
type Command = Text.Text

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

toText :: HistoryRecord -> Text.Text
toText (HistoryRecord t c) = Text.unlines $ Text.pack ('#' : show t) : c

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

data HmbhO = HmbhO FilePath FilePath

opParseFilePath :: OP.Parser FilePath
opParseFilePath = OP.argument OP.str $ OP.metavar "FILE"

opParseHmbhO :: OP.Parser HmbhO
opParseHmbhO = HmbhO <$> opParseFilePath
                     <*> opParseFilePath

execOpParserHmbhO :: IO HmbhO
execOpParserHmbhO = OP.execParser $ OP.info (OP.helper <*> opParseHmbhO) (OP.fullDesc <> OP.header "bash_history merger")

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left l)  = Left $ f l
mapLeft _ (Right r) = Right r

parseFile :: Text.Text -> Either HmbhError [HistoryRecord]
parseFile t = mapLeft ParseError (parseOnly historyLines t) >>= linesToRecords

handleFile :: FilePath -> IO [HistoryRecord]
handleFile fp =
  do i <- TIO.readFile fp
     case parseFile i of Left e -> error $ show e
                         Right r -> return r

hmbhHandler :: HmbhO -> IO ()
hmbhHandler (HmbhO fp1 fp2) =
  do f1 <- handleFile fp1
     f2 <- handleFile fp2
     mapM_ (TIO.putStr . toText) $ mergeRecords f1 f2

main :: IO ()
main = execOpParserHmbhO >>= hmbhHandler
