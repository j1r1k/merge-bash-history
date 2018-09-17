module Main where

import MergeBashHistory

import Data.Monoid ((<>))
import Data.Text.IO as TIO (readFile, putStr)

import Options.Applicative

data HmbhO = HmbhO FilePath FilePath

opParseFilePath :: Parser FilePath
opParseFilePath = argument str $ metavar "FILE"

opParseHmbhO :: Parser HmbhO
opParseHmbhO = HmbhO <$> opParseFilePath
                     <*> opParseFilePath

execOpParserHmbhO :: IO HmbhO
execOpParserHmbhO = execParser $ info (helper <*> opParseHmbhO) (fullDesc <> header "bash_history merger")

handleFile :: FilePath -> IO [HistoryRecord]
handleFile fp =
  do i <- TIO.readFile fp
     case parseFile i of Left e -> error $ show e
                         Right r -> return r

hmbhHandler :: HmbhO -> IO ()
hmbhHandler (HmbhO fp1 fp2) =
  do f1 <- handleFile fp1
     f2 <- handleFile fp2
     mapM_ (TIO.putStr . recordToText) $ mergeRecords f1 f2


main :: IO ()
main = execOpParserHmbhO >>= hmbhHandler
