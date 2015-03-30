
import Data.List (sort)

import System.Environment (getArgs)

import Text.Printf (printf)
import Text.Regex.Posix ((=~))

type Timestamp = Integer
type Record = (Timestamp, [String])

isTimestamp :: String -> Bool
isTimestamp t = t =~ "^#[0-9]+$"

parseTimestamp :: String -> Timestamp
parseTimestamp t
  | isTimestamp t = read $ tail t
  | otherwise     = error $ printf "Timestamp format not recognized. [%s]" t

tuples :: [String] -> [Record]
tuples []       = []
tuples [x]      = error "Corrupted input file. Hanging record"
tuples (x : xs) = (parseTimestamp x, y) : tuples ys
  where (y, ys) = break isTimestamp xs

dedup :: [Record] -> [Record]
dedup [] = []
dedup [a] = [a]
dedup (x : y : xs)
  | x == y    = x : (dedup $ dropWhile (== x) xs)
  | otherwise = x : dedup (y : xs)

merge :: [Record] -> [Record] -> [Record]
merge [] [] = []
merge [] b  = b
merge a  [] = a
merge a@((timeA, dataA):as) b@((timeB, dataB):bs)
  | timeA < timeB = (timeA, dataA) : merge as b
  | timeA > timeB = (timeB, dataB) : merge a  bs
  | otherwise     = (timeA, dataA) : merge as bs

format :: [Record] -> [String]
format [] = []
format ((x, y) : xs) = t : y ++ format xs
  where t = printf "#%d" x

main = do
         [a, b, x] <- getArgs
         pa <- readFile a
         pb <- readFile b
         writeFile x $ unlines
                     $ format
                     $ dedup
                     $ sort
                     $ merge (tuples $ lines pa)
                             (tuples $ lines pb)
