{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Applicative
import Utils
import Parser

line :: Parser ()
line = void $ many tokens
  where tokens = void (noneOf "\"()[]{}\n")
                 <|> braced '[' ']'
                 <|> braced '(' ')'
                 <|> braced '{' '}'
                 <|> br '"' '"' (void $ many $ noneOf "\"")
        br o c = between (char o) (char c)
        braced o c = br o c $ void $ line `sepBy` newline

lang :: Parser [SourcePos]
lang = sepBy (getPosition <* line) newline

exec :: [String] -> String -> IO [String]
exec text (cmd:' ':(words -> t))
  | cmd == '?' = do
      case parse lang $ unlines text of
       Left e -> fail $ show e
       Right r -> putStrLn $ show $ length $ takeWhile ((<= l + 1) . fst) r
      return text
  | otherwise = return $ update (update (const cmd) c) l text
  where l = read (t !! 0) - 1
        c = read (t !! 1) - 1

main :: IO ()
main = do
  (text, _:cmds) <- partitionWhile (/= "#") <$> lines <$> getContents
  foldM_ exec text cmds
