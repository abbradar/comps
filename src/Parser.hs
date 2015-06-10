{-# LANGUAGE TupleSections #-}

module Parser
       ( Parser
       , SourcePos
       , Error
       , parse
       , getPosition
       , anyChar
       , satisfy
       , eof
       , char
       , newline
       , upper
       , lower
       , space
       , spaces
       , alphaNum
       , string
       , number
       , try
       , anyOf
       , noneOf
       , sepBy
       , between
       , (<&>)
       , strOf
       , chainl1
       , chainr1
       , trace
       ) where

import Data.Char
import Data.Monoid
import Control.Monad
import Control.Arrow
import Control.Applicative
import qualified Debug.Trace as T

type SourcePos = (Int, Int)
type Error = (SourcePos, String)

data State = State { remaining :: !String
                   , sourcePos :: !SourcePos
                   }
             deriving (Show, Eq)

newtype Parser a = Parser { runParser :: State -> Either Error (a, State) }

instance Functor Parser where
  fmap f p = Parser $ \s -> first f <$> runParser p s

instance Applicative Parser where
  pure a = Parser $ pure . (a, )
  a <*> b = do
    f <- a
    r <- b
    return $ f r

instance Alternative Parser where
  empty = fail "empty: no suitable parser"
  a <|> b = Parser $ \s -> case runParser a s of
    Left _ -> runParser b s
    r -> r

instance Monad Parser where
  return = pure
  a >>= b = Parser $ \s -> do
    (r, s') <- runParser a s
    runParser (b r) s'
  fail e = Parser $ \s -> Left (sourcePos s, e)

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

parse :: Parser a -> String -> Either Error a
parse p s = fst <$> runParser p State { remaining = s, sourcePos = (1, 1) }

anyChar :: Parser Char
anyChar = Parser $ \s -> case remaining s of
  [] -> Left (sourcePos s, "anyChar: empty input")
  (h:t) -> let sourcePos' = if h == '\n'
                            then (fst (sourcePos s) + 1, 1)
                            else second (+1) $ sourcePos s
           in pure (h, State { remaining = t
                             , sourcePos = sourcePos'
                             }
                   )

getPosition :: Parser SourcePos
getPosition = Parser $ \s -> pure (sourcePos s, s)

satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy f p = Parser $ \s -> let check o@(r, _)
                                   | not $ f r = Left (sourcePos s, "satisfy: failed")
                                   | otherwise = pure o
                             in runParser p s >>= check

-- Derived functions

eof :: Parser ()
eof = try anyChar >>= maybe (pure ()) (const $ fail "eof: non-empty input")

char :: Char -> Parser Char
char c = satisfy (== c) anyChar

newline :: Parser Char
newline = char '\n'

upper :: Parser Char
upper = satisfy isUpper anyChar

lower :: Parser Char
lower = satisfy isLower anyChar

space :: Parser Char
space = satisfy (== ' ') anyChar

spaces :: Parser String
spaces = many space

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum anyChar

number :: Parser Integer
number = read <$> some (satisfy isDigit anyChar)

string :: String -> Parser String
string [] = pure []
string (h:t) = (:) <$> char h <*> string t

try :: Parser a -> Parser (Maybe a)
try p = Just <$> p <|> pure Nothing

anyOf :: [Char] -> Parser Char
anyOf cs = satisfy (`elem` cs) anyChar

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (not . flip elem cs) anyChar

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p s = do
  h <- p
  t <- (s >> sepBy p s) <|> pure []
  return $ h:t

between :: Parser sep1 -> Parser sep2 -> Parser a -> Parser a
between o c p = o *> p <* c

(<&>) :: Monoid a => Parser a -> Parser a -> Parser a
a <&> b = (<>) <$> a <*> b

infixl 4 <&>

strOf :: [String] -> Parser String
strOf = foldr1 (<|>) . map string

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where rest x = do
          f <- op
          y <- p
          rest (f x y)
         <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = expr
  where expr = first <*> expr <|> p
        first = do
          x <- p
          f <- op
          return (f x)

trace :: String -> Parser a -> Parser a
trace str exp = do
  --r <- T.trace ("entering " ++ str) exp
  --return $ T.trace ("exiting " ++ str) r
  exp
