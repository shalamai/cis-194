{-# LANGUAGE LambdaCase #-}
import qualified Data.Bifunctor as B
import qualified Control.Monad as M
import qualified Control.Applicative as A
import Data.Char (isAlphaNum, isSpace)

import System.Environment
import System.IO
import System.Exit
import Control.Applicative (Alternative ((<|>)))

-- exercise 1
data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA a) where
  fmap f (Con1 a b) = Con1 a (f b)
  fmap f (Con2 ms) = Con2 (fmap (fmap (f .)) ms)


data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor g => Functor (ComplicatedB f g a) where
  fmap f (Con3 fa) = Con3 fa
  fmap f (Con4 gb) = Con4 (f <$> gb)
  fmap f (Con5 ggbs) = Con5 (fmap (fmap $ map f) ggbs)


-- exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' xs = (\x -> (x,x)) <$> xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' xs = ((,) <$> xs) <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' xs = (\a _ -> (a, a)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\a b -> a + b + 2) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' = fmap (\x -> if x > 0 then (x, 0) else (0, x))

-- not possible if we care about layzness behaviour. e.g isJust (func7 (Just undefined))
func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+ x) <$> xs

-- not possible as there is a sequencing of calculations
func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap (\x -> (x * x) + 10)


-- exercise 3
newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap f p = P (fmap (B.first f) . runParser p)

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P $ \s -> do
                            (f, s') <- runParser fp s
                            (x, s'') <- runParser fx s'
                            return (f x, s'')

instance Monad Parser where
  return = pureParser
  fa >>= k = P $ \s -> do
                          (a, s') <- runParser fa s
                          runParser (k a) s'

instance Alternative Parser where
  empty = noParser
  p1 <|> p2 = P $ \s -> runParser p1 s A.<|> runParser p2 s

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = runParser p s >>= \(a, s') -> if s' == "" then Just a else Nothing

noParser :: Parser a
noParser = P (const Nothing)

pureParser :: a -> Parser a
pureParser a = P (\s -> Just (a, s))

anyChar :: Parser Char
anyChar = P $ \case "" -> Nothing
                    h:t -> Just(h, t)

charThat :: (Char -> Bool) -> Parser Char
charThat p = anyChar >>= \a -> if p a then return a else noParser

char :: Char -> Parser ()
char c = M.void $ charThat (c ==)

anyCharBut :: Char -> Parser Char
anyCharBut c = charThat (c /=)

letterOrDigit :: Parser Char
letterOrDigit = charThat isAlphaNum

space :: Parser Char
space = charThat (\c -> c /= '\n' && isSpace c)

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) <|> return []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> many (p2 *> p1)) <|> return []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content


-- exercise 4

type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = many parseSection

parseSection :: Parser Section
parseSection = A.liftA2 (,) parseTitle (many parseDeclaration)

parseTitle :: Parser Identifer
parseTitle = char '[' *> parseIdentifier <* char ']' <* parseJunk

parseDeclaration :: Parser Declaration
parseDeclaration = do
            key <- parseIdentifier
            many space <* char '=' <* many space
            value <- parseRestOfTheRaw
            parseJunk
            return (key, value)

parseIdentifier :: Parser Identifer
parseIdentifier = many letterOrDigit

parseRestOfTheRaw :: Parser String
parseRestOfTheRaw = many (anyCharBut '\n') <* char '\n'

parseJunk :: Parser ()
parseJunk = M.void $ many (parseEmptyLine <|> parseComment)
  where
    parseEmptyLine = M.void $ many space <* char '\n'
    parseComment = M.void $ char '#' <* parseRestOfTheRaw



main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [fileName] -> readFile fileName
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure
