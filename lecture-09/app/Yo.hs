{-# LANGUAGE LambdaCase #-}
import qualified Data.Bifunctor as B
import qualified Data.Functor as F
import qualified Control.Applicative as A
import Control.Applicative (Alternative (..))
import Data.Char (isAlphaNum, isSpace, isLetter, isDigit)
import Data.List (nub)

import System.Environment
import System.IO
import System.Exit ( exitFailure )

-- bnf in bnf

bnf :: Descr f => f BNF
bnf = some production
  where
    identifier = nonTerminal "identifier" $ (:) <$> letter <*> many (letter <|> digit <|> char '-')
    quotedChar = nonTerminal "quoted-char" $ notQuoteOrBackslash <|> (char '\\' *> char '\\') <|> (char '\\' *> char '\'')
    terminal = nonTerminal "terminal" $ Terminal <$> (char '\'' *> many quotedChar <* char '\'' <* spaces)
    nonTerminal' = nonTerminal "non-terminal" $ NonTerminal <$> identifier <* spaces
    option = nonTerminal "option" $ Optional <$> (char '[' *> spaces *> rhs <* spaces <* char ']' <* spaces)
    repetition = nonTerminal "repetition" $ Repetition <$> (char '{' *> spaces *> rhs <* spaces <* char '}' <* spaces)
    group = nonTerminal "group" $ char '(' *> spaces *> rhs <* spaces <* char ')' <* spaces
    atom = nonTerminal "atom" $ terminal <|> nonTerminal' <|> option <|> repetition <|> group
    sequence' = nonTerminal "sequence" $ mkSequences <$> atom <*> many (spaces *> char ',' *> spaces *> atom) <* spaces
    choice = nonTerminal "choice" $ mkChoices <$> sequence' <*> many (spaces *> char '|' *> spaces *> sequence') <* spaces
    rhs = nonTerminal "rhs" choice
    production = nonTerminal "production" $ (,) <$> identifier <* spaces <* char '=' <* spaces <*> rhs <* char ';' <* spaces

-- ebnf

data RHS
  = Terminal String
  | NonTerminal String
  | Choice RHS RHS
  | Sequence RHS RHS
  | Optional RHS
  | Repetition RHS
  deriving (Show, Eq)

mkChoices :: RHS -> [RHS] -> RHS
mkChoices = foldl Choice

mkSequences :: RHS -> [RHS] -> RHS
mkSequences = foldl Sequence

ppRHS :: RHS -> String
ppRHS = go 0
  where
    go _ (Terminal s)     = surround "'" "'" $ concatMap quote s
    go _ (NonTerminal s)  = s
    go a (Choice x1 x2)   = p a 1 $ go 1 x1 ++ " | " ++ go 1 x2
    go a (Sequence x1 x2) = p a 2 $ go 2 x1 ++ ", "  ++ go 2 x2
    go _ (Optional x)     = surround "[" "]" $ go 0 x
    go _ (Repetition x)   = surround "{" "}" $ go 0 x

    surround c1 c2 x = c1 ++ x ++ c2

    p a n | a > n     = surround "(" ")"
          | otherwise = id

    quote '\'' = "\\'"
    quote '\\' = "\\\\"
    quote c    = [c]

type Production = (String, RHS)
type BNF = [Production]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i,rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")

-- parser

newtype Parser a = P { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = P (fmap (B.first f) . runParser p)

instance Applicative Parser where
    pure a = P (\s -> Just (a, s))
    ff <*> fa = P$ \s -> do
      (f,s') <- runParser ff s
      (a, s'') <- runParser fa s'
      return (f a, s'')

instance Alternative Parser where
  empty = P (const Nothing)
  p1 <|> p2 = P $ \s ->
    runParser p1 s <|> runParser p2 s

parse :: Parser a -> String -> Maybe a
parse p s = do
  (a, s') <- runParser p s
  if s' == "" then Just a else Nothing

charThatP :: (Char -> Bool) -> Parser Char
charThatP p = P $ \case "" -> Nothing
                        h:t -> if p h then Just(h, t) else empty

-- grammar

newtype Grammar a = G ([String] -> (BNF, RHS))

runGrammer :: String -> Grammar a -> BNF
runGrammer main (G f) = case f [] of
  (prods, NonTerminal nt) | main == nt -> prods
  (prods, rhs) -> prods ++ [(main, rhs)]

ppGrammar :: String -> Grammar a -> String
ppGrammar main g = ppBNF $ runGrammer main g

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

instance Functor Grammar where
    fmap _ (G bnf) = G bnf

instance Applicative Grammar where
    pure x = G (const ([], Terminal ""))
    G f1 <*> G f2 = G $ \seen -> 
      case (f1 seen, f2 seen) of
        ((prods1, Terminal ""), (prods2, rhs2)) -> (mergeProds prods1 prods2, rhs2)
        ((prods1, rhs1), (prods2, Terminal "")) -> (mergeProds prods1 prods2, rhs1)
        ((prods1, rhs1), (prods2, rhs2)) -> (mergeProds prods1 prods2, Sequence rhs1 rhs2)

instance Alternative Grammar where
  empty = G (const ([], Terminal ""))
  some g = (:) <$> g <*> many g

  (G f1) <|> (G f2) = G $ \seen -> 
    let (prods1, rhs1) = f1 seen
        (prods2, rhs2) = f2 seen
    in (mergeProds prods1 prods2, Choice rhs1 rhs2)

  many (G f) = G $ \seen -> 
    let (prods, rhs) = f seen 
    in (prods, Repetition rhs)

-- descr

class (Applicative f, Alternative f) => Descr f where
    char :: Char -> f Char
    primitive :: String -> Parser a -> f a
    nonTerminal :: String -> f a -> f a

instance Descr Parser where
    char c = charThatP (c ==)
    primitive _ p = p
    nonTerminal _ p = p

instance Descr Grammar where
    char c = G (const ([], Terminal [c]))
    primitive s _ = G (const ([] ,NonTerminal s))
    nonTerminal name (G g) = G $ \seen ->
        if name `elem` seen
        then ([], NonTerminal name)
        else let (prods, rhs) = g (name : seen)
             in (prods ++ [(name, rhs)], NonTerminal name)

sepBy :: Descr f => f a -> f b -> f [a]
sepBy p1 p2 = ((:) <$> p1 <*> many (p2 *> p1)) <|> pure []

anyChar :: Descr f => f Char
anyChar = primitive "char" (charThatP (const True))

newline :: Descr f => f Char
newline = primitive "newline" (char '\n')

notQuote :: Descr f => f Char
notQuote = primitive "not-quote" (charThatP ('"' /=))

alphaNum :: Descr f => f Char
alphaNum = primitive "alphanum" (charThatP isAlphaNum)

notNewLine :: Descr f => f Char
notNewLine = primitive "not-newline" (charThatP ('\n' /=))

letter :: Descr f => f Char
letter = primitive "letter" $ charThatP isLetter

digit :: Descr f => f Char
digit = primitive "digit" $ charThatP isDigit

notQuoteOrBackslash :: Descr f => f Char
notQuoteOrBackslash = primitive "non-quote-or-backslash" $ charThatP (\c -> c /= '\\' && c /= '\'')

space :: Descr f => f Char
space = primitive "space" (charThatP (\c -> c /= '\n' && isSpace c))

spaces :: Descr f => f String
spaces = nonTerminal "spaces" $ many (char ' ' <|> newline)

-- The main function

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStr $ ppGrammar "ini" ini
        [fileName] -> do
            input <- readFile fileName
            case parse ini input of
                Just i -> putStr $ show i
                Nothing -> do
                    hPutStrLn stderr "Failed to parse INI file."
                    exitFailure
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure


-- e.g.

dottedWords :: Descr f => f [String]
dottedWords = some (many anyChar <* char '.')

csv :: Descr f => f [[String]]
csv = many parseLine
  where
    parseLine = nonTerminal "line" $
        parseCell `sepBy` char ',' <* newline
    parseCell = nonTerminal "cell" $
        char '"' *> many notQuote <* char '"'

type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

ini :: Descr f => f INIFile
ini = many parseSection
  where
    parseSection = nonTerminal "section" $ A.liftA2 (,) parseTitle (many parseDeclaration)
    parseTitle = nonTerminal "title" $ char '[' *> parseIdentifier <* char ']' <* parseJunk
    parseDeclaration = nonTerminal "delcaration" $ (,) <$> (parseIdentifier <* many space <* char '=' <* many space) <*> (parseRestOfTheRaw <* parseJunk)
    parseIdentifier = nonTerminal "identifier" $ many alphaNum
    parseRestOfTheRaw = many notNewLine <* newline
    parseJunk = nonTerminal "junk" $ F.void $ many (parseEmptyLine <|> parseComment)
    parseEmptyLine = nonTerminal "empty-line" $ F.void $ many space <* newline
    parseComment = nonTerminal "comment" $ F.void $ char '#' <* parseRestOfTheRaw