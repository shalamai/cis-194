{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.Char (isAlphaNum, isAscii, isControl, isNumber, isDigit)
import Data.List (intercalate, maximumBy, nub, tails, inits)
import Data.String.Interpolate (__i)
import Data.Text (Text, count, pack, split, unpack)
import Data.Ord (comparing)

-- exercise 1

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

safeString :: String -> String
safeString = map (\c -> if isControl c || not (isAscii c) then '_' else c)

holes :: [a] -> [[a]]
holes as = zipWith (++) (inits as) (tail (tails as))

longestText :: Show a => [a] -> a
longestText = maximumBy (comparing (length . show))

adjacents :: [a] -> [(a, a)]
adjacents as = zip as (tail as)

commas :: [String] -> String
commas = intercalate ", "

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1 (zipWith (+))

sumNumbers :: String -> Integer
sumNumbers = foldl sum 0 . filter (/= "") . split (not . isDigit) . pack
  where
    sum = \acc a -> acc + read (unpack a)

-- exercise 2

wordCount :: String -> String
wordCount s =
  [__i| 
      Number of lines: #{numberOfLines}
      Number of empty lines: #{numberOfEmptyLines}
      Number of words: #{numberOfWords}
      Number of unique words: #{numberOfUniqueWords}
      Number of words followed by themselves: #{numberOfWordsFolledByThemselves}
      Length of the longest line: #{lengthOfTHeLongestLine}
  |]
  where
    lines' = lines s
    words' = words s

    numberOfLines = length lines'
    numberOfEmptyLines = length . filter (== "") $ lines'
    numberOfWords = length words'
    numberOfUniqueWords = length . nub $ words'
    numberOfWordsFolledByThemselves = length . filter (uncurry (==)) . adjacents $ words'
    lengthOfTHeLongestLine = longestText lines'

-- exercise 3

ex_halveEvens =
  [ null (halveEvens []),
    halveEvens [1, 2, 3, 4, 5] == [1, 2],
    halveEvens [6, 6, 6, 3, 3, 3, 2, 2, 2] == [3, 3, 3, 1, 1, 1]
  ]

ex_safeString =
  [ null (safeString []),
    safeString "Hello World!" == "Hello World!",
    safeString "That’s your line:\n" == "That_s your line:_",
    safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
  ]

ex_holes =
  [ null (holes ""),
    holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
  ]

ex_longestText =
  [ not (longestText [True, False]),
    longestText [2, 4, 16, 32] == (32 :: Int),
    longestText (words "Hello World") == "World",
    longestText (words "Olá mundo") == "Olá"
  ]

ex_adjacents =
  [ null (adjacents ""),
    null (adjacents [True]),
    adjacents "Hello" == [('H', 'e'), ('e', 'l'), ('l', 'l'), ('l', 'o')]
  ]

ex_commas =
  [ commas [] == "",
    commas ["Hello"] == "Hello",
    commas ["Hello", "World"] == "Hello, World",
    commas ["Hello", "", "World"] == "Hello, , World",
    commas ["Hello", "new", "World"] == "Hello, new, World"
  ]

ex_addPolynomials =
  [ null (addPolynomials [[]]),
    addPolynomials [[0, 1], [1, 1]] == [1, 2],
    addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
  ]

ex_sumNumbers =
  [ sumNumbers "" == 0,
    sumNumbers "Hello world!" == 0,
    sumNumbers "a1bc222d3f44" == 270,
    sumNumbers "words0are1234separated12by3integers45678" == 46927,
    sumNumbers "000a." == 0,
    sumNumbers "0.00a." == 0
  ]

testResults :: [(String, [Bool])]
testResults =
  [ ("halveEvens", ex_halveEvens),
    ("safeString", ex_safeString),
    ("holes", ex_holes),
    ("longestText", ex_longestText),
    ("adjacents", ex_adjacents),
    ("commas", ex_commas),
    ("addPolynomials", ex_addPolynomials),
    ("sumNumbers", ex_sumNumbers)
  ]

formatTests :: [(String, [Bool])] -> String
formatTests = unlines . map (uncurry formatTest)
  where
    formatTest :: String -> [Bool] -> String
    formatTest name results =
      [__i|#{name}: #{successful results}/#{length results} passed!     #{failedReport results}|]

    failedReport :: [Bool] -> String
    failedReport results = if someFailed results then [__i|failed: #{failed results} :(|] else ""

    successful = length . filter id
    failed = commas . map (\(i, _) -> show i) . filter (\(_, r) -> not r) . zip [1 ..]
    someFailed = (/=) 0 . length . failed

main :: IO ()
-- main = putStrLn (wordCount "yoo yoo \n boom \n\n slfjsdf")
main = putStrLn (formatTests testResults)