{-# LANGUAGE TupleSections #-}

-- fib

import Control.Monad (replicateM)
import qualified Data.Bifunctor as Bi
import System.Random

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- stream

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

streamRepeat :: a -> Stream a
streamRepeat a = let as = Con a as in as

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f a = Cons a (streamIterate f (f a))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a as) bs = Cons a (streamInterleave bs as)

nats :: Stream Integer
nats = streamIterate (1 +) 0

ruler :: Stream Integer
ruler = streamMap (1 +) $ streamInterleave (streamRepeat (-1)) ruler

-- supply

data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons h t) -> (h, t))

pureSupply :: a -> Supply s a
pureSupply a = S (a,)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S su) = S (Bi.first f . su)

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S su1) (S su2) = S ( \s -> let (a, s') = su1 s
                                             (b, s'') = su2 s'
                                         in (f a b, s''))

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S su) f = S ( \s -> let (a, s') = su s
                                    S su2 = f a
                                in su2 s')

runSupply :: Stream s -> Supply s a -> a
runSupply s (S su) = fst (su s)

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply

-- label tree

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show)

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf a) = Leaf <$> get
    go (Node l r) = Node <$> go l <*> go r

-- dice throws

type Rand a = Supply Integer a

randomDice :: RandomGen g => g -> Stream Integer
randomDice gen =
  let (roll, gen') = randomR (1, 6) gen
   in Cons roll (randomDice gen')

runRand :: Rand a -> IO a
runRand r = do
  stdGen <- getStdGen
  let diceRolls = randomDice stdGen
  return $ runSupply diceRolls r

averageOfTwo :: Rand Double
averageOfTwo = do
  d1 <- get
  d2 <- get
  return $ fromIntegral (d1 + d2) / 2

bestOutOfTwo :: Rand Double
bestOutOfTwo = do
  d1 <- get
  d2 <- get
  return $ fromIntegral $ if d1 > d2 then d1 else d2

-- Look, ma, I’m recursive!
sumUntilOne :: Rand Double
sumUntilOne = do
  d <- get
  if d == 1
    then return 0
    else do
      s <- sumUntilOne
      return (s + fromIntegral d)

sample :: Int -> Rand Double -> Rand (Double, Double)
sample n what = do
  samples <- replicateM n what
  return (maximum samples, sum samples / fromIntegral n)

main =
  mapM_
    go
    [ ("average of two", averageOfTwo),
      ("bestOutOfTwo", bestOutOfTwo),
      ("sumUntilOne", sumUntilOne)
    ]
  where
    n = 10000
    go (name, what) = do
      (max, avg) <- runRand (sample n what)
      putStrLn $
        "Playing \"" ++ name ++ "\" " ++ show n ++ " times "
          ++ "yields a max of "
          ++ show max
          ++ " and an average of "
          ++ show avg
          ++ "."