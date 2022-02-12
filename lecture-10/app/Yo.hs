{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}

import Test.QuickCheck
import qualified Data.Bifunctor as Bi

-- stream 

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f a = Cons a (streamIterate f (f a))

nats :: Stream Integer
nats = streamIterate (1 +) 0

-- supply

newtype Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons h t) -> (h, t))

runSupply :: Stream s -> Supply s a -> a
runSupply s (S su) = fst (su s)

instance Functor (Supply s) where
  fmap f (S su) = S (Bi.first f . su)

instance Applicative (Supply s) where
  pure a = S (a,)
  (S suf) <*> (S sua) = S $ \s -> let (f, s') = suf s
                                      (a, s'') = sua s'
                                  in (f a, s'')

-- tree

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq, Foldable)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized $ \size -> do
    frequency [(1, Leaf <$> arbitrary),
               (size, let size' = round (fromIntegral size / 2)
                      in Node <$> resize size' arbitrary <*> resize size' arbitrary)]

size :: Tree a -> Int
size = foldr (\_ acc -> acc + 1) 0

toList :: Tree a -> [a]
toList = foldr (:) []

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf a) = Leaf <$> get
    go (Node l r) = Node <$> go l <*> go r


-- props

prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList t = size t == length (toList t)

prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size t == size (labelTree t)

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0..toInteger (size t) - 1]

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = let t' = labelTree t in t' == labelTree t'

prop_all :: Tree Integer -> Bool
prop_all t =
    prop_lengthToList t &&
    prop_sizeLabelTree t &&
    prop_labelTree t &&
    prop_labelTreeIdempotent t
