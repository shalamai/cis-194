{-#LANGUAGE RankNTypes #-}
data Atom = Atom { _element :: String, _point :: Point } deriving Show
data Point = Point { _x :: Double, _y :: Double } deriving Show

getAtomX :: Atom -> Double
getAtomX = _x . _point

setPoint :: Point -> Atom -> Atom
setPoint p a = a { _point = p }

setElement :: String -> Atom -> Atom
setElement e a = a { _element = e }

setX :: Double -> Point -> Point
setX x p = p { _x = x }

setY :: Double -> Point -> Point
setY y p = p { _y = y }

setAtomX :: Double -> Atom -> Atom
setAtomX x a = setPoint (setX x (_point a)) a

-----------------------

newtype I a = MkI a

unI :: I a -> a
unI (MkI a) = a

instance Functor I where
  fmap f (MkI a) = MkI $ f a

instance Applicative I where
  pure = MkI
  (MkI f) <*> (MkI x) = MkI $ f x

newtype C a b = MkC a

unC :: C a b -> a
unC (MkC a) = a

instance Functor (C a) where
  fmap _ (MkC a) = MkC a

newtype CL a b = MkCL [a]

unCL :: CL a b -> [a]
unCL (MkCL as) = as

instance Functor (CL a) where
  fmap _ (MkCL as) = MkCL as

instance Applicative (CL a) where
  pure _ = MkCL []
  (MkCL as) <*> (MkCL bs) = MkCL (as ++ bs)  


type Lens a b = forall t. Functor t => (b -> t b) -> (a -> t a)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set' f a = (\b' -> set' b' a) <$> f (view a)

view :: Lens a b -> a -> b
view l = unC . l MkC

type Traversal a b = forall t . Applicative t => (b -> t b) -> (a -> t a)

set :: Traversal a b -> b -> a -> a
set l b = over l (const b)

over :: Traversal a b -> (b -> b) -> (a -> a)
over l f = unI . l (MkI . f)

listOf :: Traversal a b -> a -> [b]
listOf l = unCL . l (\b -> MkCL [b])

-----------------------

point :: Lens Atom Point
point = mkLens _point setPoint

element :: Lens Atom String
element = mkLens _element setElement

x :: Lens Point Double
x = mkLens _x setX

y :: Lens Point Double
y = mkLens _y setY

this :: Traversal (Maybe a) a
this f Nothing  = pure Nothing
this f (Just x) = Just <$> f x

elems :: Traversal [a] a
elems f []     = pure []
elems f (x:xs) = (:) <$> f x <*> elems f xs

-----------------------

setAtomX' :: Double -> Atom -> Atom
setAtomX' = set (point . x)

moveAtom :: Atom -> Atom
moveAtom = over (point . x) (+1)

askX :: Atom -> IO Atom
askX = (point . x) askUser
  where
    askUser :: Double -> IO Double
    askUser x = do
      putStrLn $ "Current position is " ++ show x ++ ". New Position?"
      answer <- getLine
      return (read answer :: Double)

main :: IO ()
main = do
  let a = listOf elems [1,2,3]
  print a


  -- let a = Atom "abc" (Point 1 2)
  -- a' <- askX a
  -- print a'

-- setAtomX' 2 (Atom "abc" (Point 1 2))