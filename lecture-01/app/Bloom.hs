import CodeWorld

tree :: Integer -> Picture -> Picture
tree 0 bloom = bloom
tree n bloom = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) bloom) & rotated (-pi/10) (tree (n-1) bloom))


bloom :: Double -> Picture
bloom size =  bud size & petals 6 size

bud :: Double -> Picture
bud size = colored (lighter 0.1 purple) (solidCircle (size * 0.3))

petals :: Integer -> Double -> Picture
petals 0 _ = blank
petals n size = colored pink (petal size & rotated (pi/3) (petals (n - 1) size))

petal :: Double -> Picture
petal size = thickClosedCurve 0.05 [(0,0), (0.25 * size,0.75 * size), (1 * size,1 * size)]

maxBloomSize = 0.4

treeController :: Double -> Picture
treeController t
  | t > 10       = tree 8 (bloom maxBloomSize)
  | otherwise    = tree 8 (bloom (maxBloomSize / (11 - t)))

main :: IO ()
main = animationOf treeController
