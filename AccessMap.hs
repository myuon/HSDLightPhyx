module AccessMap where

import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Lens

import qualified Data.Vector as Vector
import qualified Data.Bits as Bits

import Util
import Particle

import Debug.Trace

type AccessMap = Vector.Vector [Int]
type Particles = Vector.Vector Particle

mapCollisionPair :: AccessMap -> Vector.Vector (Int, Int)
mapCollisionPair = zipPairByGeneration 0
  where
    zipPairByGeneration :: Int -> AccessMap -> Vector.Vector (Int, Int)
    zipPairByGeneration n amap
      | amap == Vector.empty = Vector.empty
      | otherwise = ((,) <$> previous' <*> current') Vector.++
                    (Vector.filter (\(x,y) -> x/=y) $ (,) <$> current' <*> current') Vector.++ zipPairByGeneration (n+1) rest

      where
        (previous, (current, rest)) = cutByGeneration n amap
        
        previous' = Vector.concatMap Vector.fromList previous
        current' = Vector.concatMap Vector.fromList current

cutByGeneration :: Int -> AccessMap -> (AccessMap, (AccessMap, AccessMap))
cutByGeneration n = let n' = 4^n
                    in second (first (Vector.take n') . (id &&& id)) . Vector.splitAt ((n'-1) `div` 3)


register :: Particles -> AccessMap -> AccessMap
register p' amap = amap Vector.// update (makeSeats p') amap

makeSeats :: Particles -> [(Int, Int)]
makeSeats p'
  | Vector.null p' = []
  | otherwise = ((getIndex p), (p ^. index)) : (makeSeats ps)
  where
    (p,ps) = Vector.head &&& Vector.tail $ p'

update :: [(Int, Int)] -> AccessMap -> [(Int, [Int])]
update [] _ = []
update ((n,v):ps) amap = (n, v : (amap Vector.! n)) : update ps amap


collideMap :: Vector.Vector (Int, Int) -> Particles -> Particles
collideMap indexes ps = ps Vector.// newPairs indexes ps

newPairs :: Vector.Vector (Int, Int) -> Particles -> [(Int, Particle)]
newPairs pairs ps
  | Vector.null pairs = []
  | otherwise = case newParticle head' ps of 
    Just (a,b) -> a : b : newPairs tail' ps
    Nothing -> newPairs tail' ps
  where
    (head', tail') = Vector.head &&& Vector.tail $ pairs

newParticle :: (Int, Int) -> Particles -> Maybe ((Int, Particle), (Int, Particle))
newParticle (a,b) ps = let pa = ps Vector.! a;
                           pb = ps Vector.! b
                       in fmap ((\x->(a,x))***(\x->(b,x))) (pa `collideTo` pb)

