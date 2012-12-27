module AccessMap where

import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow

import qualified Data.Vector as Vector
import qualified Data.Bits as Bits

import Util

type AccessMap = Vector.Vector (Vector.Vector Int)

mapCollisionPair :: AccessMap -> Vector.Vector (Int, Int)
mapCollisionPair = zipPairByGeneration 0

zipPairByGeneration :: Int -> AccessMap -> Vector.Vector (Int, Int)
zipPairByGeneration n amap
  | amap == Vector.empty = Vector.empty
  | otherwise = ((,) <$> join prevs <*> join current) Vector.++ zipPairByGeneration (n+1) amap

  where
    (prevs, current) = cutByGeneration n amap

cutByGeneration :: Int -> AccessMap -> (AccessMap, AccessMap)
cutByGeneration n = let n' = 4^n
                    in second (Vector.take n') . Vector.splitAt ((n'-1) `div` 3)

putElement :: Int -> Int -> AccessMap -> AccessMap
putElement n v amap = amap Vector.// [(n, (amap Vector.! n) `Vector.snoc` v)]

