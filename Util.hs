module Util where

import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow

winWidth = 800 :: Int
winHeight = 600 :: Int

constG = 0.1 :: Float
ballE = 0.88 :: Float
airK = 0.9998 :: Float
deltaT = 0.5 :: Float

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f = f *** f

mapPair3 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
mapPair3 f x = flatPair $ f *** f $ x
  where
    flatPair :: (b -> c, b' -> c') -> (b,b') -> (c,c')
    flatPair t = arr (fst t) *** arr (snd t) 

makeColor :: SDL.Surface -> Int -> Int -> Int -> IO SDL.Pixel
makeColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) (fromIntegral r) (fromIntegral g) (fromIntegral b)

(<*$>) :: (a, a) -> (a -> b -> c) -> (b, b) -> (c, c)
(<*$>) = flip mapPair3

(<$*>) :: (a -> b) -> a -> b
(<$*>) = ($)

pairMaybe :: (a, Maybe b) -> Maybe (a, b)
pairMaybe (a, Just b) = Just (a,b)
pairMaybe _ = Nothing

flatMaybeList :: [Maybe a] -> [a]
flatMaybeList [] = []
flatMaybeList (p:ps) = case p of
  Just x -> x : flatMaybeList ps
  Nothing -> flatMaybeList ps

