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

mapPairBinary :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
mapPairBinary = (>>> uncurry (***)) . mapPair

makeColor :: SDL.Surface -> Int -> Int -> Int -> IO SDL.Pixel
makeColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) (fromIntegral r) (fromIntegral g) (fromIntegral b)

(<*$>) :: (a, a) -> (a -> b -> c) -> (b, b) -> (c, c)
(<*$>) = flip mapPairBinary

(<$*>) :: (a -> b) -> a -> b
(<$*>) = ($)

norm :: (Float, Float) -> Float
norm (x,y) = sqrt $ x^2 + y^2

(|*|) :: (Float, Float) -> (Float, Float) -> Float
(a,b) |*| (c,d) = a*c + b*d

scale :: (Num t) => t -> (t,t) -> (t,t)
scale k = mapPair (*k)
