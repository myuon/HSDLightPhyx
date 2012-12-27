module Util where

import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow

win_width = 640 :: Int
win_height = 480 :: Int

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f = f *** f

mapPair3 :: (a -> b -> c) -> (a,a) -> ((b,b) -> (c,c))
mapPair3 f x = flatPair $ f *** f $ x
  where
    flatPair :: (b -> c, b' -> c') -> ((b,b') -> (c,c'))
    flatPair t = arr (fst t) *** arr (snd t) 

makeColor :: SDL.Surface -> Int -> Int -> Int -> IO SDL.Pixel
makeColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) (fromIntegral r) (fromIntegral g) (fromIntegral b)

(<*$>) :: (a, a) -> (a -> b -> c) -> ((b, b) -> (c, c))
(<*$>) = flip mapPair3

(<$*>) :: (a -> b) -> a -> b
(<$*>) = ($)

