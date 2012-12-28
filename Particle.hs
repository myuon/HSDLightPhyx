{-# LANGUAGE TemplateHaskell #-}
module Particle where

import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Lens

import qualified Data.Vector as Vector
import qualified Data.Bits as Bits

import Util

data Particle = Particle {    
  _pos :: (Float, Float),
  _vector :: (Float, Float),
  _size :: Int,
  _index :: Int,
  _color :: (Int, Int, Int)
  }

$(makeLenses ''Particle)

draw :: SDL.Surface -> Particle -> IO Bool
draw screen p = let (x,y) = mapPair floor $ p ^. pos; (r,g,b) = p ^. color; s = p ^. size
                in SDL.fillRect screen (Just (SDL.Rect x y s s))
                   =<< makeColor screen r g b

putG :: Float -> Particle -> Particle
putG g p = vector .~ (p ^. vector) <*$> (+) <$*> (0,g) $ p

move :: Particle -> Particle
move p = pos .~ (p ^. pos) <*$> (+) <$*> mapPair (*deltaT) (p ^. vector) $ 
         vector %~ mapPair (*airK) $ p

wall :: Particle -> Particle
wall = adjustPosByWall

adjustPosByWall :: Particle -> Particle
adjustPosByWall = adjustX . adjustY
  where
    adjustX :: Particle -> Particle
    adjustX p
      | floor x - p^.size < 0 = pos .~ (0.0,y) $
                          vector .~ (p^.vector) <*$> (*) <$*> (-ballE,1) $ p
      | floor x + p^.size > winWidth = pos .~ (fromIntegral (winWidth - p^.size),y) $
                                 vector .~ (p^.vector) <*$> (*) <$*> (-ballE,1) $ p
      | otherwise = p
                                    
      where (x,y) = p ^. pos
            
    adjustY :: Particle -> Particle
    adjustY p
      | floor y - p^.size < 0 = pos .~ (x,0) $ 
                          vector .~ (p^.vector) <*$> (*) <$*> (1,-ballE) $ p
      | floor y + p^.size > winHeight = pos .~ (x,fromIntegral (winHeight-p^.size)) $ 
                                  vector .~ (p^.vector) <*$> (*) <$*> (1,-ballE) $ p
      | otherwise = p
    
      where (x,y) = p ^. pos
    
get2DMortonNumber :: (Int, Int) -> Int
get2DMortonNumber (x,y) = bitSeperate x Bits..|. 
                          bitSeperate y `Bits.shiftL` 1
  where
    bitSeperate :: Int -> Int
    bitSeperate = shiftBitPart 1 0x55555555 . 
                  shiftBitPart 2 0x33333333 . 
                  shiftBitPart 4 0x0f0f0f0f .
                  shiftBitPart 8 0x00ff00ff
    
    shiftBitPart :: Int -> Int -> Int -> Int
    shiftBitPart digit bit n = (n Bits..|. (n `Bits.shiftL` digit)) Bits..&. bit

getEdgePos :: Particle -> ((Int, Int), (Int, Int))
getEdgePos p = mapPairSize (+) &&& mapPairSize (flip (-)) $ mapPair floor $ p ^. pos
  where
    mapPairSize :: (Int -> b -> c) -> (b,b) -> (c,c)
    mapPairSize = flip mapPair3 (p ^. size, p ^. size)

getSpacePos :: ((Int, Int), (Int, Int)) -> (Int, Int)
getSpacePos p@(x,_) = findSpace x $ uncurry Bits.xor $ mapPair get2DMortonNumber p
  where
    findSpace :: (Int, Int) -> Int -> (Int, Int)
    findSpace n morton = ((6-shiftNumber) `div` 2, get2DMortonNumber n `Bits.shiftR` shiftNumber)
      where 
        shiftNumber = getShiftNumber morton
    
    getShiftNumber :: Int -> Int
    getShiftNumber x' = case shiftNumber of
      Just n -> n+2
      Nothing -> 6
      
      where
        shiftNumber = isShiftable 4 x' <|>
                      isShiftable 2 x' <|>
                      isShiftable 0 x'
    
    isShiftable :: Int -> Int -> Maybe Int
    isShiftable n x' = if x' `Bits.shiftR` n == 0 then Nothing else Just n
    

getAccessPos :: Particle -> (Int, Int)
getAccessPos = getSpacePos . getEdgePos

getTreeIndex :: (Int, Int) -> Int
getTreeIndex (x,y) = (4^x - 1) `div` 3 + y
