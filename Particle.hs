{-# LANGUAGE TemplateHaskell #-}
module Particle where

import qualified Graphics.UI.SDL as SDL
--import qualified Graphics.UI.SDL.Primitives as SDL.P

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Lens

import qualified Data.Vector as Vector
import qualified Data.Bits as Bits

import Util

import Debug.Trace

maybeDistance :: Int -> Int -> (Float, Float) -> (Float, Float) -> Maybe Float
maybeDistance r r' (x,y) (x',y') = let pdist = sqrt $ (x-x')^2+(y-y')^2;
                                       rdist = (fromIntegral r + fromIntegral r')
                                   in if pdist <= rdist then Just (rdist - pdist) else Nothing
    
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

getGenerationShift :: Int -> Int
getGenerationShift x = case shiftNumber of
  Just n -> n+2
  Nothing -> 6
      
  where
    shiftNumber = isShiftable 4 x <|>
                  isShiftable 2 x <|>
                  isShiftable 0 x

    isShiftable :: Int -> Int -> Maybe Int
    isShiftable n p = if p `Bits.shiftR` n == 0 then Nothing else Just n
    
data Particle = Particle {    
  _pos :: (Float, Float),
  _vector :: (Float, Float),
  _size :: Int,
  _color :: (Int, Int, Int),
  _index :: Int
  }

$(makeLenses ''Particle)

init :: Particle
init = Particle {
  _pos = (0,0), 
  _vector = (0,0),
  _color = (255, 128, 0),
  _size = 50,
  _index = -1
  }

draw :: SDL.Surface -> Particle -> IO Bool
draw screen p = let (x,y) = mapPair floor $ p ^. pos; (r,g,b) = p ^. color; s = p ^. size
                in SDL.fillRect screen (Just (SDL.Rect (x-s) (y-s) (2*s) (2*s)))
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
      | floor x - p^.size < 0 = pos .~ (fromIntegral (p ^. size),y) $
                                vector .~ (p^.vector) <*$> (*) <$*> (-ballE,1) $ p
      | floor x + p^.size > winWidth = pos .~ (fromIntegral (winWidth - p^.size),y) $
                                       vector .~ (p^.vector) <*$> (*) <$*> (-ballE,1) $ p
      | otherwise = p
    
      where (x,y) = p ^. pos
    
    adjustY :: Particle -> Particle
    adjustY p
      | floor y - p^.size < 0 = pos .~ (x,fromIntegral (p ^. size)) $ 
                                vector .~ (p^.vector) <*$> (*) <$*> (1,-ballE) $ p
      | floor y + p^.size > winHeight = pos .~ (x,fromIntegral (winHeight - p^.size)) $ 
                                        vector .~ (p^.vector) <*$> (*) <$*> (1,-ballE) $ p
      | otherwise = p

      where (x,y) = p ^. pos
    
collideTo :: Particle -> Particle -> Maybe (Particle, Particle)
collideTo p q = collide q p

collide :: Particle -> Particle -> Maybe (Particle, Particle)
collide q p = case maybeDistance (p ^. size) (q ^. size) (q ^. pos) (p ^. pos) of 
  Just d -> Just $
          (pos .~ ((negate $ r1*d) `scale` c) <*$> (+) <$*> (p^.pos) $
           vector .~ (r1 `scale` k) <*$> (+) <$*> (p^.vector) $ 
           color .~ (0, 128, 255) $ p,
           pos .~ ((negate $ r2*d) `scale` c) <*$> (+) <$*> (q^.pos) $
           vector .~ (r2 `scale` k) <*$> (+) <$*> (q^.vector) $ 
           color .~ (128, 255, 0) $ q)
  Nothing -> Nothing
  
  where
    r1 = (fromIntegral $ q^.size)/(fromIntegral $ p^.size + q^.size)
    r2 = r1-1
    
    c :: (Float, Float)
    c = let sepV = (q ^. pos) <*$> (-) <$*> (p ^. pos) 
        in mapPair (/(norm sepV + 0.5)) sepV
    
    k :: (Float, Float)
    k = let sepV = (q ^. vector) <*$> (-) <$*> (p ^. vector)
        in ((1+ballE)*(sepV |*| c)) `scale` c

getAccessPos :: Particle -> (Int, Int)
getAccessPos = getSpacePos . getEdgePos
  where
    getSpacePos :: ((Int, Int), (Int, Int)) -> (Int, Int)
    getSpacePos (x,y) = findSpace . uncurry Bits.xor $ (x',y')
      where
        (x',y') = mapPair (get2DMortonNumber . toSpaceNumber) (x,y)
    
        findSpace :: Int -> (Int, Int)
        findSpace morton = ((6-shiftNumber) `div` 2, x' `Bits.shiftR` shiftNumber)
          where
            shiftNumber = getGenerationShift morton
    
        toSpaceNumber :: (Int, Int) -> (Int, Int)
        toSpaceNumber = mapPair floor . (divByUnit winWidth *** divByUnit winHeight)
          where
            divByUnit :: Int -> Int -> Float
            divByUnit u t = let u' = fromIntegral u; 
                                t' = fromIntegral t in t'/(u'/8)

    getEdgePos :: Particle -> ((Int, Int), (Int, Int))
    getEdgePos p = let x = p ^. pos; 
                       v = p ^. vector;
                   in mapPair (mapPair floor) $ compareThenAddY $ compareThenAddX $ (,) x (x <*$> (+) <$*> v)
      where
        s = fromIntegral $ p ^. size
        
        compareThenAddX :: ((Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float))
        compareThenAddX ((a,b), (c,d)) = if a<=c then ((a-s,b),(c+s,d)) else ((a+s,b),(c-s,d))

        compareThenAddY :: ((Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float))
        compareThenAddY ((a,b), (c,d)) = if b<=d then ((a,b-s),(c,d+s)) else ((a,b+s),(c,d-s))

getIndex :: Particle -> Int
getIndex = tableIndex . getAccessPos
  where
    tableIndex :: (Int, Int) -> Int
    tableIndex (x,y) = (4^x - 1) `div` 3 + y

