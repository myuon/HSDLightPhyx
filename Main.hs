{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Lens

import qualified Data.Vector as Vector

import Util
import qualified Particle as P
import qualified AccessMap as M

import Debug.Trace

data World = World {
  _objects :: P.Particles,
  _tree :: M.AccessMap,
  _treeInit :: M.AccessMap
  }

$(makeLenses ''World)

initWorld :: World
initWorld = World {
  _objects = Vector.empty,
  _tree = Vector.fromList $ zipWith const (repeat Vector.empty) [1..(4^4-1)`div`3],
  _treeInit = Vector.fromList $ zipWith const (repeat Vector.empty) [1..(4^4-1)`div`3]
  }

mouseLeftPos :: SDL.Event -> Maybe (Int, Int)
mouseLeftPos (SDL.MouseButtonUp x y SDL.ButtonLeft) = Just (fromIntegral x, fromIntegral y)
mouseLeftPos _ = Nothing

gameinit :: IO World
gameinit = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode winWidth winHeight 32 []
     SDL.setCaption "HSDLightPhyx" ""
     
     return $ initWorld

clearDisplay :: SDL.Surface -> IO Bool
clearDisplay screen = do
  color <- makeColor screen 0 0 30
  SDL.fillRect screen (Just (SDL.Rect 0 0 winWidth winHeight)) color

mainLoop :: World -> IO ()
mainLoop world = do
  screen <- SDL.getVideoSurface
  event <- SDL.pollEvent
  
  clearDisplay screen
  drawWorld screen world
  
  SDL.flip screen
  SDL.delay 1
  
  world' <- addParticleByMouseEvent event world
  
  unless (isQuit event) $ mainLoop $ stepWorld world'
  
  where
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyUp key) = SDL.symKey key == SDL.SDLK_ESCAPE
    isQuit event = event == SDL.Quit

main :: IO ()
main = gameinit >>= mainLoop >> SDL.quit

drawWorld :: SDL.Surface -> World -> IO ()
drawWorld screen w = Vector.mapM_ (P.draw screen) (w ^. objects)

stepWorld :: World -> World
stepWorld = collide . regist . action

  where
    action :: World -> World
    action = objects %~ Vector.map (P.wall . P.move . P.putG constG)
    
    regist :: World -> World
    regist w = tree .~ registParticle (w ^. objects) (w ^. treeInit) $ w
    
    collide :: World -> World
    collide w = objects %~ collideParticles (M.mapCollisionPair (w ^. tree)) $ w

addParticleByMouseEvent :: SDL.Event -> World -> IO World
addParticleByMouseEvent event w = return $ case mouse of
  Just p -> objects .~ (w ^. objects) `Vector.snoc`
            (
              P.pos .~ mapPair fromIntegral p $ 
              P.index .~ Vector.length (w ^. objects) $ 
              P.init
            ) $ w
  Nothing -> w

  where
    mouse = mouseLeftPos event

registParticle :: P.Particles -> M.AccessMap -> M.AccessMap
registParticle p' amap
  | Vector.null p' = amap 
  | otherwise = registParticle ps (M.putElement (P.getIndex p) (p ^. P.index) amap)
  where
    (p,ps) = Vector.head &&& Vector.tail $ p'
    
collideParticles :: Vector.Vector (Int, Int) -> P.Particles -> P.Particles
collideParticles indexes ps = ps Vector.// (flatMaybeList $ newParticlePairs indexes ps)

newParticlePairs :: Vector.Vector (Int, Int) -> P.Particles -> [Maybe (Int, P.Particle)]
newParticlePairs pairs ps
  | Vector.null pairs = []
  | otherwise = pairMaybe a : pairMaybe b : newParticlePairs others ps
  where
    (pair,others) = Vector.head &&& Vector.tail $ pairs
    
    (a,b) = newParticle pair ps

newParticle :: (Int, Int) -> P.Particles -> ((Int, Maybe P.Particle), (Int, Maybe P.Particle))
newParticle (a,b) ps = let pa = ps Vector.! a; pb = ps Vector.! b
                       in ((a, pa `P.collideTo` pb),
                           (b, pb `P.collideTo` pa))

