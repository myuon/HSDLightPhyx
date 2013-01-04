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

mouseLeftPos :: SDL.Event -> Maybe (Int, Int)
mouseLeftPos (SDL.MouseButtonUp x y SDL.ButtonLeft) = Just (fromIntegral x, fromIntegral y)
mouseLeftPos _ = Nothing

clearDisplay :: SDL.Surface -> IO Bool
clearDisplay screen = do
  color <- makeColor screen 0 0 30
  SDL.fillRect screen (Just (SDL.Rect 0 0 winWidth winHeight)) color

data World = World {
  _objects :: M.Particles,
  _tree :: M.AccessMap,
  _treeInit :: M.AccessMap
  }

$(makeLenses ''World)

initWorld :: World
initWorld = World {
  _objects = Vector.empty,
  _tree = Vector.fromList $ zipWith const (repeat []) [1..(4^4-1)`div`3],
  _treeInit = Vector.fromList $ zipWith const (repeat []) [1..(4^4-1)`div`3]
  }

gameinit :: IO World
gameinit = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode winWidth winHeight 32 []
     SDL.setCaption "HSDLightPhyx" ""
     
     return $ initWorld

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

drawWorld :: SDL.Surface -> World -> IO ()
drawWorld screen w = Vector.mapM_ (P.draw screen) (w ^. objects)

stepWorld :: World -> World
stepWorld = collide . register . move
  where
    move :: World -> World
    move = objects %~ Vector.map (P.wall . P.move . P.putG constG)
    
    register :: World -> World
    register w = tree .~ M.register (w ^. objects) (w ^. treeInit) $ w
    
    collide :: World -> World
    collide w = objects %~ M.collideMap (M.mapCollisionPair (w ^. tree)) $ w

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

main :: IO ()
main = gameinit >>= mainLoop >> SDL.quit
