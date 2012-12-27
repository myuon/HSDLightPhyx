{-# LANGUAGE TemplateHaskell #-}
import qualified Graphics.UI.SDL as SDL

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Lens

import qualified Data.Vector as Vector

import Util
import qualified Particle as P
import qualified AccessMap as M

data World = World {
  _objects :: Vector.Vector P.Particle
  }

$(makeLenses ''World)

mouseLeftPos :: SDL.Event -> Maybe (Int, Int)
mouseLeftPos (SDL.MouseButtonUp x y SDL.ButtonLeft) = Just (fromIntegral x, fromIntegral y)
mouseLeftPos _ = Nothing

gameinit :: IO World
gameinit = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode win_width win_height 32 []
     SDL.setCaption "HSDLightPhyx" ""
     
     return $ objects .~ Vector.empty $ World {}

clearDisplay :: SDL.Surface -> IO Bool
clearDisplay screen = do
  color <- makeColor screen 0 0 30
  SDL.fillRect screen (Just (SDL.Rect 0 0 win_width win_height)) color

mainLoop :: World -> IO ()
mainLoop world = do
  screen <- SDL.getVideoSurface
  event <- SDL.pollEvent
  
  clearDisplay screen
  drawWorld screen world
  
  SDL.flip screen
  SDL.delay 1
  
  world' <- addParticleByMouseEvent event world
  
  if isQuit event then return () else mainLoop $ stepWorld world'
  
  where
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyUp key) = SDL.symKey key == SDL.SDLK_ESCAPE
    isQuit event = event == SDL.Quit

main :: IO ()
main = gameinit >>= mainLoop >> SDL.quit

drawWorld :: SDL.Surface -> World -> IO ()
drawWorld screen w = Vector.mapM_ (P.draw screen) (w ^. objects)

stepWorld :: World -> World
stepWorld w = objects %~ Vector.filter P.isOutside . 
              Vector.map (P.move . (P.putG 0.005)) $ w

addParticleByMouseEvent :: SDL.Event -> World -> IO World
addParticleByMouseEvent event w = return $ case mouse of 
  Just p -> objects .~ (w ^. objects) `Vector.snoc`
            (P.pos .~ p $
             P.vector .~ (0,0) $
             P.color .~ (255,255,255) $
             P.size .~ 10 $
             P.index .~ 0 $ P.Particle {}) $ w
  Nothing -> w
  
  where
    mouse = mouseLeftPos event
