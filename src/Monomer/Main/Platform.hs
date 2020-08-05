module Monomer.Main.Platform where

import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Foreign (free)
import Foreign.C (peekCString, withCString)
import NanoVG (Context(..), beginFrame, endFrame)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified SDL.Input.Mouse as Mouse
import qualified SDL.Raw as Raw

import Monomer.Common.Geometry
import Monomer.Event.Types

getCurrentMousePos :: (MonadIO m) => m Point
getCurrentMousePos = do
  SDL.P (SDL.V2 x y) <- Mouse.getAbsoluteMouseLocation
  return $ Point (fromIntegral x) (fromIntegral y)

getDrawableSize :: (MonadIO m) => SDL.Window -> m Size
getDrawableSize window = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  return $ Size (fromIntegral fbWidth) (fromIntegral fbHeight)

getWindowSize :: (MonadIO m) => SDL.Window -> Double -> m Size
getWindowSize window dpr = do
  Size rw rh <- getDrawableSize window

  return $ Size (rw / dpr) (rh / dpr)

getPlatform :: (MonadIO m) => m Text
getPlatform = do
  platform <- liftIO . peekCString =<< Raw.getPlatform

  return $ T.pack platform

getKeyCode :: String -> Maybe KeyCode
getKeyCode name = unsafePerformIO $ withCString name getKeyCodeFFI where
  getKeyCodeFFI cname = fmap convert (Raw.getKeyFromName cname)
  convert Raw.SDLK_UNKNOWN = Nothing
  convert code = Just (KeyCode $ fromIntegral code)

doInDrawingContext :: (MonadIO m) => SDL.Window -> Context -> m s -> m s
doInDrawingContext window c action = do
  SDL.V2 fbWidth fbHeight <- SDL.glGetDrawableSize window
  let pxRatio = fromIntegral fbWidth / fromIntegral fbHeight

  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ beginFrame c fbWidth fbHeight pxRatio

  ret <- action

  liftIO $ endFrame c
  SDL.glSwapWindow window
  return ret
