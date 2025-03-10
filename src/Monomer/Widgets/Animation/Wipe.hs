{-|
Module      : Monomer.Widgets.Animation.Wipe
Copyright   : (c) 2023 Ruslan Gadeev, Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Wipe animation widget. Wraps a child widget whose content will be animated.

Messages:

- Accepts a 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Animation.Wipe (
  -- * Configuration
  WipeCfg,
  wipeLeft,
  wipeRight,
  wipeTop,
  wipeBottom,
  wipeDoorH,
  wipeDoorV,
  wipeRect,
  -- * Constructors
  animWipeIn,
  animWipeIn_,
  animWipeOut,
  animWipeOut_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe

import Monomer.Helper
import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Transform

import qualified Monomer.Lens as L

data WipeDirection
  = WipeLeft
  | WipeRight
  | WipeTop
  | WipeBottom
  | WipeDoorH
  | WipeDoorV
  | WipeRect
  deriving (Eq, Show)

{-|
Configuration options for wipe:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
- 'onFinishedReq': 'WidgetRequest' to generate when animation is complete.
- Individual combinators for direction.
-}
data WipeCfg s e = WipeCfg {
  _wpcDirection :: Maybe WipeDirection,
  _wpcTransformCfg :: TransformCfg s e
} deriving (Eq, Show)

instance Default (WipeCfg s e) where
  def = WipeCfg {
    _wpcDirection = Nothing,
    _wpcTransformCfg = def
  }

instance Semigroup (WipeCfg s e) where
  (<>) wc1 wc2 = WipeCfg {
    _wpcDirection = _wpcDirection wc2 <|> _wpcDirection wc1,
    _wpcTransformCfg = _wpcTransformCfg wc1 <> _wpcTransformCfg wc2
  }

instance Monoid (WipeCfg s e) where
  mempty = def

instance CmbAutoStart (WipeCfg s e) where
  autoStart_ start = def {
    _wpcTransformCfg = autoStart_ start
  }

instance CmbDuration (WipeCfg s e) Millisecond where
  duration dur = def {
    _wpcTransformCfg = duration dur
  }

instance WidgetEvent e => CmbOnFinished (WipeCfg s e) e where
  onFinished handler = def {
    _wpcTransformCfg = onFinished handler
  }

instance CmbOnFinishedReq (WipeCfg s e) s e where
  onFinishedReq req = def {
    _wpcTransformCfg = onFinishedReq req
  }

-- | Wipe from/to left.
wipeLeft :: WipeCfg s e
wipeLeft = def { _wpcDirection = Just WipeLeft }

-- | Wipe from/to right.
wipeRight :: WipeCfg s e
wipeRight = def { _wpcDirection = Just WipeRight }

-- | Wipe from/to top.
wipeTop :: WipeCfg s e
wipeTop = def { _wpcDirection = Just WipeTop }

-- | Wipe from/to bottom.
wipeBottom :: WipeCfg s e
wipeBottom = def { _wpcDirection = Just WipeBottom }

-- | Wipe horizontally in a door shape.
wipeDoorH :: WipeCfg s e
wipeDoorH = def { _wpcDirection = Just WipeDoorH }

-- | Wipe vertically in a door shape.
wipeDoorV :: WipeCfg s e
wipeDoorV = def { _wpcDirection = Just WipeDoorV }

-- | Wipe in a rectangle shape.
wipeRect :: WipeCfg s e
wipeRect = def { _wpcDirection = Just WipeRect }

-- | Animates a widget from the left to fully visible.
animWipeIn
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animWipeIn managed = animWipeIn_ def managed

-- | Animates a widget from the provided direction to fully visible (defaults
--   to left). Accepts config.
animWipeIn_
  :: WidgetEvent e
  => [WipeCfg s e]   -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animWipeIn_ configs managed = makeNode configs managed True
  & L.info . L.widgetType .~ "animWipeIn"

-- | Animates a widget to the left from visible to not visible.
animWipeOut
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animWipeOut managed = animWipeOut_ def managed

-- | Animates a widget to the provided direction from visible to not
--   visible (defaults to left). Accepts config.
animWipeOut_
  :: WidgetEvent e
  => [WipeCfg s e]   -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animWipeOut_ configs managed = makeNode configs managed False
  & L.info . L.widgetType .~ "animWipeOut"

makeNode
  :: WidgetEvent e
  => [WipeCfg s e]
  -> WidgetNode s e
  -> Bool
  -> WidgetNode s e
makeNode configs managed isWipeIn = node where
  node = animTransform_ [_wpcTransformCfg] f managed
  f t (Rect x y w h) = [animScissor vp] where
    vp = case dir of
      WipeLeft -> Rect x y dw h
      WipeRight -> Rect (x+(1-(step t))*w) y dw h
      WipeTop -> Rect x y w dh
      WipeBottom -> Rect x (y+(1-(step t))*h) w dh
      WipeDoorH -> Rect dx y dw h
      WipeDoorV -> Rect x dy w dh
      WipeRect -> Rect dx dy dw dh
    (dx, dy) = (x+(1-(step t))*w/2, y+(1-(step t))*h/2)
    (dw, dh) = ((step t)*w, (step t)*h)
  step t = if isWipeIn
    then fwdStep t
    else 1-(fwdStep t)
  fwdStep t = clamp 0 1 $ t/(fromIntegral dur)
  dir = fromMaybe WipeLeft _wpcDirection
  dur = fromMaybe 500 _tfcDuration
  TransformCfg{..} = _wpcTransformCfg
  WipeCfg{..} = mconcat configs
