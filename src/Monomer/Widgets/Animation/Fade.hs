{-|
Module      : Monomer.Widgets.Animation.Fade
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Fade animation widget. Wraps a child widget whose content will be animated.

Messages:

- Accepts an 'AnimationMsg', used to control the state of the animation.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Animation.Fade (
  -- * Configuration
  FadeCfg,
  -- * Constructors
  animFadeIn,
  animFadeIn_,
  animFadeOut,
  animFadeOut_
) where

import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe

import Monomer.Widgets.Container
import Monomer.Widgets.Animation.Transform

import qualified Monomer.Lens as L

{-|
Configuration options for fade:

- 'autoStart': whether the first time the widget is added, animation should run.
- 'duration': how long the animation lasts in ms.
- 'onFinished': event to raise when animation is complete.
- 'onFinishedReq': 'WidgetRequest' to generate when animation is complete.
-}
newtype FadeCfg s e = FadeCfg {
  _fdcTransformCfg :: TransformCfg s e
} deriving (Eq, Show)

instance Default (FadeCfg s e) where
  def = FadeCfg {
    _fdcTransformCfg = def
  }

instance Semigroup (FadeCfg s e) where
  (<>) fc1 fc2 = FadeCfg {
    _fdcTransformCfg = _fdcTransformCfg fc1 <> _fdcTransformCfg fc2
  }

instance Monoid (FadeCfg s e) where
  mempty = def

instance CmbAutoStart (FadeCfg s e) where
  autoStart_ start = def {
    _fdcTransformCfg = autoStart_ start
  }

instance CmbDuration (FadeCfg s e) Millisecond where
  duration dur = def {
    _fdcTransformCfg = duration dur
  }

instance WidgetEvent e => CmbOnFinished (FadeCfg s e) e where
  onFinished handler = def {
    _fdcTransformCfg = onFinished handler
  }

instance CmbOnFinishedReq (FadeCfg s e) s e where
  onFinishedReq req = def {
    _fdcTransformCfg = onFinishedReq req
  }

-- | Animates a widget from not visible state to fully visible.
animFadeIn
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeIn managed = animFadeIn_ def managed

-- | Animates a widget from not visible state to fully visible. Accepts config.
animFadeIn_
  :: WidgetEvent e
  => [FadeCfg s e]   -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeIn_ configs managed = makeNode configs managed True
  & L.info . L.widgetType .~ "animFadeIn"

-- | Animates a widget from visible state to not visible.
animFadeOut
  :: WidgetEvent e
  => WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeOut managed = animFadeOut_ def managed

-- | Animates a widget from visible state to not visible. Accepts config.
animFadeOut_
  :: WidgetEvent e
  => [FadeCfg s e]   -- ^ The config options.
  -> WidgetNode s e  -- ^ The child node.
  -> WidgetNode s e  -- ^ The created animation container.
animFadeOut_ configs managed = makeNode configs managed False
  & L.info . L.widgetType .~ "animFadeOut"

makeNode
  :: WidgetEvent e
  => [FadeCfg s e]
  -> WidgetNode s e
  -> Bool
  -> WidgetNode s e
makeNode configs managed isFadeIn = node where
  node = animTransform_ [_fdcTransformCfg] f managed
  f t _ = [animGlobalAlpha $ alpha t]
  alpha t = if isFadeIn
    then (currStep t)
    else 1-(currStep t)
  currStep t = clampAlpha $ t/(fromIntegral dur)
  dur = fromMaybe 500 _tfcDuration
  TransformCfg{..} = _fdcTransformCfg
  FadeCfg{..} = mconcat configs
