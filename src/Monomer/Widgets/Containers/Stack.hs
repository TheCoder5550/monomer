{-|
Module      : Monomer.Widgets.Containers.Stack
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Container that stacks its children along a main axis.

An hstack widget will assign horizontal space to its children according to their
size requests. The inverse happens with vstack and vertical space, which assigns
vertical space as requested and all the horizontal space available.

For example, a label will get enough space to be displayed completely, and it
will also get all the vertical space the hstack has. This means that if the
hstack is the top level widget in the window, the label will also be as tall as
the window.

Both can be combined to create complex layouts. Considering the situation of a
top-level hstack which created a large vertical label, we could wrap the hstack
with a vstack to only use as much horizontal and vertical space as needed. Both
stack widgets will request space from their parent, along their corresponding
axis, based on the requests of their children.

The layout algorithm considers the different type of size requirements and
assigns space according to the logic defined in 'SizeReq'. If the requested
fixed space is larger that the viewport of the stack, the content will overflow.

@
vstack_ [childSpacing] [
    label "Selected image",
    image "assets/large-image.jpg"
      \`styleBasic\` [maxHeight 400],
    button \"Complete\" CompleteAction
  ]
@
-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.Stack (
  -- * Configuration
  StackCfg,
  -- * Constructors
  hstack,
  hstack_,
  vstack,
  vstack_,
  -- * Helpers
  assignStackAreas
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (%~))
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Helper (applyFnList)
import Monomer.Widgets.Container

import qualified Monomer.Lens as L

{-|
Configuration options for stack:

- 'childSpacing': spacing between the child widgets.
- 'ignoreEmptyArea': when the widgets do not use all the available space,
  ignoring the unassigned space allows for mouse events to pass through. This is
  useful in zstack layers.
- 'sizeReqUpdater': allows modifying the 'SizeReq' generated by the stack.
-}
data StackCfg = StackCfg {
  _stcChildSpacing :: Maybe Double,
  _stcIgnoreEmptyArea :: Maybe Bool,
  _stcSizeReqUpdater :: [SizeReqUpdater]
}

instance Default StackCfg where
  def = StackCfg {
    _stcChildSpacing = Nothing,
    _stcIgnoreEmptyArea = Nothing,
    _stcSizeReqUpdater = []
  }

instance Semigroup StackCfg where
  (<>) s1 s2 = StackCfg {
    _stcChildSpacing = _stcChildSpacing s2 <|> _stcChildSpacing s1,
    _stcIgnoreEmptyArea = _stcIgnoreEmptyArea s2 <|> _stcIgnoreEmptyArea s1,
    _stcSizeReqUpdater = _stcSizeReqUpdater s1 <> _stcSizeReqUpdater s2
  }

instance Monoid StackCfg where
  mempty = def

instance CmbChildSpacing StackCfg where
  childSpacing_ spacing = def {
    _stcChildSpacing = Just spacing
  }

instance CmbIgnoreEmptyArea StackCfg where
  ignoreEmptyArea_ ignore = def {
    _stcIgnoreEmptyArea = Just ignore
  }

instance CmbSizeReqUpdater StackCfg where
  sizeReqUpdater updater = def {
    _stcSizeReqUpdater = [updater]
  }

-- | Creates a horizontal stack.
hstack
  :: (Traversable t)
  => t (WidgetNode s e)  -- ^ The list of items.
  -> WidgetNode s e      -- ^ The created stack.
hstack children = hstack_ def children

-- | Creates a horizontal stack. Accepts config.
hstack_
  :: (Traversable t)
  => [StackCfg]          -- ^ The config options.
  -> t (WidgetNode s e)  -- ^ The list of items.
  -> WidgetNode s e      -- ^ The created stack.
hstack_ configs children = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "hstack" (makeStack True config)
    & L.children .~ foldl' (|>) Empty children

-- | Creates a vertical stack.
vstack
  :: (Traversable t)
  => t (WidgetNode s e)  -- ^ The list of items.
  -> WidgetNode s e      -- ^ The created stack.
vstack children = vstack_ def children

-- | Creates a vertical stack. Accepts config.
vstack_
  :: (Traversable t)
  => [StackCfg]          -- ^ The config options.
  -> t (WidgetNode s e)  -- ^ The list of items.
  -> WidgetNode s e      -- ^ The created stack.
vstack_ configs children = newNode where
  config = mconcat configs
  newNode = defaultWidgetNode "vstack" (makeStack False config)
    & L.children .~ foldl' (|>) Empty children

makeStack :: Bool -> StackCfg -> Widget s e
makeStack isHorizontal config = widget where
  widget = createContainer () def {
    containerIgnoreEmptyArea = ignoreEmptyArea,
    containerLayoutDirection = getLayoutDirection isHorizontal,
    containerUseCustomSize = True,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  isVertical = not isHorizontal
  ignoreEmptyArea = fromMaybe False (_stcIgnoreEmptyArea config)
  childSpacing = fromMaybe 0 (_stcChildSpacing config)

  getSizeReq wenv node children = newSizeReq where
    sizeReqFns = _stcSizeReqUpdater config
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    newSizeReqW = getDimSizeReq isHorizontal (_wniSizeReqW . _wnInfo) vchildren
    newSizeReqH = getDimSizeReq isVertical (_wniSizeReqH . _wnInfo) vchildren
    newSizeReq = applyFnList sizeReqFns (newSizeReqW, newSizeReqH)

  getDimSizeReq mainAxis accessor vchildren
    | Seq.null vreqs = fixedSize 0
    | mainAxis = foldl1 sizeReqMergeSum vreqs & L.fixed %~ (+ totalSpacing)
    | otherwise = foldl1 sizeReqMergeMax vreqs
    where
      vreqs = accessor <$> vchildren
      totalSpacing = fromIntegral (Seq.length vchildren - 1) * childSpacing

  resize wenv node viewport children = resized where
    style = currentStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    (newVps, newDim) = assignStackAreas isHorizontal contentArea childSpacing children
    newCa
      | isHorizontal = contentArea & L.w .~ newDim
      | otherwise = contentArea & L.h .~ newDim
    newNode = node
      & L.info . L.viewport .~ fromMaybe newCa (addOuterBounds style newCa)
    resized = (resultNode newNode, newVps)

{-|
Assigns space from rect to each of the provided widgets based on their size
requirements.
-}
assignStackAreas
  :: Bool                 -- ^ True if horizontal, False for vertical.
  -> Rect                 -- ^ The available space to assign.
  -> Double               -- ^ The spacing between adjacent children.
  -> Seq (WidgetNode s e) -- ^ The widgets that will be assigned space.
  -> (Seq Rect, Double)   -- ^ The assigned areas and used space in main axis.
assignStackAreas isHorizontal contentArea childSpacing children = result where
  Rect x y w h = contentArea
  mainSize = if isHorizontal then w else h
  mainStart = if isHorizontal then x else y
  rectSelector
    | isHorizontal = _rW
    | otherwise = _rH
  vchildren = Seq.filter (_wniVisible . _wnInfo) children
  reqs = fmap (mainReqSelector isHorizontal) vchildren

  sumSizes accum req = newStep where
    (cFixed, cFlex, cFlexFac, cExtraFac) = accum
    newFixed = cFixed + sizeReqFixed req
    newFlex = cFlex + sizeReqFlex req
    newFlexFac = cFlexFac + sizeReqFlex req * sizeReqFactor req
    newExtraFac = cExtraFac + sizeReqExtra req * sizeReqFactor req
    newStep = (newFixed, newFlex, newFlexFac, newExtraFac)

  visibleSpacings = max 0 (Seq.length vchildren - 1)
  totalSpacing = fromIntegral visibleSpacings * childSpacing

  (fixed, flex, flexFac, extraFac) = foldl' sumSizes def reqs
  flexAvail = min flex (mainSize - fixed - totalSpacing)
  extraAvail = max 0 (mainSize - fixed - totalSpacing - flexAvail)

  -- flexCoeff can only be negative
  flexCoeff
    | flexAvail < flex && flexFac > 0 = (flexAvail - flex) / flexFac
    | otherwise = 0
  extraCoeff
    | extraAvail > 0 && extraFac > 0 = extraAvail / extraFac
    | otherwise = 0

  foldHelper (accum, offset, vIndex) child = (newAccum, newOffset, newVIndex) where
    newRect = resizeChild isHorizontal contentArea flexCoeff extraCoeff offset child
    newAccum = accum |> newRect
    newOffset = offset + rectSelector newRect + spacing
    newVIndex = vIndex + if visible then 1 else 0
    spacing = if vIndex < Seq.length vchildren - 1 && visible then childSpacing else 0
    visible = _wniVisible (_wnInfo child)

  (areas, usedDim, _) = foldl' foldHelper (Seq.empty, mainStart, 0) children
  result = (areas, usedDim - mainStart)

resizeChild :: Bool -> Rect -> Factor -> Factor -> Double -> WidgetNode s e -> Rect
resizeChild horizontal contentArea flexCoeff extraCoeff offset child = result where
  Rect l t w h = contentArea
  emptyRect = Rect l t 0 0
  -- Either flex or extra is active (flex is negative or extra is >= 0)
  SizeReq fixed flex extra factor = mainReqSelector horizontal child

  tempMainSize = fixed
    + (1 + flexCoeff * factor) * flex
    + extraCoeff * factor * extra
  mainSize = max 0 tempMainSize

  hRect = Rect offset t mainSize h
  vRect = Rect l offset w mainSize
  result
    | not $ (_wniVisible . _wnInfo) child = emptyRect
    | horizontal = hRect
    | otherwise = vRect

mainReqSelector :: Bool -> WidgetNode s e -> SizeReq
mainReqSelector isHorizontal
  | isHorizontal = _wniSizeReqW . _wnInfo
  | otherwise = _wniSizeReqH . _wnInfo
