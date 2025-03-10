{-|
Module      : Monomer.Widgets.Animation.TransformSpec
Copyright   : (c) 2023 Ruslan Gadeev, Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Unit tests for Transform animation.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monomer.Widgets.Animation.TransformSpec (spec) where

import Control.Lens ((&), (^.), (.~), (?~), (^?!), _1, _3, ix)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Animation.Transform
import Monomer.Widgets.Animation.Types
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data TestEvt
  = OnTestFinished
  deriving (Eq, Show)

spec :: Spec
spec = describe "Transform" $ do
  initWidget
  handleMessage
  getSizeReq

initWidget :: Spec
initWidget = describe "initWidget" $ do
  it "should not request rendering if autoStart = False" $
    reqs nodeNormal `shouldBe` Seq.empty

  it "should request rendering if autoStart = True" $ do
    reqs nodeAuto ^?! ix 0 `shouldSatisfy` isRunTask
    reqs nodeAuto ^?! ix 1 `shouldSatisfy` isRenderEvery

  where
    wenv = mockWenvEvtUnit ()
    f _ _ = []
    nodeNormal = animTransform f (label "Test")
    nodeAuto = animTransform_ [autoStart, duration 100] f (label "Test")
    reqs node = nodeHandleEvents_ wenv WInit [] node ^?! ix 0 . _1 . _3

handleMessage :: Spec
handleMessage = describe "handleMessage" $ do
  it "should not request rendering if an invalid message is received" $
    reqs ScrollReset `shouldBe` Seq.empty

  it "should request rendering if AnimationStart is received" $ do
    reqs AnimationStart ^?! ix 0 `shouldSatisfy` isRunTask
    reqs AnimationStart ^?! ix 1 `shouldSatisfy` isRenderEvery
    evts AnimationStart `shouldBe` Seq.empty

  it "should cancel rendering if AnimationStop is received" $ do
    reqs AnimationStop ^?! ix 0 `shouldSatisfy` isRenderStop
    evts AnimationStop `shouldBe` Seq.empty

  it "should generate an event if AnimationFinished is received" $
    evts (AnimationFinished 0) `shouldBe` Seq.singleton OnTestFinished

  where
    wenv = mockWenv ()
    f _ _ = []
    baseNode = animTransform_ [autoStart, duration 100, onFinished OnTestFinished] f (label "Test")
    node = nodeInit wenv baseNode
    res msg = widgetHandleMessage (node^. L.widget) wenv node rootPath msg
    evts msg = eventsFromReqs (reqs msg)
    reqs msg = maybe Seq.empty (^. L.requests) (res msg)

getSizeReq :: Spec
getSizeReq = describe "getSizeReq" $ do
  it "should return same reqW as child node" $
    tSizeReqW `shouldBe` lSizeReqW

  it "should return same reqH as child node" $
    tSizeReqH `shouldBe` lSizeReqH

  where
    wenv = mockWenvEvtUnit ()
    lblNode = label "Test label"
    f _ _ = []
    (lSizeReqW, lSizeReqH) = nodeGetSizeReq wenv lblNode
    (tSizeReqW, tSizeReqH) = nodeGetSizeReq wenv (animTransform f lblNode)
