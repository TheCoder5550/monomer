{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.CompositeSpec (spec) where

import Debug.Trace
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestEventUtil
import Monomer.TestUtil
import Monomer.Widgets.Button
import Monomer.Widgets.Composite
import Monomer.Widgets.Label
import Monomer.Widgets.TextField
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

data MainEvt
  = MainBtnClicked
  | ChildClicked
  deriving (Eq, Show)

data ChildEvt
  = ChildBtnClicked
  deriving (Eq, Show)

data MainModel = MainModel {
  _tmClicks :: Int,
  _tmChild :: ChildModel
} deriving (Eq, Show)

instance Default MainModel where
  def = MainModel {
    _tmClicks = 0,
    _tmChild = def
  }

newtype ChildModel = ChildModel {
  _cmClicks :: Int
} deriving (Eq, Show)

instance Default ChildModel where
  def = ChildModel {
    _cmClicks = 0
  }

data TestModel = TestModel {
  _tmText1 :: Text,
  _tmText2 :: Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''MainModel
makeLensesWith abbreviatedFields ''ChildModel
makeLensesWith abbreviatedFields ''TestModel

spec :: Spec
spec = describe "Composite" $ do
  handleEvent
  updateSizeReq
  resize

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
  handleEventBasic
  handleEventChild
  handleEventLocalKey
  handleEventGlobalKey

handleEventBasic :: Spec
handleEventBasic = describe "handleEventBasic" $ do
  it "should not generate an event if clicked outside" $
    model [evtClick (Point 3000 3000)] ^. clicks `shouldBe` 0

  it "should generate a user provided event when clicked" $
    model [evtClick (Point 10 10)] ^. clicks `shouldBe` 1

  where
    wenv = mockWenv def
    handleEvent :: MainModel -> MainEvt -> [EventResponse MainModel MainEvt ()]
    handleEvent model evt = [Model (model & clicks %~ (+1))]
    buildUI wenv model = button "Click" MainBtnClicked
    cmpNode = composite "main" id Nothing buildUI handleEvent
    model es = nodeHandleEventCtxModel wenv es cmpNode

handleEventChild :: Spec
handleEventChild = describe "handleEventChild" $ do
  it "should not generate an event if clicked outside" $ do
    model [evtClick (Point 3000 3000)] ^. clicks `shouldBe` 0
    model [evtClick (Point 3000 3000)] ^. child . clicks `shouldBe` 0

  it "should generate a main event when clicked in main button" $ do
    model [evtClick (Point 10 10)] ^. clicks `shouldBe` 1
    model [evtClick (Point 10 10)] ^. child . clicks `shouldBe` 0

  it "should generate a child event when clicked in child button" $ do
    model [evtClick (Point 10 30)] ^. clicks `shouldBe` 0
    model [evtClick (Point 10 30)] ^. child . clicks `shouldBe` 1

  where
    wenv = mockWenv def
    handleChild :: ChildModel -> ChildEvt -> [EventResponse ChildModel ChildEvt MainEvt]
    handleChild model evt = [Model (model & clicks %~ (+1))]
    buildChild wenv model = button "Click" ChildBtnClicked
    handleEvent :: MainModel -> MainEvt -> [EventResponse MainModel MainEvt ()]
    handleEvent model evt = [Model (model & clicks %~ (+1))]
    buildUI wenv model = vstack [
        button "Click" MainBtnClicked,
        composite "child" child Nothing buildChild handleChild
      ]
    cmpNode = composite "main" id Nothing buildUI handleEvent
    model es = nodeHandleEventCtxModel wenv es cmpNode

handleEventLocalKey :: Spec
handleEventLocalKey = describe "handleEventLocalKey" $
  it "should insert new text at the beginning, since its merged with a local key" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "bbaacc"
    modelM ^. text2 `shouldBe` ""

  where
    wenv = mockWenv (TestModel "" "")
    handleEvent :: TestModel -> () -> [EventResponse TestModel () ()]
    handleEvent model evt = []
    buildUI1 wenv model = hstack [
        vstack [
          textField text1 `key` "localTxt1"
        ],
        vstack [
          textField text1 `key` "localTxt2"
        ]
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text1 `key` "localTxt2"
        ],
        vstack [
          textField text1 `key` "localTxt1"
        ]
      ]
    cmpNode1 = composite "main" id Nothing buildUI1 handleEvent
    cmpNode2 = composite_ "main" id Nothing buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cmpNode1
    (wenv1, _, oldRoot1) = fst $ nodeHandleEvents wenv evts1 cmpNode1
    cntResM = widgetMerge (cmpNode2 ^. L.widget) wenv1 oldRoot1 cmpNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    modelM = nodeHandleEventModelNoInit wenv1 evts2 (cntResM ^. L.node)

handleEventGlobalKey :: Spec
handleEventGlobalKey = describe "handleEventGlobalKey" $
  it "should insert new text at the correct location, since its merged with a global key" $ do
    model1 ^. text1 `shouldBe` "aacc"
    model1 ^. text2 `shouldBe` ""
    modelM ^. text1 `shouldBe` "aabbcc"
    modelM ^. text2 `shouldBe` ""

  where
    wenv = mockWenv (TestModel "" "")
    handleEvent :: TestModel -> () -> [EventResponse TestModel () ()]
    handleEvent model evt = []
    buildUI1 wenv model = hstack [
        vstack [
          textField text1 `globalKey` "globalTxt1"
        ],
        vstack [
          textField text1 `globalKey` "globalTxt2"
        ]
      ]
    buildUI2 wenv model = hstack [
        vstack [
          textField text1 `globalKey` "globalTxt2"
        ],
        vstack [
          textField text1 `globalKey` "globalTxt1"
        ]
      ]
    cmpNode1 = composite "main" id Nothing buildUI1 handleEvent
    cmpNode2 = composite_ "main" id Nothing buildUI2 handleEvent [mergeRequired (\_ _ -> True)]
    evts1 = [evtK keyTab, evtT "aacc", moveCharL, moveCharL]
    model1 = nodeHandleEventModel wenv evts1 cmpNode1
    (wenv1, _, oldRoot1) = fst $ nodeHandleEvents wenv evts1 cmpNode1
    cntResM = widgetMerge (cmpNode2 ^. L.widget) wenv1 oldRoot1 cmpNode2
    evts2 = [evtK keyTab, evtK keyTab, evtT "bb"]
    modelM = nodeHandleEventModelNoInit wenv1 evts2 (cntResM ^. L.node)

updateSizeReq :: Spec
updateSizeReq = describe "updateSizeReq" $ do
  it "should return width = Flex 70 0.01" $
    sizeReqW `shouldBe` FlexSize 70 0.01

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` FixedSize 40

  where
    wenv = mockWenv ()
    handleEvent model evt = []
    buildUI :: WidgetEnv () () -> () -> WidgetNode () ()
    buildUI wenv model = vstack [
        label "label 1",
        label "label 2"
      ]
    cmpNode = composite "main" id Nothing buildUI handleEvent
    (sizeReqW, sizeReqH) = nodeUpdateSizeReq wenv cmpNode

resize :: Spec
resize = describe "resize" $ do
  it "should have the provided viewport size" $
    --viewport `shouldBe` vp
    pendingWith "Instance tree data not yet implemented"

  it "should assign the same viewport size to its child" $
    --childrenVp `shouldBe` Seq.singleton cvp1
    pendingWith "Instance tree data not yet implemented"

  it "should assign the same renderArea size to its child" $
    --childrenRa `shouldBe` Seq.singleton cvp1
    pendingWith "Instance tree data not yet implemented"

  where
    wenv = mockWenv () & L.windowSize .~ Size 640 480
    vp   = Rect 0 0 640 480
    cvp1 = Rect 0 0 640 480
    handleEvent model evt = []
    buildUI :: WidgetEnv () () -> () -> WidgetNode () ()
    buildUI wenv model = hstack []
    cmpNode = composite "main" id Nothing buildUI handleEvent
    newNode = nodeInit wenv cmpNode
    viewport = newNode ^. L.info . L.viewport
    childrenVp = (^. L.info . L.viewport) <$> newNode ^. L.children
    childrenRa = (^. L.info . L.renderArea) <$> newNode ^. L.children
