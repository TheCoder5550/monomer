{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Directory
import Control.Concurrent (threadDelay)
import Control.Lens
import Monomer
import TextShow
import Data.Text (Text, replace, unpack, pack, intercalate)
import qualified Data.Maybe

data ProofLine = ProofLine {
  _indentLevel :: Int,
  _statement :: Text,
  _rule :: Text
} deriving (Eq, Show)

data File = File {
  _path :: FilePath,
  _name :: Text,
  _subname :: Text,
  _content :: Text
} deriving (Eq, Show)

data AppModel = AppModel {
  _clickCount :: Int,

  _newFilePopupOpen :: Bool,
  _newFileName :: Text,
  _loadedFiles :: [File],
  _openFiles :: [File],
  _currentFile :: Maybe File,

  _conclusion :: Text,
  _proofLines :: [ProofLine]
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | NextFocus Int
  | AddLine
  | RemoveLine Int
  | OutdentLine Int
  | IndentLine Int
  | SetLoadedFiles [File]
  | OpenFile File
  | CloseFile File
  | SetCurrentFile File
  | OpenCreateProofPopup
  | CreateEmptyProof Text
  deriving (Eq, Show)

makeLenses 'ProofLine
makeLenses 'File
makeLenses 'AppModel

h1 :: Text -> WidgetNode s e
h1 t = label t `styleBasic` [ textSize 24, textFont "Bold" ]

iconButton iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textColor orangeRed, bgColor transparent, border 0 transparent]

trashButton action = iconButton remixDeleteBinFill action

type SymbolDict = [(Text, Text)]

symbolLookup :: SymbolDict
symbolLookup = [
  ("->", "→"),
  ("!", "¬"),
  ("-", "¬"),
  ("&&", "∧"),
  ("||", "∨"),
  ("bot", "⊥"),
  ("forall", "∀"),
  ("exists", "∃"),
  ("|-/", "⊬"),
  ("|-", "⊢")
  ]

replaceFromLookup :: Text -> SymbolDict -> Text
replaceFromLookup s [] = s
replaceFromLookup s ((key, value):ls) = replace key value $ replaceFromLookup s ls

replaceSpecialSymbols :: Text -> Text
replaceSpecialSymbols s = replaceFromLookup s symbolLookup

exportProof :: AppModel -> Text
exportProof model = "conclusion: " <> model ^. conclusion <> "\n" <> exportProofHelper (model ^. proofLines) 0

tabs :: Int -> Text
tabs n = pack $ replicate n '\t'

exportProofHelper :: [ProofLine] -> Int -> Text
exportProofHelper [] lastIndex = intercalate "" (map (\n -> tabs n <> "}\n") (reverse [0..lastIndex-1]))
exportProofHelper (line:rest) lastIndex
  | ind > lastIndex = intercalate "" (map (\n -> tabs n <> "{\n") [lastIndex..ind-1]) <> l
  | ind < lastIndex = intercalate "" (map (\n -> tabs n <> "}\n") (reverse [ind..lastIndex-1])) <> l
  | otherwise = l
  where
    l = tabs ind <> _statement line <> " : " <> _rule line <> ";\n" <> exportProofHelper rest ind
    ind = _indentLevel line

isProofCorrect :: Text -> Bool
isProofCorrect p = True

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = hstack [
      fileWindow,
      editWindow
    ]

  fileWindow = vstack [
      box (label "Manage proofs") `styleBasic` [padding 10],
      vstack $ map fileItem (model ^. loadedFiles),
      spacer,

      box $ button "+ New proof" OpenCreateProofPopup `styleBasic` [padding 10],
      popup newFilePopupOpen (vstack [
        label "This will appear on top of the widget tree",
        spacer,
        textField newFileName,
        spacer,
        let cep = (CreateEmptyProof $ model ^. newFileName) in
          keystroke [("Enter", cep)] $ button "Create proof" cep
      ] `styleBasic` [bgColor gray, padding 10])
    ] `styleBasic` [ width 250, borderR 1 gray ]

  fileItem file = box_ [expandContent, onClick (OpenFile file)] $ vstack [
      label $ _name file,
      label $ _subname file
    ] `styleBasic` [borderB 1 gray, padding 8, bgColor darkGray, cursorHand]

  fileNavBar files = hscroll (hstack (map boxedLabel files))
    `styleBasic` [bgColor black, maxHeight 50, minHeight 50]
    where
      boxedLabel f = hstack [
          button (_name f) (SetCurrentFile f) `styleBasic` [textColor white, bgColor transparent, paddingV 8, paddingH 16, radius 0, border 0 transparent],
          button "x" (CloseFile f) `styleBasic` [textColor white, bgColor transparent, radius 0, border 0 transparent]
        ] `styleBasic` [bgColor darkGray, border 1 gray, styleIf isCurrent (bgColor darkSlateGray), styleIf isCurrent (borderB 0 transparent)]
          where isCurrent = (model ^. currentFile) == Just f

  editWindow = vstack [
      fileNavBar (model ^. openFiles),
      widgetIf (Data.Maybe.isJust (model ^. currentFile)) (proofWindow $ model ^. currentFile)
    ]

  proofWindow Nothing = label "No proof selected"
  proofWindow (Just file) = vstack [
      h1 $ _name file,
      label $ _subname file,
      spacer,
      -- label "→ ¬ ∧ ∨ ⊕ ⊥ ∀ ∃ ⊢ ⊬ ⟛",
      -- label $ replaceSpecialSymbols "P -> Q && L",
      -- vscroll $ label_ (exportProof $ model ^. proofLines) [multiline],
      -- label "Hello world will you update? 99 :))))",
      -- spacer,
      -- hstack [
      --   label $ "Click count: " <> showt (model ^. clickCount),
      --   spacer,
      --   button "Increase count" AppIncrease
      -- ],
      -- spacer,

      -- vstack (map myLabel labelContents),
      -- spacer,

      -- label_ "Save plz\nTest ruh\nbruh\nwhriahh\ttest" [ multiline ],
      -- spacer,

      -- textField (fieldInputs . singular (ix 0)),
      -- textField (fieldInputs . singular (ix 1)),

      hstack [
        label "Conclusion",
        spacer,
        textField conclusion
      ] `styleBasic` [paddingV 8],
      spacer,

      vscroll $ vstack [
        vstack (zipWith proofLineUI [0..] (model ^. proofLines)),
        spacer,
        button "+ New line" AddLine `styleBasic` [ maxWidth 150 ]
      ],

      spacer,

      hstack [
        widgetIf (isProofCorrect (exportProof model)) (label "Proof is correct :)" `styleBasic` [textColor lime]),
        widgetIf (not $ isProofCorrect (exportProof model)) (label "Proof is not correct!" `styleBasic` [textColor pink])
      ]
    ] `styleBasic` [padding 10]

  proofLineUI idx line = stack
    where
      stack = hstack [
        label_ (showt $ idx + 1) [ellipsis] `styleBasic` [textSize 12, paddingH 8, width 50],

        label $ pack $ take (2 * (line ^. indentLevel)) (cycle "|\t"),

        keystroke [("Enter", NextFocus 1)] $ textField (proofLines . singular (ix idx) . statement),
        spacer,

        keystroke (if isLastLine then [("Enter", AddLine), ("Enter", NextFocus 4)] else [("Enter", NextFocus 4)]) $ textField (proofLines . singular (ix idx) . rule) `styleBasic` [width 175],
        spacer,

        trashButton (RemoveLine idx),

        button "<-" (OutdentLine idx),
        spacer,
        button "->" (IndentLine idx)
        ]
          `nodeKey` showt idx
          `styleBasic` [paddingT 10]

      isLastLine = idx == length (model ^. proofLines) - 1

directoryFilesProducer :: (AppEvent -> IO ()) -> IO ()
directoryFilesProducer sendMsg = do
  all <- listDirectory "./myProofs"
  sendMsg (SetLoadedFiles $ map packFile all)
  threadDelay $ 2 * seconds
  directoryFilesProducer sendMsg
    where
      seconds = 1000 * 1000
      packFile path = File path name subname content
        where
          name = pack path
          subname = "subname"
          content = "This is the content"


handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [
      Producer directoryFilesProducer
    ]
  AppIncrease -> [Model (model & clickCount +~ 1)]

  -- Bruh
  NextFocus 1 -> [
      MoveFocusFromKey Nothing FocusFwd
    ]
  NextFocus 4 -> [
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd,
      MoveFocusFromKey Nothing FocusFwd
    ]

  AddLine -> [
      Model $ model & proofLines .~ (model ^. proofLines ++ [newLine])
      -- SetFocusOnKey "Last"
      ]
    where
      newLine = ProofLine lastLineIndent "" ""
      lastLineIndent = model ^. proofLines . singular (ix lastIndex) . indentLevel
      lastIndex = length (model ^. proofLines) - 1

  RemoveLine idx -> [Model $ model
    & proofLines .~ removeIdx idx (model ^. proofLines)]

  OutdentLine idx -> [Model $ model & proofLines . singular (ix idx) . indentLevel .~  max 0 (currentIndent - 1)]
    where currentIndent = model ^. proofLines . singular (ix idx) . indentLevel
  IndentLine idx -> [Model $ model & proofLines . singular (ix idx) . indentLevel .~ currentIndent + 1]
    where currentIndent = model ^. proofLines . singular (ix idx) . indentLevel

  OpenCreateProofPopup -> [
      Model $ model & newFilePopupOpen .~ True
    ]

  CreateEmptyProof fileName -> [
      Producer (\_ -> writeFile ("./myProofs/" <> unpack fileName <> ".logic") ""),
      Model $ model
        & newFilePopupOpen .~ False
        & newFileName .~ ""
    ]

  SetLoadedFiles fs -> [
      Model $ model & loadedFiles .~ fs
    ]

  OpenFile f -> [
      Model $ model
        & openFiles .~ (model ^. openFiles ++ [f | f `notElem` model ^. openFiles])
        & (currentFile ?~ f)
    ]

  CloseFile f -> [
      Model $ model
        & openFiles .~ filter (f/=) (model ^. openFiles)
        & currentFile .~ (if c == Just f then maybeHead (model ^. openFiles) else c)
    ]
    where c = model ^. currentFile

  SetCurrentFile f -> [ Model $ model & (currentFile ?~ f) ]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "● proof.logic - Proof Editor",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      -- appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      -- appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      -- appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Regular" "./assets/fonts/MPLUS1p-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/MPLUS1p-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/MPLUS1p-Bold.ttf",
      appFontDef "Remix" "./assets/fonts/remixicon.ttf",
      appInitEvent AppInit,
      appModelFingerprint show
      ]
    model = AppModel {
      _clickCount = 0,
      _newFileName = "",
      _newFilePopupOpen = False,
      _loadedFiles = [],
      _openFiles = [],
      _currentFile = Nothing,
      _conclusion = "((P -> Q) && (!R -> !Q)) -> (P -> R)",
      _proofLines = [
        ProofLine 1 "(P -> Q) && (!R -> !Q)" "Assumption",
        ProofLine 2 "p" "Assumption",
        ProofLine 2 "(P -> Q) && (!R -> !Q)" "1, Reiteration",
        ProofLine 2 "P -> Q" "3, ||E",
        ProofLine 2 "Q" "2, 4, ->E",
        ProofLine 2 "!R -> !Q" "3, &&E",
        ProofLine 3 "!R" "Assumption",
        ProofLine 3 "!R -> !Q" "6, Reiteration",
        ProofLine 3 "!Q" "7, 8, ->E",
        ProofLine 3 "Q" "5, Reiteration",
        ProofLine 2 "!!R" "7-10, !I",
        ProofLine 2 "R" "11, !!E",
        ProofLine 1 "P -> R" "2-12, ->I",
        ProofLine 0 "((P -> Q) && (!R -> !Q)) -> (P -> R)" "1-13, ->I"
      ]
    }

removeIdx :: Int -> [a] -> [a]
removeIdx idx lst = part1 ++ drop 1 part2 where
  (part1, part2) = splitAt idx lst

-- https://www.youtube.com/watch?v=aS8O-F0ICxw
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (h:t) = Just h