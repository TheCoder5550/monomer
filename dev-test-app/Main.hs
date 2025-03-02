{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Monomer
import TextShow
import Data.Text (Text, replace, unpack, pack, intercalate)

data ProofLine = ProofLine {
  _indentLevel :: Int,
  _statement :: Text,
  _rule :: Text
} deriving (Eq, Show)

data AppModel = AppModel {
  _clickCount :: Int,
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
  | ExportProof
  deriving (Eq, Show)

makeLenses 'ProofLine
makeLenses 'AppModel

h1 :: Text -> WidgetNode s e
h1 t = label t `styleBasic` [ textSize 24, textFont "Bold" ]

iconButton iconIdent action = button iconIdent action
  `styleBasic` [textFont "Remix", textMiddle, textColor red, bgColor transparent, border 0 transparent]

trashButton action = iconButton remixDeleteBinLine action

type SymbolDict = [(Text, Text)]

symbolLookup :: SymbolDict
symbolLookup = [
  ("->", "→"),
  ("!", "¬"),
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

exportProof :: [ProofLine] -> Text
exportProof proof = exportProofHelper proof 0

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
  proofLineUI idx line = stack
    where
      stack = hstack [
        label_ (showt $ idx + 1) [ellipsis] `styleBasic` [textSize 12, paddingH 8, width 50],

        label $ pack $ replicate (line ^. indentLevel) '|',

        keystroke [("Enter", NextFocus 1)] $ textField (proofLines . singular (ix idx) . statement),
        spacer,

        keystroke [("Enter", if isLastLine then AddLine else NextFocus 2)] $ textField (proofLines . singular (ix idx) . rule) `styleBasic` [width 100],
        spacer,

        trashButton (RemoveLine idx),

        button "<-" (OutdentLine idx),
        button "->" (IndentLine idx)
        ]
          `nodeKey` showt idx
          `styleBasic` [paddingT 10]

      isLastLine = idx == length (model ^. proofLines) - 1

  widgetTree = vstack [
      h1 "Edit proof",
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

      vscroll $ vstack [
        vstack (zipWith proofLineUI [0..] (model ^. proofLines)),
        spacer,
        button "+ New line" AddLine `styleBasic` [ maxWidth 150 ]
      ],

      spacer,

      hstack [
        button "Export proof" ExportProof,
        spacer,
        widgetIf (isProofCorrect (exportProof $ model ^. proofLines)) (label "Proof is correct :)" `styleBasic` [textColor lime]),
        widgetIf (not $ isProofCorrect (exportProof $ model ^. proofLines)) (label "Proof is not correct!" `styleBasic` [textColor red])
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]

  -- Bruh
  NextFocus 1 -> [
      MoveFocusFromKey Nothing FocusFwd
    ]
  NextFocus 2 -> [
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

  ExportProof -> [
    Producer (\_ -> writeFile "./export.logic" (unpack (exportProof $ model ^. proofLines)))
    ]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Dev test app",
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