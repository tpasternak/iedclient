{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Tui where

import Data.Either(isRight)
import Control.Monad
import Iec61850.Client
import Iec61850.Mms
import Iec61850.Enums.FC
import System.Console.CmdArgs
import System.Directory
import System.IO
import Text.Read
import Text.Regex.TDFA(Regex,CompOption(..),ExecOption(..),matchTest)
import Text.Regex.TDFA.String(compile)
import Text.Regex (mkRegexWithOpts)
import Control.Lens
import Control.Lens.TH
import Brick.Types
import Brick
import Brick.Util
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Graphics.Vty
import Control.Concurrent
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Focus as F
import qualified Data.Map as DM
import qualified Brick.Types as T
import qualified System.Console.Terminal.Size as Size
import Data.Data (toConstr, Constr)
import Data.Bits ((.|.))

data Name = FilterEditor | ValueEditor | Viewport1 deriving (Eq, Show, Ord)

data Request = ReadRequest [(String,FunctionalConstraint)] |
               WriteRequest String FunctionalConstraint MmsVar

data AppState = AppState {
  _fields :: DM.Map (String,FunctionalConstraint) (Maybe MmsVar),
  _matchingFields :: [((String,FunctionalConstraint),Maybe MmsVar)],
  _selection :: Int,
  _filterReg :: String,
  _focusRing :: F.FocusRing Name,
  _editFilter :: Editor String Name,
  _validFilter :: Bool,
  _editValue :: Editor String Name,
  _mv :: MVar Request,
  _refreshing :: Bool,
  _start :: Int,
  _size :: Int,
  _message :: String
  }

makeLenses ''AppState

fieldsListV :: AppState -> Widget Name
fieldsListV m =
  border $ vLimit (m ^. size) $ vBox (vLimit 1 <$> visibleXs) <=> fill ' '
 where
  visibleXs     = take (m ^. size) $ drop (m ^. start) highlightedXs
  highlightedXs = over (element $ m ^. selection)
                       (visible . withAttr (attrName "blueBg"))
                       stringedXs
  stringedXs = map
    ( \((x, y), z) ->
      hLimit 50 (str x <+> fill ' ')
        <+> str (show y)
        <+> str "  "
        <+> str (showMaybe z)
        <+> fill ' '
    )
    (m ^. matchingFields)
  showMaybe = maybe "" show

blackOnWhite = withAttr (attrName "whiteBg") . withAttr (attrName "blackFg")

selectedField st = if st ^. selection < length (st ^. matchingFields)
  then Just ((st ^. matchingFields) !! (st ^. selection))
  else Nothing

selectedReference :: AppState -> Maybe String
selectedReference st = case selectedField st of
  Just f  -> Just $ f ^. _1 ^. _1
  Nothing -> Nothing


selectedFc :: AppState -> Maybe FunctionalConstraint
selectedFc st = case selectedField st of
  Just f  -> Just $ f ^. _1 ^. _2
  Nothing -> Nothing

selectedType st = do
  field <- selectedField st
  val   <- field ^. _2
  return $ toConstr val

selectedValue st = maybe "" (\f -> maybe "-" show (f ^. _2)) $ selectedField st

selectionWidget m = vBox
  [ vLimit 1 $ hLimit 20 (str "DA reference" <+> fill ' ') <+> str selectedId
  , vLimit 1 $ hLimit 20 (str "FC" <+> fill ' ') <+> str selectedFc_
  , vLimit 1 $ hLimit 20 (str "Type" <+> fill ' ') <+> str selectedType_
  , vLimit 1 $ hLimit 20 (str "Value" <+> fill ' ') <+> str selectedValue
  , vLimit 1 $ hLimit 20 (newValueLabel <+> fill ' ') <+> F.withFocusRing
    (m ^. focusRing)
    renderEditor
    (m ^. editValue)
  ]
 where
  currentSelectedField = selectedField m
  selectedId           = fromMaybe "" (selectedReference m)
  selectedFc_          = maybe "" show (selectedFc m)
  selectedType_        = maybe "" show (selectedType m)
  selectedValue =
    maybe "" (\f -> maybe "-" show (f ^. _2)) currentSelectedField
  newValueLabel = if isNothing $ writeRequest m
    then withAttr (attrName "redFg") (str "New Value")
    else str "New Value"

drawUI :: AppState -> [Widget Name]
drawUI m =
  [ (e <+> selectionW)
      <=> refreshingStatus
      <=> fieldsListV m
      <=> helpBar
      <=> messageBar
  ]
 where
  e = vLimit 1 $ hLimit 50 $ filterLabel <+> F.withFocusRing
    (m ^. focusRing)
    renderEditor
    (m ^. editFilter)
  filterLabel = if m ^. validFilter
    then str "Filter: "
    else withAttr (attrName "redFg") (str "Filter: ")
  refreshingStatus = str (if m ^. refreshing then "Refreshing" else " ")
  helpBar          = str "F5" <+> str " read | " <+> str "Esc" <+> str " exit"
  selectionW       = selectionWidget m
  messageBar       = vLimit 1 $ str (m ^. message)

app :: M.App AppState Tick Name
app = M.App
  { M.appDraw         = drawUI
  , M.appStartEvent   = return
  , M.appHandleEvent  = appEvent
  , M.appAttrMap      = const $ attrMap
    V.defAttr
    [ (attrName "blueBg" , Brick.Util.bg Graphics.Vty.blue)
    , (attrName "blackFg", Brick.Util.fg Graphics.Vty.black)
    , (attrName "whiteBg", Brick.Util.bg Graphics.Vty.white)
    , (attrName "redFg"  , Brick.Util.fg Graphics.Vty.red)
    ]
  , M.appChooseCursor = F.focusRingCursor (^.focusRing)
  }

mmsReadSeries con model = forM model $ \(ref, fc) -> do
  val <- readVal con ref fc
  return ((ref, fc), Just val)

initialFilter :: String
initialFilter = "."

initialState sts mv initSize = AppState
  sts
  (DM.toList sts)
  0
  initialFilter
  (F.focusRing [FilterEditor, ValueEditor])
  (editor FilterEditor (str . unlines) Nothing initialFilter)
  (isRight $ getRegex initialFilter)
  (editor ValueEditor (str . unlines) Nothing "")
  mv
  False
  0
  initSize
  "OK"

moveDown st
  | st ^. selection == length (st ^. matchingFields) - 1
  = st
  | (st ^. selection) - (st ^. start) == (st ^. size) - 1
  = over selection (+1) . over start (+1) $ st
  | otherwise
  = over selection (+1) st

moveUp st
  | st ^. selection == 0
  = st
  | st ^. selection == st ^. start
  = over selection (\x -> x - 1) . over start (\x -> x - 1) $ st
  | otherwise
  = over selection (\x -> x - 1) st

newtype Tick = Read (Either String [((String,FunctionalConstraint),Maybe MmsVar)])

getRegex :: String -> Either String Regex
getRegex regexString =
  let compOption = CompOption False False False False False
      execOption = ExecOption False
  in  compile compOption execOption regexString

getMatchingFields
  :: Regex -> AppState -> DM.Map (String, FunctionalConstraint) (Maybe MmsVar)
getMatchingFields regex st =
  DM.filterWithKey (\(x, _) _ -> matchTest regex x) (st ^. fields)

updateMatchingXs :: AppState -> AppState
updateMatchingXs ss =
  let regex = getRegex $ head $ getEditContents $ ss ^. editFilter
      ss2   = case regex of
        Right r -> set validFilter True
          $ set matchingFields (DM.toList $ getMatchingFields r ss) ss
        Left l -> set validFilter False ss
  in  set selection 0 . set start 0 $ ss2

createMmsVar :: String -> Constr -> Maybe MmsVar
createMmsVar strVal t
  | t == toConstr (MmsInteger 0) = do
    newValInt <- readMaybe strVal
    let newValMms = MmsInteger newValInt
    return newValMms
  | t == toConstr (MmsFloat 4.0) = do
      newValDouble <- readMaybe strVal
      let newValMms = MmsFloat newValDouble
      return newValMms
  | t == toConstr (MmsUnsigned 4) = do
      newValUnsigned <- readMaybe strVal
      let newValMms = MmsUnsigned newValUnsigned
      return newValMms
  | t == toConstr (MmsString "") = do
      return $ MmsString strVal
  | t == toConstr (MmsVisibleString "") = do
      return $ MmsVisibleString strVal
  | t == toConstr (MmsBoolean True) =
      let ans = case strVal of 
                  "True" -> Just $ MmsBoolean True
                  "False" -> Just $ MmsBoolean False
                  _ -> Nothing
      in ans
  | otherwise = Nothing

writeRequest st = case selectedType st of
  Just t ->
    let newValString = head $ getEditContents $ st ^. editValue
        newValMms    = createMmsVar newValString t
        reference    = selectedReference st
        fc           = selectedFc st
    in  WriteRequest <$> reference <*> fc <*> newValMms
  Nothing -> Nothing

appEvent
  :: AppState -> T.BrickEvent Name Tick -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent (V.EvKey V.KEsc  [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KDown [])) = M.continue $ moveDown st
appEvent st (AppEvent   (Read received     )) = case received of
  Right sts -> do
    let stsMerged = DM.unionWith (flip const) (st ^. fields) (DM.fromList sts)
    M.continue
      $ set refreshing False
      . updateMatchingXs
      . set fields  stsMerged
      . set message "OK"
      $ st
  Left err -> M.continue $ set message err st
appEvent st (T.VtyEvent (V.EvKey (V.KFun 5) [])) = if not $ st ^. refreshing
  then M.suspendAndResume $ do
    putMVar (st ^. mv) $ ReadRequest $ map fst (st ^. matchingFields)
    return $ set refreshing True st
  else continue st
appEvent st (T.VtyEvent (V.EvKey V.KUp    [])) = M.continue $ moveUp st
appEvent st (T.VtyEvent (V.EvKey V.KEnter [])) = case writeRequest st of
  Just req -> M.suspendAndResume $ do
    putMVar (st ^. mv) req
    return st
  Nothing -> continue st
appEvent st (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
  M.continue $ st & focusRing %~ F.focusNext
appEvent st (T.VtyEvent (V.EvResize x y)) =
  let newSt = set size (y - 10) st in M.continue newSt
appEvent st (T.VtyEvent e) = do
  newSt <- case F.focusGetCurrent (st ^. focusRing) of
    Just FilterEditor -> do
      editorHandledSt <- T.handleEventLensed st editFilter handleEditorEvent e
      return $ updateMatchingXs editorHandledSt
    Just ValueEditor -> T.handleEventLensed st editValue handleEditorEvent e
  continue newSt

tuiMain chan sts mv = do
  initSize <- Size.size
  customMain
    (V.mkVty V.defaultConfig)
    (Just chan)
    app
    (initialState (DM.fromList sts) mv (Size.height (fromJust initSize) - 10))
