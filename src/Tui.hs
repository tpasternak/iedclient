{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Tui where

import Control.Monad
import Iec61850.Client
import Iec61850.Mms
import Iec61850.Enums.FC
import System.Console.CmdArgs
import System.Directory
import System.IO
import Text.Read
import Text.Regex.Posix
import Control.Lens
import Control.Lens.TH
import Brick.Types
import Brick
import Brick.Util
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Data.Bits
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

data Name = FilterEditor | ValueEditor | Viewport1 deriving (Eq, Show, Ord)

data IedClient = IedClient {
  address      :: String,
  port         :: Integer,
  filterExp    :: String,
  refreshCache :: Bool,
  tui          :: Bool
  } deriving (Show, Data, Typeable)

iedclient =
  IedClient
      { address      = "localhost" &= help "IP Address"
      , port         = 102 &= help "IP Port"
      , refreshCache = False &= help "Refresh cached model for the device"
      , filterExp    = def &= help "Filter fields by this regex"
      , tui          = False &= help "Terminal user interface"
      }
    &= summary "IEC 61850 device client"

fetchAndSaveModel con modelsDir modelFile = do
  model_ <- discover con
  createDirectoryIfMissing True modelsDir
  (path, file) <- openTempFile modelsDir "temporaryModel"
  hPutStr file (show model_)
  hClose file
  renameFile path modelFile
  return model_

data Request = ReadRequest [(String,FunctionalConstraint)] |
               WriteRequest String FunctionalConstraint MmsVar

data AppState = AppState {
  _fields :: DM.Map (String,FunctionalConstraint) (Maybe MmsVar),
  _matchingFields :: [((String,FunctionalConstraint),(Maybe MmsVar))],
  _selection :: Int,
  _filterReg :: String,
  _focusRing :: F.FocusRing Name,
  _editFilter :: Editor String Name,
  _editValue :: Editor String Name,
  _mv :: MVar Request,
  _refreshing :: Bool,
  _start :: Int,
  _size :: Int
  }

makeLenses ''AppState

fieldsListV :: AppState -> Widget Name
fieldsListV m = border $ vLimit (m ^. size) $ vBox  (vLimit 1 <$> visibleXs) <=> fill ' '
 where
  visibleXs = take (m ^. size) $ drop (m ^. start) $ highlightedXs
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
  [ vLimit 1 $ (hLimit 20 $ str "DA reference" <+> fill ' ') <+> str selectedId
  , vLimit 1 $ (hLimit 20 $ str "FC" <+> fill ' ') <+> str selectedFc_
  , vLimit 1 $ (hLimit 20 $ str "Type" <+> fill ' ') <+> str selectedType_
  , vLimit 1 $ (hLimit 20 $ str "Value" <+> fill ' ') <+> str selectedValue
  , vLimit 1 $ (hLimit 20 $ newValueLabel <+> fill ' ') <+> F.withFocusRing
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
  [(e <+> selectionW) <=> refreshingStatus <=> fieldsListV m <=> helpBar]
 where
  e = vLimit 1 $ hLimit 50 $ str "Filter: " <+> F.withFocusRing
    (m ^. focusRing)
    renderEditor
    (m ^. editFilter)
  refreshingStatus = str (if m ^. refreshing then "Refreshing" else " ")
  helpBar =
    blackOnWhite (str "F5")
      <+> str " read | "
      <+> blackOnWhite (str "Esc")
      <+> str " exit"
  selectionW = selectionWidget m

app :: M.App AppState Tick Name
app = M.App
  { M.appDraw        = drawUI
  , M.appStartEvent  = return
  , M.appHandleEvent = appEvent
  , M.appAttrMap     = const $ attrMap
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

initialState sts mv initSize = AppState
  sts
  (DM.toList sts)
  0
  ""
  (F.focusRing [FilterEditor, ValueEditor])
  (editor FilterEditor (str . unlines) Nothing "")
  (editor ValueEditor (str . unlines) Nothing "")
  mv
  False
  0
  initSize

moveDown st | st ^. selection == length (st ^. matchingFields) - 1 = st
            | (st ^. selection) - (st ^. start) == (st ^. size) - 1 = over selection (+1) . over start (+1) $ st
            | otherwise = over selection (+1) st

moveUp st | st ^. selection == 0 = st
          | st ^. selection == st ^. start = over selection (\x -> x - 1) . over start (\x -> x - 1) $ st
          | otherwise = over selection (\x -> x - 1) st

data Tick = Read [((String,FunctionalConstraint),Maybe MmsVar)]

getMatchingFields :: AppState -> DM.Map (String, FunctionalConstraint) (Maybe MmsVar)
getMatchingFields st =
  let regexString = head $ getEditContents $ st ^. editFilter
  in  DM.filterWithKey (\(x, _) _ -> x =~ regexString) (st ^. fields)

updateMatchingXs :: AppState -> AppState
updateMatchingXs ss =
  let matchingXs = getMatchingFields ss
      ss2        = set matchingFields (DM.toList matchingXs) ss
  in  set selection 0 . set start 0 $ ss2

createMmsVar :: String -> Constr -> Maybe MmsVar
createMmsVar strVal t = if t == toConstr (MmsInteger 0)
  then do
    newValInt <- readMaybe strVal
    let newValMms = MmsInteger newValInt
    return newValMms
  else if t == toConstr (MmsFloat 4.0)
  then do
    newValDouble <- readMaybe strVal
    let newValMms = MmsFloat newValDouble
    return newValMms
  else Nothing

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
appEvent st (AppEvent   (Read sts          )) = do
  let stsMerged = DM.unionWith (flip const) (st ^. fields) (DM.fromList sts)
  M.continue
    $ set refreshing False
    . updateMatchingXs
    . set fields stsMerged
    $ st
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
  let newSt = set size (y - 10) st
  in  M.continue $ newSt
appEvent st (T.VtyEvent e) = do
  newSt <- case F.focusGetCurrent (st ^. focusRing) of
    Just FilterEditor -> do
      editorHandledSt <- T.handleEventLensed st editFilter handleEditorEvent e
      return $ updateMatchingXs editorHandledSt
    Just ValueEditor -> T.handleEventLensed st editValue handleEditorEvent e
  continue newSt

tuiMain chan sts mv = do
  initSize <- Size.size
  customMain (V.mkVty V.defaultConfig)
    (Just chan)
    app
    (initialState (DM.fromList sts) mv ((Size.height $ fromJust initSize) - 10))