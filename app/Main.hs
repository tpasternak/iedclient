{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Iec61850.Client
import           Iec61850.Mms
import           Iec61850.Enums.FC
import           System.Console.CmdArgs
import           System.Directory
import           System.IO
import           Text.Read
import           Text.Regex.Posix
import           Control.Lens
import           Control.Lens.TH
import qualified Brick.Types as T
import Brick.Types
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Brick hiding (clamp)
import Brick.Util hiding (clamp)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Data.Bits
import Graphics.Vty
import Control.Concurrent
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as F
import qualified Data.Map as DM
import Data.Data (toConstr)

data Name = FilterField | ListField | Viewport1 deriving (Eq, Show, Ord)

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

data AppState = AppState {
  _fields :: DM.Map (String,FunctionalConstraint) (Maybe MmsVar),
  _matchingFields :: [((String,FunctionalConstraint),(Maybe MmsVar))],
  _selection :: Int,
  _filterReg :: String,
  _focusRing :: F.FocusRing Name,
  _edit1 :: Editor String Name,
  _mv :: MVar [(String,FunctionalConstraint)],
  _refreshing :: Bool
  }

makeLenses ''AppState

fieldsListV :: AppState -> Widget Name
fieldsListV m = border $ viewport Viewport1 Vertical $ vBox
  (vLimit 1 <$> visibleXs)
 where
  visibleXs = over (element $ m ^. selection)
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

selectionWidget m = vBox
  [ vLimit 1 ((hLimit 20 $ str "DA reference" <+> fill ' ') <+> str selectedId)
  , vLimit 1 ((hLimit 20 $ str "FC" <+> fill ' ') <+> str selectedFc)
  , vLimit 1 ((hLimit 20 $ str "Type" <+> fill ' ') <+> str selectedType)
  ]
 where
  selectedField = if m ^. selection < length (m ^. matchingFields)
    then Just ((m ^. matchingFields) !! (m ^. selection))
    else Nothing
  selectedId = maybe "" (\f -> f ^. _1 ^. _1) selectedField
  selectedFc = maybe "" (\f -> show $ f ^. _1 ^. _2) selectedField
  selectedType =
    maybe "" (\f -> maybe "-" (show . toConstr) (f ^. _2)) selectedField

drawUI :: AppState -> [Widget Name]
drawUI m =
  [(e <+> selectionW) <=> refreshingStatus <=> fieldsListV m <=> helpBar]
 where
  e = vLimit 1 $ hLimit 80 $ str "Filter: " <+> F.withFocusRing
    (m ^. focusRing)
    renderEditor
    (m ^. edit1)
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
    ]
  , M.appChooseCursor = F.focusRingCursor (^.focusRing)
  }

mmsReadSeries con model = forM model $ \(ref, fc) -> do
  val <- readVal con ref fc
  return ((ref, fc), Just val)


main :: IO ()
main = do
  args    <- cmdArgs iedclient
  con     <- connect (address args) (fromInteger . port $ args)
  homeDir <- getHomeDirectory
  let modelsDir = homeDir ++ "/" ++ ".iedclient.d/models/"
  let modelFile = modelsDir ++ "/" ++ address args
  modelExists <- doesPathExist modelFile
  model       <- if not modelExists || refreshCache args
    then fetchAndSaveModel con modelsDir modelFile
    else do
      file     <- openFile modelFile ReadMode
      contents <- hGetContents file
      case readMaybe contents of
        Just x -> do
          hClose file
          return x
        Nothing -> do
          hClose file
          fetchAndSaveModel con modelsDir modelFile

  if tui args
    then do
      let sts = zip model $ repeat Nothing
      chan <- newBChan 1
      mv   <- newEmptyMVar
      forkIO $ forever $ do
        listOfFieldsToUpdate <- takeMVar mv
        sts                  <- mmsReadSeries con listOfFieldsToUpdate
        writeBChan chan $ Tick sts
      customMain (V.mkVty V.defaultConfig)
                 (Just chan)
                 app
                 (initialState (DM.fromList sts) mv)
      return ()
    else do
      let modelFiltered = filter (\(ref, _) -> ref =~ filterExp args) model
      sts <- mmsReadSeries con modelFiltered
      forM_ sts $ \((ref, fc), val) ->
        putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ maybe "" show val

initialState sts mv = AppState
  sts
  (DM.toList sts)
  0
  ""
  (F.focusRing [FilterField, FilterField])
  (editor FilterField (str . unlines) Nothing "")
  mv
  False

moveDown st | st ^. selection == length (st ^. matchingFields) - 1 = st
            | otherwise = over selection (+1) st

moveUp st | st ^. selection == 0 = st
          | otherwise            = over selection (\x -> x - 1) st

data Tick = Tick [((String,FunctionalConstraint),Maybe MmsVar)]

getMatchingFields st =
  let regexString = head $ getEditContents $ st ^. edit1
  in  DM.filterWithKey (\(x, _) _ -> x =~ regexString) (st ^. fields)

updateMatchingXs ss =
  let regexString = head $ getEditContents $ ss ^. edit1
      matchingXs  = getMatchingFields ss
      ss2         = set matchingFields (DM.toList matchingXs) ss
  in  over selection (clamp 0 (length matchingXs - 1)) ss2

clamp lower upper x = max lower (min x upper)

appEvent
  :: AppState -> T.BrickEvent Name Tick -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent (V.EvKey V.KEsc  [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey V.KDown [])) = M.continue $ moveDown st
appEvent st (AppEvent   (Tick sts          )) = do
  let stsMerged = DM.unionWith (flip const) (st ^. fields) (DM.fromList sts)
  M.continue
    $ set refreshing False
    . updateMatchingXs
    . set fields stsMerged
    $ st
appEvent st (T.VtyEvent (V.EvKey (V.KFun 5) [])) = if not $ st ^. refreshing
  then M.suspendAndResume $ do
    putMVar (st ^. mv) $ map fst (st ^. matchingFields)
    return $ set refreshing True st
  else continue st
appEvent st (T.VtyEvent (V.EvKey V.KUp [])) = M.continue $ moveUp st
appEvent st (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
  M.continue $ st & focusRing %~ F.focusNext
appEvent st (T.VtyEvent e) = do
  newSt <- case F.focusGetCurrent (st ^. focusRing) of
    Just FilterField -> do
      editorHandledSt <- T.handleEventLensed st edit1 handleEditorEvent e
      return $ updateMatchingXs editorHandledSt
  continue $ newSt

