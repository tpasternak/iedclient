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
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Brick
import Brick.Util
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Data.Bits
import Graphics.Vty
import Control.Concurrent
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as F

data Name = FilterField | ListField | Viewport1 deriving (Eq,Show,Ord)

data IedClient = IedClient {
  address      :: String,
  port         :: Integer,
  filterExp    :: String,
  refreshCache :: Bool,
  tui          :: Bool
  } deriving (Show, Data, Typeable)

iedclient = IedClient
              { address = "localhost" &= help "IP Address"
              , port = 102 &= help "IP Port"
              , refreshCache = False &= help "Refresh cached model for the device"
              , filterExp = def &= help "Filter fields by this regex"
              , tui = False &= help "Terminal user interface"
              } &= summary "IEC 61850 device client"

fetchAndSaveModel con modelsDir modelFile = do
                   model_ <- discover con
                   createDirectoryIfMissing True modelsDir
                   (path,file) <- openTempFile modelsDir "temporaryModel"
                   hPutStr file (show model_)
                   hClose file
                   renameFile path modelFile
                   return model_

data Model = Model {
  _fields :: [(String,FunctionalConstraint,MmsVar)],
  _matchingFields :: [(String,FunctionalConstraint,MmsVar)],
  _selection :: Int,
  _filterReg :: String,
  _focusRing :: F.FocusRing Name,
  _edit1 :: Editor String Name
  }

makeLenses ''Model

fieldsListV :: Model -> Widget Name
fieldsListV m = border $ viewport Viewport1 Vertical $ vBox (vLimit 1 <$> visibleXs)
  where visibleXs = over (element $ m ^. selection) (visible . withAttr (attrName "blueBg")) stringedXs
        stringedXs = map (\(x,y,z) -> hLimit 50 (str x <+> fill ' ') <+> str (show y) <+> str "  "  <+> str(show z) <+> fill ' ') (m ^. matchingFields)

drawUI :: Model -> [Widget Name]
drawUI m = [e <=> fieldsListV m]
  where e = vLimit 3 $ border (F.withFocusRing (m^. focusRing) renderEditor (m^. edit1)) 

app :: M.App Model Tick Name
app =
    M.App { M.appDraw = drawUI
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [ (attrName "blueBg", Brick.Util.bg Graphics.Vty.blue)]                            
          , M.appChooseCursor =  F.focusRingCursor (^.focusRing)
          }

mmsReadSeries con model = do
  sts <- forM model $ \(ref, fc) -> do
    val <- readVal con ref fc
    return (ref, fc, val)
  return sts

main :: IO ()
main = do
  args <- cmdArgs iedclient
  con <- connect (address args) (fromInteger . port $ args)
  homeDir <- getHomeDirectory
  let modelsDir = homeDir ++ "/" ++ ".iedclient.d/models/"
  let modelFile = modelsDir ++ "/" ++ address args
  modelExists <- doesPathExist modelFile
  model <- if not modelExists || refreshCache args
              then fetchAndSaveModel con modelsDir modelFile
              else do
                 file <- openFile modelFile ReadMode
                 contents <- hGetContents file
                 case readMaybe contents of
                   Just x -> do
                     hClose file
                     return x
                   Nothing -> do
                     hClose file
                     fetchAndSaveModel con modelsDir modelFile

  let modelFiltered = filter (\(ref, _) -> ref =~ filterExp args) model
  sts <- mmsReadSeries con modelFiltered
  if tui args then do
    chan <- newBChan 10
    forkIO $ forever $ do
      sts <- mmsReadSeries con modelFiltered
      writeBChan chan (Tick sts)
      threadDelay 5000000
    x <- customMain (V.mkVty V.defaultConfig) (Just chan) app (initialState sts)
    return ()
  else
    forM_ sts $ \(ref, fc, val) -> putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ show val

initialState sts =Model sts sts 0 "" (F.focusRing [FilterField, FilterField])
                  (editor FilterField (str . unlines) Nothing "")

moveDown st
  | st ^. selection == length (st ^. matchingFields) -1 = st
  | otherwise = over selection (+1) st

moveUp st
  | st ^. selection == 0 = st
  | otherwise = over selection (\x -> x - 1) st

data Tick = Tick [(String,FunctionalConstraint,MmsVar)]

updateMatchingXs ss = 
  let regexString = head $ getEditContents $ ss ^. edit1
      matchingXs = filter ((=~ regexString) . \(x,_,_) -> x)  (ss ^. fields)
      ss2 = set matchingFields matchingXs ss
  in over selection (\x -> max  0 (min x (length matchingXs - 1))) ss2


appEvent :: Model -> T.BrickEvent Name Tick -> T.EventM Name (T.Next Model)
appEvent st (T.VtyEvent (V.EvKey V.KDown [])) = M.continue $ moveDown st
appEvent st (AppEvent (Tick sts)) = do
  let ss = set fields sts st
  let ss2 = updateMatchingXs ss
  M.continue ss2
appEvent st (T.VtyEvent (V.EvKey V.KUp [])) = M.continue $ moveUp st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = M.continue $ st & focusRing %~ F.focusNext
appEvent st (T.VtyEvent e) = do
  ss <- case F.focusGetCurrent (st ^. focusRing) of
    Just FilterField -> T.handleEventLensed st edit1 handleEditorEvent e
  let ss2 = updateMatchingXs ss
  continue ss2

