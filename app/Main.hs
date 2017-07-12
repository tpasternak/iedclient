{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Iec61850.Client
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
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Data.Bits

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
  _fields :: [String],
  _selection :: Int,
  _filterReg :: String,
  _focusRing :: F.FocusRing Name,
  _edit1 :: Editor String Name
  }

makeLenses ''Model

fieldsListV :: Model -> Widget Name
fieldsListV m = border $ viewport Viewport1 Vertical $ vBox $ visibleXs
  where regexString = head $ getEditContents $ m ^. edit1
        visibleXs = over (element $ m ^. selection) visible selectedXs
        selectedXs = str <$> (over (element $ m ^. selection) ('*':) matchingXs)
        matchingXs= filter (=~ regexString) (m ^. fields)        

drawUI :: Model -> [Widget Name]
drawUI m = [e <=> fieldsListV m]
  where e = vLimit 3 $ border (F.withFocusRing (m^. focusRing) renderEditor (m^. edit1)) 



app :: M.App Model e Name
app =
    M.App { M.appDraw = drawUI
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor =  F.focusRingCursor (^.focusRing)
          }

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
  sts <- forM modelFiltered $ \(ref, fc) -> do
    val <- readVal con ref fc
    return (ref, fc, val)
  if tui args then do
    x <- defaultMain app (initialState sts)
    return ()
  else
    forM_ sts $ \(ref, fc, val) -> putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ show val

initialState sts =(Model (map (^._1) sts) 0 "" (F.focusRing [FilterField, FilterField])
       (editor FilterField (str . unlines) Nothing ""))

moveDown st
  | st ^. selection == length (st ^. fields) -1 = st
  | otherwise = over selection (+1) st

moveUp st
  | st ^. selection == 0 = st
  | otherwise = over selection (\x -> x - 1) st


appEvent :: Model -> T.BrickEvent Name e -> T.EventM Name (T.Next Model)
appEvent st (T.VtyEvent (V.EvKey V.KDown [])) = M.continue $ moveDown st
appEvent st (T.VtyEvent (V.EvKey V.KUp [])) = M.continue $ moveUp st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = M.continue $ st & focusRing %~ F.focusNext
appEvent st (T.VtyEvent e) = M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just FilterField -> T.handleEventLensed st edit1 handleEditorEvent e

