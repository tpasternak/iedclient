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

import Brick.Widgets.Edit
import qualified Brick.Focus as F

data Name = Edit1
          deriving (Ord, Show, Eq)


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

fieldsList :: Model -> [Widget ()]
fieldsList (Model xs skipNo sel _ _ _) = [border $ vBox (str <$> visibleXs)]
  where visibleXs = (take 20 . drop skipNo) selectedXs
        selectedXs = over (element sel) ('*':) xs 


drawUI :: Model -> [Widget ()]
drawUI m = [vBox $ border (str $ "Filter: " ++ _filterReg m ) : fieldsList m]


data Model = Model {
  _fields :: [String],
  _firstRow :: Int,
  _selection :: Int,
  _filterReg :: String,
  _focusRing :: F.FocusRing Name,
  _edit1 :: Editor String Name  
  }

makeLenses ''Model

app :: M.App Model e ()
app =
    M.App { M.appDraw = drawUI
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
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
    x <- defaultMain app (Model (map (^._1) sts) 0 0 "" (F.focusRing [Edit1])
       (editor Edit1 (str . unlines) Nothing ""))
    return ()
  else
    forM_ sts $ \(ref, fc, val) -> putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ show val


moveDown st
  | st ^. selection == length (st ^. fields) -1 = st
  | st ^. firstRow + 19 ==  st ^. selection = over selection (+1) . over firstRow (+1) $ st
  | otherwise = over selection (+1) st

moveUp st
  | st ^. selection == 0 = st
  | st ^. firstRow ==  st ^. selection = over selection (\x -> x -1 ) . over firstRow (\x -> x-1) $ st
  | otherwise = over selection (\x -> x - 1) st


appEvent :: Model -> T.BrickEvent () e -> T.EventM () (T.Next Model)
appEvent st (T.VtyEvent (V.EvKey V.KDown [])) =
   M.continue $ moveDown st
appEvent st (T.VtyEvent (V.EvKey V.KUp [])) =
   M.continue $ moveUp st





