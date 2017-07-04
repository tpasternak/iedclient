{-# LANGUAGE DeriveDataTypeable #-}
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
import           Lens.Micro

import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Brick

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

drawUI :: [String] -> [Widget ()]
drawUI (xs) = [vBox (str <$> xs)]

app :: M.App [String] e ()
app =
    M.App { M.appDraw = drawUI
          , M.appStartEvent = return
          , M.appHandleEvent = resizeOrQuit
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
    x <- defaultMain app (map (^._1) sts)
    return ()
  else
    forM_ sts $ \(ref, fc, val) -> do
      putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ show val
