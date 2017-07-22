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
import Control.Concurrent
import Brick.BChan (newBChan, writeBChan)
import Tui(Tick(..), Request(..), tuiMain)

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
      chan <- newBChan 10
      mv   <- newEmptyMVar
      forkIO $ forever $ do
        req <- takeMVar mv
        case req of
          ReadRequest r -> do
            let listOfFieldsToUpdate = r
            sts <- mmsReadSeries con listOfFieldsToUpdate
            writeBChan chan $ Read sts
          WriteRequest ref fc val -> do
            writeVal con ref fc val
            sts <- mmsReadSeries con [(ref,fc)]
            writeBChan chan $ Read sts
      tuiMain chan sts mv
      return ()
    else do
      let modelFiltered = filter (\(ref, _) -> ref =~ filterExp args) model
      sts <- mmsReadSeries con modelFiltered
      forM_ sts $ \((ref, fc), val) ->
        putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ maybe "" show val

