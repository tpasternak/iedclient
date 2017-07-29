{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Monad
import           Iec61850.Client
import           System.Directory
import           System.IO
import           Text.Read
import           Text.Regex.Posix
import Control.Concurrent
import Brick.BChan (newBChan, writeBChan)
import Tui(Tick(..), Request(..), tuiMain)
import Control.DeepSeq(($!!))
import System.Console.CmdArgs 

data IedClient = IedClient {
  address      :: String,
  port         :: Integer,
  filterExp    :: String,
  refreshCache :: Bool,
  tui          :: Bool
  } deriving (Show, Data, Typeable)

iedclient :: IedClient
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
  ic    <- cmdArgs iedclient
  con     <- connect (address ic) (fromInteger . port $ ic)
  homeDir <- getHomeDirectory
  let modelsDir = homeDir ++ "/" ++ ".iedclient.d/models/"
  let modelFile = modelsDir ++ "/" ++ address ic
  modelFileExists <- doesPathExist modelFile
  model       <- if not modelFileExists || refreshCache ic
    then fetchAndSaveModel con modelsDir modelFile
    else do
      contents <- withFile modelFile ReadMode $
        \h -> do
          contents <- hGetContents h
          return $!! contents
      case readMaybe contents of
        Just x -> return x
        Nothing -> fetchAndSaveModel con modelsDir modelFile

  if tui ic
    then do
      chan <- newBChan 10
      mv   <- newEmptyMVar
      _ <- forkIO $ forever $ do
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
      let sts = zip model $ repeat Nothing
      _ <- tuiMain chan sts mv
      return ()
    else do
      let modelFiltered = filter (\(ref, _) -> ref =~ filterExp ic) model
      sts <- mmsReadSeries con modelFiltered
      forM_ sts $ \((ref, fc), val) ->
        putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ maybe "" show val

