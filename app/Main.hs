{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Monad
import Control.Monad.Except(liftIO, runExceptT)
import Data.Either.Utils(fromRight)
import Iec61850.Client
import System.Directory
import System.IO
import Text.Read
import Text.Regex.TDFA
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
      , filterExp    = ".*" &= help "Filter fields by this regex"
      , tui          = False &= help "Terminal user interface"
      }
    &= summary "IEC 61850 device client"

fetchAndSaveModel con modelsDir modelFile = do
  model_ <- discover con
  liftIO $ createDirectoryIfMissing True modelsDir
  (path, file) <- liftIO $ openTempFile modelsDir "temporaryModel"
  liftIO $ hPutStr file (show model_)
  liftIO $ hClose file
  liftIO $ renameFile path modelFile
  return model_

mmsReadSeries con model = forM model $ \(ref, fc) -> do
  val <- readVal con ref fc
  return ((ref, fc), Just val)

main :: IO ()
main = do
  ic    <- cmdArgs iedclient
  con     <- fromRight <$> runExceptT (connect (address ic) (fromInteger . port $ ic))
  homeDir <- getHomeDirectory
  let modelsDir = homeDir ++ "/" ++ ".iedclient.d/models/"
  let modelFile = modelsDir ++ "/" ++ address ic
  modelFileExists <- doesPathExist modelFile
  model       <- if not modelFileExists || refreshCache ic
    then fromRight <$> runExceptT (fetchAndSaveModel con modelsDir modelFile)
    else do
      contents <- withFile modelFile ReadMode $
        \h -> do
          contents <- hGetContents h
          return $!! contents
      case readMaybe contents of
        Just x -> return x
        Nothing -> fromRight <$> runExceptT (fetchAndSaveModel con modelsDir modelFile)
  if tui ic
    then do
      chan <- newBChan 10
      mv   <- newEmptyMVar
      _ <- forkIO $ forever $ do
        req <- takeMVar mv
        case req of
          ReadRequest listOfFieldsToUpdate -> do
            sts <- runExceptT $ mmsReadSeries con listOfFieldsToUpdate
            writeBChan chan $ Read sts
          WriteRequest ref fc val -> do
            writeRes <- runExceptT $ writeVal con ref fc val
            case writeRes of 
              Right _ -> do
                sts <- runExceptT $ mmsReadSeries con [(ref,fc)]
                writeBChan chan $ Read sts
              Left err -> writeBChan chan $ Read (Left err)
      let sts = zip model $ repeat Nothing
      _ <- tuiMain chan sts mv
      return ()
    else do
      let modelFiltered = filter (\(ref, _) -> ref =~ filterExp ic) model
      res <- runExceptT $ mmsReadSeries con modelFiltered
      case res of
        Right sts -> 
          forM_ sts $ \((ref, fc), val) ->
                        putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ maybe "" show val
        Left err -> hPutStrLn stderr err

