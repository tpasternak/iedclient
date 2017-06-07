{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Monad
import           Iec61850.Client
import           System.Console.CmdArgs
import           Text.Regex.Posix

data IedClient = IedClient { address :: String, port :: Integer, filterExp :: String }
  deriving (Show, Data, Typeable)

iedclient = IedClient
              { address = "localhost" &= help "IP Address"
              , port = 102 &= help "IP Port"
              , filterExp = def &= help "Filter fields by this regex"
              } &= summary "IEC 61850 device client"

main :: IO ()
main = do
  args <- cmdArgs iedclient
  con <- connect (address args) (fromInteger . port $ args)
  model <- discover con
  let modelFiltered = filter (\(ref, _) -> ref =~ filterExp args) model
  forM_ modelFiltered $ \(ref, fc) -> do
    val <- readVal con ref fc
    putStrLn $ ref ++ "[" ++ show fc ++ "]: " ++ show val
