{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Monad
import           Iec61850.Client
import           System.Console.CmdArgs

data IedClient = IedClient { address :: String, port :: Integer }
  deriving (Show, Data, Typeable)

iedclient = IedClient { address = "localhost" &= help "IP Address", port = 102 &= help "IP Port" } &= summary
                                                                                                        "IEC 61850 device client"

main :: IO ()
main = do
  args <- cmdArgs iedclient
  print args
  con <- connect (address args) (fromInteger . port $ args)
  model <- discover con
  forM_ model print
