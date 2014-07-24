module Teevy.Shared.Log where

import System.Posix.Syslog

import qualified Data.Text.Lazy as TL

type Tag = String

info :: Tag -> String -> IO ()
info _ msg = withSyslog "teevy" [PID] USER (syslog Debug msg)

debug :: Tag -> String -> IO ()
debug _ msg = withSyslog "teevy" [PID] USER (syslog Debug msg)

err :: Tag -> String -> IO ()
err _ msg = withSyslog "teevy" [PID] USER (syslog Error msg)

authErr :: TL.Text -> IO ()
authErr addr = withSyslog "teevy" [PID] AUTH (syslog Error $ "Invalid login from: " ++ show (addr))