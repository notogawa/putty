module Main where

import PuttyMacro

main :: IO ()
main = runPuttyMacro "putty.exe" ["-load","scarlet"] userMacro

userMacro :: PuttyMacro IO ()
userMacro = do
  wait "$ "
  sendLn "ls"
  wait "$ "
  sendLn "pwd"
