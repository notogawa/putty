{-# LANGUAGE GADTs, Rank2Types #-}
module PuttyMacro(PuttyMacro,runPuttyMacro,sendLn,wait) where

import System.IO
    (Handle, hPutStrLn, hGetBufNonBlocking, hFlush,
     hSetBuffering, BufferMode(NoBuffering))
import System.Process
    (CreateProcess(..), createProcess, proc, StdStream(CreatePipe))
import Data.List(isInfixOf,isSuffixOf)
import Foreign.Ptr(Ptr)
import Foreign.C.String(castCCharToChar)
import Foreign.Marshal.Array(mallocArray, peekArray, pokeArray)
import Control.Concurrent(forkIO)
import Control.Concurrent.STM
    (newTVarIO, readTVarIO, readTVar, writeTVar, atomically, TVar(..), retry)
import Control.Monad(forever)
import Control.Monad.Operational
    (ProgramT, singleton, viewT, ProgramViewT(Return, (:>>=)))
import Control.Applicative((<$>))

data PuttyMacroI a where
    SendLn :: String -> PuttyMacroI ()
    Wait :: String -> PuttyMacroI String

type PuttyMacro m a = ProgramT PuttyMacroI m a

sendLn :: String -> PuttyMacro m ()
sendLn = singleton . SendLn

wait :: String -> PuttyMacro m String
wait = singleton . Wait

runPuttyMacro :: String -> [String] -> PuttyMacro IO () -> IO ()
runPuttyMacro putty opts macro = do
  (Just i, Just o, Just e, pid) <-
      createProcess (proc putty opts) { std_in = CreatePipe,
                                        std_out = CreatePipe,
                                        std_err = CreatePipe,
                                        close_fds = True }
  hSetBuffering o NoBuffering
  hSetBuffering e NoBuffering
  termData <- newTVarIO ""
  let putStrLnFlush str = hPutStrLn i str >> hFlush i
      eval m = viewT m >>= eval'
      eval' :: ProgramViewT PuttyMacroI IO () -> IO ()
      eval' (Return _) = return ()
      eval' (SendLn str :>>= m) = putStrLnFlush ("SENDLN "++str) >>= eval . m
      eval' (Wait str :>>= m) = waitWhile str >>= eval . m
      readTerm :: IO ()
      readTerm = forever $ do
                   buf <- mallocArray 65536
                   s <- map castCCharToChar <$>
                        (hGetBufNonBlocking o buf 65536 >>=
                         flip peekArray buf)
                   atomically $ readTVar termData >>= writeTVar termData . (++s)
      waitWhile :: String -> IO String
      waitWhile str = atomically $
                      do
                        buf <- readTVar termData
                        if str `isInfixOf` buf
                        then let (a, b) = splitBy str buf
                             in writeTVar termData b >> return a
                        else retry
  forkIO readTerm
  putStrLnFlush "BEGIN"
  eval macro
  putStrLnFlush "END"

splitBy str target = head [ (a, b) |
                            i <- [0..length target],
                            let (a, b) = splitAt i target,
                            str `isSuffixOf` a]
