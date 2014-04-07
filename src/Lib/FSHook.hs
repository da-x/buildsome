{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Lib.FSHook
  ( FSHook
  , with
  , Input(..), Output(..), IsDelayed(..)
  , FSAccessHandler
  , AccessDoc
  , runCommand, timedRunCommand
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, myThreadId, killThread)
import Control.Concurrent.MVar
import Control.Exception.Async (handleSync)
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Lib.Argv0 (getArgv0)
import Lib.ByteString (unprefixed)
import Lib.FSHook.AccessType (AccessType(..))
import Lib.FilePath (FilePath, takeDirectory, (</>))
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Lib.TimeIt (timeIt)
import Network.Socket (Socket)
import Paths_buildsome (getDataFileName)
import Prelude hiding (FilePath)
import System.IO (hPutStrLn, stderr)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.FSHook.Protocol as Protocol
import qualified Lib.Fresh as Fresh
import qualified Lib.Process as Process
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Posix.ByteString as Posix

type AccessDoc = ByteString

type JobId = ByteString

-- Hook is delayed waiting for handler to complete
data IsDelayed = Delayed | NotDelayed

data Input = Input
  { inputAccessType :: AccessType
  , inputPath :: FilePath
  }

newtype Output = Output
  { outputPath :: FilePath
  }

type FSAccessHandler = IsDelayed -> AccessDoc -> [Input] -> [Output] -> IO ()

data RunningJob = RunningJob
  { jobLabel :: ByteString
  , jobActiveConnections :: IORef (Map Int (ThreadId, MVar ()))
  , jobFreshConnIds :: Fresh Int
  , jobThreadId :: ThreadId
  , jobFSAccessHandler :: FSAccessHandler
  , jobRootFilter :: FilePath
  }

data FSHook = FSHook
  { fsHookRunningJobs :: IORef (Map JobId RunningJob)
  , fsHookFreshJobIds :: Fresh Int
  , fsHookLdPreloadPath :: FilePath
  , fsHookServerAddress :: FilePath
  }

data ProtocolError = ProtocolError String deriving (Typeable)
instance E.Exception ProtocolError
instance Show ProtocolError where
  show (ProtocolError msg) = "ProtocolError: " ++ msg

serve :: FSHook -> Socket -> IO ()
serve fsHook conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed Protocol.helloMsg helloLine of
    Nothing ->
      E.throwIO $ ProtocolError $ concat
      [ "Bad hello message from connection: ", show helloLine, " expected: "
      , show Protocol.helloMsg, " (check your fs_override.so installation)" ]
    Just pidJobId -> do
      runningJobs <- readIORef (fsHookRunningJobs fsHook)
      case M.lookup jobId runningJobs of
        Nothing -> do
          let jobIds = M.keys runningJobs
          E.throwIO $ ProtocolError $ "Bad slave id: " ++ show jobId ++ " mismatches all: " ++ show jobIds
        Just job -> handleJobConnection fullTidStr conn job
      where
        fullTidStr = BS8.unpack pidStr ++ ":" ++ BS8.unpack tidStr
        [pidStr, tidStr, jobId] = BS8.split ':' pidJobId

maxMsgSize :: Int
maxMsgSize = 65536

-- Except thread killed
printRethrowExceptions :: String -> IO a -> IO a
printRethrowExceptions msg =
  E.handle $ \e -> do
    case E.fromException e of
      Just E.ThreadKilled -> return ()
      _ -> hPutStrLn stderr $ msg ++ show e
    E.throwIO e

with :: (FSHook -> IO a) -> IO a
with body = do
  ldPreloadPath <- getLdPreloadPath
  pid <- Posix.getProcessID
  freshJobIds <- Fresh.new 0
  let serverFilename = "/tmp/fshook-" <> BS8.pack (show pid)
  withUnixSeqPacketListener serverFilename $ \listener -> do
    runningJobsRef <- newIORef M.empty
    let
      fsHook = FSHook
        { fsHookRunningJobs = runningJobsRef
        , fsHookFreshJobIds = freshJobIds
        , fsHookLdPreloadPath = ldPreloadPath
        , fsHookServerAddress = serverFilename
        }
    AsyncContext.new $ \ctx -> do
      _ <-
        AsyncContext.spawn ctx $ printRethrowExceptions "BUG: Listener loop threw exception: " $ forever $
        do
          (conn, _srcAddr) <- Sock.accept listener
          AsyncContext.spawn ctx $ printRethrowExceptions "BUG: Job connection failed: " $ serve fsHook conn
      body fsHook

{-# INLINE sendGo #-}
sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS8.pack "GO")

{-# INLINE handleJobMsg #-}
handleJobMsg :: String -> Socket -> RunningJob -> Protocol.Func -> IO ()
handleJobMsg _tidStr conn job msg =
  case msg of
    -- outputs
    Protocol.OpenW path _openWMode _creationMode -> handleOutput path
                 -- TODO ^ need to make sure ReadWriteMode only ever
                 -- opens files created by same job or inexistent
    Protocol.Creat path _ -> handleOutput path
    Protocol.Rename a b -> handleOutputs [a, b]
    Protocol.Unlink path -> handleOutput path
    Protocol.Truncate path _ -> handleOutput path
    Protocol.Chmod path _ -> handleOutput path
    Protocol.Chown path _ _ -> handleOutput path
    Protocol.MkNod path _ _ -> handleOutput path -- TODO: Special mkNod handling?
    Protocol.MkDir path _ -> handleOutput path
    Protocol.RmDir path -> handleOutput path

    -- I/O
    Protocol.SymLink target linkPath ->
      -- TODO: We don't actually read the input here, but we don't
      -- handle symlinks correctly yet, so better be false-positive
      -- than false-negative
      handle [Input AccessTypeFull target] [Output linkPath]
    Protocol.Link src dest -> error $ unwords ["Hard links not supported:", show src, "->", show dest]

    -- inputs
    Protocol.OpenR path -> handleInput AccessTypeFull path
    Protocol.Access path _mode -> handleInput AccessTypeModeOnly path
    Protocol.Stat path -> handleInput AccessTypeFull path
    Protocol.LStat path -> handleInput AccessTypeFull path
    Protocol.OpenDir path -> handleInput AccessTypeFull path
    Protocol.ReadLink path -> handleInput AccessTypeFull path
    Protocol.Exec path -> handleInput AccessTypeFull path
    Protocol.ExecP mPath attempted ->
      handleAccess Delayed actDesc inputs []
      where
        inputs =
          [Input AccessTypeFull path | Just path <- [mPath]] ++
          map (Input AccessTypeModeOnly) attempted
  where
    handleAccess = jobHandleAccess job conn
    handle inputs outputs = handleAccess isDelayed actDesc inputs outputs
      where
        isDelayed
          | all ("/" `BS8.isPrefixOf`) (map inputPath inputs ++ map outputPath outputs) = NotDelayed
          | otherwise = Delayed
    actDesc = BS8.pack (Protocol.showFunc msg) <> " done by " <> jobLabel job
    handleInput accessType path = handle [Input accessType path] []
    handleOutput path = handleOutputs [path]
    handleOutputs paths = handle [] (map Output paths)

jobHandleAccess :: RunningJob -> Socket -> IsDelayed -> AccessDoc -> [Input] -> [Output] -> IO ()
jobHandleAccess job conn isDelayed desc inputs outputs = do
  forwardExceptions $ jobFSAccessHandler job isDelayed desc inputs outputs
  case isDelayed of
    Delayed -> sendGo conn
    NotDelayed -> return ()
  where
    forwardExceptions = handleSync $ \e@E.SomeException {} -> E.throwTo (jobThreadId job) e

withRegistered :: Ord k => IORef (Map k a) -> k -> a -> IO r -> IO r
withRegistered registry key val =
  E.bracket_ register unregister
  where
    register = atomicModifyIORef_ registry $ M.insert key val
    unregister = atomicModifyIORef_ registry $ M.delete key

handleJobConnection :: String -> Socket -> RunningJob -> IO ()
handleJobConnection tidStr conn job = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connId <- Fresh.next $ jobFreshConnIds job
  tid <- myThreadId

  connFinishedMVar <- newEmptyMVar
  (`E.finally` putMVar connFinishedMVar ()) $
    withRegistered (jobActiveConnections job) connId (tid, connFinishedMVar) $ do
      sendGo conn
      recvLoop_ maxMsgSize
        (handleJobMsg tidStr conn job <=< Protocol.parseMsg) conn

mkEnvVars :: FSHook -> FilePath -> JobId -> Process.Env
mkEnvVars fsHook rootFilter jobId =
  (map . fmap) BS8.unpack
  [ ("LD_PRELOAD", fsHookLdPreloadPath fsHook)
  , ("BUILDSOME_MASTER_UNIX_SOCKADDR", fsHookServerAddress fsHook)
  , ("BUILDSOME_JOB_ID", jobId)
  , ("BUILDSOME_ROOT_FILTER", rootFilter)
  ]

timedRunCommand ::
  FSHook -> FilePath -> (Process.Env -> IO r) -> ByteString ->
  FSAccessHandler -> IO (NominalDiffTime, r)
timedRunCommand fsHook rootFilter cmd label fsAccessHandler = do
  pauseTimeRef <- newIORef 0
  let
    addPauseTime delta = atomicModifyIORef'_ pauseTimeRef (+delta)
    measurePauseTime act = do
      (time, res) <- timeIt act
      addPauseTime time
      return res
    wrappedFsAccessHandler isDelayed accessDoc inputs outputs = do
      let act = fsAccessHandler isDelayed accessDoc inputs outputs
      case isDelayed of
        Delayed -> measurePauseTime act
        NotDelayed -> act
  (time, res) <- runCommand fsHook rootFilter (timeIt . cmd) label wrappedFsAccessHandler
  subtractedTime <- (time-) <$> readIORef pauseTimeRef
  return (subtractedTime, res)

runCommand ::
  FSHook -> FilePath -> (Process.Env -> IO r) -> ByteString ->
  FSAccessHandler -> IO r
runCommand fsHook rootFilter cmd label fsAccessHandler = do
  activeConnections <- newIORef M.empty
  freshConnIds <- Fresh.new 0
  jobIdNum <- Fresh.next $ fsHookFreshJobIds fsHook
  tid <- myThreadId

  let jobId = BS8.pack ("cmd" ++ show jobIdNum)
      job = RunningJob
            { jobLabel = label
            , jobActiveConnections = activeConnections
            , jobFreshConnIds = freshConnIds
            , jobThreadId = tid
            , jobRootFilter = rootFilter
            , jobFSAccessHandler = fsAccessHandler
            }
  -- Don't leak connections still running our handlers once we leave!
  let onActiveConnections f = mapM_ f . M.elems =<< readIORef activeConnections
  (`E.finally` onActiveConnections awaitConnection) $
    (`E.onException` onActiveConnections killConnection) $
    withRegistered (fsHookRunningJobs fsHook) jobId job $
    cmd (mkEnvVars fsHook rootFilter jobId)
  where
    killConnection (tid, _mvar) = killThread tid
    awaitConnection (_tid, mvar) = readMVar mvar

data CannotFindOverrideSharedObject = CannotFindOverrideSharedObject deriving (Show, Typeable)
instance E.Exception CannotFindOverrideSharedObject

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  installedFilePath <- BS8.pack <$> (getDataFileName . BS8.unpack) fileName
  installedExists <- Posix.fileExist installedFilePath
  if installedExists
    then return installedFilePath
    else do
      argv0 <- getArgv0
      let nearExecPath = takeDirectory argv0 </> fileName
      nearExecExists <- Posix.fileExist nearExecPath
      if nearExecExists
        then return nearExecPath
        else E.throwIO CannotFindOverrideSharedObject
  where
    fileName = "cbits/fs_override.so"
