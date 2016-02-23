{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Lib.Makefile.Monad
  ( PutStrLn, runPutStrLn
  , M, runM
  ) where


import Prelude.Compat hiding (FilePath)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State (StateT(..))
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.IORef (newIORef, IORef, readIORef, modifyIORef')

import GHC.Generics (Generic)
import Lib.Directory (getMFileStatus)
import Lib.FilePath (FilePath)
import Lib.Makefile.MonadClass (MonadMakefileParser(..))
import qualified Buildsome.Meddling as Meddling
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix
import Control.Exception ( SomeException )

-- Specific Monad type for makefile parsing that tracks all inputs/outputs:
data PutStrLnTarget = Out | Err deriving (Generic, Show)
data PutStrLn = PutStrLn PutStrLnTarget ByteString deriving (Generic, Show)
instance Binary PutStrLnTarget
instance Binary PutStrLn

targetHandle :: PutStrLnTarget -> IO.Handle
targetHandle Out = IO.stdout
targetHandle Err = IO.stderr

type ReadFiles = Map FilePath (Maybe Posix.FileStatus)

data W = W
  { wPutStrLns :: [PutStrLn]
  , wReadFiles :: Map FilePath [Maybe Posix.FileStatus]
  , wFiles    :: Maybe (IORef (Map FilePath (Either SomeException ByteString, Maybe Posix.FileStatus)))
  }

instance Monoid W where
  mempty = W mempty mempty Nothing
  mappend (W ax ay Nothing) (W bx by bz) =
    W (mappend ax bx) (Map.unionWith (++) ay by) bz
  mappend (W ax ay az) (W bx by Nothing) =
    W (mappend ax bx) (Map.unionWith (++) ay by) az
  mappend _ _ = error "internal error"

runPutStrLn :: PutStrLn -> IO ()
runPutStrLn (PutStrLn target bs) = BS8.hPutStrLn (targetHandle target) bs

-- IMPORTANT: Use StateT and not WriterT, because WriterT's >>= leaks
-- stack-space due to tuple unpacking (and mappend) *after* the inner
-- bind, whereas StateT uses a tail call for the inner bind.
newtype M a = M (StateT W IO a)
  deriving (Functor, Applicative, Monad)
tell :: W -> M ()
tell = M . State.modify . flip mappend

doPutStrLn :: PutStrLnTarget -> ByteString -> M ()
doPutStrLn tgt bs = do
  M $ liftIO $ runPutStrLn p
  tell $ mempty { wPutStrLns = [p] }
  where
    p = PutStrLn tgt bs

instance MonadMakefileParser M where
  outPutStrLn = doPutStrLn Out
  errPutStrLn = doPutStrLn Err
  tryReadFile filePath = do
    w <- M State.get
    let miss = do
          mFileStat <- M $ liftIO $ getMFileStatus filePath
          tell $ mempty { wReadFiles = Map.singleton filePath [mFileStat] }
          mFileData <- M $ liftIO $ tryReadFile filePath
          let v = (mFileData, mFileStat)
          case wFiles w of
              Just ioref -> M $ liftIO $ modifyIORef' ioref (Map.insert filePath v)
              Nothing -> return ()
          return v
        hit (content, mFileStat) = do
          tell $ mempty { wReadFiles = Map.singleton filePath [mFileStat] }
          return content

    case wFiles w of
        Just ioref ->
            do m <- M $ liftIO $ readIORef ioref
               case Map.lookup filePath m of
                   Nothing -> fmap fst miss
                   Just content -> hit content
        Nothing -> fmap fst miss

mergeAllFileStatuses :: FilePath -> [Maybe Posix.FileStatus] -> IO (Maybe Posix.FileStatus)
mergeAllFileStatuses filePath = go
  where
    go [] = fail "Empty list impossible"
    go [x] = return x
    go (x:xs) = do
      r <- go xs
      Meddling.assertSameMTime "When checking Makefile inputs" filePath x r
      return x

runM :: M a -> IO (ReadFiles, [PutStrLn], a)
runM (M act) = do
  ref <- newIORef Map.empty
  (res, w) <- runStateT act mempty { wFiles = Just ref }
  readFiles <- Map.traverseWithKey mergeAllFileStatuses (wReadFiles w)
  return (readFiles, wPutStrLns w, res)
