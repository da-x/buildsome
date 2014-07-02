{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.Makefile.Monad
  ( MonadMakefileParser(..)
  , M, runM
  ) where

import Control.Applicative (Applicative)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State (StateT(..))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lib.Directory (getMFileStatus)
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Buildsome.Meddling as Meddling
import qualified Control.Exception as E
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

class Monad m => MonadMakefileParser m where
  outPutStrLn :: ByteString -> m ()
  errPutStrLn :: ByteString -> m ()
  tryReadFile :: FilePath -> m (Either E.SomeException ByteString)

instance MonadMakefileParser IO where
  outPutStrLn = BS8.putStrLn
  errPutStrLn = BS8.hPutStrLn IO.stderr
  tryReadFile = E.try . BS8.readFile . BS8.unpack

-- Specific Monad type for makefile parsing that tracks all inputs/outputs:
data PutStrLnTarget = Out | Err

targetHandle :: PutStrLnTarget -> IO.Handle
targetHandle Out = IO.stdout
targetHandle Err = IO.stderr

type ReadFiles = Map FilePath (Maybe Posix.FileStatus)

data W = W
  { wPutStrLns :: [(PutStrLnTarget, ByteString)]
  , wReadFiles :: Map FilePath [Maybe Posix.FileStatus]
  }
instance Monoid W where
  mempty = W mempty mempty
  mappend (W ax ay) (W bx by) =
    W (mappend ax bx) (Map.unionWith (++) ay by)

runPutStrLn :: (PutStrLnTarget, ByteString) -> IO ()
runPutStrLn (target, bs) = BS8.hPutStrLn (targetHandle target) bs

-- IMPORTANT: Use StateT and not WriterT, because WriterT's >>= leaks
-- stack-space due to tuple unpacking (and mappend) *after* the inner
-- bind, whereas StateT uses a tail call for the inner bind.
newtype M a = M (StateT W IO a)
  deriving (Functor, Applicative, Monad)
tell :: W -> M ()
tell = M . State.modify . mappend

instance MonadMakefileParser M where
  outPutStrLn bs = tell $ mempty { wPutStrLns = [(Out, bs)] }
  errPutStrLn bs = tell $ mempty { wPutStrLns = [(Err, bs)] }
  tryReadFile filePath = do
    mFileStat <- M $ liftIO $ getMFileStatus filePath
    tell $ mempty { wReadFiles = Map.singleton filePath [mFileStat] }
    M $ liftIO $ tryReadFile filePath

mergeAllFileStatuses :: FilePath -> [Maybe Posix.FileStatus] -> IO (Maybe Posix.FileStatus)
mergeAllFileStatuses filePath = go
  where
    go [] = fail "Empty list impossible"
    go [x] = return x
    go (x:xs) = do
      r <- go xs
      Meddling.assertSameMTime filePath x r
      return x

runM :: M a -> IO (ReadFiles, a)
runM (M act) = do
  (res, w) <- runStateT act mempty
  forM_ (wPutStrLns w) runPutStrLn
  readFiles <- Map.traverseWithKey mergeAllFileStatuses (wReadFiles w)
  return $ (readFiles, res)