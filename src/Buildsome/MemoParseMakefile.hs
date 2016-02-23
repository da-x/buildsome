{-# LANGUAGE NoImplicitPrelude #-}
module Buildsome.MemoParseMakefile
  ( IsHit(..), memoParse
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.Db (Db)
import Buildsome.FileContentDescCache (fileContentDescOfStat)


import Data.Map (Map)
import Lib.FileDesc (FileContentDesc)
import Lib.FilePath (FilePath)
import Lib.Makefile (Makefile, Vars)
import qualified Buildsome.Db as Db
import qualified Buildsome.Meddling as Meddling

import qualified Data.Map as Map

import qualified Lib.Makefile as Makefile
import qualified Lib.Makefile.Monad as MakefileMonad
import qualified System.Posix.ByteString as Posix

mkFileDesc :: Db -> FilePath -> Maybe Posix.FileStatus -> IO (Db.FileDesc () FileContentDesc)
mkFileDesc _ _ Nothing = return $ Db.FileDescNonExisting ()
mkFileDesc db path (Just stat) =
  Db.FileDescExisting <$>
  fileContentDescOfStat "When parsing Makefile" db path stat

parse :: Db -> FilePath -> Vars -> IO (Map FilePath (Db.FileDesc () FileContentDesc), [MakefileMonad.PutStrLn], Makefile)
parse db absMakefilePath vars = do
  (readFiles, putStrLns, res) <-
    MakefileMonad.runM $
    Makefile.parse absMakefilePath vars
  contentDescs <- Map.traverseWithKey (mkFileDesc db) readFiles
  -- This must come after:

  -- TODO: This seems redundant, but is it? The mkFileDesc only
  -- asserts mtimes if it's not from cache
  mapM_ (uncurry (Meddling.assertFileMTime "When parsing Makefile")) $
    Map.toList readFiles
  return (contentDescs, putStrLns, res)

data IsHit = Hit | Miss

memoParse :: Db -> FilePath -> Vars -> IO (IsHit, Makefile)
memoParse db absMakefilePath vars = do
  (_, _, makefile) <- parse db absMakefilePath vars
  return (Miss, makefile)
