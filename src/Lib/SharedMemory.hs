{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.SharedMemory
  ( SharedMemory
  , RCSharedMemory
  , newSharedMemory
  , sharedMemorySendFD
  , sharedMemoryAddFile
  , recvReadonlySharedMemory
  , getShmemStrings
  ) where

import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Unsafe     as CU
import           Foreign.ForeignPtr           (ForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.C.Types
import           Foreign.Concurrent (newForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable (peek)
import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (packCStringLen)
import           Data.IORef (newIORef, readIORef, modifyIORef')

C.context (C.baseCtx <> C.bsCtx)
C.include "../../cbits/shared.h"

type SharedMemory = Ptr ()

newtype RCSharedMemory = RCSharedMemory (ForeignPtr ())

newSharedMemory :: IO SharedMemory
newSharedMemory = [CU.block| void *{ return new_shmem(); } |]

freeSharedMemory :: SharedMemory -> IO ()
freeSharedMemory sm = [CU.block| void {
        free_shmem((shmem_context *)$(void *sm));
    } |]

recvReadonlySharedMemory :: C.CInt -> IO RCSharedMemory
recvReadonlySharedMemory fd = do
    raw <- func
    fptr <- newForeignPtr raw $ do
        freeSharedMemory raw
    return $ RCSharedMemory fptr
  where
    func :: IO SharedMemory
    func = [CU.block| void *{
               return recv_readonly_shmem($(int fd));
         } |]

getShmemStrings :: RCSharedMemory -> IO [ByteString]
getShmemStrings (RCSharedMemory fshmem) = do
    withForeignPtr fshmem $ \shmem -> do
      iter <- [CU.block| void *{
        return shmem_blob_iterate_init((shmem_context *)$(void *shmem));
      } |]
      list <- newIORef []
      alloca $ \(cptr :: Ptr (Ptr CChar)) -> do
        alloca $ \(csize :: Ptr CInt) -> do
         let
           loop = do
             res <- [CU.block| int {
                 return shmem_blob_iterate_next((shmem_blob_iter *)$(void *iter),
                                                $(char **cptr),
                                                $(int *csize));
               }|]
             case res of
               0 -> do ptr <- peek cptr
                       size <- peek csize
                       str <- packCStringLen (ptr, (fromIntegral size))
                       modifyIORef' list ((:) str)
                       loop
               _ -> return ()
         loop
      readIORef list

sharedMemorySendFD :: SharedMemory -> C.CInt -> IO ()
sharedMemorySendFD sm fd = [CU.block| void {
    shmem_send_fd((shmem_context *)$(void *sm), $(int fd));
  } |]

sharedMemoryAddFile :: SharedMemory -> ByteString -> IO ()
sharedMemoryAddFile sm string = [CU.block| void {
  shmem_add_item_bs((shmem_context *)$(void *sm),
                      $bs-ptr:string, $bs-len:string
                      );
  } |]
