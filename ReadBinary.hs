module ReadBinary (
	BinaryData,
	readBinaryFile,
	byteAt
) where

import System.IO.Unsafe (unsafePerformIO)
import Foreign (peekElemOff)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.Types (CUChar,CInt(..),CLong(..),CSize(..),CFile)
import Foreign.C.String (CString,withCString)


type BinaryData = Ptr CUChar
readBinaryFile :: String -> IO (BinaryData,Int)
byteAt :: BinaryData -> Int -> Int


foreign import ccall "stdio.h fopen" fopen :: CString -> CString -> IO (Ptr CFile)
foreign import ccall "stdio.h fseek" fseek :: Ptr CFile -> CLong -> CInt -> IO CInt
foreign import ccall "stdio.h ftell" ftell :: Ptr CFile -> IO CLong
foreign import ccall "stdio.h fread" fread :: Ptr a -> CSize -> CSize -> Ptr CFile -> IO CSize
foreign import ccall "stdio.h fclose" fclose :: Ptr CFile -> IO CInt

fopen' name access =
  withCString name (\n ->
  withCString access (\a ->
  do f <- fopen n a
     if f == nullPtr
       then ioError (userError ("Unable to open the file " ++ name))
       else return f))


readBinaryFile name = do
  f <- fopen' name "rb"
  fseek f 0 2	-- seek to end of file
  len <- ftell f
  fseek f 0 0	-- seek back to beginning
  buf <- mallocBytes (fromIntegral len)
  fread buf 1 (fromIntegral len) f
  fclose f
  return (buf, fromIntegral len)


{-# INLINE byteAt #-}
buf `byteAt` ofs = fromEnum (unsafePerformIO (peekElemOff buf ofs))
