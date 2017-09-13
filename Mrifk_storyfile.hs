{-
Mrifk, a decompiler for Glulx story files.
Copyright 2004 Ben Rudiak-Gould.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You can read the GNU General Public License at this URL:
     http://www.gnu.org/copyleft/gpl.html
-}


module Mrifk_storyfile (
	wholeFile,
	byteAt, dwordAt, bytesFrom,
	getPos, isEOS, peekUByte, getUByte, getBytes,
	getSByte, getUWord, getSWord, getDword,
	repeatUntilEmpty, evalFrom, evalFromTo, fromTo,
	hdrMagic, hdrVersion, hdrRAMStart,
	hdrExtStart, hdrStartFunc, hdrDecodingTbl
) where


import qualified ReadBinary

import Mrifk_util
import Mrifk_cmdline

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State (State,get,put,evalState)
import Foreign (plusPtr)


(inputFile,inputFileLen) =
  handleBlorb $ unsafePerformIO $
    ReadBinary.readBinaryFile storyFileName

byteAt n = ReadBinary.byteAt inputFile n

wholeFile :: DataBlock
wholeFile = (0,inputFileLen)

from :: Int -> DataBlock
from n = onFst (+n) wholeFile

fromTo :: Int -> Int -> DataBlock
fromTo n k = (n,k)

evalFrom n act = evalState act (from n)
evalFromTo n k act = evalState act (fromTo n k)

dwordAt n = evalFrom n getDword

bytesFrom n = evalFrom n (repeatUntilEmpty getUByte)


{-----------}


hdrMagic       = dwordAt 0	-- "Glul"
hdrVersion     = dwordAt 4
hdrRAMStart    = dwordAt 8
hdrExtStart    = dwordAt 12
hdrEndMem      = dwordAt 16
hdrStackSize   = dwordAt 20
hdrStartFunc   = dwordAt 24
hdrDecodingTbl = dwordAt 28

hdrExtensionMagic         = dwordAt 36	-- "Info"
hdrExtensionFormatVersion = dwordAt 40
hdrExtensionInformVersion = dwordAt 44


{-------------}


handleBlorb :: (ReadBinary.BinaryData, Int)
            -> (ReadBinary.BinaryData, Int)

handleBlorb (p,size) =
  if dwordAt 0 /= 0x464F524D then
    (p,size)
  else if dwordAt 8 /= 0x49465253 || dwordAt 12 /= 0x52496478 then
    error "Unrecognized blorb file format"
  else
    let numResources = dwordAt 16
        resources = take numResources
                     [(dwordAt n, dwordAt (n+8)) | n <- [24,36..]]
    in case [pos | (0x45786563,pos) <- resources] of
         []    -> error "No story file in blorb"
         [pos] -> case dwordAt pos of
                    0x5A434F44 -> error "This appears to be a Z-machine blorb. Try Reform."
                    0x474C554C -> (p `plusPtr` (pos+8), dwordAt (pos+4))
                    _          -> error "Unrecognized blorb file format"
         _     -> error "More than one story file found. You'll have to extract one by hand."
  where
    byteAt  n = ReadBinary.byteAt p n
    dwordAt n = byteAt n * 16777216
              + byteAt (n+1) * 65536
              + byteAt (n+2) * 256
              + byteAt (n+3)


{----------}


type DataBlock = (Int,Int)

type StreamReader a = State DataBlock a

getPos :: StreamReader Int
getPos =
  do (a,z) <- get
     return a

isEOS :: StreamReader Bool
isEOS =
  do (a,z) <- get
     return (a >= z)

peekUByte :: StreamReader Int
peekUByte =
  do (a,z) <- get
     return (byteAt a)

getUByte :: StreamReader Int
getUByte =
  do (a,z) <- get
     put (a+1,z)	-- should probably bounds-check
     return (byteAt a)

getBytes :: Int -> StreamReader DataBlock
getBytes n =
  do (a,z) <- get
     put (a+n,z)	-- definitely bounds-check
     return (a,a+n)

getSByte :: StreamReader Int
getSByte =
  do x <- getUByte
     return (if x < 128 then x else x - 256)

getUWord :: StreamReader Int
getUWord =
  do a <- getUByte
     b <- getUByte
     return (a*256+b)

getSWord :: StreamReader Int
getSWord =
  do a <- getSByte
     b <- getUByte
     return (a*256+b)

getDword :: StreamReader Int
getDword =
  do hi <- getSWord
     lo <- getUWord
     return (hi*65536+lo)

repeatUntilEmpty :: StreamReader a -> StreamReader [a]
repeatUntilEmpty action =
  do eos <- isEOS
     if eos then
       return []
      else do
       first <- action
       s <- get
       let lazyRest = evalState (repeatUntilEmpty action) s
       return (first : lazyRest)
