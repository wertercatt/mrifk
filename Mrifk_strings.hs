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


module Mrifk_strings (
	decodeString, strings
) where


import Mrifk_storyfile

import Data.Char (chr)
import Data.Bits (testBit)
import Control.Monad.State (evalState)


{------------------------------- Huffman table ----------------------------------}

data HuffNode =
  HuffBranch HuffNode HuffNode |
  HuffLiteral String | HuffStop |
  HuffIndir Int [Int] | HuffDoubleIndir Int [Int]
  deriving Show


huffmanTree = huffmanSubtreeAt (dwordAt (hdrDecodingTbl+8))

huffmanSubtreeAt n =
  case byteAt n of
    0 -> HuffBranch (huffmanSubtreeAt (dwordAt (n+1)))
                    (huffmanSubtreeAt (dwordAt (n+5)))
    1 -> HuffStop
    2 -> HuffLiteral [chr (byteAt (n+1))]
    3 -> HuffLiteral (map chr (takeWhile (/= 0) (bytesFrom (n+1))))
    8 -> HuffIndir (dwordAt (n+1)) []
    9 -> HuffDoubleIndir (dwordAt (n+1)) []
    10 -> HuffIndir (dwordAt (n+1)) (take (dwordAt (n+5)) (dwordsFrom (n+9)))
    11 -> HuffDoubleIndir (dwordAt (n+1)) (take (dwordAt (n+5)) (dwordsFrom (n+9)))


dwordsFrom n = evalFrom n (repeatUntilEmpty getDword)


-- I do this in the monad so that I can find where
-- the string ends by using getPos afterwards

huffDecode = huffDecode' huffmanTree []

huffDecode' branch@(HuffBranch _ _) [] =
  do bits <- getUByte
     huffDecode' branch (map (testBit bits) [0..7])

huffDecode' branch@(HuffBranch zero one) (bit:bits) =
  huffDecode' (if bit then one else zero) bits

huffDecode' (HuffLiteral s) bits =
  do rest <- huffDecode' huffmanTree bits
     return (s ++ rest)

huffDecode' HuffStop bits = return []


{-------------------------------  ----------------------------------}


strings :: [(Int,String)]

strings = evalState decodeStrings (hdrDecodingTbl + dwordAt hdrDecodingTbl, hdrRAMStart)


decodeStrings =
  do eos   <- isEOS
     if eos then return [] else do
     type_ <- peekUByte
     if type_ < 0xE0 then return [] else do
     pos   <- getPos
     s  <- decodeString
     ss <- decodeStrings
     return ((pos,s):ss)


decodeString =
  do type_ <- getUByte
     case type_ of
       0xE0 -> getCString
       0xE1 -> huffDecode
       0xE2 -> getCString

getCString =
  do x <- getUByte
     if x == 0 then return []
               else do rest <- getCString
                       return (chr x : rest)
