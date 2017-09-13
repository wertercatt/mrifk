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


module Mrifk_objects (
	Object(..),
	objects, numObjects, objectForest
) where


import Mrifk_strings
import Mrifk_storyfile
import Mrifk_memmap
import Mrifk_util

import Control.Monad (replicateM)
import Control.Monad.State (evalState)
import Data.Array
import Data.Bits (testBit)
import Data.Tree
import Data.Maybe (fromJust)


--                    name  attrs prop# priv data
data Object = Object String [Int] [(Int,Bool,[Int])]

type Object' = ((Int,Object),Int,Int,Int)


objectForest :: Forest Int

objectForest =
  map treeFrom roots
  where parents  = makeLookupTable [(a,p) | ((a,_),p,_,_) <- objectList]
        siblings = makeLookupTable [(a,s) | ((a,_),_,s,_) <- objectList]
        children = makeLookupTable [(a,c) | ((a,_),_,_,c) <- objectList]
        roots    = [a | ((a,_),0,_,_) <- objectList]
        treeFrom n =
          let kids = takeWhile (/= 0) $
                       iterate (fromJust . flip tableLookup siblings)
                               (fromJust $ tableLookup n children)
          in Node n (map treeFrom kids)


objects :: [(Int,Object)]
objects = [o | (o,_,_,_) <- objectList]

numObjects :: Int
numObjects = length objectList


objectList :: [Object']
objectList =
  evalState (repeatUntilEmpty getObject) informObjectTable

getObject =
  do addr     <- getPos
     typeByte <- getUByte  -- we already verified this is 0x70 when searching for the object table
     attribBytes <- replicateM numAttribBytes getUByte
     nextAddr <- getDword
     nameAddr <- getDword
     propAddr <- getDword
     parent   <- getDword
     sibling  <- getDword
     child    <- getDword
     let name    = evalFrom nameAddr decodeString
         attribs = decodeAttribsFrom 0 attribBytes
         props   = evalFrom propAddr getProps
     return ((addr, Object name attribs props), parent, sibling, child)

decodeAttribsFrom n [] = []
decodeAttribsFrom n (b:bs) =
  [n+i | i <- [0..7], testBit b i] ++ decodeAttribsFrom (n+8) bs

getProps =
  do numProps <- getDword
     replicateM numProps getProp

getProp =
  do id    <- getUWord
     len   <- getUWord
     addr  <- getDword
     flags <- getUWord
     let private = testBit flags 0
         data_   = evalFrom addr (replicateM len getDword)
     return (id,private,data_)
