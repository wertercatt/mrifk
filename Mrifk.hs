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


module Main (
    main
) where


import Mrifk_cmdline
import Mrifk_storyfile
import Mrifk_disasm
import Mrifk_decompile
import Mrifk_strings
import Mrifk_memmap
import Mrifk_grammar
import Mrifk_objects
import Mrifk_print

import Control.Monad.State (evalState)
import Data.Array
import Data.Tree
import Data.Maybe (fromJust)
import Numeric (showHex)


mrifkRelease = "1-patched-v2"
mrifkSerial  = "160321"


main =
  do putStrLn ("! \""++storyFileName++"\"")
     putStrLn ("! Decompiled by Mrifk release "++mrifkRelease++", serial "++mrifkSerial)
--     putStrLn "\n! Dictionary\n"
--     mapM_ print (elems dictionary)
     putStrLn "\n! Grammar"
     mapM_ putStrLn (ppVerbs verbs)
     putStrLn "\n! Object tree\n"
     mapM_ putStrLn (concatMap ppObjectTree objectForest)
     putStrLn "\n! Routines"
     mapM_ putStrLn ppRoutines
     putStrLn "\n! Strings\n"
     mapM_ putStrLn ppStrings


routines = map (onFth4 maybeDecompile) $ evalState disasmRoutines informCode

maybeDecompile | disassembleOnly = updateLabels
               | otherwise       = decompile

onFth4 f (a,b,c,d) = (a,b,c,f d)


ppRoutines = concatMap ppRoutine routines

ppStrings = map ppString strings

ppString (addr,str) = "! 0x" ++ showHex addr (' ' : ppQuotedString str)


ppObjectTree = ppObjectTree' 0

ppObjectTree' depth (Node obj children) =
  ppObject depth obj (fromJust (lookup obj objects))
    ++ concatMap (ppObjectTree' (depth+1)) children
