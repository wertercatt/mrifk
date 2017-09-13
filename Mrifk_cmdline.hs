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


module Mrifk_cmdline (
	storyFileName, disassembleOnly
) where


import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,stderr)
import System.IO.Unsafe (unsafePerformIO)


storyFileName :: String
disassembleOnly :: Bool

(storyFileName,disassembleOnly) = unsafePerformIO parseArgs


parseArgs =
  do args <- getArgs
     case args of
       [story]      -> return (story,False)
       ["-S",story] -> return (story,True)
       _            -> usage


usage =
  do hPutStrLn stderr "usage: mrifk [-S] storyfile.ulx"
     hPutStrLn stderr "  -S  disassemble only (omit decompilation pass)"
     exitFailure
