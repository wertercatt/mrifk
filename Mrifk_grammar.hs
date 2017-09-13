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


module Mrifk_grammar (
	GrammarLine(..), GrammarToken(..),
	verbs, actionRoutines,
	DictWordTag(..),
	dictionary,
	dictWordAt, isDictWord
) where


import Mrifk_storyfile
import Mrifk_memmap


import Data.Char (chr)
import Control.Monad
import Data.Array
import Data.Bits
import Data.Maybe (isJust)
import Numeric (showHex)


--                             words after verb action reverse
data GrammarLine = GrammarLine [GrammarToken]   Int    Bool
  deriving Show


verbs :: [([String],[GrammarLine])]

verbs = reverse (zip verbWords grammarEntries)

verbWords =
  [[x | (x,_,tags) <- elems dictionary, DictVerb n <- tags, n == n']
   | n' <- [255,254..1]]


grammarEntries :: [[GrammarLine]]
actionRoutines :: [Int]
dictionary     :: Array Int (String,String,[DictWordTag])
numEntries,entryBaseAddr,entryLength :: Int

(grammarEntries, actionRoutines,
  (dictionary,numEntries,entryBaseAddr,entryLength)) =
    evalFrom informGrammarTable parseGV2


data GrammarToken = ElementaryToken String | Preposition String
                  | Attribute Int | ParseRoutine String Int
                  | Alternatives [GrammarToken]
  deriving (Show,Eq,Ord)


{-----------}


parseGV2 =
  do numVerbs     <- getDword
     verbGrammars <- replicateM numVerbs getDword
     entries      <- mapM parseGV2Verb verbGrammars
     numActionRoutines <- getDword
     actionRoutines    <- replicateM numActionRoutines getDword
     dictionary   <- parseDictionary
     return (entries, actionRoutines, dictionary)

parseGV2Verb addr =
  do pos <- getPos
     if pos == addr then
       do numLines <- getUByte
          replicateM numLines parseGV2Line
      else
       error ("Grammar table not contiguous: next pointer is 0x" ++ showHex addr (", but expected 0x" ++ showHex pos ""))

parseGV2Line = do
  actionNum <- getUWord
  flags     <- getUByte
  let reverseParams = testBit flags 0
  tokens <- parseGV2Tokens
  return $ GrammarLine (groupGV2Tokens tokens) actionNum reverseParams

parseGV2Tokens = do
  tokenType <- getUByte
  if tokenType == 15
    then return []
    else do
      tokenData <- getDword
      tokens <- parseGV2Tokens
      return ((tokenType .&. 0x30,parseGV2Token (tokenType .&. 15) tokenData) : tokens)

parseGV2Token 1 d = ElementaryToken (elementaryTokenTypes !! d)
parseGV2Token 2 d = Preposition (dictWordAt d)
parseGV2Token 3 d = ParseRoutine "noun=" d
parseGV2Token 4 d = Attribute d
parseGV2Token 5 d = ParseRoutine "scope=" d
parseGV2Token 6 d = ParseRoutine "" d


elementaryTokenTypes =
 ["noun", "held", "multi", "multiheld", "multiexcept",
  "multiinside", "creature", "special", "number", "topic"]


groupGV2Tokens ((0,x):rest) =
  x : groupGV2Tokens rest
groupGV2Tokens ((32,x):rest) =
  Alternatives (x : map snd xs) : groupGV2Tokens rest'
    where (xs,rest') = break (\(n,_) -> not (testBit n 4)) rest
groupGV2Tokens ((n,x):rest) = error (show n)
groupGV2Tokens [] = []


{---------------}


-- Dictionary


data DictWordTag
  = DictVerb Int | DictPrep
  | DictNoun | DictPlural | DictMeta
  | DictMaybeTruncated
  deriving (Show,Eq)


parseDictionary = do
  numWords  <- getDword
  pos       <- getPos
  case filter (possibleDictLength pos numWords) [8..100] of
    []      -> error "Unable to determine dictionary word length"
    (len:_) ->
      do words <- replicateM numWords (getDictWord len)
         return (listArray (0,numWords-1) words, numWords, pos, len)

getDictWord len =
  do _         <- getUByte	-- 0x60, already checked
     wordBytes <- replicateM (len-7) getUByte
     let word      = map chr (takeWhile (/= 0) wordBytes)
         truncated = if last wordBytes == 0 then [] else [DictMaybeTruncated]
     flags     <- getUWord
     verbNum   <- getUWord
     unused    <- getUWord
     let tags = truncated ++ parseFlags flags verbNum
     return (mungeDictWord word tags, word, tags)


parseFlags flags verbNum =
     (if testBit flags 0 then [DictVerb verbNum] else [])
  ++ (if testBit flags 3 then [DictPrep] else [])
  ++ (if testBit flags 7 then [DictNoun] else [])
  ++ (if testBit flags 2 then [DictPlural] else [])
  ++ (if testBit flags 1 then [DictMeta] else [])


mungeDictWord word tags = word' ++ attr
  where word' = {-sfFullDictWord-} word
        attr | '/' `elem` word'        = ""
             | DictPlural `elem` tags  = "//p"
             | length word == 1        = "//"
             | otherwise               = ""


possibleDictLength pos numWords len =
  (pos + numWords * len <= snd wholeFile)
   && all (== 0x60) (map byteAt (take numWords [pos,pos+len..]))


dictWordAt :: Int -> String

dictWordAt addr =
  case addrToDictWordIndex addr of
    Just n  -> case dictionary!n of (word,_,_) -> word
    Nothing -> "unknownDictWord" ++ show addr


addrToDictWordIndex addr
  | addr < entryBaseAddr  = Nothing
  | otherwise =
      case (addr - entryBaseAddr) `divMod` entryLength of
        (d,0) -> if d < numEntries then Just d else Nothing
        (_,_) -> Nothing


isDictWord addr = isJust (addrToDictWordIndex addr)
