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


module Mrifk_print (
	ppRoutine, ppObject, ppQuotedString, ppVerbs
) where


import Mrifk_code
import Mrifk_grammar
import Mrifk_objects
import Mrifk_memmap
import Mrifk_storyfile
import Mrifk_strings
import Mrifk_util

import Data.Char (ord,isDigit)
import Data.Array
import Data.Ix (inRange)
import Data.Maybe (fromMaybe)
import Numeric (showHex)


{--------------}


ppRoutine (addr,type_,params,body) =
  (if type_ == 193 then [] else ["! Stack-parameter routine"])
  ++ (if all (== 4) params
        then []
        else ["! Local lengths: " ++ unwords [show n | n <- params]])
  ++ (unwords ("[" : nameRoutine addr : [nameLocal n | n <- scanl (+) 0 params | _ <- params] ++ [";"]))
   : indentBlock (ppStatements body)
  ++ ["];",""]


ppStatements x@(Print _ _ : _) =
  let (prints,rest) = break (not.isPrint) x
      (isRet,rest') = case rest of
                        (NewLine : Eval "return " (Imm 1) : rest') -> (True,rest')
                        _ -> (False,rest)
  in  (ppPrints prints isRet ++ ";") : ppStatements rest'

ppStatements (x : xs) = ppStatement x ++ ppStatements xs

ppStatements [] = []


ppPrints [Print "" s@(ImmString _)] True =
  ppExpr 0 s

ppPrints prints isRet =
  let prefix = if isRet then "print_ret " else "print "
      args = map ppPrint prints
  in  prefix ++ join ", " args

isPrint (Print _ _) = True
isPrint _           = False
ppPrint (Print prefix val) = prefix ++ ppExpr 1 val


ppStatement (Label addr Phantom) = []
ppStatement (Label addr _)  = ["<." ++ nameLabel addr ++ ":"]

ppStatement (GInstr (name,loads,stores,branches,_) args) =
  let prefixes = replicate loads "" ++ replicate stores "-> " ++ replicate branches "label"
      printableArgs = zipWith (++) prefixes (map (ppExpr 99) args)
  in  [join " " (('@' : name) : printableArgs) ++ ";"]

ppStatement (Eval cmd expr) = [cmd ++ ppExpr 0 expr ++ ";"]

ppStatement (Push expr) = ["{{{PUSH}}} " ++ ppExpr 0 expr ++ ";"]

ppStatement NewLine = ["new_line;"]

ppStatement (IfThenElse cond thenClause []) =
  ("if (" ++ ppExpr 0 cond ++ ") {") : indentBlock (ppStatements thenClause) ++ ["}"]

ppStatement (IfThenElse cond thenClause elseClause) =
  let elseClauseText = ppStatements elseClause
      elseText = case elseClauseText of
                   (line@('i':'f':' ':_) : lines@(_:_)) -> ("} else " ++ line) : lines
                   _ -> "} else {" : indentBlock elseClauseText ++ ["}"]
  in ("if (" ++ ppExpr 0 cond ++ ") {") : indentBlock (ppStatements thenClause) ++ elseText

ppStatement (JCond cond label) =
  ["if (" ++ ppExpr 0 cond ++ ") " ++ ppJump label ++ ";"]

ppStatement (Jump label) =
  [ppJump label ++ ";"]

ppStatement (Give obj b attr) =
  ["give " ++ ppExpr 0 obj ++ (if b then " " else " ~") ++ nameAttr attr ++ ";"]

ppStatement x = ["{{{" ++ show x ++ "}}}"]


ppJump 0 = "rfalse"
ppJump 1 = "rtrue"
ppJump label = "jump " ++ nameLabel label


ppExpr prec (Assign dst src) =
  parenIf (1 <= prec) (ppExpr 1 dst ++ " = " ++ ppExpr 0 src)

ppExpr prec (Call func args) =
  parenIf (11 <= prec) (ppExpr 10 func ++ "(" ++ join "," (map (ppExpr 0) args) ++ ")")

ppExpr prec (Binary left op right) =
  let p = binopPrec op in
  parenIf (p <= prec) (ppExpr (p-1) left ++ binopName op ++ ppExpr p right)

ppExpr prec (Unary op opPrec expr) =
  parenIf (opPrec <= prec) (op ++ ppExpr (opPrec-1) expr)

ppExpr prec (PostIncDec expr op) =
  parenIf (9 <= prec) (ppExpr 8 expr ++ op)

ppExpr prec SP = "sp"
ppExpr prec (Imm n)   = ppValue n
ppExpr prec (ImmString n)  = ppQuotedString (evalFrom n decodeString)	-- FIXME: quote
ppExpr prec (Mem n)   = "mem" ++ show n
ppExpr prec (Local n) = nameLocal n
ppExpr prec (SpecialName x) = x

-- ppExpr prec foo = "{{{" ++ show foo ++ "}}}"


{--------------}


ppObject treeDepth n (Object name attribs props) =
  "" : firstLine : indentWith (map ppProp props)
     ++ ["  has\t" ++ ppAttribs attribs ++ ";"]
  where
    firstLine = "Object" ++ take (3*treeDepth+1) (cycle " ->")
                 ++ nameObject n ++ ' ' : ppQuotedString name


ppProp (n, private, vals) =
  unwords (nameProp n : map ppValue vals) ++ ";"


ppAttribs attribs =
  join " " [nameAttr attr | attr <- attribs]

indentWith [] = []
indentWith (firstLine : lines) =
  ("  with\t" ++ firstLine) : map ('\t' :) lines


{--------------}


-- FIXME: leaves something to be desired

ppValue val
  | val <= 16 || not (inRange wholeFile val)
      = show val	-- FIXME: negatives?
  | otherwise
      = case byteAt val of
          0x70 -> nameObject val
          0x60 -> ppDictWord (dictWordAt val)
          0xC0 -> nameRoutine val
          0xC1 -> nameRoutine val
          0xE0 -> ppQuotedString (evalFrom val decodeString)
          0xE1 -> ppQuotedString (evalFrom val decodeString)
          _    -> show val


{--------------}


ppQuotedString s = '"' : ppString s ++ "\""

ppString "" = ""
ppString (x : rest)
  | x == '\10'   = '^' : ppString rest
  | x == '"'     = '~' : ppString rest
  | x <= '\x153' = (informEscapes ! x) ++ ppString rest
  | otherwise    = informEscapeChar x (ppString rest)

ppString' "" = ""
ppString' (x : rest)
  | x == '\''    = '^' : ppString' rest
  | x == '"'     = '"' : ppString' rest
  | x <= '\x153' = (informEscapes ! x) ++ ppString' rest
  | otherwise    = informEscapeChar x (ppString' rest)


-- correct behavior of showHex requires GHC 6.2
informEscapeChar x rest =
  "@{" ++ showHex (ord x) ('}' : rest)


informEscapes :: Array Char String

informEscapes =
  accumArray (\a b -> b) undefined ('\0','\x153') $
             [(x,[x]) | x <- ['\32'..'\126']]
              ++ [(x,informEscapeChar x "") | x <- ['\0'..'\31'] ++ "^~@\"" ++ ['\127'..'\x153']]
              ++ informSpecialEscapes


informSpecialEscapes :: [(Char,String)]
informSpecialEscapes =
 [('\xE4',"@:a"), ('\xF6',"@:o"), ('\xFC',"@:u"),
  ('\xC4',"@:A"), ('\xD6',"@:O"), ('\xDC',"@:U"),
  ('\xDF',"@ss"), ('\xAB',"@>>"), ('\xBB',"@<<"),
  ('\xEB',"@:e"), ('\xEF',"@:i"), ('\xFF',"@:y"),
  ('\xCB',"@:E"), ('\xCF',"@:I"), ('\xE1',"@'a"),
  ('\xE9',"@'e"), ('\xED',"@'i"), ('\xF3',"@'o"),
  ('\xFA',"@'u"), ('\xFD',"@'y"), ('\xC1',"@'A"),
  ('\xC9',"@'E"), ('\xCD',"@'I"), ('\xD3',"@'O"),
  ('\xDA',"@'U"), ('\xDD',"@'Y"), ('\xE0',"@`a"),
  ('\xE8',"@`e"), ('\xEC',"@`i"), ('\xF2',"@`o"),
  ('\xF9',"@`u"), ('\xC0',"@`A"), ('\xC8',"@`E"),
  ('\xCC',"@`I"), ('\xD2',"@`O"), ('\xD9',"@`U"),
  ('\xE2',"@^a"), ('\xEA',"@^e"),
  ('\xEE',"@^i"), ('\xF4',"@^o"),
  ('\xFB',"@^u"), ('\xC2',"@^A"),
  ('\xCA',"@^E"), ('\xCE',"@^I"),
  ('\xD4',"@^O"), ('\xDB',"@^U"),
  ('\xE5',"@oa"), ('\xC5',"@oA"),
  ('\xF8',"@/o"), ('\xD8',"@/O"),
  ('\xE3',"@~a"), ('\xF1',"@~n"), ('\xF5',"@~o"),
  ('\xC3',"@~A"), ('\xD1',"@~N"), ('\xD5',"@~O"),
  ('\xE6',"@ae"), ('\xC6',"@AE"),
  ('\xE7',"@cc"), ('\xC7',"@cC"),
  ('\xFE',"@th"), ('\xF0',"@et"), ('\xDE',"@Th"), ('\xD0',"@Et"),
  ('\xA3',"@LL"),
  ('\x0153',"@oe"), ('\x0152',"@OE"),
  ('\xA1',"@!!"), ('\xBF',"@??")]


{---------------}


ppVerbs :: [([String],[GrammarLine])] -> [String]

ppVerbs = concatMap ppVerb

ppVerb (verbs,grammars) =
  -- FIXME: check meta
  "" : unwords ("Verb" : map ppDictWord verbs)
     : map ppVerbGrammarLine grammars ++ [";"]

ppVerbGrammarLine (GrammarLine tokens action reverse) =
  tab 7 ("    * " ++ unwords (map ppVerbToken tokens))
        ("-> " ++ nameAction action ++ if reverse then " reverse" else "")

-- FIXME
ppVerbToken (ElementaryToken s) = s
ppVerbToken (Preposition p) = ppDictWord p
ppVerbToken (Attribute n) = nameAttr n
ppVerbToken (ParseRoutine prefix n) = prefix ++ nameRoutine n
ppVerbToken (Alternatives tokens) =
  join "/" (map ppVerbToken tokens)


ppDictWord w = '\'' : ppString' w ++ "'"


{--------------}


parenIf True x  = '(' : x ++ ")"
parenIf False x = x


nameObject :: Int -> String
nameObject 0 = "nothing"
nameObject n =
  case tableLookup n objectNames of
    Just name -> name
    Nothing   -> "unknownObj" ++ show n

objectNames =
  makeLookupTable $
    -- earlier in the list takes precedence
    map guessObjName objects

guessObjName (n,obj) =
  case obj of
    Object "" _ _   -> (n,baseName)
    Object desc _ _ -> (n,baseName ++ '_' : makeIdent desc)
  where baseName = "obj" ++ show n

makeIdent name =
  map helper name
  where helper c = if isLegalIdentChar c then c else '_'

isLegalIdentChar c =
  inRange ('0','9') c || inRange ('A','Z') c || inRange ('a','z') c


nameProp n =
  maybeIdx commonPropNames n $
  maybeIdx indivPropNames n $
  "prop" ++ show n

nameAttr n =
  maybeIdx attribNames n $
  "attr" ++ show n

nameAction n =
  maybeIdx actionNames n $
  "Action" ++ show n

nameRoutine :: Int -> String
nameRoutine addr =
  fromMaybe ("routine" ++ show addr)
            (addr `tableLookup` routineNames)

routineNames =
  makeLookupTable $
    -- earlier in the list takes precedence
    [(addr, nameAction n ++ "Sub")
       | addr <- actionRoutines | n <- [0..]]

nameLocal n   = "local" ++ show n

nameLabel n   = "label" ++ show n


maybeIdx array n def
  | inRange (bounds array) n  = fromMaybe def (array ! n)
  | otherwise                 = def


indentBlock = map indentBlock'

indentBlock' ('<':rest) = ' ':' ':rest
indentBlock' rest = ' ':' ':' ':' ':rest


join sep [] = []
join sep xs = foldr1 (\x y -> x ++ sep ++ y) xs


tab :: Int -> String -> String -> String
tab n x y =
  x ++ makeTabs (n - length x `div` 8) ++ y

makeTabs n | n <= 0    = " "
           | otherwise = replicate n '\t'
