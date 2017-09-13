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


module Mrifk_decompile (
	decompile, updateLabels
) where


import Mrifk_code
import Mrifk_util

import Data.List (sort)


updateLabels code = updateLabels' (sort (findJumps code)) code

updateLabels' jumps (Label addr oldType : rest) =
  let (jumpsHere,jumpsLater) = break (> addr) (dropWhile (< addr) jumps)
      rest' = updateLabels' jumpsLater rest
  in updateLabel addr oldType jumpsHere ++ rest'

updateLabels' jumps (IfThenElse expr a b : rest) =
  IfThenElse expr (updateLabels' jumps a) (updateLabels' jumps b) : updateLabels' jumps rest

updateLabels' jumps (x:xs) = x : updateLabels' jumps xs
updateLabels' jumps [] = []

updateLabel ad _       []  = []
updateLabel ad Phantom _   = [Label ad Phantom]
updateLabel ad _       [_] = [Label ad Single]
updateLabel ad _       _   = [Label ad Multi]


findJumps = concatMap findJumps'

findJumps' (GInstr (_,_,_,0,_) _) = []
findJumps' (GInstr (_,loads,stores,_,_) args) =
  [x | Imm x <- drop (loads+stores) args]
findJumps' (JCond _ target) = [target]
findJumps' (Jump target) = [target]
findJumps' (IfThenElse _ a b) = findJumps a ++ findJumps b
findJumps' (Give _ _ _) = []
findJumps' (Label _ _) = []
findJumps' (Push _) = []
findJumps' (Eval _ _) = []
findJumps' (Print _ _) = []
findJumps' NewLine = []
-- findJumps' x = error (show x)


{------------------------------- decompiler ----------------------------------}


doToDeath f code =
  findFixedPoint (iterate (f . updateLabels) code)
    where findFixedPoint (x:y:z)
            | x == y    = x
            | otherwise = findFixedPoint (y:z)


decompile = doToDeath decompileJumps . doToDeath decompile'


decompile' (JCond cond l : Push (Imm 0) : Jump m : Label l' Single : Push (Imm 1) : rest@(Label m' _ : _)) | l == l' && m == m' =
  decompile' (Push (negateExpr (negateExpr cond)) : rest)


decompile' (IfThenElse cond thenClause elseClause : rest) =
  IfThenElse cond (decompile' thenClause) (decompile' elseClause) : decompile' rest


decompile' (JCond cond1 label : JCond cond2 label' : rest) | label == label'  =
  decompile' (JCond (Binary cond1 binopOr cond2) label : rest)

decompile' (JCond cond1 label1 : JCond cond2 label2 : rest@(Label label1' _ : _)) | label1 == label1'  =
  decompile' (JCond (Binary (negateExpr cond1) binopAnd cond2) label2 : rest)


decompile' (Push (Local n) : Eval "" (Assign (Local n') (Binary (Local n'') (NormalOp op 5) (Imm 1))) : rest) | n == n' && n == n''  =
  decompile' (Push (PostIncDec (Local n) (incDecOp op)) : rest)

decompile' (Push (Mem n) : Eval "" (Assign (Mem n') (Binary (Mem n'') (NormalOp op 5) (Imm 1))) : rest) | n == n' && n == n''  =
  decompile' (Push (PostIncDec (Mem n) (incDecOp op)) : rest)


-- FIXME: Might want to use stkswap as a clue that
-- the compiler is doing something special
decompile' (Push expr1 : Push expr2 : GInstr (_,_,_,_,OStkSwap) _ : rest) =
  decompile' (Push expr2 : Push expr1 : rest)


decompile' (Push expr : (GInstr info@(_,loads,_,_,_) args) : rest)
  | not (ready loads args) = decompile' (GInstr info (substSP expr args) : rest)

decompile' (instr@(GInstr (_,loads,_,_,type_) args) : rest)
  | ready loads args = case decompileReady type_ args of
                         Just instr' -> decompile' (instr' : rest)
                         Nothing     -> instr : decompile' rest
  | otherwise        = instr : decompile' rest

decompile' (x:xs) = x : decompile' xs
decompile' [] = []


incDecOp " + " = "++"
incDecOp " - " = "--"


ready loads args =
  all (/= SP) (take loads args)

substSP expr (SP:rest) = expr:rest
substSP expr (x:rest)  = x:substSP expr rest


decompileJumps (first@(JCond cond midLabel) : rest) =
  case findMatch (findIfThenElse midLabel) rest of
    Just (thenClause,(elseClause,(endLabel,rest'))) ->
      let exit = [Label endLabel Phantom] in
      IfThenElse (negateExpr cond) (thenClause ++ exit) (elseClause ++ exit) : decompileJumps rest'
    Nothing ->
      case findMatch (findIfThen midLabel) rest of
        Just (thenClause,rest') ->
          let exit = [Label midLabel Phantom] in
          IfThenElse (negateExpr cond) (thenClause ++ exit) [] : decompileJumps rest'
        Nothing -> first : decompileJumps rest

decompileJumps (IfThenElse cond thenClause elseClause : rest) =
  IfThenElse cond (decompileJumps thenClause) (decompileJumps elseClause) : decompileJumps rest

decompileJumps (x:xs) = x : decompileJumps xs
decompileJumps [] = []



findMatch = findMatch' id
findMatch' before f [] = Nothing
findMatch' before f (x:xs) =
  case f (x:xs) of
    Just result -> Just (before [],result)
    Nothing     -> findMatch' (before.(x:)) f xs

findIfThenElse l (Jump endLabel : Label l' Single : rest) | l == l'  =
  findMatch findElse rest where
    findElse all@(Label endLabel' _ : rest) | endLabel == endLabel' = Just (endLabel,all)
    findElse _ = Nothing

findIfThenElse _ _ = Nothing

findIfThen l all@(Label l' _ : _) | l == l'  = Just all
findIfThen _ _ = Nothing


negateExpr (Binary left (LogicalOp op notOp) right) =
  Binary (negateExpr left) (LogicalOp notOp op) (negateExpr right)

negateExpr (Binary left (PredicateOp op notOp) right) =
  Binary left (PredicateOp notOp op) right

negateExpr x = Unary "~~" 2 x


decompileReady type_@(OJCond _) [val,label] =
  decompileReady type_ [val,Imm 0,label]

decompileReady (OJCond op) [val1,val2,Imm label] =
  Just (JCond (Binary val1 op val2) label)

decompileReady (OBinary op) [left,right,dst] =
  Just (pushOrStore dst (Binary left op right))

decompileReady OJump [Imm label] =
  Just (Jump label)

decompileReady OReturn [expr] = Just (Eval "return " expr)

decompileReady OStreamStr [Imm s] = Just (Print "" (ImmString s))
decompileReady OStreamStr [s]     = Just (Print "(s) " s)

decompileReady OStreamNum [n] = Just (Print "" n)

decompileReady OStreamChar [Imm 10] = Just NewLine
decompileReady OStreamChar [ch]     = Just (Print "(char) " ch)

decompileReady OCopy [src,dst] =
  Just (pushOrStore dst src)

decompileReady OCallI (func:rest) =
  let (args,store) = separateStore rest
  in  Just (pushOrStore store (Call func args))

decompileReady OCall [func,Imm argc,dest] =
  Just (GInstr ("callfi*",argc+1,1,0,OCallI) (func : replicate argc SP ++ [dest]))

decompileReady OGlk [func,Imm argc,dest] =
  Just (GInstr ("callfi*",argc+2,1,0,OCallI) (SpecialName "glk" : func : replicate argc SP ++ [dest]))

decompileReady OAStoreB [array,offset,value] =
  Just $ Eval "" (Assign (Binary array (NormalOp "->" 7) offset) value)

decompileReady OAStore [array,offset,value] =
  Just $ Eval "" (Assign (Binary array (NormalOp "-->" 7) offset) value)

decompileReady OALoadBit [obj,Imm attrPlus8,store] | attrPlus8 >= 8  =
  Just (pushOrStore store (Binary obj binopHas (Imm (attrPlus8 - 8))))

decompileReady OALoadBit [obj,Binary attr (NormalOp " + " _) (Imm 8),store] =
  Just (pushOrStore store (Binary obj binopHas attr))

decompileReady OAStoreBit [obj,Imm attrPlus8,Imm bit] | attrPlus8 >= 8 && (bit == 0 || bit == 1)  =
  Just $ Give obj (odd bit) (attrPlus8 - 8)

decompileReady _ _ = Nothing


separateStore [x] = ([],x)
separateStore (x:xs) = onFst (x:) (separateStore xs)

pushOrStore SP      expr = Push expr
pushOrStore (Imm 0) expr = Eval "" expr
pushOrStore dest    expr = Eval "" (Assign dest expr)
