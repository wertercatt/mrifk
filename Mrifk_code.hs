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


module Mrifk_code (
	Statement(..), LabelType(..), Expr(..), Opcode(..), BinaryOp (..),
	binopEQ, binopNE, binopLT, binopGT, binopLE, binopGE,
	binopHas, binopOr, binopAnd,
	binopName, binopPrec
) where


data Statement =
  Push Expr | Eval String Expr |
  Print String Expr | NewLine |
  IfThenElse Expr [Statement] [Statement] |
  Label Int LabelType | JCond Expr Int | Jump Int |
  Give Expr Bool Int |
  GInstr (String,Int,Int,Int,Opcode) [Expr]
    deriving (Show,Eq)

data LabelType = Single | Multi | Phantom  deriving (Show,Eq)

data Expr =
  SP | Imm Int | ImmString Int | Mem Int | Local Int |
  Unary String Int Expr | PostIncDec Expr String |
  Binary Expr BinaryOp Expr |
  Assign Expr Expr | Call Expr [Expr] |
  SpecialName String
    deriving (Show,Eq)

data Opcode =
  OCopy | OJCond BinaryOp | OJump | OCall | OCallI | OReturn |
  OALoadBit | OAStore | OAStoreB | OAStoreBit |
  OStreamChar | OStreamNum | OStreamStr |
  OBinary BinaryOp | OGlk | OStkSwap | OSpecial
    deriving (Show,Eq)


data BinaryOp =
  NormalOp String Int | PredicateOp String String | LogicalOp String String  deriving (Show,Eq)


binopEQ  = PredicateOp " == " " ~= "
binopNE  = PredicateOp " ~= " " == "
binopLT  = PredicateOp " < "  " >= "
binopGT  = PredicateOp " > "  " <= "
binopLE  = PredicateOp " <= " " > "
binopGE  = PredicateOp " >= " " < "
binopHas = PredicateOp " has " " hasnt "
binopOr  = LogicalOp " || " " && "
binopAnd = LogicalOp " && " " || "

binopName (PredicateOp s _) = s
binopName (LogicalOp s _) = s
binopName (NormalOp s _) = s

binopPrec (PredicateOp _ _) = 3
binopPrec (LogicalOp _ _) = 2
binopPrec (NormalOp _ p) = p
