
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Widely used stuff for FM3. *)

INTERFACE FM3Base 

; IMPORT VarArray_Int_Refany 

; TYPE TokTyp = INTEGER 
; CONST TokNull = FIRST ( TokTyp )
; CONST TokFirstReal = TokNull + 1 

(* Note lack of periods, which Pathname.Join inserts itself. *)  
; CONST InterfaceFileNameSuffix = "i3" 
; CONST ModuleFileNameSuffix = "m3"
(* Any need for generics? *) 

; TYPE MapTyp = VarArray_Int_Refany . T 

(* Type declarations that are 
   intended to be the same size regardless of a 
   particular compiler's representation choices. 
   It is intended to provide portability.  Change 
   compilers or compiler options and the programs
   that use these declarations will continue to work
   as before. 
*) 

; TYPE Card8Typ = [ 0 .. 255 ] 
  (* ^8 bit cardinal *) 

; TYPE Int8Typ = [ - 128 .. 127 ] 
  (* ^8 bit integer *) 

; TYPE Card16Typ = [ 0 .. 65535 ] 
  (* ^16 bit cardinal *) 

; TYPE Int16Typ = [ - 32768 .. 32767 ] 
  (* ^16 bit integer *) 

; TYPE Card32Typ = [ 0 .. 16_7FFFFFFF ] 
  (* ^32 bit cardinal *) 

; TYPE Int32Typ = [ - 16_7FFFFFFF - 1 .. 16_7FFFFFFF ] 
  (* ^32 bit integer *) 

; TYPE Card64Typ = [ 0L .. 16_7FFFFFFFFFFFFFFFL ] 
  (* ^64 bit cardinal *)
  (* NOTE: This is only the positive half of the 64-bit signed range. *) 
  (* Works on a 32- or 64-bit host. *) 

; TYPE Int64Typ = [ - 16_7FFFFFFFFFFFFFFFL - 1L .. 16_7FFFFFFFFFFFFFFFL ] 
  (* ^64 bit integer *) 
  (* Works on a 32- or 64-bit host. *) 

; PROCEDURE IntImage ( Value : INTEGER ) : TEXT 
  (* Result never has blanks. *) 
  (* Works for Int* and Card* types with subset value ranges. *) 

; PROCEDURE Int64Image ( Value : LONGINT ) : TEXT 
  (* Result never has blanks. *) 
  (* Works on a 32- or 4-bit host. *) 

; CONST Card64Image = Int64Image 
; CONST Int32Image = IntImage 
; CONST Card32Image = IntImage 
; CONST Int16Image = IntImage 
; CONST Card16Image = IntImage 
; CONST Int8Image = IntImage 
; CONST Card8Image = IntImage 

; CONST INTEGERImage = IntImage 
; CONST LONGINTImage = Int64Image  

; TYPE CompareTyp = [ -1 .. 2 ]
; CONST CmpLT = -1 
; CONST CmpEQ = 0 
; CONST CmpGT = 1 
; CONST CmpNR = 2 (* No relation.*) 

; TYPE HashTyp = LONGINT
; CONST HashNull = 0L

(*FIXME: M2SHORTCARD is for compatibility with reusem3/Positions.tPosition.
         These are probably adequate, but maybe 32 bits would be better
         here.  Exactly how to do this without undue
         dependency of cocktail m3 on FM3 is unclear.
*) 
; TYPE M2SHORTCARD = [ 0 .. 16_FFFF ]

; TYPE PosIntTyp = M2SHORTCARD  

; TYPE tPosition (*Mandated by lalr.*)
    = RECORD
        Line : PosIntTyp 
      ; Column : PosIntTyp 
      END (*tPosition*)

; CONST PositionNull = tPosition { LAST ( PosIntTyp ) , LAST ( PosIntTyp ) }

; TYPE SizeTyp = LONGINT (* Runtime sizes. *) 
; TYPE AlignTyp = [ 0 .. 64 ]

; TYPE AtomTyp = INTEGER
    (* Negative values are used as pseudo-atoms for certain predefined
       identifiers, and also operation codes.
    *) 
; CONST AtomNull = FIRST ( AtomTyp ) 
; CONST AtomFirstReal = 1 (* For use as a true atom in a dictionary. *) 

(* 

; CONST PassNoNull = 0  
; CONST PassNo1 = 1 
; CONST PassNo2 = 2
; CONST PassNoMax = PassNo2 + 1
; TYPE PassNoRangeTyp = [ PassNoNull .. PassNoMax ]

; TYPE PassNoSetTyp = SET OF PassNoRangeTyp
; CONST PassNoSetEmpty = PassNoSetTyp { }
; CONST PassNoSetAll = PassNoSetTyp { PassNo1 .. PassNoMax }
; CONST PassNoSetUniv = PassNoSetTyp { PassNoNull .. PassNoMax }

; PROCEDURE PassNoSetUnion ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

; PROCEDURE PassNoSetDiff ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

; PROCEDURE InclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoRangeTyp )

; PROCEDURE ExclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoRangeTyp )
*) 

; END FM3Base
.
