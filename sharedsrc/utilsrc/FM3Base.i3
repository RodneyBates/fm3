
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Base 
(* Widely used stuff for FM3. *) 

; TYPE TokTyp = INTEGER
; CONST TokNull = FIRST ( INTEGER )

; TYPE AtomTyp = INTEGER
; CONST AtomNull = 0 

(* Type declarations that are 
   intended to be the same size regardless of a 
   particular compiler's representation choices. 
   It is intended to provide portability.  Change 
   the declarations here and programs that use 
   them will continue to work as before. 
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

; END FM3Base
.