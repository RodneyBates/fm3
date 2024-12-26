
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3CTIntArith

; IMPORT FM3Exprs
; IMPORT FM3SrcToks 

(* Compile time integer arithmetic. *)

; TYPE T = LONGINT
  (* All operands and results are passedin a LONGINT, which is assumed to be
     64 bits, even on a 32-bit compiling machine.  Internal arithmetic may
     use more bits and other techniques.
  *)      

; EXCEPTION ArithError ( TEXT ) 

; PROCEDURE FromCTInt ( IntVal : INTEGER ; Signed : BOOLEAN ) : T 

; PROCEDURE ToCTInt ( Value : T ; Signed : BOOLEAN ) : INTEGER
  RAISES { ArithError } (* If the value won't fit. *) 

; PROCEDURE FromLong ( LongVal : LONGINT ) : T
; PROCEDURE ToLong ( Value : T ) : LONGINT
(* Just in case T ever changes. *) 

; PROCEDURE Unary
    ( Opnd : T ; Opcode : FM3SrcToks . TokTyp ; Integer  : BOOLEAN ) : T
  RAISES { ArithError }
  
; PROCEDURE Binary
    ( Lt , Rt : T ; Opcode : FM3SrcToks . TokTyp ; Integer : BOOLEAN ) : T
  RAISES { ArithError }

  (* For Unary and Binary, Integer means do the arithmetic in the size of
     RT INTEGER, which will be be taken from CT Target options.  Opcodes
     of functions in  Word or Long will do Modula-3 overflow-less modulo
     INTEGER or LONGINT. Opcodes for operator symbols will raise ArithError
     IFF called-for by CT option.
  *) 
;
 END FM3CTIntArith
.
