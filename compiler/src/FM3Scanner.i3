
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scanner

; IMPORT UniRd 

; IMPORT FM3Base
; IMPORT FM3OpenArray_Char 
; IMPORT FM3OpenArray_WideChar 
; IMPORT FM3Utils

(* Things expected from a scanner by an lalr-generated parser: *)

; TYPE tPosition
    = RECORD
        Line : INTEGER
      ; Column : INTEGER 
      END tPosition 

; TYPE tScanAttribute
    = RECORD
        Position : tPosition (* Lalr-generated code stores into this. *)
      ; TokRec : TokRecTyp
        (* Is this too extravagant, putting all these fields in here? *)
      END (* tScanAttribute *)

; VAR Attribute : REF tScanAttribute
  (* We want multiple instances of this for nested source files, but lalr
     treats it as a global record.  We make it a REF and, unusually, rely
     on Modula-3's implicit dereferencing of REF RECORD for lalr-generated
     code's access to its fields. *) 

(* End of things expected from a scanner by an lalr-generated parser: *) 

; TYPE TokRecTyp
  = RECORD
    ; TrArgValue : LONGINT
      (* If TrHasArg, one of TrWideChars, TrChars, TrWCh. *) 
    ; TrHash : FM3Base . HashTyp
      (* ^Of anything with a meaningful TrWideChars or TrChars field. *) 
    ; TrAtom : FM3Base . AtomTyp
      (* ^Of anything with a meaningful TrWideChars or TrChars field. *) 
    ; TrLineNo : INTEGER := 0 
    ; TrCharPos : INTEGER := 0 
    ; TrWideChars : FM3OpenArray_WideChar . T 
      (* ^Converted RT memory value of wide TEXT literal or lex error chars. *) 
    ; TrChars : FM3OpenArray_Char . T 
      (* ^Identifier, Numeric literal, or Converted RT memory value of
         TEXT literal, or . *) 
    ; TrTok : FM3Base . TokTyp := FM3Base . TokNull  
    ; TrWCh : WIDECHAR (* Value of [WIDE]CHAR literal. *)
    ; TrArgCt : Fm3Base . Card8Typ
        (* 0 means TrArgValue.  3 means TrArgValue, TrHash, and TrAtom. *)  
    END (* TokRecTyp *)

; TYPE TokRefTyp = REF TokRecTyp 

; VAR GCurTokRef : TokRefTyp 

; PROCEDURE PushState 
     ( NewUniRd : UniRd . T ; FileName : TEXT ; UnitNo : INTEGER ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

; PROCEDURE PopState ( ) : UniRd . T (* Previous reader. *)  

; PROCEDURE CurrentUnitNo ( ) : INTEGER 

; PROCEDURE GetToken ( ) 

; END FM3Scanner 
. 
