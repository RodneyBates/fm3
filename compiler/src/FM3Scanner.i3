
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
; IMPORT FM3Units 
; IMPORT FM3Utils

(* Things expected from a scanner by an lalr-generated parser: *)

; VAR Attribute : REF tScanAttribute
  (* We want multiple instances of this for nested source files, but lalr
     treats it as a global record.  We make it a REF and, unusually, rely
     on Modula-3's implicit dereferencing of REF RECORD for lalr-generated
     code's access to its fields. *) 

; TYPE tPosition
    = RECORD
        Line : INTEGER
      ; Column : INTEGER 
      END (*tPosition*) 

; TYPE tScanAttribute
    = RECORD
        Position : tPosition (* Lalr-generated code stores into this. *)
      (* Fields beyond here are not accessed by Lalr-generated parser code. *) 
      ; SaArgValue : LONGINT
        (* ORD (either TrAtom or TrWCh). *) 
      ; SaHash : FM3Base . HashTyp
        (* ^Of any TrTok with a meaningful TrWideChars or TrChars field. *) 
      ; SaAtom : FM3Base . AtomTyp
        (* ^Of any TrTok with a meaningful TrWideChars or TrChars field. *) 
      ; SaLineNo : INTEGER := 0 
      ; SaCharPos : INTEGER := 0 
      ; SaWideChars : FM3OpenArray_WideChar . T 
        (* ^Converted RT memory value of wide TEXT literal or lex error
            chars. *) 
      ; SaChars : FM3OpenArray_Char . T 
        (* ^Identifier, Numeric literal, or Converted RT memory value of
           TEXT literal, or . *) 
      ; SaTok : FM3Base . TokTyp := FM3Base . TokNull  
      ; SaWCh : WIDECHAR (* Value of [WIDE]CHAR literal. *)
      END (* tScanAttribute *)

(* End of things expected from a scanner by an lalr-generated parser: *) 

; PROCEDURE PushState 
     ( NewUniRd : UniRd . T ; UnitRef : FM3Units . UnitRefTyp ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

; PROCEDURE PopState ( ) : UniRd . T (* Previous reader. *)  

; PROCEDURE CurrentUnitNo ( ) : FM3Units . UnitNoTyp 

; PROCEDURE GetToken ( ) 

; END FM3Scanner 
. 
