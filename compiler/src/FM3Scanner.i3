
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

(* Things expected from a scanner by an lalr-generated parser: *)
(* Accomodate lalr-generated parser's spellings. *)

; TYPE M2SHORTCARD = [ 0 .. 16_FFFF ]

; TYPE tPosition (*lalr*)
    = RECORD
        Line : M2SHORTCARD 
      ; Column : M2SHORTCARD
(*FIXME: M2SHORTCARD is for compatibility with reusem3/Positions.tPosition.
         These are probably adiquate, but maybe 32 bits would be better
         here.  Exactly how to do this without undue
         dependency of cocktail m3 on FM3 is unclear.
*) 
      END (*tPosition*) 

; VAR Attribute : tScanAttribute (*lalr*)

; TYPE tScanAttribute (*lalr*)
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

; PROCEDURE Init ( )

; PROCEDURE GetToken ( ) : INTEGER (*lalr*)

; PROCEDURE ErrorAttribute
    ( Token : CARDINAL ; VAR Attribute : tScanAttribute ) (*lalr*)
  (* This is dependent only on the language, not code being compiled, so 
     a single instance suffices. *) 

; PROCEDURE PushState 
     ( NewUniRd : UniRd . T ; UnitRef : FM3Units . UnitRefTyp ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

; PROCEDURE PopState ( ) : UniRd . T (* Previous reader. *)  

; PROCEDURE CurrentUnitNo ( ) : FM3Units . UnitNoTyp 

; END FM3Scanner 
. 
