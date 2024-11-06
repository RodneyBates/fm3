
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scanner

; IMPORT Text 
; IMPORT UniRd 

; IMPORT FM3Base
; IMPORT FM3Globals 
; IMPORT FM3OpenArray_Char
; IMPORT FM3OpenArray_WideChar
; IMPORT FM3Units 

(* Things expected from a scanner by an lalr-generated parser: *)
(* Accomodate lalr-generated parser's spellings. *)

; TYPE tPosition = FM3Base . tPosition 

; VAR Attribute : tScanAttribute (*lalr*)

(* Lalr mandates the existence (but not the name, I don't think,) of
   record type tScanAttribute, its Position field, by name, and
   Position's fields, by name. *) 
; TYPE tScanAttribute
    = RECORD
        Position : tPosition (* Lalr-generated code stores into this. *)
        
      (* Fields beyond here are not accessed by Lalr-emitted parsing
         algorithm code, but may be by semantic action code taken from the
         .lalr file and translated and inserted into the parser by lalr.
      *) 
      ; SaArgValue : LONGINT
        (* Binary value of any scalar literal, LOOPHOLEd to LONGINT. *) 
      ; SaHash : FM3Base . HashTyp
        (* ^Of any TrTok with a meaningful TrWideChars or TrChars field. *) 
      ; SaAtom : FM3Base . AtomTyp
        (* ^Of any TrTok with a meaningful TrWideChars or TrChars field. *) 
      ; SaWideChars : FM3OpenArray_WideChar . T 
        (* ^Converted RT memory value of wide TEXT literal or lex error
            chars. *) 
      ; SaChars : FM3OpenArray_Char . T 
        (* ^Identifier, Numeric literal, or Converted RT memory value of
           TEXT literal, or . *) 
      ; SaTok : FM3Base . TokTyp := FM3Base . TokNull  
      ; SaPredefTok : FM3Base . TokTyp (* A source token. *) 
      ; SaWCh : WIDECHAR (* Value of [WIDE]CHAR literal. *)
      ; SaIsPredefId : BOOLEAN 
      END (* tScanAttribute *)

; CONST ScanAttrNull
    = tScanAttribute
        { Position := FM3Base . PositionNull
        , SaArgValue := 0L
        , SaHash := FM3Base . HashNull
        , SaAtom := FM3Base . AtomNull
        , SaWideChars := NIL
        , SaChars := NIL
        , SaTok := FM3Base . TokNull  
        , SaPredefTok := FM3Base . TokNull 
        , SaWCh := W'\x0000'
        , SaIsPredefId := FALSE 
        } 

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

; PROCEDURE CurrentUnitNo ( ) : FM3Globals . UnitNoTyp 

; END FM3Scanner 
. 
