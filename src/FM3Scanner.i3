
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scanner

; IMPORT CharVarArray 
; IMPORT WidecharVarArray 

; IMPORT FM3SrcToks 
; IMPORT FM3Base 

; TYPE TokRecTyp
  = RECORD
      TrLineNo : INTEGER := 0 
    ; TrCharPos : INTEGER := 0 
    ; TrWidechars : REF ARRAY OF WIDECHAR 
      (* ^Converted RT memory value of wide TEXT literal or lex error chars. *) 
    ; TrChars : REF ARRAY OF CHAR  
      (* ^Identifier, Numeric literal, or Converted RT memory value of
         TEXT literal, or . *) 
    ; TrHash : FM3Utils . HashTyp
      (* ^Of anything with a meaningful TrWideChars or TrChars field. *) 
    ; TrAtom : FM3Base . AtomTyp
      (* ^Of anything with a meaningful TrWideChars or TrChars field. *) 
    ; TrWCh : WIDECHAR (* Value of [WIDE]CHAR literal. *) 
    ; TrTok : Tok : FM3Base . TokTyp := FM3Base . TokNull  
    END (* TokRecTyp *)

; TYPE TokRefTyp = REF TokRecTyp 

; VAR GCurTokRef : TokRefTyp 

; PROCEDURE Scan ( Cr : SchutzCoroutine . T ) 

; END FM3Scanner 
. 
