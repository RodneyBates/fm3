
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scanner

; IMPORT FM3SrcToks 
; IMPORT FM3Base 

; TYPE TokRecTyp
  = RECORD
      TrHash : FM3Utils . HashTyp
    ; TrLineNo : INTEGER := 0 
    ; TrCharPos : INTEGER := 0 
    ; TrTok : Tok : FM3Base . TokTyp := FM3Base . TokNull  
    ; TrText   : TEXT := "" 
    END (* TokRecTyp *)

; VAR GCurrentTok : TokRecTyp 

; PROCEDURE Scan ( Cr : SchutzCoroutine . T ) 

; END FM3Scanner 
. 
