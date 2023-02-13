
(* -----------------------------------------------------------------------1- *)
(* This file is part of the RM3 Modla-3 compiler.                            *)
(* Copyright 2023..2023, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE RMScanner

; IMPORT RM3SrcToks 

; TYPE TokRecTyp
  = RECORD
      TrTok : RM3SrcToks . TokTyp := - 1 
    ; TrLineNo : INTEGER := 0 
    ; TrCharPos : INTEGER := 0 
    ; TrText   : TEXT := "" 
    END (* TokRecTyp *)

; VAR GCurrentTok : TokRecTyp 

; PROCEDURE Scan ( Cr : SchutzCoroutine . T ) 

; END RMScanner 
. 
