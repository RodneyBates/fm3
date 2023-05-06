
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Things needed by the Cocktail-lalr-generated parser. *)

INTERFACE FM3ParseSem

; IMPORT FM3Scanner
; IMPORT FM3TokDef 

; TYPE tParsAttribute (* Lalr expects this and field Scan to be so named. *) 
    = RECORD
        Scan : FM3Scanner . tScanAttribute
      ; PaConstructPtr : INTEGER
      ; PaListItemNo : INTEGER
      ; PaInt : INTEGER
      ; PaLong : LONGINT 
      ; PaBool : BOOLEAN 
      END (* tParsAttribute *)

; PROCEDURE PushTok ( Tok : FM3TokDef . TokTyp ; Arg0 : LONGINT )

; PROCEDURE PushTokPatch ( Tok : FM3TokDef . TokTyp ; Arg0 , Arg1 : LONGINT )

; END FM3ParseSem 
.


