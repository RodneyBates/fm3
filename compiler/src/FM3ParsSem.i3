
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Things needed by the Cocktail-lalr-generated parser. *)

INTERFACE ParsSem

; TYPE tScanAttribute
    = RECORD
        SaPosition : 
      END (* tScanAttribute *) 

; TYPE tParsAttribute
    = RECORD
        PaScan : tScanAttribute
      ; PaConstructPtr : INTEGER
      ; PaListItemNo : INTEGER 
      END (* tParsAttribute *) 

; END ParsSem 
.

