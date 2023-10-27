
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Predeclared identifiers and their meanings. *) 

MODULE FM3PreDecl

(* Keep these around in case more lists are needed: 

; CONST Rid%1ABS%2
; CONST Rid%1ADDRESS%2
; CONST Rid%1ADR%2
; CONST Rid%1ADRSIZE%2
; CONST Rid%1BITSIZE%2
; CONST Rid%1BOOLEAN%2
; CONST Rid%1BYTESIZE%2
; CONST Rid%1CARDINAL%2
; CONST Rid%1CEILING%2
; CONST Rid%1CHAR%2
; CONST Rid%1DEC%2
; CONST Rid%1DISPOSE%2
; CONST Rid%1EXTENDED%2
; CONST Rid%1FALSE%2
; CONST Rid%1FIRST%2
; CONST Rid%1FLOAT%2
; CONST Rid%1FLOOR%2
; CONST Rid%1INC%2
; CONST Rid%1INTEGER%2
; CONST Rid%1ISTYPE%2
; CONST Rid%1LAST%2
; CONST Rid%1LONGREAL%2
; CONST Rid%1LOOHOLE%2
; CONST Rid%1MAX%2
; CONST Rid%1MIN%2
; CONST Rid%1MUTEX%2
; CONST Rid%1NARROW%2
; CONST Rid%1NEW%2
; CONST Rid%1NIL%2
; CONST Rid%1NULL%2
; CONST Rid%1NUMBER%2
; CONST Rid%1ORD%2
; CONST Rid%1REAL%2
; CONST Rid%1REFANY%2
; CONST Rid%1ROUND%2
; CONST Rid%1SUBARRAY%2
; CONST Rid%1TEXT%2
; CONST Rid%1TRUE%2
; CONST Rid%1TRUNC%2
; CONST Rid%1TYPECODE%2
; CONST Rid%1VAL%2
*) 

; CONST RidSpellings = ARRAY [ RidNull .. RidVAL ] OF TEXT
    { (* RidNull *) "<RidNull>"
    , (* RidABS *) "ABS"
    , (* RidADDRESS *) "ADDRESS"
    , (* RidADR *) "ADR"
    , (* RidADRSIZE *) "ADRSIZE"
    , (* RidBITSIZE *) "BITSIZE"
    , (* RidBOOLEAN *) "BOOLEAN"
    , (* RidBYTESIZE *) "BYTESIZE"
    , (* RidCARDINAL *) "CARDINAL"
    , (* RidCEILING *) "CEILING"
    , (* RidCHAR *) "CHAR"
    , (* RidDEC *) "DEC"
    , (* RidDISPOSE *) "DISPOSE"
    , (* RidEXTENDED *) "EXTENDED"
    , (* RidFALSE *) "FALSE"
    , (* RidFIRST *) "FIRST"
    , (* RidFLOAT *) "FLOAT"
    , (* RidFLOOR *) "FLOOR"
    , (* RidINC *) "INC"
    , (* RidINTEGER *) "INTEGER"
    , (* RidISTYPE *) "ISTYPE"
    , (* RidLAST *) "LAST"
    , (* RidLONGREAL *) "LONGREAL"
    , (* RidLOOHOLE *) "LOOHOLE"
    , (* RidMAX *) "MAX"
    , (* RidMIN *) "MIN"
    , (* RidMUTEX *) "MUTEX"
    , (* RidNARROW *) "NARROW"
    , (* RidNEW *) "NEW"
    , (* RidNIL *) "NIL"
    , (* RidNULL *) "NULL"
    , (* RidNUMBER *) "NUMBER"
    , (* RidORD *) "ORD"
    , (* RidREAL *) "REAL"
    , (* RidREFANY *) "REFANY"
    , (* RidROUND *) "ROUND"
    , (* RidSUBARRAY *) "SUBARRAY"
    , (* RidTEXT *) "TEXT"
    , (* RidTRUE *) "TRUE"
    , (* RidTRUNC *) "TRUNC"
    , (* RidTYPECODE *) "TYPECODE"
    , (* RidVAL *) "VAL"
    }

(*EXPORTED:*)
; PROCEDURE Spelling ( Rid : INTEGER ) : TEXT

  = BEGIN
      CASE Rid
      OF RidNull .. RidVAL
      => RETURN RidSpellings [ Rid ]
      ELSE RETURN "<RidNull>"
      END (*CASE*) 
    END Spelling 

; BEGIN END FM3PreDecl
.

