
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT TextIntTbl

; IMPORT RdBackFile
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3Dict_OAChars_Int
; IMPORT FM3Units

(* File names of internal and generated files: *)

(** Suffixes are W/O  '.', so can use Pathname.Join. *) 
; CONST PatchStackSuffix = "FM3Patch"
      (* The patch stack, used and emptied during parse pass 1. *)
; CONST PatchSackEmptySentinal = -1L
        (* ^An artificial coordinate to the pass 1 output file. *) 
; CONST Pass2OutSuffix = "FM3Pass2" (* Output of pass 1. *) 
; CONST Pass1OutSuffix = "FM3Pass1" (* Output of pass 2. *)
; CONST CopyFileSuffix = "Copy" 
; CONST DisAsmFileSuffix = "DisAsm"
; CONST UnitLogSuffix = "Log" 

(* These are cached copies of Unt* fields of the current Unit, for faster
   access via static addressing.
*) 
; VAR P1RdBack : RdBackFile . T := NIL 
; VAR PatchRdBack : RdBackFile . T := NIL
; VAR P2RdBack : RdBackFile . T := NIL

(* One set of skip numbering should suffice for all units. *)
; VAR InitSkipStackCt := 255 (* Sometimes many compile errors? *) 
; VAR SkipNoStack : VarArray_Int_Int . T
; VAR NextSkipNo : INTEGER := 1 

; VAR M3RwDict : FM3Dict_OAChars_Int . GrowableTyp  
; VAR PgRwDict : FM3Dict_OAChars_Int . GrowableTyp 

; VAR IdentAtomInitSize := 1500
; VAR NumberAtomInitSize := 600
; VAR CharsAtomInitSize := 600
; VAR WideCharsAtomInitSize := 600

; VAR InitUnitsCt := 40
; VAR InitImportsCt := 100 
; VAR InitScopeCtPerUnit := 60 
; VAR InitDeclCtPerUnit := 100
; VAR InitDefCtPerUnit := 300

; PROCEDURE Init ( ) 

; PROCEDURE Finalize ( )

; END FM3Globals
.




