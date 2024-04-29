
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT TextIntTbl

; IMPORT RdBackFile
; IMPORT IntIntVarArray 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3Dict_OAChars_Int
; IMPORT FM3Units

(* File names of internal and generated files: *)

(** Suffixes are W/O  '.', so can use Pathname.Join. *) 
; CONST PatchStackSuffix = "FM3Patch"
      (* The patch stack, used and emptied during parse pass 1. *) 
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
; VAR InitSkipStackCt := 64 (* Seems liberal. *) 
; VAR SkipNoStack : IntIntVarArray . T 
; VAR NextSkipNo : INTEGER := 1 

; VAR M3RwDict : FM3Dict_OAChars_Int . GrowableTyp  
; VAR PgRwDict : FM3Dict_OAChars_Int . GrowableTyp 

; VAR IdentAtomInitSize := 1500
; VAR NumberAtomInitSize := 600
; VAR CharsAtomInitSize := 600
; VAR WideCharsAtomInitSize := 600

; VAR InitUnitsCt := 40
; VAR InitScopeCtPerUnit := 60 
; VAR InitDeclCtPerUnit := 300

; VAR FirstRealAtom := 1

; PROCEDURE Init ( ) 

; END FM3Globals
.




