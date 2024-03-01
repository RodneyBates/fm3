
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
; IMPORT FM3Dict_OAChars_Int
; IMPORT FM3Units

(* File names of internal and generated  files: *) 
; VAR PatchStackSuffix := ".FM3Patch"
      (* The patch stack, used and emptied during parse pass 1. *) 
; VAR UnnestStackSuffix := ".FM3Pass1" (* Output of pass 1. *) 
; VAR ParsePassSuffix := ".FM3Pass2" (* Output of pass 2. *)
; VAR BuildDirRelPath := "../build"
      (* ^Relative to where the current unit's source file lives. *)
; VAR CopyFileSuffix := "Copy" (* W/O  '.'. so can use Pathname.Join. *) 
; VAR DisAsmFileSuffix := "DisAsm" (* W/O  '.'. so can use Pathname.Join. *) 

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

; VAR InitUnitCt := 30
; VAR InitScopeCtPerUnit := 60 
; VAR InitDeclCtPerUnit := 300 

; VAR FirstRealAtom := 1

; VAR ResourcePathName : TEXT := "."
  (* ^Push this from a CLI option. *)

; PROCEDURE Init ( ) 

; END FM3Globals
.




