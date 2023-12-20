
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT TextIntTbl 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Dict_OAChars_Int
; IMPORT FM3Units

(* File names of internal and generated  files: *) 
; VAR PatchStackSuffix := ".Patch" (* The patch stack, during parse pass. *) 
; VAR UnnestStackSuffix := ".Unnest" (* The Unnest stack, during parse pass. *) 
; VAR ParsePassSuffix := ".ParsePass" (* Output of the parse pass. *)
; VAR BuildDirRelPath := "../build"
      (* ^Relative to where the current unit's source file lives. *)
; VAR CopyFileSuffix := "Copy" (* W/O  '.'. so can use Pathname.Join. *) 
; VAR DisAsmFileSuffix := "DisAsm" (* W/O  '.'. so can use Pathname.Join. *) 

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


