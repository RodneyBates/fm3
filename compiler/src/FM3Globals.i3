
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT TextIntTbl 

; IMPORT FM3Dict_OAChars_Int  

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars

; VAR PathSep := "/"
(*FIXME: ^Get the host-OS-dependent value for this. *)

(* File names of internal and generated  files: *) 
; VAR PatchStackSuffix := ".Patch"
; VAR UnnestStackSuffix := ".Unnest"
; VAR ParsePassSuffix := ".ParsePass"

; VAR M3RwDict : FM3Dict_OAChars_Int . GrowableTyp  
; VAR PgRwDict : FM3Dict_OAChars_Int . GrowableTyp 

; VAR IdentInitAtomSize := 1500
; VAR NumberInitAtomSize := 600
; VAR CharsInitAtomSize := 600
; VAR WCharsInitTextAtomSize := 600
; VAR FirstRealAtom := 1 

; PROCEDURE Init ( ) 

; END FM3Globals
.


