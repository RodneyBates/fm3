
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

; VAR M3RwDict : FM3Dict_OAChars_Int . GrowableTyp  
; VAR PgRwDict : FM3Dict_OAChars_Int . GrowableTyp 

; VAR IdentInitAtomSize := 1500
; VAR NumberInitAtomSize := 600
; VAR CharsInitAtomSize := 600
; VAR WideCharsInitAtomSize := 600

; VAR FirstRealAtom := 1

; VAR CurrentUnitRef : FM3Units . UnitRefTyp

; VAR ResourcePathName : TEXT := "."
  (* ^Set this from a CLI option. *)


; PROCEDURE Init ( ) 

; END FM3Globals
.


