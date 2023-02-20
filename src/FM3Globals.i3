
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

; VAR M3RwDict : FM3Dict_OAChars_Int . T 
; VAR PgRwDict : FM3Dict_OAChars_Int . T

(* All 3 Atom dictionaries need to be one per comp. unit. *) 

; VAR IdentAtom : FM3Atom_OAChars . T (* Identifiers. *) 
; VAR TextAtom : FM3Atom_OAChars . T (* TEXT literal values. *) 
; VAR WideTextAtom : FM3Atom_OAWideChars . T (* Wide TEXT literal valuies. *) 

; VAR IdentInitAtomSize := 1500
; VAR NumberInitAtomSize := 600
; VAR TextInitAtomSize := 600
; VAR WideInitTextAtomSize := 600

; PROCEDURE Init ( ) 

; END FM3Globals
.


