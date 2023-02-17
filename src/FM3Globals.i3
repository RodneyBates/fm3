
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT TextIntTbl 

; IMPORT FM3TextDict 

; VAR M3RwDict : Fm3CharsIntDict . I 
; VAR PgRwDict : Fm3CharsIntDict . T 
; VAR IdentAtomDict : FM3CharsAtomDict . T 

; VAR IdentDictSize := 1500
; VAR CharsDictSize := 600
; VAR WideCharsDictSize := 600

; PROCEDURE Init ( ) 

; END FM3Globals
.


