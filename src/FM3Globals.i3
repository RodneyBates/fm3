
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT FM3TextDict 

; VAR RwDict : FM3Dict . T 
; VAR IdentDict : FM3Dict . T 
; VAR TextDict : FM3Dict . T (* TEXT literals, as in source code. *)  
; VAR TextValDict : FM3Dict . T (* Internal text values *)

; VAR IdentDictSize := 1500
; VAR TextDictSize := 600
; VAR TextValDictSize := 600

; PROCEDURE Init ( ) 

; END FM3Globals
.


