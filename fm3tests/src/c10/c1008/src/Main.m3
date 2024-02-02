
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Test variable declarations, some with CT errors. *) 

MODULE Main

; TYPE T = LONG

; VAR V0 : T
; VAR INTEGER : T  (* Reserved id declared. *)
; VAR V : REAL 
; VAR V0 : CHAR    (* Duplicate declaration . *) 
; VAR U : BOOLEAN

; VAR
    V1 : T 
  ; INTEGER : T (* Reserved id declared. *)
  ; V2 : REAL 
  ; V1 : CHAR    (* Duplicate declaration . *) 
  ; U1 : BOOLEAN

  ; V3 : BOOLEAN := TRUE
  ; V4 := FALSE
  ; V5            (* Neither type nor initial value. *) 
  

; BEGIN
  END Main 
.
