
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Main

; TYPE T = INTEGER
; TYPE INTEGER = T (* Reserved id declared. *)
; TYPE V = REAL 
; TYPE T = CHAR    (* Duplicate declaration . *) 
; TYPE U = BOOLEAN

; TYPE
    T1 = INTEGER
  ; INTEGER = T (* Reserved id declared. *)
  ; V1 = REAL 
  ; T1 = CHAR    (* Duplicate declaration . *) 
  ; U1 = BOOLEAN

; BEGIN
  END Main 
.

