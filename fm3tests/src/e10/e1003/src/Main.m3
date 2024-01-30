
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Main

; TYPE T = INTEGER
; TYPE INTEGER = T (* Reserved id declared. *) 
; TYPE T = CHAR    (* Duplicate declaration . *) 
; TYPE U = BOOLEAN 

; BEGIN
  END Main 
.

