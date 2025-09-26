
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Test CONST declarations, some with CT errors. *) 

MODULE Main

; CONST T = 10 
; CONST INTEGER = 5 (* Reserved id declared. *)
; CONST U : INTEGER = 15 
; CONST T : CHAR = 'K'     (* Duplicate declaration . *) 
; CONST V = TRUE 

; CONST
    T1 = 11
  ; INTEGER = 6 (* Reserved id declared. *)
  ; U1 : REAL = 1.2 
  ; T1 : CHAR = 'L'    (* Duplicate declaration . *) 
  ; V1 = FALSE 
 
; BEGIN
  END Main 
.

