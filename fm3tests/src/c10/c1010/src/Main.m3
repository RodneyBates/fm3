
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Test revelations. *) 

MODULE Main

; TYPE T = CHAR 
; TYPE V = WIDECHAR 
; TYPE U = BOOLEAN  

; TYPE T1 = CHAR 
; TYPE V1 = WIDECHAR 
; TYPE U1 = BOOLEAN  

; REVEAL T = ROOT 
; REVEAL V . X  <: REFANY  
; REVEAL U <: UNTRACED ROOT 

; REVEAL
    T1 = OBJECT END   
  ; V1 <: Mumble
  ; U1 = NULL 

; BEGIN
  END Main 
.

