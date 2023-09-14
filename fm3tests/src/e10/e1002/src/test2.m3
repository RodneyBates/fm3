
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE test2

EXPORTS A , B

; IMPORT X , Y

; FROM Mumble IMPORT I , J 

; PROCEDURE P ( VAR F1 : R ) 

; PROCEDURE P ( VALUE  F1 ) 

; PROCEDURE P ( F1 ) 
; PROCEDURE P ( READONLY F1 : U ) 
; PROCEDURE P ( VALUE F1 := U ) 

; BEGIN
  END test2

.
