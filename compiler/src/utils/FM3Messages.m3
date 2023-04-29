        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Messages

  (* Fatal amd Log go immediatly to stderr and optionally to a log file. *) 

(*EXPPORTED*)
; PROCEDURE Fatal ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Terminate }
  (* Also terminates the program. *)

  = BEGIN
    END Fatal  

(*EXPPORTED*)
; PROCEDURE Log ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

  = BEGIN
    END Log

(* Within a unit, Info, Warning, and Error are collected, sorted by
   line/column, and written to stdout at the end of the unit. *)

(*EXPPORTED*)
; PROCEDURE StartUnit ( )

  = BEGIN
    END StartUnit 

(*EXPPORTED*)
; PROCEDURE EndUnit ( )

  = BEGIN
    END EndUnit 

(*EXPPORTED*)
; PROCEDURE Info ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = BEGIN
    END Info 

(*EXPPORTED*)
; PROCEDURE Warning ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = BEGIN
    END Warning

(*EXPPORTED*)
; PROCEDURE Error ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = BEGIN
    END Error

; BEGIN
  END FM3Messages
.

