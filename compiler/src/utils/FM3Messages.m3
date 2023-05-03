        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Messages

; IMPORT AtomList 

; IMPORT FM3Utils

  (* Fatal amd Log go immediatly to stderr and optionally to a log file. *) 

(*EXPORTED*)
; PROCEDURE Fatal ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Terminate }
  (* Also terminates the program. *)

  = BEGIN
    END Fatal  

(*EXPORTED*)
; PROCEDURE Log ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

  = BEGIN
    END Log

(* Within a unit, Info, Warning, and Error are collected, sorted by
   line/column, and written to stdout at the end of the unit. *)

(*EXPORTED*)
; PROCEDURE StartUnit ( )

  = BEGIN
    END StartUnit 

(*EXPORTED*)
; PROCEDURE EndUnit ( )

  = BEGIN
    END EndUnit 

(*EXPORTED*)
; PROCEDURE Info ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = BEGIN
    END Info 

(*EXPORTED*)
; PROCEDURE Warning ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = BEGIN
    END Warning

(*EXPORTED*)
; PROCEDURE Error ( t1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = BEGIN
    END Error

(*EXPORTED*)
; PROCEDURE AtomListToOSError ( AL : AtomList . T ): TEXT

  = BEGIN
      RETURN "OSError.E("
             & FM3Utils . AtomListToText ( AL )
             & ")" 
    END AtomListToOSError  

; BEGIN
  END FM3Messages
.

