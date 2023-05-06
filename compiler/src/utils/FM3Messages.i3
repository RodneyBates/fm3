        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Messages

; IMPORT AtomList 

; <*IMPLICIT*> EXCEPTION Terminate 

  (* Fatal amd Log go immediatly to stderr and optionally to a log file. *) 

; VAR LogFileName := "FM3Log"

; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Terminate }
  (* Also terminates the program. *) 

; PROCEDURE Log ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

(* Within a unit, Info, Warning, and Error are collected, sorted by
   line/column, and written to stdout at the end of the unit. *)

; PROCEDURE Info ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

; PROCEDURE Warning ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

; PROCEDURE Error ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

; PROCEDURE StartUnit ( UnitName : TEXT )

; PROCEDURE EndUnit ( UnitName : TEXT )

; PROCEDURE AtomListToOSError ( AL : AtomList . T ): TEXT 

; END FM3Messages
.

