        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Messages

; IMPORT AtomList 
; IMPORT Thread

; IMPORT FM3SharedUtils

  (* Fatal and Log go immediatly to stderr and optionally to a log file. *) 

; VAR LogFileName := "FM3Log"

; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { FM3SharedUtils . Terminate }
  (* Also terminates the program. *) 

; PROCEDURE Log
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

(* Within a unit, Info, Warning, and Error are collected, sorted by
   line/column, and written to stdout at the end of the unit. *)

; PROCEDURE Info
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

; PROCEDURE
    Warning ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

; PROCEDURE Error
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

; PROCEDURE StartUnit ( UnitName : TEXT ) RAISES { Thread . Alerted } 

; PROCEDURE EndUnit ( UnitName : TEXT ) RAISES { Thread . Alerted } 

; PROCEDURE AtomListToOSError ( AL : AtomList . T ): TEXT 

; END FM3Messages
.

