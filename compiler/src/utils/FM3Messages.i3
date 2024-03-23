        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Messages

; IMPORT AtomList 
; IMPORT Thread
; IMPORT Wr

; IMPORT FM3Base 
; IMPORT FM3SharedUtils

; CONST IndentPrefix = "       "
; CONST IndentLen = 7 (*Text.Length(IndentPrefix)*)
; CONST NLIndent = Wr . EOL & IndentPrefix
; CONST MsgLineLen = 80

(* The Do* options and *LogFileWrT can be set by client code.  This
   allows FM3Messages to be shared among >1 main program, without
   dragging in the entire compiler.
*)

; PROCEDURE PutStdErr ( Msg : TEXT ) RAISES { Thread . Alerted } 

; PROCEDURE PutStdOut ( Msg : TEXT ) RAISES { Thread . Alerted }

(* The following is for messages about how the compilation or other process
   is going. Fatal and Log messages go immediatly to stderr (If DoStdErr)
   and immediately to a log file (if DoFM3Log).
*)

; VAR FM3LogFileName := "FM3Log" 

; VAR FM3LogFileWrT : Wr . T
      (* DoFM3Log => Client code has set this Non-NIL and opened it. *) 

; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { FM3SharedUtils . FatalError } 
  (* Also terminates the program. *) 

; PROCEDURE FatalArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
   RAISES { FM3SharedUtils . FatalError }
  (* Also terminates the program. *) 

; PROCEDURE FM3Log
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted }

; PROCEDURE FM3LogArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
  RAISES { FM3SharedUtils . Terminate }

(* The following is for messages about code being compiled or other
   input being processed.  They go immediately to stdout, if DoStdOut. 
   If DoUnitLog, then between StartUnit and EndUnit calls, messages
   are collected, sorted by line/column, and written to UnitLogWrT
   at the end by EndUnit.
*)

; PROCEDURE Info
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

; PROCEDURE InfoArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
  RAISES { Thread . Alerted } 

; PROCEDURE
    Warning ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

(*EXPORTED*)
; PROCEDURE WarningArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
  RAISES { Thread . Alerted }

; PROCEDURE Error
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Thread . Alerted } 

; PROCEDURE ErrorArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull ) 
  RAISES { Thread . Alerted } 

; PROCEDURE Indent
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

; PROCEDURE IndentArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull ) 
  RAISES { Thread . Alerted } 

; VAR xxUnitLogWrT : Wr . T := NIL 

; PROCEDURE PutPositionImage ( WrT : Wr . T ; Pos : FM3Base . tPosition )

; PROCEDURE StartUnit
    ( UnitName : TEXT ; UnitLogWrT : Wr . T := NIL )
  RAISES { Thread . Alerted } 

; PROCEDURE EndUnit ( UnitName : TEXT ) RAISES { Thread . Alerted } 

; PROCEDURE AtomListToOSError ( AL : AtomList . T ): TEXT 

; END FM3Messages
.

