        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Messages

; IMPORT AtomList
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr

; IMPORT FM3CLArgs 
; IMPORT FM3Globals 
; IMPORT FM3SharedUtils

  (* Fatal amd Log go immediatly to stderr and optionally to a log file. *)

; PROCEDURE PutStdErr ( Msg : TEXT ) RAISES { Thread . Alerted } 

  = BEGIN
      IF FM3CLArgs . DoStdErr
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( Stdio . stderr , Msg )
        ; Wr . PutText ( Stdio . stderr , Wr . EOL )
        ; Wr . Flush ( Stdio . stderr )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutStdErr 

; PROCEDURE PutStdOut ( Msg : TEXT ) RAISES { Thread . Alerted }

  = BEGIN
      IF FM3CLArgs . DoStdOut
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( Stdio . stdout , Msg )
        ; Wr . PutText ( Stdio . stdout , Wr . EOL )
        ; Wr . Flush ( Stdio . stdout ) 
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutStdOut 

; PROCEDURE PutCompileLog ( Msg : TEXT ) RAISES { Thread . Alerted } 

  = BEGIN
      IF FM3CLArgs . DoCompLog
         (* => FM3Globals . CurrentUnitRef . UntCompLogWrT # NIL and is open. *)
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( FM3Globals . CurrentUnitRef . UntCompLogWrT , Msg )
        ; Wr . PutText ( FM3Globals . CurrentUnitRef . UntCompLogWrT , Wr . EOL )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutCompileLog

; PROCEDURE PutLog ( Msg : TEXT ) RAISES { Thread . Alerted } 

  = BEGIN
      IF FM3CLArgs . DoLog (* => FM3CLArgs . LogWrT # NIL and is open. *) 
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( FM3CLArgs . LogFileWrT , Msg )
        ; Wr . PutText ( FM3CLArgs . LogFileWrT , Wr . EOL )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutLog 

(*EXPORTED*)
; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Terminate }
  (* Also terminates the program. *)

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "FM3 FATAL:"
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             )
    ; TRY (*EXCEPT*)
        PutStdErr ( LMsg ) 
      ; PutLog ( LMsg ) 
      ; RAISE Terminate
      EXCEPT Thread . Alerted => END (*EXCEPT*) 
    END Fatal  

(*EXPORTED*)
; PROCEDURE Log
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "FM3: "
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    END Log

(* Within a unit, Info, Warning, and Error are collected, sorted by
   line/column, and written to stdout at the end of the unit. *)

(*EXPORTED*)
; PROCEDURE Info
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "Info: "
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg )
    ; PutCompileLog ( LMsg ) 
    END Info 

(*EXPORTED*)
; PROCEDURE Warning
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "Warning: "
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg ) 
    ; PutCompileLog ( LMsg ) 
    END Warning

(*EXPORTED*)
; PROCEDURE Error
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "Error " 
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg ) 
    ; PutCompileLog ( LMsg ) 
    END Error

(*EXPORTED*)
; PROCEDURE StartUnit ( UnitName : TEXT ) RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatStrings ( "Start unit " , UnitName ) 
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    END StartUnit 

(*EXPORTED*)
; PROCEDURE EndUnit ( UnitName : TEXT ) RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatStrings ( "End unit " , UnitName ) 
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    END EndUnit 

(*EXPORTED*)
; PROCEDURE AtomListToOSError ( AL : AtomList . T ): TEXT

  = BEGIN
      RETURN "OSError.E("
             & FM3SharedUtils . AtomListToText ( AL )
             & ")" 
    END AtomListToOSError  

; BEGIN
  END FM3Messages
.

