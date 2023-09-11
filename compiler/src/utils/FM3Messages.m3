        
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

; IMPORT FM3SharedUtils

  (* Fatal amd Log go immediatly to stderr and optionally to a log file. *)

; PROCEDURE PutStdErr ( Msg : TEXT ) RAISES { Thread . Alerted } 

  = BEGIN
      IF DoStdErr
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
      IF DoStdOut
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( Stdio . stdout , Msg )
        ; Wr . PutText ( Stdio . stdout , Wr . EOL )
        ; Wr . Flush ( Stdio . stdout ) 
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutStdOut

; VAR GUnitLogWrT : Wr . T := NIL 

; PROCEDURE PutUnitLog ( Msg : TEXT ) RAISES { Thread . Alerted }
  (* Or, resort to compiler log, if can't do that. *)

  = BEGIN
      IF DoCompLog
         AND GUnitLogWrT # NIL 
         AND NOT Wr . Closed ( GUnitLogWrT ) 
      THEN
        TRY (*EXCEPT*)
          Wr . PutText ( GUnitLogWrT , Msg )
        ; Wr . PutText ( GUnitLogWrT , Wr . EOL )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      ELSE PutLog ( Msg ) 
      END (*IF*) 
    END PutUnitLog

; PROCEDURE PutLog ( Msg : TEXT ) RAISES { Thread . Alerted } 

  = BEGIN
      IF DoLog 
         AND LogFileWrT # NIL 
         AND NOT Wr . Closed ( LogFileWrT ) 
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( LogFileWrT , Msg )
        ; Wr . PutText ( LogFileWrT , Wr . EOL )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutLog 

(*EXPORTED*)
; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  (* Also terminates the program. *)

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "FM3 FATAL: " , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 )
    ; TRY (*EXCEPT*)
        PutStdErr ( LMsg ) 
      ; PutLog ( LMsg ) 
      ; RAISE FM3SharedUtils . Terminate ( LMsg ) 
      EXCEPT Thread . Alerted => END (*EXCEPT*) 
    END Fatal  

(*EXPORTED*)
; PROCEDURE FatalArr ( READONLY Frags : ARRAY OF REFANY )
  RAISES { FM3SharedUtils . Terminate }
  (* Also terminates the program. *) 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatArrT ( Frags , "FM3 FATAL: " ) 
    ; TRY (*EXCEPT*)
        PutStdErr ( LMsg ) 
      ; PutLog ( LMsg ) 
      ; RAISE FM3SharedUtils . Terminate ( LMsg ) 
      EXCEPT Thread . Alerted => END (*EXCEPT*) 
    END FatalArr  

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
    ; PutUnitLog ( LMsg ) 
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
    ; PutUnitLog ( LMsg ) 
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
    ; PutUnitLog ( LMsg ) 
    END Error

(*EXPORTED*)
; PROCEDURE ErrorArr ( READONLY Frags : ARRAY OF REFANY ) 
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatArrT ( Frags , "Error " ) 
    ; PutStdOut ( LMsg ) 
    ; PutUnitLog ( LMsg ) 
    END ErrorArr

(*EXPORTED*)
; PROCEDURE StartUnit
    ( UnitName : TEXT ; UnitLogWrT : Wr . T := NIL )
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      GUnitLogWrT := UnitLogWrT 
    ; LMsg := FM3SharedUtils . CatStrings ( "Start unit " , UnitName ) 
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
    ; GUnitLogWrT := UnitLogWrT 
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

