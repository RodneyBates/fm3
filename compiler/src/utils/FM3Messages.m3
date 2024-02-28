        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Messages

; IMPORT AtomList
; IMPORT Fmt 
; IMPORT Stdio
; IMPORT TextWr 
; IMPORT Thread 
; IMPORT Wr

; IMPORT FM3Base 
; IMPORT FM3SharedUtils
; IMPORT FM3TextColors
; IMPORT FM3Units

  (* Fatal and Log go immediatly to stderr and optionally to a log file. *)

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
      IF DoUnitLog
         AND GUnitLogWrT # NIL 
         AND NOT Wr . Closed ( GUnitLogWrT ) 
      THEN
        TRY (*EXCEPT*)
          Wr . PutText ( GUnitLogWrT , Msg )
        ; Wr . PutText ( GUnitLogWrT , Wr . EOL )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      ELSE PutFM3Log ( Msg ) 
      END (*IF*) 
    END PutUnitLog

; PROCEDURE PutFM3Log ( Msg : TEXT ) RAISES { Thread . Alerted } 

  = BEGIN
      IF DoFM3Log 
         AND FM3LogFileWrT # NIL 
         AND NOT Wr . Closed ( FM3LogFileWrT ) 
      THEN
        TRY (*EXCEPT*) 
          Wr . PutText ( FM3LogFileWrT , Msg )
        ; Wr . PutText ( FM3LogFileWrT , Wr . EOL )
        EXCEPT Wr . Failure =>
        END (*EXCEPT*)
      END (*IF*) 
    END PutFM3Log 

; VAR GFM3LabelT := 
        FM3TextColors . FGDkGreen & "FM3: " & FM3TextColors . Reset 

; VAR GFM3FatalLabelT
        := FM3TextColors . FGRed & "FM3 FATAL: " & FM3TextColors . Reset 

(*EXPORTED*)
; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { FM3SharedUtils . FatalError }
  (* Also terminates the program. *)

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( GFM3FatalLabelT , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 )
    ; TRY (*EXCEPT*)
        PutStdErr ( LMsg ) 
      ; PutFM3Log ( LMsg ) 
      ; RAISE FM3SharedUtils . FatalError ( "" ) 
      EXCEPT Thread . Alerted => END (*EXCEPT*) 
    END Fatal  

(*EXPORTED*)
; PROCEDURE FatalArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
  RAISES { FM3SharedUtils . FatalError }
  (* Also terminates the program. *) 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatArrT ( Frags , GFM3FatalLabelT ) 
    ; TRY (*EXCEPT*)
        PutStdErr ( LMsg ) 
      ; PutFM3Log ( LMsg ) 
      ; RAISE FM3SharedUtils . FatalError ( "" ) 
      EXCEPT Thread . Alerted => END (*EXCEPT*) 
    END FatalArr  

(*EXPORTED*)
; PROCEDURE FM3Log
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( GFM3LabelT
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdErr ( LMsg ) 
    ; PutFM3Log ( LMsg ) 
    END FM3Log

(*EXPORTED*)
; PROCEDURE FM3LogArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatArrT ( Frags , GFM3LabelT ) 
    ; TRY (*EXCEPT*)
        PutStdErr ( LMsg ) 
      ; PutFM3Log ( LMsg ) 
      EXCEPT Thread . Alerted => END (*EXCEPT*) 
    END FM3LogArr
    
(* -------------- Messages about code being compiled. --------------- *)

; PROCEDURE CodeMsgText
    ( READONLY Pos : FM3Base . tPosition
    ; READONLY Label , Body: ARRAY OF REFANY
    )
  : TEXT
  (* Will look funny if either Label or Body displays as empty. *) 

  = VAR LWrT : Wr . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LBlankNeeded := FALSE 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; FM3SharedUtils . PutTextishArr ( LWrT , Label ) 
    ; LUnitRef := FM3Units . UnitStackTopRef 
    ; IF LUnitRef # NIL
      THEN
      (* I think we don't really want the whole path:  
        Wr . PutText ( LWrT , LUnitRef . UntSrcFilePath )
      ; Wr . PutChar ( LWrT , ' ' ) 
      ; *)
        Wr . PutText ( LWrT , LUnitRef . UntSrcFileName )
      ; LBlankNeeded := TRUE 
      END (*IF*)

    ; IF Pos # FM3Base . PositionNull
      THEN
        PutPositionImage ( LWrT , Pos )
      ; LBlankNeeded := TRUE 
      END (*IF*)
      
    ; IF LBlankNeeded THEN Wr . PutChar ( LWrT , ' ' ) END (*IF*)
    ; FM3SharedUtils . PutTextishArr ( LWrT , Body ) 

    ; RETURN TextWr . ToText ( LWrT ) 
    END CodeMsgText

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

; VAR GInfoLabel := ARRAY [ 0 .. 2 ] OF REFANY 
        { FM3TextColors . FGDkGreen 
        , "INFO: "
        , FM3TextColors . Reset 
        }

(*EXPORTED*)
; PROCEDURE InfoArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := CodeMsgText ( Pos , GInfoLabel , Frags ) 
    ; PutStdOut ( LMsg )
    ; PutUnitLog ( LMsg ) 
    END InfoArr 

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

; VAR GWarningLabel := ARRAY [ 0 .. 2 ] OF REFANY
        { FM3TextColors . FGDkOrange 
        , "WARNING: "
        , FM3TextColors . Reset 
        }

(*EXPORTED*)
; PROCEDURE WarningArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := CodeMsgText ( Pos , GWarningLabel , Frags ) 
    ; PutStdOut ( LMsg ) 
    ; PutUnitLog ( LMsg ) 
    END WarningArr

(*EXPORTED*)
; PROCEDURE Error
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "Error: " 
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg ) 
    ; PutUnitLog ( LMsg ) 
    END Error

; VAR GErrorLabel := ARRAY [ 0 .. 2 ] OF REFANY
        { FM3TextColors . FGDkRed 
        , "ERROR: "
        , FM3TextColors . Reset 
        }

(*EXPORTED*)
; PROCEDURE ErrorArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull ) 
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := CodeMsgText ( Pos , GErrorLabel , Frags ) 
    ; PutStdOut ( LMsg ) 
    ; PutUnitLog ( LMsg ) 
    END ErrorArr

(*EXPORTED*)
; PROCEDURE Indent
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )
  RAISES { Thread . Alerted }

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := FM3SharedUtils . CatStrings
             ( "    " 
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg ) 
    ; PutUnitLog ( LMsg ) 
    END Indent

(*EXPORTED*)
; PROCEDURE IndentArr
    ( READONLY Frags : ARRAY OF REFANY ; Pos := FM3Base . PositionNull ) 
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatArrT ( Frags , "    " ) 
    ; PutStdOut ( LMsg ) 
    ; PutUnitLog ( LMsg ) 
    END IndentArr

(*EXPORTED*)
; PROCEDURE PutPositionImage ( WrT : Wr . T ; Pos : FM3Base . tPosition )

  = BEGIN 
      Wr . PutChar ( WrT , '[' )
    ; Wr . PutText ( WrT , Fmt . Int ( Pos . Line ) ) 
    ; Wr . PutChar ( WrT , ',' ) 
    ; Wr . PutText ( WrT , Fmt . Int ( Pos . Column ) ) 
    ; Wr . PutChar ( WrT , ']' )
    END PutPositionImage 


(*EXPORTED*)
; PROCEDURE StartUnit
    ( UnitName : TEXT ; UnitLogWrT : Wr . T := NIL )
  RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      GUnitLogWrT := UnitLogWrT 
    ; LMsg := FM3SharedUtils . CatStrings ( "Start unit " , UnitName ) 
    ; PutStdErr ( LMsg ) 
    ; PutFM3Log ( LMsg ) 
    END StartUnit 

(*EXPORTED*)
; PROCEDURE EndUnit ( UnitName : TEXT ) RAISES { Thread . Alerted } 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := FM3SharedUtils . CatStrings ( "End unit " , UnitName ) 
    ; PutStdErr ( LMsg ) 
    ; PutFM3Log ( LMsg ) 
    ; Wr . Close ( GUnitLogWrT ) 
    ; GUnitLogWrT := NIL 
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

