        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3CLArgs

; IMPORT FileWr
; IMPORT Wr
; IMPORT OSError 
; IMPORT Stdio

; IMPORT FM3Messages 

; PROCEDURE SetDefaults ( )

  = BEGIN
      SrcFileName := "test1.m3" (* Temporary, during development *)

    ; DoKeep := TRUE (* Temporary, during development *)
        (* Keep intermediate files. *)

    ; DoStdErr := TRUE
        (* Write compilation process messages to stderr. *)

    ; DoLog := TRUE
        (* Write compilation process messages to cmpiler log file. *)
    ; LogFileName := "FM3Log"
    ; LogFileWrT := FileWr . Open ( LogFileName )
      (* ^DoLog => Non-NIL and open. *) 

    ; DoStdOut := TRUE
        (* Write compiled code messages to stdout. *)

    ; DoCompLog := TRUE
        (* Write compiled code messages to unit-specific log file. *)

    END SetDefaults

; PROCEDURE HandleOptions ( ) 

  = BEGIN
    END HandleOptions 

; PROCEDURE ComputeDerivedInfo ( ) 

  = BEGIN
      IF DoLog
      THEN 
        TRY LogFileWrT := FileWr . Open ( LogFileName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => Wr . PutText ( Stdio . stderr , "Unable to open log file " ) 
        ; Wr . PutText ( Stdio . stderr , LogFileName ) 
        ; Wr . PutText ( Stdio . stderr , ": " ) 
        ; Wr . PutText
            ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText ( Stdio . stderr , "Will proceed without it." ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . Flush ( Stdio . stderr ) 
        ; DoLog := FALSE  
        END (*EXCEPT*)
      END (*IF*) 
    END ComputeDerivedInfo

(*EXPORTED*)
; PROCEDURE Process ( ) 

  = BEGIN
      SetDefaults ( )
    ; HandleOptions ( )
    ; ComputeDerivedInfo ( ) 
    END Process 

(*EXPORTED*)
; PROCEDURE Cleanup ( )

  = BEGIN
      Wr . Close ( LogFileWrT ) 
    END Cleanup 

; BEGIN
  END FM3CLArgs
.

