        
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

; IMPORT FM3Globals 
; IMPORT FM3Messages 

; PROCEDURE SetDefaults ( )

  = BEGIN
      SrcFileName := "test1.m3" (* Temporary, during development *)

    ; DoKeep := TRUE (* Temporary, during development *)
        (* Keep intermediate files. *)

    ; FM3Messages . DoStdErr := TRUE
        (* Write compilation process messages to stderr. *)

    ; FM3Messages . DoLog := TRUE
        (* Write compilation process messages to compiler log file. *)
    ; FM3Messages . LogFileName := "FM3Log"

    ; FM3Messages . DoStdOut := TRUE
        (* Write compiled code messages to stdout. *)

    ; FM3Messages . DoCompLog := TRUE
        (* Write compiled code messages to unit-specific log file. *)

    ; FM3Globals . ResourcePathName := "../lib" 

    END SetDefaults

; PROCEDURE HandleOptions ( ) 

  = BEGIN
(* COMPLETEME *)
    END HandleOptions 

; PROCEDURE ComputeDerivedInfo ( ) 

  = BEGIN
      IF FM3Messages . DoLog
      THEN 
        TRY FM3Messages . LogFileWrT
              := FileWr . Open ( FM3Messages . LogFileName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => Wr . PutText ( Stdio . stderr , "Unable to open log file " ) 
        ; Wr . PutText ( Stdio . stderr , FM3Messages . LogFileName ) 
        ; Wr . PutText ( Stdio . stderr , ": " ) 
        ; Wr . PutText
            ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText ( Stdio . stderr , "Will proceed without it." ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . Flush ( Stdio . stderr ) 
        ; FM3Messages . DoLog := FALSE  
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
      Wr . Close ( FM3Messages . LogFileWrT ) 
    END Cleanup 

; BEGIN
  END FM3CLArgs
.

