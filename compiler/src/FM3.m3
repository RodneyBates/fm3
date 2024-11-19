
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* The main module of the FM3 Modula-3 compiler. *)

MODULE FM3 EXPORTS Main

; IMPORT RTProcess 
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr

; IMPORT FM3CLArgs
; IMPORT FM3Compile
; IMPORT FM3Globals 
; IMPORT FM3Scanner
; IMPORT FM3SharedUtils

; PROCEDURE Work ( )
  RAISES { FM3SharedUtils . FatalError , FM3SharedUtils . Terminate } 

  = VAR LDebug : INTEGER := 5 (* For breakpoint *) 
  ; VAR LTerminate : INTEGER := 7 (* For breakpoint *)

  ; <*FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      TRY (*EXCEPT*)
        TRY (*FINALLY*)
          FM3CLArgs . Process ( )
        ; FM3SharedUtils . LoadSets ( )
        ; FM3Globals . Init ( ) 
        ; FM3Scanner . Init ( )
        ; FM3Compile . CompileOrLoadCLUnit ( "Main.i3" )
(*        
        ; FM3Compile . CompileOrLoadCLUnit ( "Word.i3" ) 
        ; FM3Compile . CompileOrLoadCLUnit ( "Word.m3" )
*) 
        ; FM3Compile . CompileCLUnits ( )
        ; FM3Globals . Finalize ( ) 
        FINALLY FM3CLArgs . Cleanup ( ) 
        END (*FINALLY*)
      ; LDebug := 11 (* Ordinary completion.*)
      
      EXCEPT
      | FM3SharedUtils . Terminate ( EMsg ) 
      =>  Wr . PutText ( Stdio . stderr , Wr . EOL )
        ; IF EMsg # NIL
          THEN 
            Wr . PutText ( Stdio . stderr , EMsg ) 
          ; Wr . PutText ( Stdio . stderr , Wr . EOL )
          END (*IF*) 
        ; Wr . Flush ( Stdio . stderr )
        ; LDebug := 13 (* Complete by exception Terminate. *) 
        ; RTProcess . Exit ( 11 ) 
         
      | FM3SharedUtils . AllocationFailure ( EMsg ) 
      =>  Wr . PutText ( Stdio . stderr , Wr . EOL )
        ; IF EMsg # NIL
          THEN 
            Wr . PutText ( Stdio . stderr , EMsg ) 
          ; Wr . PutText ( Stdio . stderr , Wr . EOL )
          END (*IF*) 
        ; Wr . Flush ( Stdio . stderr )
        ; LDebug := 15 (* Complete by exception "AllocationFailure. *) 
        ; RTProcess . Exit ( 15 ) 
         
      | FM3SharedUtils . FatalError ( EMsg ) 
      =>  Wr . PutText ( Stdio . stderr , Wr . EOL )
        ; IF EMsg # NIL
          THEN 
            Wr . PutText ( Stdio . stderr , EMsg ) 
          ; Wr . PutText ( Stdio . stderr , Wr . EOL )
          END (*IF*) 
        ; Wr . Flush ( Stdio . stderr )
        ; LDebug := 17 (* Complete by exception FatalError. *)
        ; RTProcess . Exit ( 17 ) 
        
      END (*EXCEPT*) 
    ; LTerminate := 19 
    END Work 

; BEGIN
    Work ( )
  END FM3
.


