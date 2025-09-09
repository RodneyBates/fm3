
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* The main module of the FM3 Modula-3 compiler. *)

MODULE FM3 EXPORTS Main

; IMPORT RT0 
; IMPORT RTProcess 
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr

; IMPORT FM3CLArgs
; IMPORT FM3CLOptions
; IMPORT FM3CLToks AS Clt 
; IMPORT FM3Compile
; IMPORT FM3Globals

(* W A R N I N G ! ! -------------------------
  The code in FM3Introspection is intended to be called by m3gdb commands.
  There may not be any calls on it in compiled code.  But if it is not
  named in any IMPORT in the export/import closure, the compiler will
  not set up global variable addressing for it in the way that m3gdb depends
  on.  So Don't delete this IMPORT even though it is not needed to compile
  and will provoke an "unused" warning.
*) 
; IMPORT FM3Introspection

; IMPORT FM3Messages 
; IMPORT FM3RTFailures 
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
        ; FM3Messages . FM3LogArr
            ( ARRAY OF REFANY
                { "Compiling with command line: "
                , FM3Messages . NLIndent
                , "  " 
                , FM3SharedUtils . ArgListAsText ( ) 
                , "\""
                }
            ) 
        ; FM3SharedUtils . LoadSets ( )
        ; FM3Globals . Init ( ) 
        ; FM3Scanner . Init ( )
        ; IF FALSE (* Rely on EXPORTS to bring these in. *) 
             AND Clt . CltStdSources IN FM3CLOptions . OptionTokSet
          THEN 
            FM3Compile . CompileOrLoadCLUnit ( "Main.i3" )
(*        
          ; FM3Compile . CompileOrLoadCLUnit ( "Word.i3" ) 
          ; FM3Compile . CompileOrLoadCLUnit ( "Word.m3" )
*)
          END (*IF*) 
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

 ; PROCEDURE Failure
     ( <* UNUSED *> READONLY Act : RT0 . RaiseActivation
     ; <* UNUSED *> StoppedReason : TEXT 
     ; <* UNUSED *> AllowedActions : FM3RTFailures . FailureActionSetTyp
     )
   : FM3RTFailures . FailureActionTyp

   = BEGIN
       RETURN FM3RTFailures . FailureActionTyp . FaCrash 
     END Failure
     
; BEGIN 
    FM3RTFailures . RegisterQueryProc ( Failure ) 
  ; TRY 
      Work ( )
    EXCEPT FM3RTFailures . Terminate => 
    END (*EXCEPT*) 
  END FM3
.


