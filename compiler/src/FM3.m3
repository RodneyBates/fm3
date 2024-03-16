
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3 EXPORTS Main

; IMPORT Stdio
; IMPORT Wr

; IMPORT FM3CLArgs
; IMPORT FM3Messages 
; IMPORT FM3Pass1 
; IMPORT FM3Pass2 
; IMPORT FM3Scanner
; IMPORT FM3SharedUtils
; IMPORT FM3Units 

; PROCEDURE Work ( )
  RAISES { FM3SharedUtils . FatalError , FM3SharedUtils . Terminate } 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPoppedUnitRef : FM3Units . UnitRefTyp
  ; VAR LDebug : INTEGER := 5 (* For breakpoint *) 
  ; VAR LTerminate : INTEGER := 7 (* For breakpoint *)

  ; BEGIN
      TRY (*EXCEPT*)
        TRY (*FINALLY*)
          FM3CLArgs . Process ( )
        ; FM3SharedUtils . LoadSets ( ) 
        ; FM3Scanner . Init ( )
        ; FM3Pass1 . RunPass1 ( FM3CLArgs . SrcFileName )
          (* ^POST:  A UnitRef is pushed. *)
        ; LUnitRef := FM3Units . UnitStackTopRef 
        ; FM3Pass2 . RunPass2 ( LUnitRef )

        ; LPoppedUnitRef := FM3Units . PopUnit ( )
        ; <* ASSERT LPoppedUnitRef = LUnitRef *> 
          FM3Messages . EndUnit ( LPoppedUnitRef ^ . UntSrcFileSimpleName ) 

        FINALLY FM3CLArgs . Cleanup ( ) 
        END (*FINALLY*)
      ; LDebug := 11 (* Ordinary completion.*)
      
      EXCEPT FM3SharedUtils . FatalError ( EMsg ) 
      =>  Wr . PutText ( Stdio . stderr , Wr . EOL )
        ; Wr . PutText ( Stdio . stderr , EMsg ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL )
        ; Wr . Flush ( Stdio . stderr )
        ; LDebug := 13 (* Complete by exception FatalError. *)
      | FM3SharedUtils . Terminate
      => LDebug := 17 (* Complete by exception Terminate. *) 
         
      END (*EXCEPT*)
    ; LTerminate := 19 
    END Work 

; BEGIN
    Work ( )
  END FM3
.


