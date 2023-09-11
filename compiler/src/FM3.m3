MODULE FM3 EXPORTS Main

; IMPORT Stdio
; IMPORT Wr 

; IMPORT FM3CLArgs
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3ParsePass 
; IMPORT FM3Scanner
; IMPORT FM3SharedUtils 

; PROCEDURE Work ( )
  RAISES { FM3SharedUtils . FatalError , FM3SharedUtils . Terminate } 

  = VAR LDebug : INTEGER := 5 (* For breakpoint *) 
  ; VAR LTerminate : INTEGER := 7 (* For breakpoint *)

  ; BEGIN
      TRY (*EXCEPT*)
        TRY (*FIMALLY*)
          FM3CLArgs . Process ( )
        ; FM3SharedUtils . ResourcePathName := FM3Globals . ResourcePathName
        ; FM3SharedUtils . LoadSets ( ) 
        ; FM3Scanner . Init ( ) 
        ; FM3ParsePass . Run ( )
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


