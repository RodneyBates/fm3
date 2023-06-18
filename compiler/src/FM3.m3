MODULE FM3 EXPORTS Main 

; IMPORT FM3CLArgs
; IMPORT FM3Messages 
; IMPORT FM3ParsePass 
; IMPORT FM3Scanner
; IMPORT FM3SharedUtils 

; PROCEDURE Work ( )

  = BEGIN
      FM3CLArgs . Process ( )
    ; FM3Scanner . Init ( ) 
    ; FM3ParsePass . Run ( ) 
    END Work 

; BEGIN
    TRY (*EXCEPT*)
      TRY Work ( )
      FINALLY
        FM3CLArgs . Cleanup ( ) 
      END (*FINALLY*)
    EXCEPT FM3SharedUtils . Terminate =>
    END (*EXCEPT*) 
  END FM3
.


