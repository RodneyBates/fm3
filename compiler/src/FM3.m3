MODULE FM3 EXPORTS Main 

; IMPORT FM3CLArgs
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3ParsePass 
; IMPORT FM3Scanner
; IMPORT FM3SharedUtils 

; PROCEDURE Work ( ) RAISES { FM3SharedUtils . FatalError } 

  = BEGIN
      FM3CLArgs . Process ( )
    ; FM3SharedUtils . ResourcePathName := FM3Globals . ResourcePathName
    ; FM3SharedUtils . LoadSets ( ) 
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
    | FM3SharedUtils . FatalError =>
    END (*EXCEPT*) 
  END FM3
.


