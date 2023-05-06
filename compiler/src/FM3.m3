MODULE FM3 EXPORTS Main 

; IMPORT FM3CLArgs
; IMPORT FM3Messages 
; IMPORT FM3ParsePass 
; IMPORT FM3Scanner

; PROCEDURE Work ( )

  = BEGIN
      FM3CLArgs . Process ( ) 
    ; FM3ParsePass . Run ( ) 
    END Work 

; BEGIN
    TRY Work ( )
    FINALLY
      FM3CLArgs . Cleanup ( ) 
    END (*FINALLY*) 
  END FM3
.

