MODULE main

(* Compile-time error tests for FM3Pass1. *) 

; IMPORT Misnamed

; VAR V

; TYPE R
    = RECORD
        RF1 
      END
; TYPE O
    = OBJECT
        OOOOF1 
      END 

; PROCEDURE P
    ( PF1 
    ; VALUE PF2  
    ; VAR PF3  
    ; READONLY PF4  
    )
  = BEGIN
    END ppp

; BEGIN
  END mainn
.

