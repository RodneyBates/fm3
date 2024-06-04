MODULE Main

(* Test local hiding of exported and imported names. *) 

; IMPORT I1
; IMPORT I1 AS IA1
; FROM I1 IMPORT IC

; PROCEDURE P ( )

  = VAR V : INTEGER
  
  (* Local declarations, same names as exported & imported names. *) 
  ; CONST MV1 = 0 
  ; CONST MV2 = 1 
  ; CONST M3 = 2

  ; CONST I1 = 3
  ; CONST IA1 = 4
  ; CONST IC = 5
  
  ; BEGIN 
    (* References to the local names. *) 
      V := MV1 
    ; V := MV2 
    ; V := M3 

    ; V := I1 
    ; V := IA1 
    ; V := IC
    END P 

; BEGIN
  END Main
.

