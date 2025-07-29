MODULE Main 

(* C test, identifier references. *)

; VAR X : CHAR 

; TYPE P = REF R

; TYPE Q = BRANDED REF X

; TYPE R = BRANDED "brand" REF X 

; TYPE S
  = RECORD
      F1 : REF X
    ; F2 : INTEGER := 10 + 20 
    END 

; BEGIN
  END Main
.

