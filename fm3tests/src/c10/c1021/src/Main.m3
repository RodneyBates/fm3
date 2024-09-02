MODULE Main 

(* C test, legal recursive decls. *)

; TYPE P = REF R

; TYPE R
  = RECORD
      F1 : P 
    END 

; BEGIN
  END Main
.
