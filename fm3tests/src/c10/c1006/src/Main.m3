MODULE Main 

(* C test, identifier references. *)

; TYPE P <: R . Y
; TYPE R <: ROOT

; REVEAL P .  Q = REF R

; REVEAL R1
  <: REF RECORD
      F1 : R 
    ; F2 : P
    END 

; BEGIN
  END Main
.

