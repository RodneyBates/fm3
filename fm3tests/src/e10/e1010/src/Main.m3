MODULE Main

(* Illegal recursive declarations. *)

; TYPE R = REF R (* This is legal. *)

; TYPE A = ARRAY OF A 

; TYPE D = ARRAY OF E

; TYPE B = ARRAY OF C 

; TYPE Rec = RECORD
      F1 : Rec 
    ; F2 : Rec
    END 

; TYPE C = ARRAY OF D  

; TYPE E = ARRAY OF B  

; TYPE S = REF S (* This is legal. *)

; BEGIN
  END Main
. 