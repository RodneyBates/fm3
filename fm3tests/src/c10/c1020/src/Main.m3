MODULE Main EXPORTS Exp

; IMPORT Imp
; FROM Imp IMPORT I2 

(* Test accessing tokens for various exported and imported things. *)

; CONST M1 = 10

(* Cases handled by FM3Pass2.IdentRefR2L: *)
; CONST M2 = M1 (* Local decl. *)
; CONST M3 = I2
; CONST M4 = E1

(* Cases handled by FM3Pass2.QualIdentRefR2L: *)

; CONST M5 = M1 . Field (* Make separate Id and dot tokens. *)
; CONST M6 = Imp . I1
; CONST M7 = I2 . Field 
; CONST M8 = E1 . Field 

; BEGIN
  END Main
. 