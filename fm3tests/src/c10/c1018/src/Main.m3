MODULE Main

(* Test various ways of referencint things in other interfaces. *) 

; IMPORT I1
; IMPORT I1 AS IA1
; FROM I1 IMPORT IC

; CONST MainM1 = MV1 
; CONST MainM2 = MV2 
; CONST MainM3 = M3 

; CONST MainC1 = I1 . IC
; CONST MainC2 = IA1 . IC
; CONST MainC3 = IC

; BEGIN
  END Main
.

