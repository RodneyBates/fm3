MODULE Main

(* CT errors concerning exports and imports,
   mostly detected in FM3ExpImp.m3.
*) 

EXPORTS Main
, Exp  

; FROM Exp IMPORT E1
; FROM Exp IMPORT
    E2
  , E3
  , E4

; FROM Exp IMPORT D1
; FROM Exp IMPORT M3 
; FROM Exp IMPORT D2 

; IMPORT Exp
; IMPORT Exp 
; BEGIN
  END Main
.

