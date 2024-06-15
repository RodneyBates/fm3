MODULE Main EXPORTS Lotsa 

(* Compile-time error tests for FM3Pass2. *) 

; IMPORT Intf
; IMPORT Nonexistent 

; FROM Intf IMPORT Mumble

; TYPE T1 = Intf
; TYPE IntfDotYadda = Intf . Yadda  
; TYPE IntfDotImppp = Intf . Imppp
; TYPE Undecl = NoSuch 
; TYPE NoQual = NoIntf . Nada
; TYPE Worthless = Nonexistent
; TYPE Good = E1
; TYPE Bad = E2

; BEGIN
  END Main
.

