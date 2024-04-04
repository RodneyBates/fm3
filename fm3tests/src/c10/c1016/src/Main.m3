MODULE Main

(* Test E7 expression combinations, isolated. *)

; CONST A = B . C

; CONST B = D ^ 
; CONST C = F ( X , B := Y ) 
; CONST D = G [ A , B ]

; CONST E = ( C ) ^ 
; CONST F = ( D ) ( ) 
; CONST G = ( E ) ( F ) 
; CONST H = ( F ) [ G , H ]
; CONST I = L . M . N

; CONST J = 11 ( )  
; CONST K = 13L [ 0 ] 
; CONST L = 'O' ^ 

; BEGIN
  END Main
.
