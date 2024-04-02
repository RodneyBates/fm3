MODULE Main

(* Test isolated expresssion operators. *)

; CONST A = B OR C
; CONST B = D AND E
; CONST C = NOT F

; CONST D = G = H 
; CONST E = I # J 
; CONST F = K < L 
; CONST G = M <= N 
; CONST H = O > P 
; CONST I = Q >= R
; CONST J = S IN T

; CONST K = A + B
; CONST L = C - D
; CONST M = E & F

; CONST N = G * H 
; CONST O = I / J
; CONST P = K DIV L 
; CONST Q = M MOD N

; CONST R = + O
; CONST S = - P

; CONST T = Q 

; BEGIN
  END Main
.
