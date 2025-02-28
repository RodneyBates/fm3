INTERFACE Intfxx

(* FM3 test INTERFACE case for errors detected in Pass1. *)

; VAR Va : R1 
; VAR Vb := C0 
; VAR V 

; PROCEDURE Pa ( F : R1 ; G := C0 )
; PROCEDURE P ( F ; VAR G := Ss ; READONLY H ; VALUE I )
; PROCEDURE Body ( ) = BEGIN END Body 

; TYPE Ra = RECORD S : R1 ; T := C0 END 
; TYPE R = RECORD S ; T END 

; TYPE Obja = OBJECT METHODS m ( F : CHAR ) END
; TYPE Obj = OBJECT METHODS m ( G ) END
; TYPE Objk = OBJECT Field END
; TYPE Objl = OBJECT METHODS m ( G ) END
; TYPE Objm = OBJECT METHODS CHAR ( ) END

; TYPE Overa = Obja OBJECT OVERRIDES TEXT = BOOLEAN END
; TYPE Over = Obja OBJECT OVERRIDES m = BOOLEAN END

; CONST INTEGER = FALSE

; CONST C0 = 0 

; PROCEDURE real ( ) (* FIX Illegal recursive reql *) 
; CONST C1a = real ( ) 
; CONST C1 = REAL ( Ss )

; CONST Ss = 3 
; VAR char : ARRAY [ 0 ..9 ] OF CHAR 
; CONST C2a = char [ Ss ]
; CONST C2 = CHAR [ Ss ]

; VAR Fake : Ra
; CONST C3a = Ra . S  
; CONST C3b = Fake . S  
; CONST C3 = CARDINAL . MUTEX

; VAR Fake : Ra
; CONST C3b = Fake . S  

; CONST C4a = P ^  
; CONST C4 = FALSE ^ 

; TYPE R1 = INTEGER 
; TYPE Not_R1 = INTEGER

; VAR W0 :=  ABS
; VAR W1 :=  ABS ( )
; VAR W2 :=  ABS ( Va )
; VAR W3 :=  ABS ( Va , Va )
; VAR W4 :=  ABS ( Va , Va )

; VAR W5 :=  MIN ( Va )  
; VAR W6 :=  MIN ( Va , Va )  
; VAR W7 :=  MIN ( Va , Va , Va )

; VAR W8 := NEW ( ) 
; VAR W9 := NEW ( Va , Va )
; VAR W10 := NEW ( Va ) 
; VAR W11 := NEW ( Va , Va , Va ,Va , Va )
; VAR W12 := NEW ( Va , Va )

; VAR S1 := SUBARRAY ( )
; VAR S1 := SUBARRAY ( Va ) 
; VAR S1 := SUBARRAY ( Va , Va ) 
; VAR S1 := SUBARRAY ( Va , Va , Va )

; VAR S1 := SUBARRAY ( Va , Va , Va , Va ) 

; TYPE R2 = INTEGER 

; TYPE R3 = INTEGER 

; TYPE R4 = INTEGER 

; TYPE R5 = INTEGER 

; TYPE T1 = REF Not_R1 

; TYPE T2 = BRANDED REF R2 

; TYPE T3 = BRANDED "Brand3" REF R3 

; TYPE T4 = UNTRACED REF R4 

; TYPE T1 = UNTRACED BRANDED "Brand5" REF R5 

; END Intfyy
.
  