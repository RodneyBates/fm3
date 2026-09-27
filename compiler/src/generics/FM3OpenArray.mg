
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2026        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3OpenArray ( Elem )

(* Elem declares T, A type that has built-in "<" and ">" defined on it. *)

; IMPORT FM3Base
; FROM FM3Base IMPORT CmpLT , CmpEQ , CmpGT
; IMPORT FM3Utils 

(*EXPORTED:*)
; PROCEDURE Hash ( Key : T ) : FM3Base . HashTyp 

  = VAR LResult : FM3Utils . HashTyp 
  ; VAR LTerm : FM3Utils . HashTyp 

  ; BEGIN (*Hash*) 
      IF Key = NIL THEN RETURN FM3Base . HashConst END (*IF*) 
    ; LResult := FM3Base . HashNull
    ; FOR RI := FIRST ( Key ^ ) TO  LAST ( Key ^ )
      DO LTerm
           := VAL ( ORD ( Elem . Hash ( Key ^ [ RI ] ) ) , FM3Base . HashTyp )
      ; FM3Utils . ContribToHashI ( (*IN OUT*) LResult , LTerm )  
      END (*FOR*)
    ; IF LResult = FM3Base . HashNull
      THEN LResult := FM3Base . HashConst
      END (*IF*) 
    ; RETURN LResult 
    END Hash

(*EXPORTED:*)
; PROCEDURE Compare ( READONLY Left , Right : T ) : FM3Base . CompareTyp 

  = VAR LLeftLen , LRightLen : CARDINAL
  ; VAR LSs : CARDINAL 

  ; BEGIN
      IF Left = Right THEN RETURN CmpEQ END (*IF*)
    ; IF Left = NIL THEN RETURN CmpLT END (*IF*) 
    ; IF Right = NIL THEN RETURN CmpGT END (*IF*) 
    ; LLeftLen := NUMBER ( Left ^ )  
    ; LRightLen := NUMBER ( Left ^ )
    ; IF LLeftLen < LRightLen THEN RETURN CmpLT END (*IF*)
    ; IF LLeftLen > LRightLen THEN RETURN CmpGT END (*IF*)
    ; IF LLeftLen = 0 THEN RETURN CmpEQ END (*IF*) 
    ; LSs := 0
    ; LOOP
        IF LSs >= LLeftLen THEN RETURN CmpEQ END (*IF*)
      ; IF Left ^ [ LSs ] < Right ^ [ LSs ] THEN RETURN CmpLT END (*IF*)
      ; IF Left ^ [ LSs ] > Right ^ [ LSs ] THEN RETURN CmpGT END (*IF*)
      ; INC ( LSs ) 
      END (*LOOP*) 
    END Compare

; BEGIN
  END FM3OpenArray 
.
