
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3OpenArray ( Elem )

(* Elem declares T, A type that has built-in "<" and ">" defined on it. *)

; IMPORT FM3Base
; FROM FM3Base IMPORT CmpLT , CmpEQ , CmpGT 

; PROCEDURE Compare ( Left , Right : T ) : FM3Base . CompareTyp 

  = VAR LLeftLen , LRightLen : CARDINAL
  ; VAR LSs : CARDINAL 

  ; BEGIN
      IF Left = Right THEN RETURN CmpEQ END (* IF *)
    ; IF Left = NIL THEN RETURN CmpLT END (* IF *) 
    ; IF Right = NIL THEN RETURN CmpGT END (* IF *) 
    ; LLeftLen := NUMBER ( Left ^ )  
    ; LRightLen := NUMBER ( Left ^ )
    ; IF LLeftLen < LRightLen THEN RETURN CmpLT END (* IF *)
    ; IF LLeftLen > LRightLen THEN RETURN CmpGT END (* IF *)
    ; IF LLeftLen = 0 THEN RETURN CmpEQ END (* IF *) 
    ; LSs := 0
    ; LOOP
        IF LSs >= LLeftLen THEN RETURN CmpEQ END (* IF *)
      ; IF Left ^ [ LSs ] < Right ^ [ LSs ] THEN RETURN CmpLT END (* IF *)
      ; IF Left ^ [ LSs ] < Right ^ [ LSs ] THEN RETURN CmpGT END (* IF *)
      ; INC ( LSs ) 
      END (*LOOP*) 
    END Compare  

; BEGIN
  END FM3OpenArray 
.
