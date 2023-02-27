
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3OpenArray ( Elem )

(* Elem declares T, A type that has built-in "<" and ">" defined on it.  

; IMORT FM3Base
; FROM FM3Base IMPORT CmpLT , CmpEQ , CmpGT 

; TYPE T = REF ARRAY OF Elem . T 

; CONST Brand = "FM3OpenArray"

; PROCEDURE Compare ( Left , Right : T ) : FM3Base . CmpTyp 

  = VAR LLeftLen , LRightLen : CARDINAL
  ; VAR LSs : CARDINAL 

  ; BEGIN
      IF Left = Right THEN RETURN CmpTyp . EQ END (* IF *)
    ; IF Left = NIL THEN RETURN CmpTyp . LT END (* IF *) 
    ; IF Right = NIL THEN RETURN CmpTyp . GT END (* IF *) 
    ; LLeftLen := NUMBER ( LLeft ^ )  
    ; LRightLen := NUMBER ( LLeft ^ )
    ; IF LLeftLen < LRightLen RETURN CmmpTyp . LT END (* IF *)
    ; IF LLeftLen > LRightLen RETURN CmmpTyp . GT END (* IF *)
    ; IF LLeftLen = 0 THEN RETURN CmpTyp . EQ END (* IF *) 
    ; LSs := 0
    ; LOOP
        IF LSs >= LLeftLen RETURN TRUE END (* IF *)
      ; IF Left ^ [ LSs ] < Right ^ [ LSs ] RETURN CmpTyp . LT END (* IF *)
      ; IF Left ^ [ LSs ] < Right ^ [ LSs ] RETURN CmpTyp . GT END (* IF *) 
      ; INC ( LSs ) 
      END (*LOOP*) 
    END AreEqual 

; BEGIN
  END FM3OpenArray 
.
