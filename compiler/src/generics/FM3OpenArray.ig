
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2026        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE FM3OpenArray ( Elem ) 

(* Elem declares :
     . T, A type that has built-in "<" and ">" defined on it. 
     . Hash, a hash function returning Word.T.
*) 

; IMPORT FM3Base 

; TYPE T = REF ARRAY OF Elem . T 

; CONST Brand = "FM3OpenArray0.2" & Elem . Brand 

; PROCEDURE Hash ( Key : T ) : FM3Base . HashTyp 

; PROCEDURE Compare ( READONLY Left , Right : T ) : FM3Base . CompareTyp 

; END FM3OpenArray 
.

