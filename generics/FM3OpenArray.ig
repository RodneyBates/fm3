
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE FM3OpenArray ( Elem ) 

(* Elem declares T, A type that has built-in "<" and ">" defined on it.  

; IMPORT FM3Base 

; TYPE T = REF ARRAY OF Elem . T 

; PROCEDURE Compare ( Left , Right : T ) : FM3Base . CmpTyp 

; END FM3OpenArray 
.

