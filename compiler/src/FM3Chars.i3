
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Chars

; TYPE T = REF ARRAY OF CHAR

; CONST Brand = "FM3Chars"

; PROCEDURE AreEqual ( Left , Right : T ) : BOOLEAN

; END FM3Chars
.

