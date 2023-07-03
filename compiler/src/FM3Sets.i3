
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Sets

(* The original purpose of this is for calling from a debugger. *) 

; IMPORT IntSets 

; PROCEDURE IntImage ( Val : INTEGER ) : TEXT

; PROCEDURE IntSetImage ( Val : IntSets . T ) : TEXT  

; END FM3Sets
.

