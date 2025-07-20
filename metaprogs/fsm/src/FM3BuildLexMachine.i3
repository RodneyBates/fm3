
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3BuildLexMachine 

(* Build a table for FM3LexTable to use.  Entire table must be built
   before it can be used.  Calls must follow the protocol given by\
   the regular expression:
     MakeEmpty AddPair* Build
*) 

; IMPORT FM3LexTable 

; EXCEPTION Error ( TEXT ) 

; PROCEDURE MakeEmpty ( ) 

; PROCEDURE AddPair 
    ( AddString : TEXT 
    ; Value : FM3LexTable . ValueTyp 
    ; ReverseMap : BOOLEAN := TRUE
      (* Also build the reverse mapping for this pair. *)   
    ) 

(* TODO: Make this detect and report
         1) duplicate strings 
     and 2) duplicate values with ReverseMap
*) 

; PROCEDURE Build ( ) : FM3LexTable . T RAISES { Error } 

; END FM3BuildLexMachine 
. 
