
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Globals

; IMPORT FM3TextDict 

; PROCEDURE Init ( )

  = BEGIN
      IdentDict := FM3TextDict . NewGrowable ( IdentDictSize ) 
    ; TextDict : FM3Dict . . NewGrowable ( TextDictSize ) 
    ; TextValDict : FM3Dict . . NewGrowable ( TextValSize ) 
    END Init

; BEGIN
    Init ( ) 
  END FM3Globals
.
