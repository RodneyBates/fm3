        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Compile

; IMPORT FM3Units

; PROCEDURE DisAsm
    ( UnitRef : FM3Units . UnitRefTyp ; RdBackFileName : TEXT )
  (*PRE: RdBackFile.Copy is closed. *) 
  (*POST: RdBackFile.Copy is reclosed. *) 

; END FM3Compile
.

