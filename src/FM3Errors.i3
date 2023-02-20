 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Errors

; PROCEDURE Err
    ( FileName : TEXT ; LineNo : INTEGER ; CharPos : INTEGER ; Msg : TEXT ) 

; END FM3Errors 
.
