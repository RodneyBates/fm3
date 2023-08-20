 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Errors

; IMPORT FM3Base

(*EXPORTED:*)
; PROCEDURE Err
    ( FileName : TEXT ; LineNo : INTEGER ; CharPos : INTEGER ; Msg : TEXT )

  = BEGIN
    END Err 

; PROCEDURE Error ( Position : FM3Base . tPosition ; Msg : TEXT )

  = BEGIN
    END Error 

; BEGIN
  END FM3Errors 
.

