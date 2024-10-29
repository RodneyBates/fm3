 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Extern

; IMPORT FM3Base 
; IMPORT FM3Globals 

; PROCEDURE GetExternInterface
    ( NameAtom : FM3Base . AtomTyp ; IsImport : BOOLEAN )
  : FM3Globals . UnitNoTyp

  = BEGIN
(* IMPLEMENT ME. *)
     RETURN FM3Base . AtomNull 
    END GetExternInterface 

; BEGIN
  END FM3Extern
.

