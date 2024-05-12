
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3ExpImp

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Base
; IMPORT FM3OpenArray_Char
; IMPORT FM3Units 

; VAR CompAtomDict : FM3Atom_OAChars . T := NIL (* Interface names. *)
  (* Imported or exported interface names.  Only one of these for
     an entire compiler run.
  *) 

; UnitRefMap : FM3Base . MapTyp
  (* Interface name atom to UnitRef.  Only one of these for
     an entire compiler run.
  *) 

; PROCEDURE GetInterface
    ( IdentChars : FM3OpenArray_Char . T
    ; Position : FM3Base . tPosition 
    ; IsExport : BOOLEAN
    )
  : FM3Units . UnitRefTyp
    (* ^The interface unit that was [ex/im]ported, possibly NIL *) 

; END FM3ExpImp
.
