
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Units

; IMPORT Decls 
; IMPORT FM3Atom_OAChars
; IMPORT Scopes 

; TYPE UnitNoTyp : INTEGER
; CONST UnitNoNull := - 1 
; TYPE UnitRefTyp = REF UnitTyp
; TYPE UnitTyp
    = RECORD
        UntUnitNo : UnitNoTyp := UnitNoNull 
      ; UntSrcFileName : TEXT := NIL 
      ; UntSrcFilePath : TEXT := NIL(* Without simple name. *) 
      ; UntWorkFilePath : TEXT := NIL
      ; UntPatchStackName : TEXT := NIL
      ; UntPatchStackRdBack : RdBackFile . T := NIL
      ; UntMaxPatchStackDepth : INTEGER := 0 
      ; UntUnnestStackName : TEXT := NIL
      ; UntUnnestStackRdBack : RdBackFile . T := NIL
      ; UntMaxUnnestStackDepth : INTEGER 
      ; UntParsePassName : TEXT := NIL
      ; UntParsePassRdBack : RdBackFile . T := NIL
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL (* Identifiers. *)   
      ; UntNumberAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsAtomDict : FM3Atom_OAChars . T := NIL(* TEXT literals. *) 
      ; UntWCharsAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntScopes : Scopes . ScopeMapTyp := NIL 
      ; UntDecls : Decls . DeclMapTyp := NIL
      END (*UnitTyp*)

; TYPE UnitMapTyp = VarArray_Int_Refany . T (* Map UnitNoTyp to UnitRefTyp. *)
; VAR UnitMap : UnitMapTyp
  (* ^Only one of these. *) 

; PROCEDURE New ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitMap. *)

; END FM3Units
.

