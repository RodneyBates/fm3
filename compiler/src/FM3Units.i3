
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Units

; IMPORT VarArray_Int_Refany
; IMPORT Wr 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3Decls 
; IMPORT FM3Scopes
; IMPORT RdBackFile

; TYPE UnitKindTyp
         = { UkNull
           , UkInterface , UkGenInterface , UkInstInterface
           , UkModule , UkGenModule , UkInstModule
           } 

; TYPE UnitNoTyp = INTEGER
; CONST UnitNoNull = - 1 
; TYPE UnitRefTyp = REF UnitTyp
; TYPE UnitTyp
    = RECORD
        UntStackLink : UnitRefTyp := NIL 
      ; UntSrcFileName : TEXT := NIL 
      ; UntSrcFilePrefix : TEXT := NIL(* Without simple name. *)
      ; UntLogName : TEXT := NIL 
      ; UntLogWrT : Wr . T := NIL 
      ; UntWorkFilePrefix : TEXT := NIL
      (* ^Same for unnest stack, patch stack, and parse pass output file. *)  
      ; UntPatchStackName : TEXT := NIL
      ; UntPatchStackRdBack : RdBackFile . T := NIL
      ; UntMaxPatchStackDepth : LONGINT := 0L
      ; UntPatchStackEmpty : LONGINT := 0L
        (* ^Value of RdBackFile.LengthL when conceptually empty, but may be
           nonzero, on account of file tag, length, etc. *) 
      ; UntPatchStackTopCoord : LONGINT := 0L
      ; UntPatchStackTopToken : LONGINT := 0L
        (* ^The token and patch coordinate of the topmost item conceptually on
           the patch stack are kept, decompressed, in the above two fields,
           for easy access.  Any operands remain on the RdBack file.
        *) 
      ; UntUnnestStackName : TEXT := NIL
      ; UntUnnestStackRdBack : RdBackFile . T := NIL
      ; UntMaxUnnestStackDepth : LONGINT := 0L 
      ; UntUnnestStackEmpty : LONGINT := 0L
      ; UntParsePassName : TEXT := NIL (* Pass output file. *) 
      ; UntParsePassRdBack : RdBackFile . T := NIL
      ; UntParsePassEmpty : LONGINT := 0L
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL (* Identifiers. *)   
      ; UntNumberAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsAtomDict : FM3Atom_OAChars . T := NIL(* TEXT literals. *) 
      ; UntWCharsAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntScopesMap : FM3Scopes . ScopeMapTyp := NIL k
      ; UntDeclsMap : FM3Decls . DeclMapTyp := NIL
      ; UntScopesMap : FM3Scopes . ScopeMapTyp := NIL k
      ; UntScopeStackTop : UnitRefTyp := NIL 
      ; UntScopeStackDepth : INTEGER := 0 
      ; UntUnitNo : UnitNoTyp := UnitNoNull
      ; UntScanResult : INTEGER 
      ; UntParseResult : INTEGER 
      ; UntParsePassResult : INTEGER
      ; UntNextDeclNo : INTEGER 
      ; UntKind : UnitKindTyp 
        
      END (*UnitTyp*)

; TYPE UnitMapTyp = VarArray_Int_Refany . T (* Map UnitNoTyp to UnitRefTyp. *)
; VAR UnitMap : UnitMapTyp
  (* ^Only one of these. *) 

; PROCEDURE New ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitMap. *)

; PROCEDURE AllocateDeclNos ( Ct : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Ct Decl numbers, unique
     within the current scope, and return the lowest number.
  *) 

; PROCEDURE TextOfIdAtom ( IdAtom : FM3Base . AtomTyp ) : TEXT
  (* In the current unit. *) 

; VAR UnitStackTop : UnitRefTyp := NIL 

; END FM3Units


.

