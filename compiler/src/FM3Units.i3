INTERFACE FM3Units

; IMPORT Decls 
; IMPORT FM3Atom_OAChars
; IMPORT Scopes 

; TYPE UnitRefTyp = REF UnitTyp
; TYPE UnitTyp
    = RECORD
        UnitSrcFileName : TEXT 
      ; UnitSrcFilePath : TEXT (* Without simple name. *) 
      ; UnitWorkFilePath : TEXT
      ; UnitPatchStackName : TEXT
      ; UnitPatchStackRdBack : RdBackFile . T
      ; UnitMaxPatchStackDepth : INTEGER 
      ; UnitUnnestStackName : TEXT
      ; UnitUnnestStackRdBack : RdBackFile . T 
      ; UnitMaxUnnestStackDepth : INTEGER 
      ; UnitParsePassName : TEXT
      ; UnitParsePassRdBack : RdBackFile . T
      ; UnitIdentAtomDict : FM3Atom_OAChars . T (* Identifiers. *)   
      ; UnitNumberAtomDict : FM3Atom_OAChars . T (* Numeric literals. *)  
      ; UnitCharsAtomDict : FM3Atom_OAChars . T (* TEXT literals. *) 
      ; UnitWCharsAtomDict : FM3Atom_OAWideChars . T (* ^Wide TEXT literals. *)
      ; UnitScopes : Scopes . ScopeMapTyp 
      ; UnitDecls : Decls . DeclMapTyp  
      END (*UnitTyp*)

; TYPE UnitNoTyp : INTEGER

; TYPE UnitMapTyp = VarArray_Int_Refany . T (* Map UnitNoTyp to UnitRefTyp. *)
; VAR UnitMap : UnitMapTyp
  (* ^Only one of these. *) 

; PROCEDURE New ( ) : UnitRefTyp
  (* Create, initialize, give it a UnitNo, and put into UnitMap. *)

; END FM3Units
.

