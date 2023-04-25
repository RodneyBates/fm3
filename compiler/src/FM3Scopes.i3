INTERFACE FM3Scopes

; IMPORT Decls 
; IMPORT FM3Atom_OAChars

; CONST ScopeNoNull = LAST ( ScopeNoTyp ) 
; TYPE ScopeNoTyp : INTEGER
; TYPE ScopeRefTyp = REF ScopeTyp
; TYPE ScopeTyp
    = RECORD
        ScpNumber : ScopeNoTyp (* A self-reference. *) 
      ; ScpDecldIdSet : IntSets . T (* IdentNos declared. *) 
      ; ScpDeclDict : FM3Dict_Int_Int (* IdentNo to Decl no. *)
      ; ScpDeclCt : INTEGER
      ; ScpOwningDeclNo : Decls . DeclNoTyp := Decls . DeclNoNull 
      ; ScpNextDeclNo : Decls . DeclNoTyp 
      END (*ScopeTyp*)

; TYPE ScopeMapTyp
    = VarArray_Int_Refany . T (* Map ScopeNoTyp to ScopeRefTyp. *)

; PROCEDURE NewMap ( ) : ScopeMapTyp

; PROCEDURE New ( Map : ScopeMapTyp ) : ScopeNoTyp
  (* Allocate and connect a ScopeNo and scopeRef. *)  

; END FM3Scopes
.

