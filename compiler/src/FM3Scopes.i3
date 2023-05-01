INTERFACE FM3Scopes

; IMPORT IntSets
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Dict_Int_Int

; CONST ScopeNoNull = FM3Base . AtomNull 
; TYPE ScopeNoTyp = FM3Base . AtomTyp 
; TYPE ScopeRefTyp = REF ScopeTyp
; TYPE ScopeTyp
    = RECORD
        ScpNumber : ScopeNoTyp (* A self-reference. *) 
      ; ScpDecldIdSet : IntSets . T (* IdentNos declared. *) 
      ; ScpDeclDict : FM3Dict_Int_Int . GrowableTyp (* IdentNo to Decl no. *)
      ; ScpDeclCt : INTEGER
      (* This: would be : FM3Decls . DeclNoTyp := FM3Decls . DeclNoNull,
         but that would create cyclic imports. *)
      ; ScpOwningDeclNo : FM3Base . AtomTyp := FM3Base . AtomNull
      END (*ScopeTyp*)

; TYPE ScopeMapTyp
    = VarArray_Int_Refany . T (* Map ScopeNoTyp to ScopeRefTyp. *)

; PROCEDURE NewMap ( ) : ScopeMapTyp

; PROCEDURE New ( Map : ScopeMapTyp ) : ScopeNoTyp
  (* Allocate and connect a ScopeNo and scopeRef. *)  

; END FM3Scopes
.

