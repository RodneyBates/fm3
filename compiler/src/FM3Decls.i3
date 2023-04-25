INTERFACE FM3Decls

; CONST DeclNoNull = LAST ( DeclNoTyp ) 
; TYPE DeclNoTyp : INTEGER
; TYPE DeclRefTyp = REF DeclTyp
; TYPE DeclTyp
    = RECORD
        DclNumber : DeclNoTyp (* A self-reference. *) 
      ; DclParentScopeNo : Scopes . ScopeNoTyp (* Containing scope *) 
      ; DclSelfScopeNo : Scopes . ScopeNoTyp (* If declares a scope *) 
      END (*DeclTyp*)

; TYPE DeclMapTyp
    = VarArray_Int_Refany . T (* Map DeclNoTyp to DeclRefTyp. *)

; PROCEDURE NewMap ( ) : DeclMapTyp

; PROCEDURE New ( Map : DeclMapTyp ; ExpectedNo : INTEGER ) : DeclNoTyp
  (* IF ExpectedNo >= 0, result must match. *)  

; END FM3Decls
.

