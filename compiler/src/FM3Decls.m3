
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT IntRanges 

; IMPORT FM3Scopes
; IMPORT VarArray_Int_Refany

(* From the interface: 
; CONST DeclNoNull = LAST ( DeclNoTyp ) 
; TYPE DeclNoTyp = INTEGER
; TYPE DeclRefTyp = REF DeclTyp
; TYPE DeclTyp
    = RECORD
        DclNumber : DeclNoTyp (* A self-reference. *) 
      ; DclParentScopeRef : FM3Scopes . ScopeRefTyp (* Containing scope *) 
      ; DclSelfScopeRef : FM3Scopes . ScopeRefTyp (* If this declares a scope *) 
      END (*DeclTyp*)

; TYPE DeclMapTyp
    = VarArray_Int_Refany . T (* Map DeclNoTyp to DeclRefTyp. *)
*) 

; VAR NextDeclNo : DeclNoTyp := 0 

(*EXPORTED*) 
; PROCEDURE NewMap ( InitDeclCt := DefaultInitDeclCt ) : DeclMapTyp

  = BEGIN
      NextDeclNo := 0 
    ; RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitDeclCt - 1 } ) 
    END NewMap

(*EXPORTED*) 
; PROCEDURE NewDecl
    ( Map : DeclMapTyp
    ; ExpectedNo : DeclNoTyp 
    ; ParentScopeRef : FM3Scopes . ScopeRefTyp 
    )
  : DeclNoTyp
  (* Allocate and connect a DeclNo and DeclRef. *)
  (* IF ExpectedNo >= 0, result must match. *)  

  = VAR LDeclNo : DeclNoTyp  
  ; VAR LDeclRef : DeclRefTyp

  ; BEGIN
      LDeclNo := NextDeclNo
    ; <* ASSERT LDeclNo = ExpectedNo *>
      INC ( NextDeclNo )
    ; LDeclRef := NEW ( DeclRefTyp )
    ; LDeclRef . DclNumber := LDeclNo  
    ; LDeclRef . DclParentScopeRef := ParentScopeRef  
    ; LDeclRef . DclSelfScopeRef := NIL 
    ; VarArray_Int_Refany . Assign ( Map , LDeclNo , LDeclRef )
    ; RETURN LDeclNo 
    END NewDecl 

; BEGIN
  END FM3Decls
.

