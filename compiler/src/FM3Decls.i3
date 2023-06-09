
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Decls

; IMPORT FM3Base
; IMPORT FM3Scopes
; IMPORT VarArray_Int_Refany

; CONST DeclNoNull = FM3Base . AtomNull 
; TYPE DeclNoTyp = FM3Base . AtomTyp 
; TYPE DeclRefTyp = REF DeclTyp
; TYPE DeclTyp
    = RECORD
        DclNumber : DeclNoTyp (* A self-reference. *) 
      ; DclParentScopeRef : FM3Scopes . ScopeRefTyp (* Containing scope *) 
      ; DclSelfScopeRef : FM3Scopes . ScopeRefTyp (* If this declares a scope *) 
      END (*DeclTyp*)

; CONST DefaultInitDeclCt = 100

; TYPE DeclMapTyp
    = VarArray_Int_Refany . T (* Map  DeclNoTyp to DeclRefTyp. *)

; PROCEDURE NewMap ( InitDeclCt := DefaultInitDeclCt ) : DeclMapTyp

; PROCEDURE NewDecl
    ( Map : DeclMapTyp
    ; ExpectedNo : DeclNoTyp 
    ; ParentScopeRef : FM3Scopes . ScopeRefTyp 
    )
  : DeclNoTyp
  (* Allocate and connect a DeclNo and DeclRef. *)
  (* IF ExpectedNo >= 0, result must match. *)  

; END FM3Decls
.

