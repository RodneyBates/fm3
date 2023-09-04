
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
; TYPE DeclNoTyp = INTEGER
; CONST DeclNoNull = LASE ( INTEGER ) 

; TYPE DeclKindTyp
    = { DkNull
      , DkDuplDecl
      , DkMod 
      , DkIntf 
      , DkGenMod
      , DkGenIntf
      , DkExcp
      , DkType
      , DkConst
      , DkVar
      , DkValueFormal
      , DkVarFormal
      , DkROFormal
      , DkField
      , DkMethod
      , DkProc
      , DkWith
      , DkFor
      , DkExcAprg
      } 

; TYPE DeclRefTyp = REF DeclTyp 
; TYPE DeclTyp
    = RECORD 
        DclLink : DeclObjBastTyp
        (* For a linked list of nodes for the real decl and any 
           in its scope that duplicate the decl name .
        *) 
      ; DclParentScopeRef : FM3Scopes . ScopeRefTyp (* Containing scope *) 
      ; DclSelfScopeRef : FM3Scopes . ScopeRefTyp (* If this declares a scope *)
      ; DclIdAtom : FM3Base . AtomTyp
      ; DclNumber : DeclNoTyp (* A self-reference. *)
      ; DclPos : FM3Base . tPosition 
      ; DclKind : DclKindTyp 
      END (*DeclObjBaseTyp*)

; CONST DefaultInitDeclCt = 100

; TYPE DeclMapTyp = Vararray_Int_Refany (* Map DeclNoTyp to DeclRefTyp. *)

; PROCEDURE NewMap ( InitDeclCt := DefaultInitDeclCt ) : DeclMapTyp 

; PROCEDURE NewDeclRef
    ( ParentScopeRef : FM3Scopes . ScopeRefTyp ; DeclNo : DeclNoTyp )
  : DeclRefTyp
  (* Allocate a DeclRef and connect in into ParentScopeRef ^. *)

; END FM3Decls
.

