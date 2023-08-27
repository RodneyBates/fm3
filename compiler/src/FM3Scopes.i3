
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scopes

; IMPORT IntSets
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Dict_Int_Int

; CONST ScopeNoNull = FM3Base . AtomNull 
; TYPE ScopeNoTyp = FM3Base . AtomTyp

; TYPE ScopeKindTyp
    = { SkNull
      , SkUniverse
      , SkUnit 
      , SkEnum
      , SkRec
      , SkObj
      , SkFormals
      , SkBlock
      , SkWith
      , SkTypecase
      , SkExcept 
      } 

; TYPE ScopeRefTyp = REF ScopeTyp
; TYPE ScopeTyp
    = RECORD
        ScpNumber : ScopeNoTyp (* A self-reference. *) 
      ; ScpDecldIdSet : IntSets . T (* IdentNos declared. *) 
      ; ScpDeclDict : FM3Dict_Int_Int . GrowableTyp (* IdentNo to Decl no. *)
      ; ScpDeclCt : FM3Base . AtomTyp := 0
      ; ScpOwningDeclNo : FM3Base . AtomTyp := FM3Base . AtomNull
      ; ScpKind : ScopeKindTyp 
      END (*ScopeTyp*)
      
; CONST InitScopeCt = 10 
; TYPE ScopeMapTyp
    = VarArray_Int_Refany . T (* Map ScopeNoTyp to ScopeRefTyp. *)

; PROCEDURE NewMap ( ScopeCt := InitScopeCt ) : ScopeMapTyp

; CONST DefaultInitDictCt = 10 

; PROCEDURE NewScope
    ( Map : ScopeMapTyp ; InitDictCt := DefaultInitDictCt ) : ScopeNoTyp
  (* Allocate and connect a ScopeNo and scopeRef. *)

; PROCEDURE ScopeEmpty ( ScopeKind : ScopeKindTyp )

; PROCEDURE ScopeLt ( ScopeKind : ScopeKindTyp )

; PROCEDURE ScopeRt ( ScopeKind : ScopeKindTyp )

; PROCEDURE DeclId
    ( Atom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )

; END FM3Scopes
.

