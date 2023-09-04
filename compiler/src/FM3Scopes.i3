
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
      , SkUniverse (* {Predefined, interfaces? *) 
      , SkUnit 
      , SkEnum
      , SkRec
      , SkObj
      , SkFormals
      , SkGenFormals
      , SkBlock
      , SkWith
      , SkTypecase
      , SkExcept 
      } 






; TYPE ScopeRefTyp = REF ScopeTyp
; TYPE ScopeTyp
    = RECORD
        ScpStackLink : ScopeRefTyp
      ; ScpDeclIdSet : IntSets . T (* IdentAtoms declared within. *) 
      ; ScpDuplIdSet : IntSets . T (* IdentAtoms with multiple declarations. *) 
      ; ScpRefIdSet : IntSets . T (* IdentAtoms with multiple declarations. *) 
      ; ScpDeclMap : REF ARRAY OF REFANY (* FM3Decls . DeclRefTyp ) 
      ; ScpDeclDict : FM3Dict_Int_Int . FixedTyp (* IdentNo to Decl no. *)
      ; ScpDeclCt : FM3Base . AtomTyp := 0
      ; ScpNumber : ScopeNoTyp (* A self-reference. *)
      ; ScpOwningUnitNo : INTEGER
        (* Should be Unit . UnitNoTyp, but that would be cyclic imports. *) 
      ; ScpOwningDeclNo : FM3Base . AtomTyp := FM3Base . AtomNull
      ; ScpPosition : FM3Base . tPosition 
      ; ScpKind : ScopeKindTyp 
      END (*ScopeTyp*)
      
; CONST InitScopeCt = 10 
; TYPE ScopeMapTyp
    = VarArray_Int_Refany . T (* Map ScopeNoTyp to ScopeRefTyp. *)

; VAR ScopeStackTop : ScopeRefTyp := NIL
      (* A global linked stack with scopes from multiple units. *) 
      
; PROCEDURE NewMap ( ScopeCt := InitScopeCt ) : ScopeMapTyp

; PROCEDURE NewScope
    ( OwningUnitRef : FM3Units . UnitRefTyp ; InitDictCt := DefaultInitDictCt )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef Owned by OwningUnitRef. *) 

; CONST DefaultInitDictCt = 10

; PROCEDURE Push ( ScopeRef : ScopeRefTyp ) 

; END FM3Scopes
.

