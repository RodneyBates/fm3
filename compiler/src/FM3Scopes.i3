
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scopes

; IMPORT IntSets

; IMPORT FM3Base
; FROM FM3Base IMPORT ScopeNoTyp 
; FROM FM3Base IMPORT ScopeNoNull 
; IMPORT FM3Dict_Int_Int
; IMPORT FM3Units 

; TYPE ScopeKindTyp
    = { SkNull
      , SkUniverse (* {Predefined, interfaces? *)
      , SkComp (* Interfaces touched by a compilation.  Only one such scope. *) 
      , SkInterface (* Including generic and instantiation. *) 
      , SkModule (* Including generic and instantiation. *)
      , SkFormals
      , SkFormalsAndBody 
      , SkBlock
      , SkCompEnv (* Compiled, IMPORTed, EXPORTed. *) 
      , SkExports 
      , SkEnum
      , SkRec
      , SkObj
      , SkWith
      , SkTypecase
      , SkExcept 
      } 

; CONST ScopeKindSetBlock
    = SET OF ScopeKindTyp
        { ScopeKindTyp . SkUniverse .. ScopeKindTyp . SkBlock } 

; TYPE ScopeTyp
    = RECORD
        ScpDeclStackLink : ScopeRefTyp
      ; ScpLookupStackLink : ScopeRefTyp
      ; ScpDeclIdSet : IntSets . T (* IdentAtoms declared within. *) 
      ; ScpFormalIdSet : IntSets . T (* IdentAtoms declared within. *) 
      ; ScpDuplDeclIdSet : IntSets . T (* IdentAtoms with > 1 declaration. *) 
(* CHECK ^ Is there any need for this? *) 
      ; ScpRefIdSet : IntSets . T (* IdentAtoms referenced within. *) 
      ; ScpDeclDict : FM3Dict_Int_Int . FixedTyp (* IdentAtom to Decl no. *)
      ; ScpDeclCt : FM3Base . DeclNoTyp := FM3Base . DeclNoNull 
      ; ScpMinDeclNo := FM3Base . DeclNoNull
      ; ScpScopeNo : FM3Base . ScopeNoTyp (* A self-reference. *)
      ; ScpOwningUnitRef : FM3Units . UnitRefTyp := NIL 
      ; ScpOwningDeclNo : FM3Base . DeclNoTyp
      ; ScpOnDeclStackCt : INTEGER := 0
      ; ScpOnLookupStackCt : INTEGER := 0
        (* ^Number of times it's on either scope stack. *)
(* CHECK ^Do we really need this? *) 
      ; ScpPosition : FM3Base . tPosition 
      ; ScpKind : ScopeKindTyp 
      END (*ScopeTyp*)

; REVEAL FM3Base . ScopeRefTyp = BRANDED REF ScopeTyp 
; TYPE ScopeRefTyp = FM3Base . ScopeRefTyp 

; PROCEDURE NewScopeRef
    ( OwningUnitRef : FM3Units . UnitRefTyp
    ; ScopeKind : ScopeKindTyp
    ; READONLY Position : FM3Base . tPosition
    )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef Owned by OwningUnitRef. *) 

; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Base . ScopeNoTyp ) : ScopeRefTyp 
  (* In the current unit. *) 

; TYPE ScopeMapTyp = FM3Base . MapTyp
  (* Map ScopeNoTyp to ScopeRefTyp. One of these per unit. *) 

; PROCEDURE NewScopeMap ( ScopeCt : FM3Base . ScopeNoTyp ) : ScopeMapTyp

; VAR DeclScopeStackTopRef : ScopeRefTyp := NIL
      (* A global, linked stack containing scopes from multiple units.
         The top one is where declarations are being handled. *)
; VAR DeclScopeStackCt : INTEGER := 0 
      
; VAR LookupScopeStackTopRef : ScopeRefTyp := NIL
      (* Another global, linked stack containing scopes from multiple units.
         Unqualified ident references are searched top-to-bottom. *) 
; VAR LookupScopeStackCt : INTEGER := 0 
      
; PROCEDURE PushDeclScopeRef ( ScopeRef : ScopeRefTyp ) 
; PROCEDURE PushLookupScopeRef ( ScopeRef : ScopeRefTyp ) 

; PROCEDURE PopDeclScopeRef ( ) : ScopeRefTyp  
; PROCEDURE PopLookupScopeRef ( ) : ScopeRefTyp  

; END FM3Scopes
.

