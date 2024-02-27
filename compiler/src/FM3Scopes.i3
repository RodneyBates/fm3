
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scopes

; IMPORT IntSets

; IMPORT FM3Base
; IMPORT FM3Dict_Int_Int
; IMPORT FM3Units 

; CONST ScopeNoNull = FM3Base . AtomNull 
; TYPE ScopeNoTyp = FM3Base . AtomTyp

; TYPE ScopeKindTyp
    = { SkNull
      , SkUniverse (* {Predefined, interfaces? *)
      , SkInterface (* Including generic and instantiation. *) 
      , SkModule (* Including generic and instantiation. *)
      , SkFormals
      , SkProcBody 
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

; TYPE ScopeRefTyp = REF ScopeTyp
; TYPE ScopeTyp
    = RECORD
        ScpStackLink : ScopeRefTyp
      ; ScpDeclIdSet : IntSets . T (* IdentAtoms declared within. *) 
      ; ScpDuplDeclIdSet : IntSets . T (* IdentAtoms with > 1 declaration. *) 
(* CHECK ^ Is there any need for this? *) 
      ; ScpRefIdSet : IntSets . T (* IdentAtoms referenced within. *) 
      ; ScpDeclDict : FM3Dict_Int_Int . FixedTyp (* IdentNo to Decl no. *)
      ; ScpDeclCt : FM3Base . DeclNoTyp := FM3Base . DeclNoNull 
      ; ScpMinDeclNo := FM3Base . DeclNoNull
      ; ScpScopeNo : FM3Base . ScopeNoTyp (* A self-reference. *)
      ; ScpOwningUnitRef : FM3Units . UnitRefTyp := NIL 
      ; ScpOwningDeclNo : FM3Base . DeclNoTyp
      ; ScpOnStackCt : INTEGER := 0
        (* ^Number of times it's on either scope stack. *)
      ; ScpPosition : FM3Base . tPosition 
      ; ScpKind : ScopeKindTyp 
      END (*ScopeTyp*)
      
; PROCEDURE NewScopeRef
    ( OwningUnitRef : FM3Units . UnitRefTyp
    ; ScopeKind : ScopeKindTyp
    ; READONLY Position : FM3Base . tPosition
    )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef Owned by OwningUnitRef. *) 

; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Base . ScopeNoTyp )
  : FM3Scopes . ScopeRefTyp 
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

; PROCEDURE PopDeclScope ( ) : ScopeRefTyp  
; PROCEDURE PopLookupScope ( ) : ScopeRefTyp  

; END FM3Scopes
.

