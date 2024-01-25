
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scopes

; IMPORT IntSets
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Dict_Int_Int
; IMPORT FM3Units 

; CONST ScopeNoNull = FM3Base . AtomNull 
; TYPE ScopeNoTyp = FM3Base . AtomTyp

; TYPE ScopeKindTyp
    = { SkNull
      , SkUniverse (* {Predefined, interfaces? *)
      , SkInterface
      , SkModule
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
      ; ScpDeclCt : FM3Base . DeclNoTyp := 0
      ; ScpMinDeclNo := FM3Base . DeclNoNull
      ; ScpScopeNo : FM3Base . ScopeNoTyp (* A self-reference. *)
      ; ScpOwningUnitNo : FM3Base . UnitNoTyp 
      ; ScpOwningDeclNo : FM3Base . DeclNoTyp
      ; ScpStackDepth : INTEGER 
      ; ScpPosition : FM3Base . tPosition 
      ; ScpKind : ScopeKindTyp 
      END (*ScopeTyp*)
      
; PROCEDURE NewScopeRef ( OwningUnitRef : FM3Units . UnitRefTyp ) : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef, owned by OwningUnitRef. *) 

; TYPE ScopeMapTyp = FM3Base . MapTyp
  (* Map ScopeNoTyp to ScopeRefTyp. One of these per unit. *) 

; PROCEDURE NewScopeMap ( ScopeCt : FM3Base . ScopeNoTyp ) : ScopeMapTyp

; VAR ScopeStackTopRef : ScopeRefTyp := NIL
      (* A single, global, linked stack with scopes from multiple units. *) 
      
; VAR BlockScopeTopRef : ScopeRefTyp := NIL
      (* Like ScopeStackTopRef, but only the top block scope.  All of
         these are deeper than non-block scopes.
      *) 
      
; PROCEDURE PushScope ( ScopeRef : ScopeRefTyp )  

; PROCEDURE PopScope ( ) : ScopeRefTyp  

; END FM3Scopes
.

