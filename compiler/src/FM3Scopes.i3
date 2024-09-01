
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
; IMPORT FM3Graph 
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
      ; ScpDeclIdSet : IntSets . T
        (* ^IdentAtoms declared within, including imports of top-level scope. *)
      ; ScpFormalIdSet : IntSets . T
        (* ^Formal parameter IdentAtoms declared within, if signature scope. *) 
      ; ScpDuplDeclIdSet : IntSets . T (* IdentAtoms with > 1 declaration. *) 
(* CHECK ^ Is there any need for this? *) 
      ; ScpRefIdSet : IntSets . T
        (* ^IdentAtoms referenced within.  Gradually pruned to those
           both referenced and declared within. *)
      ; ScpDeclDict : FM3Dict_Int_Int . FixedTyp
        (* ^IdentAtom to Decl no.
            Includes formals, if signature or proc body scope. *)
        (* INVARIANT: Once ScpDeclIdSet and ScpDeclDict are both complete,
           Atom is in one IFF in the other.
        *)
      ; ScpDeclGraph : FM3Graph . GraphTyp 
        (* Arcs are intra-scope RefId to declId.  Only those that would
           contribute to an illegal recursive decl cycle.
        *) 
      ; ScpCurDeclRefIdNoSet : IntSets . T (*1*)
      ; ScpCurDeclRef : REFANY (* FM3Decls . DeclRefTyp. *) (*1*)   
      ; ScpCurExprObj : REFANY (* FM3Defs . DeclDefTyp. *) (*1*) 
      ; ScpDeclCt : FM3Base . DeclNoTyp := FM3Base . DeclNoNull
      ; ScpMinDeclNo := FM3Base . DeclNoNull
      ; ScpSelfScopeNo : FM3Base . ScopeNoTyp (* A self-reference. *)
      ; ScpOwningUnitRef : FM3Units . UnitRefTyp := NIL 
      ; ScpOwningDeclNo : FM3Base . DeclNoTyp
      ; ScpOnDeclStackCt : INTEGER := 0
      ; ScpOnLookupStackCt : INTEGER := 0
        (* ^Number of times it's on either scope stack. *)
(* CHECK ^Do we really need this? *) 
      ; ScpPosition : FM3Base . tPosition 
      ; ScpKind : ScopeKindTyp
      END (*ScopeTyp*)

      (* NOTE 1: This field retains meaning only during handling of a single
                 declaration within the scope.  It is reinitialized and reused
                 in later declarations.
      *) 

; CONST ScopeRefBrand = "ScopeRef0.1" 
; REVEAL FM3Base . ScopeRefTyp = BRANDED ScopeRefBrand REF ScopeTyp 
; TYPE ScopeRefTyp = FM3Base . ScopeRefTyp 

; PROCEDURE NewScopeRef
    ( OwningUnitRef : FM3Units . UnitRefTyp
    ; ScopeKind : ScopeKindTyp
    ; READONLY Position : FM3Base . tPosition
    )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef, owned by OwningUnitRef. *) 

; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Base . ScopeNoTyp ) : ScopeRefTyp 
  (* In the current unit. *) 

; TYPE ScopeMapTyp = FM3Base . MapTyp
  (* Map ScopeNoTyp to ScopeRefTyp. One of these per unit. *) 

; PROCEDURE NewScopeMap ( ScopeCt : FM3Base . ScopeNoTyp ) : ScopeMapTyp

; VAR DeclScopeStackTopRef : ScopeRefTyp := NIL
      (* A global, linked stack containing scopes from multiple units.
         The top one is where declarations are being inserted. *)
; VAR DeclScopeStackCt : INTEGER := 0 
      
; VAR LookupScopeStackTopRef : ScopeRefTyp := NIL
      (* Another global, linked stack containing scopes from multiple units.
         Inner scopes are on top.  Unqualified ident references are searched
         top down to the enclosing unit scope. *) 
; VAR LookupScopeStackCt : INTEGER := 0 
      
; PROCEDURE PushDeclScopeRef ( ScopeRef : ScopeRefTyp ) 
; PROCEDURE PushLookupScopeRef ( ScopeRef : ScopeRefTyp ) 

; PROCEDURE PopDeclScopeRef ( ) : ScopeRefTyp  
; PROCEDURE PopLookupScopeRef ( ) : ScopeRefTyp  

; END FM3Scopes
.

