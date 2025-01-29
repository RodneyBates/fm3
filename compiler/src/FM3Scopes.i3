
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Scopes

; IMPORT IntSets

; IMPORT FM3Base
; IMPORT FM3Globals 
; IMPORT FM3Dict_Int_Int
; IMPORT FM3Graph 
; IMPORT FM3Units

(* A scope and its directly-contained declarations are termed "Open"
   if the declarations are referred-to by unqualified identifiers,
   which can happen only from inside the scope itself and deeplier-
   nested constructs.

   A scope and its directly-contained declarations are termed "Qualified"
   if the declarations are referred-to by dot selections, <openScope>.Ident.

   An open scope does not occur inside a qualified scope.
   
   A procedure signature behaves as qualified when referred-to by the formal
   parameter name of a named binding in a call and open when referred-to
   within a procedure body.

   In FM3, a procedure signature and procedure body have separate ScopeType
   objects but with disjoint identifier sets. Similarly, a compilation unit
   has a separate scope object for identifiers brought in by EXPORTs or IMPORTs
   and those declared within the unit, with disjoint identifier sets. 

*)

(*TODO: Review these when more is implemented. *) 
; TYPE ScopeKindTyp
    = { SkNull
      , SkUniverse (* {Standard, interfaces? *)
      , SkComp (* Interfaces touched by a compilation.  Only one such scope. *)
      , SkUnit (* Exports & Imports of a single compilation unit. *) 
      , SkInterface (* Including generic and instantiation. *) 
      , SkModule (* Including generic and instantiation. *)
      , SkFormals (* Built as part of a type definition, treated as qualified
                     when ref'd by a named formal within an actual parameter list,
                     treated as open for other lookups.  No self references.
                  *) 
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

; CONST ScopeKindSetTypeDef = SET OF ScopeKindTyp
    { ScopeKindTyp . SkFormals
    , ScopeKindTyp . SkEnum
    , ScopeKindTyp . SkRec
    , ScopeKindTyp . SkObj
    } 

; CONST ScopeKindSetOpen = SET OF ScopeKindTyp
    { ScopeKindTyp . SkUnit
    , ScopeKindTyp . SkInterface 
    , ScopeKindTyp . SkModule 
    , ScopeKindTyp . SkBlock
    } 

; CONST ScopeKindSetBinding = SET OF ScopeKindTyp
    { ScopeKindTyp . SkWith
    , ScopeKindTyp . SkTypecase
    , ScopeKindTyp . SkExcept 
    }

; TYPE ScopeNoTyp = FM3Globals . ScopeNoTyp 

; TYPE ScopeTyp
    = RECORD
        ScpDeclStackLink : ScopeRefTyp
      ; ScpOpenScopeStackLink : ScopeRefTyp
      ; ScpOwningUnitRef : FM3Units . UnitRefTyp := NIL 
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
           Atom is in one IFF or the other.
        *)
      ; ScpDeclGraph : FM3Graph . GraphTyp 
        (* Arcs are intra-scope RefId to DeclId.  Only those that would
           contribute to an illegal recursive decl cycle.
        *) 
      ; ScpCurDeclRefNoSet : IntSets . T (*1*)
        (* Decl Nos of ident refs in definition(s) of the current decl to idents
           declared in the current containing open scope that do not legalize
           recursive declarations.
        *)
      ; ScpCurDefExprs
          := ARRAY BOOLEAN (*Is value expr*) OF REFANY { NIL , .. } (*1*)
(****
      ; ScpCurTypeExpr : REFANY := NIL (* FM3Defs . DeclDefTyp. *) (*1*) 
      ; ScpCurValueExpr : REFANY := NIL (* FM3Defs . DeclDefTyp. *) (*1*)
****) 
      ; ScpDeclCt : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
      ; ScpMinDeclNo := FM3Globals . DeclNoNull
      ; ScpSelfScopeNo : FM3Globals . ScopeNoTyp (* A self-reference. *)
      ; ScpOwningDeclNo : FM3Globals . DeclNoTyp
      ; ScpOnDeclStackCt : INTEGER := 0
      ; ScpOnOpenScopeStackCt : INTEGER := 0
        (* ^Number of times it's on either scope stack. *)
(* CHECK ^Do we really need this? *) 
      ; ScpPosition : FM3Base . tPosition 
      ; ScpKind : ScopeKindTyp
      ; ScpInsideDecl : BOOLEAN := FALSE (*1*)
      ; ScpCurDefIsValue : BOOLEAN := FALSE (*1*) (* As opposed to a type def. *)
      END (*ScopeTyp*)

      (* NOTE 1: This field retains meaning only during handling of a single
                 declaration within the scope.  It is reinitialized and reused in
                 later declarations.  It is NIL/Empty/FALSE when not working in
                 a declaration.
                 It would more naturally be in a Decl object, but in Pass 2, we
                 don't have one when needed, and when we finally do, there can
                 be >1.
      *) 

; CONST ScopeRefBrand = "ScopeRef0.1" 
; REVEAL FM3Globals . ScopeRefTyp = BRANDED ScopeRefBrand REF ScopeTyp 
; TYPE ScopeRefTyp = FM3Globals . ScopeRefTyp 

; PROCEDURE NewScopeRef
    ( OwningUnitRef : FM3Units . UnitRefTyp
    ; ScopeKind : ScopeKindTyp
    ; READONLY Position : FM3Base . tPosition
    )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef, owned by OwningUnitRef. *) 

; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Globals . ScopeNoTyp ) : ScopeRefTyp 
  (* In the current unit. *) 

; TYPE ScopeMapTyp = FM3Base . MapTyp
  (* Map ScopeNoTyp to ScopeRefTyp. One of these per unit. *) 

; PROCEDURE NewScopeMap ( ScopeCt : FM3Globals . ScopeNoTyp ) : ScopeMapTyp

; VAR DeclScopeStackTopRef : ScopeRefTyp := NIL
      (* A global, linked stack containing scopes from multiple units.
         The top one is where declarations are being inserted. *)
; VAR DeclScopeStackCt : INTEGER := 0 
      
; VAR OpenScopeStackTopRef : ScopeRefTyp := NIL
      (* Another global, linked stack containing scopes from multiple units.
         Inner scopes are on top.  Unqualified ident references are searched
         top down to the enclosing unit scope. *) 
; VAR OpenScopeStackCt : INTEGER := 0 
      
; PROCEDURE PushDeclScopeRef ( ScopeRef : ScopeRefTyp ) 
; PROCEDURE PushOpenScopeRef ( ScopeRef : ScopeRefTyp ) 

; PROCEDURE PopDeclScopeRef ( ) : ScopeRefTyp  
; PROCEDURE PopOpenScopeRef ( ) : ScopeRefTyp  

; END FM3Scopes
.

