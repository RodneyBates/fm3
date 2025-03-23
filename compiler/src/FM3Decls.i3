 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Decls

; IMPORT FM3Base
; IMPORT FM3Exprs
; IMPORT FM3Globals 
; IMPORT FM3SrcToks

; TYPE DeclKindTyp
    = { DkNull
      , DkDuplDecl
      , DkMod 
      , DkIntf 
      , DkGenMod
      , DkGenIntf
      , DkExports (* EXPORTS list is declaration-ish. *) 
      , DkExc
      , DkType                        
      , DkConst
      , DkVar
      , DkVALUEFormal
      , DkVARFormal
      , DkROFormal
      , DkEnumLit 
      , DkRecField
      , DkObjField
      , DkMethod
      , DkProc
      , DkWith
      , DkFor
      , DkExcArg
      , DkReveal (* Not a scope.  Here as an experiment. *) 
      } 

; PROCEDURE DeclKindImage ( Kind : DeclKindTyp ) : TEXT

; CONST DeclRefBrand = "DeclRef0.1" 
; REVEAL FM3Globals . DeclRefTyp = BRANDED DeclRefBrand REF DeclTyp 
; TYPE DeclRefTyp = FM3Globals . DeclRefTyp 
; TYPE DeclTyp
    = RECORD 
        DclLink : DeclRefTyp
        (* For a linked list of nodes giving the positions of any decls, to
           the right of the leftmost, in the same scope, with the same Ident.
           These will have DeclKind DkDuplDecl.  Such a list is mutually
           exclusive of a single node of some other DeclKind. 
        *) 
      ; DclOwningScopeRef : FM3Globals . ScopeRefTyp (* Containing scope *) 
      ; DclSelfScopeRef : FM3Globals . ScopeRefTyp (* If this declares a scope *)
      ; DclDefType : FM3Exprs . ExprTyp := NIL 
      ; DclDefValue : FM3Exprs . ExprTyp := NIL 
      ; DclIdAtom : FM3Base . AtomTyp
      ; DclIdCt : INTEGER
      ; DclIdNo : INTEGER (* Counts up while going thru' multiple idents. *) 
      ; DclSelfDeclNo : FM3Globals . DeclNoTyp (* A self-reference. *)
      ; DclPos : FM3Base . tPosition
      ; DclStdTok : FM3SrcToks . TokTyp := FM3SrcToks . StkUnknown 
      ; DclKind : DeclKindTyp
      ; DclIsUsable : BOOLEAN := TRUE 
      END (*DeclTyp*)
      
; PROCEDURE DeclRefImage ( DeclRef : DeclRefTyp ) : TEXT
  (* DeclNo, REF, and Position. *) 

; PROCEDURE DeclNoImage ( DeclRef : DeclRefTyp )  : TEXT 
  (* Unit-relative/Scope-relative. *)
  
; PROCEDURE DeclInfoImage ( DeclRef : DeclRefTyp )  : TEXT 
  (* Unit-relative/Scope-relative. *)

; PROCEDURE DeclTypImage ( DeclRef : DeclRefTyp ) : TEXT
  (* Contents of the record. *)  

; TYPE DeclMapTyp = FM3Base . MapTyp  
    (* Map DeclNoTyp to DeclRefTyp. One of these per Unit. *)

; PROCEDURE NewDeclMap ( InitDeclCt : FM3Globals . DeclNoTyp ) : DeclMapTyp 

; PROCEDURE NewDeclRef
    ( OwningScopeRef : FM3Globals . ScopeRefTyp
    ; DeclNo : FM3Globals . DeclNoTyp
    )
  : DeclRefTyp
  (* Allocate a DeclRef and initialize a couple of fields. *)

(* A stack of info about different kinds of declarations and their sometimes
   multiple identifiers, allowing for sharing among them of parser productions
   and actions.  Trying to propagate this info in parser semantic attributes
   was turning out to be insanely fragile and complicated.
*)

; TYPE DeclParseInfoTyp
    = RECORD
        DiDeclTok : FM3Base . TokTyp 
      ; DiIdListTok : FM3Base . TokTyp
      ; DiIdSepTok : FM3Base . TokTyp 
      ; DiKind : DeclKindTyp 
      END 
 
; PROCEDURE PushDeclParseInfo ( READONLY Info : DeclParseInfoTyp )
  : INTEGER (* Depth after push. *)  

; PROCEDURE PopDeclParseInfo ( )
  : INTEGER (* Depth before pop. *) 

; PROCEDURE TopDeclParseInfo ( ) : DeclParseInfoTyp
  (* <Result>.DiKind = DeclKindTyp.DkNull, => stack is empty. *) 

; END FM3Decls
.

