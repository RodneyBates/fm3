
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Decls

; IMPORT FM3Base
; IMPORT FM3Scopes

; TYPE DeclKindTyp
    = { DkNull
      , DkNotUsable 
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

; REVEAL FM3Base . DeclRefTyp = BRANDED REF DeclTyp 
; TYPE DeclRefTyp = FM3Base . DeclRefTyp 
; TYPE DeclTyp
    = RECORD 
        DclLink : DeclRefTyp
        (* For a linked list of nodes giving the positions of any decls, to
           the right of the leftmost, in the same scope, with the same Ident.
           These will have DeclKind DkDuplDecl.  Such a list is mutually
           exclusive of a single node of some other DeclKind. 
        *) 
      ; DclParentScopeRef : FM3Base . ScopeRefTyp (* Containing scope *) 
      ; DclSelfScopeRef : FM3Base . ScopeRefTyp (* If this declares a scope *)
      ; DclIdAtom : FM3Base . AtomTyp
      ; DclDeclNo : FM3Base . DeclNoTyp (* A self-reference. *)
      ; DclPos : FM3Base . tPosition 
      ; DclKind : DeclKindTyp 
      END (*DeclObjBaseTyp*)

; TYPE DeclMapTyp = FM3Base . MapTyp
    (* Map DeclNoTyp to DeclRefTyp. One of these per Unit. *)

; PROCEDURE NewDeclMap ( InitDeclCt : FM3Base . DeclNoTyp ) : DeclMapTyp 

; PROCEDURE NewDeclRef
    ( ParentScopeRef : FM3Base . ScopeRefTyp ; DeclNo : FM3Base . DeclNoTyp )
  : DeclRefTyp
  (* Allocate a DeclRef and connect in into ParentScopeRef ^. *)

(* A stack of info about different kinds of declarations and their sometimes
   multiple identifiers, allowing for sharing among them of parser productions
   and actions.  Trying to propagate this info in parser semantic attributes
   was turning out to be insanely fragile and complicated.
*)

; TYPE DeclInfoTyp
    = RECORD
        DiDeclTok : FM3Base . TokTyp 
      ; DiIdListTok : FM3Base . TokTyp
      ; DiIdTok : FM3Base . TokTyp
      ; DiIdSepTok : FM3Base . TokTyp 
      ; DiKind : DeclKindTyp 
      END 
 
; PROCEDURE PushDeclInfo ( READONLY Info : DeclInfoTyp )
  : INTEGER (* Depth after push. *)  

; PROCEDURE PopDeclInfo ( )
  : INTEGER (* Depth before pop. *) 

; PROCEDURE TopDeclInfo ( ) : DeclInfoTyp
  (* <Result>.DiKind = DeclKindTyp.DkNull, => stack is empty. *) 

; END FM3Decls
.

