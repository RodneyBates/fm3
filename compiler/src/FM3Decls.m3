
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT IntRanges 

; IMPORT FM3Base
; IMPORT FM3Scopes
; IMPORT FM3Units 
; IMPORT VarArray_Int_Refany

(*EXPORTED*) 
; PROCEDURE NewDeclMap ( InitDeclCt : FM3Base . DeclNoTyp ) : DeclMapTyp
  (* One of these per Unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitDeclCt - 1 } ) 
    END NewDeclMap

(*EXPORTED*) 
; PROCEDURE NewDeclRef
    ( ParentScopeRef : FM3Scopes . ScopeRefTyp ; DeclNo : FM3Base . DeclNoTyp )
  : DeclRefTyp
  (* Allocate a DeclRef and connect it into ParentScopeRef ^. *)

  = VAR LDeclRef : DeclRefTyp

  ; BEGIN
      LDeclRef := NEW ( DeclRefTyp )
    ; LDeclRef . DclDeclNo := DeclNo
    ; INC ( ParentScopeRef ^ . ScpDeclCt )
    ; ParentScopeRef ^ . ScpMinDeclNo
        := MIN ( ParentScopeRef ^ . ScpMinDeclNo , DeclNo ) 
    ; LDeclRef . DclParentScopeRef := ParentScopeRef  
    ; VarArray_Int_Refany . Assign
        ( FM3Units . UnitStackTopRef ^ . UntDeclMap , DeclNo , LDeclRef )
    ; RETURN LDeclRef 
    END NewDeclRef 

(* A stack of pairs of a declaration kind and an Id-declaring token. *)
(* Let's make it a linked stack.  Simpler to implement, and will never
   be very deep.
*) 

; TYPE DeclInfoTyp
    = RECORD
        DiLink : DeclInfoRefTyp
      ; DiKind : DeclKindTyp 
      ; DiTok : FM3Base . TokTyp
      END
; TYPE DeclInfoRefTyp = REF DeclInfoTyp

; VAR DeclInfoStack : DeclInfoRefTyp 

(*EXPORTED*) 
; PROCEDURE PushDeclInfo
    ( DeclKind : DeclKindTyp ; DeclIdTok : FM3Base . TokTyp ) 

  = BEGIN
      DeclInfoStack
        := NEW ( DeclInfoRefTyp
               , DiLink := DeclInfoStack
               , DiKind := DeclKind
               , DiTok := DeclIdTok
               )  
    END PushDeclInfo
    
(*EXPORTED*) 
; PROCEDURE PopDeclInfo ( ) 

  = BEGIN
      IF DeclInfoStack # NIL
      THEN DeclInfoStack := DeclInfoStack . DiLink
      END (*IF*)  
    END PopDeclInfo 

(*EXPORTED*) 
; PROCEDURE TopDeclInfo
    ( VAR DeclKind : DeclKindTyp ; VAR DeclIdTok : FM3Base . TokTyp )

  = BEGIN
      IF DeclInfoStack = NIL
      THEN 
        DeclKind := DeclKindTyp . DkNull 
      ; DeclIdTok := FM3Base . TokNull 
      ELSE 
        DeclKind := DeclInfoStack . DiKind 
      ; DeclIdTok := DeclInfoStack . DiTok
      END (*IF*) 
   END TopDeclInfo 

; BEGIN
    DeclInfoStack := NIL 
  END FM3Decls
.

