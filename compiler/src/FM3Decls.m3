
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT IntRanges 

; IMPORT FM3Base
; IMPORT FM3IntToks
; IMPORT FM3Scopes
; IMPORT FM3Units 
; IMPORT VarArray_Int_Refany

(*EXPORTED*) 
; PROCEDURE DeclKindImage ( Kind : DeclKindTyp ) : TEXT

  = BEGIN 
      CASE Kind OF 
      | DeclKindTyp . DkNull => RETURN ", DkNull"
      | DeclKindTyp . DkDuplDecl => RETURN ", DkDuplDecl"
      | DeclKindTyp . DkMod => RETURN ", DkMod" 
      | DeclKindTyp . DkIntf => RETURN ", DkIntf" 
      | DeclKindTyp . DkGenMod => RETURN ", DkGenMod"
      | DeclKindTyp . DkGenIntf => RETURN ", DkGenIntf"
      | DeclKindTyp . DkExcp => RETURN ", DkExcp"
      | DeclKindTyp . DkType => RETURN ", DkType"
      | DeclKindTyp . DkConst => RETURN ", DkConst"
      | DeclKindTyp . DkVar => RETURN ", DkVar"
      | DeclKindTyp . DkValueFormal => RETURN ", DkValueFormal"
      | DeclKindTyp . DkVarFormal => RETURN ", DkVarFormal"
      | DeclKindTyp . DkROFormal => RETURN ", DkROFormal"
      | DeclKindTyp . DkRecField => RETURN ", DkRecField"
      | DeclKindTyp . DkObjField => RETURN ", DkObjField"
      | DeclKindTyp . DkMethod => RETURN ", DkMethod"
      | DeclKindTyp . DkProc => RETURN ", DkProc"
      | DeclKindTyp . DkWith => RETURN ", DkWith"
      | DeclKindTyp . DkFor => RETURN ", DkFor"
      | DeclKindTyp . DkExcArg => RETURN ", DkExcArg"
      END (*CASE*)
    END DeclKindImage

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

; TYPE DeclInfoNodeTyp
    = RECORD
        DinLink : DeclInfoRefTyp
      ; DinInfo : DeclInfoTyp 
      END
; TYPE DeclInfoRefTyp = REF DeclInfoNodeTyp

; VAR DeclInfoStack : DeclInfoRefTyp 
; VAR DeclInfoStackDepth : INTEGER 

(*EXPORTED*)
; PROCEDURE PushDeclInfo ( READONLY Info : DeclInfoTyp )
  : INTEGER (* Depth after push. *)  

  = BEGIN
      DeclInfoStack
        := NEW ( DeclInfoRefTyp
               , DinLink := DeclInfoStack
               , DinInfo := Info 
               )
    ; INC ( DeclInfoStackDepth )
    ; RETURN DeclInfoStackDepth 
    END PushDeclInfo
    
(*EXPORTED*) 
; PROCEDURE PopDeclInfo ( ) : INTEGER (* Depth before pop. *) 

  = VAR LResult : INTEGER
  ; BEGIN
      IF DeclInfoStack = NIL
      THEN
        <* ASSERT DeclInfoStackDepth = 0 *>
        RETURN 0 
      ELSE
        LResult := DeclInfoStackDepth
      ; <* ASSERT LResult > 0 *>
        DeclInfoStack := DeclInfoStack . DinLink
      ; RETURN LResult
      END (*IF*)  
    END PopDeclInfo 

(*EXPORTED*) 
; PROCEDURE TopDeclInfo ( ) : DeclInfoTyp
  (* Result.DiKind = DeclKindTyp.DkNull, if stack is empty. *) 

  = BEGIN
      IF DeclInfoStack = NIL
      THEN RETURN
        DeclInfoTyp
          { DiIdListTok := FM3IntToks . ItkNull
          , DiDeclTok := FM3IntToks . ItkNull
          , DiKind := DeclKindTyp.DkNull
          } 
      ELSE RETURN DeclInfoStack . DinInfo 
      END (*IF*) 
   END TopDeclInfo 

; BEGIN
    DeclInfoStack := NIL
  ; DeclInfoStackDepth := 0
(* CHECK: Could there ever be a need to reinitialize this" *) 
  END FM3Decls
.

