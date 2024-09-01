
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
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
      | DeclKindTyp . DkExc => RETURN ", DkExc"
      | DeclKindTyp . DkType => RETURN ", DkType"
      | DeclKindTyp . DkConst => RETURN ", DkConst"
      | DeclKindTyp . DkVar => RETURN ", DkVar"
      | DeclKindTyp . DkVALUEFormal => RETURN ", DkVALUEFormal"
      | DeclKindTyp . DkVARFormal => RETURN ", DkVARFormal"
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
(*TODO: Maybe, create a decl object for FM3Base.DeclNoNotUsable. *) 
    END NewDeclMap

(*EXPORTED*) 
; PROCEDURE NewDeclRef
    ( ParentScopeRef : FM3Scopes . ScopeRefTyp ; DeclNo : FM3Base . DeclNoTyp )
  : DeclRefTyp
  (* Allocate a DeclRef and initialize a couple of fields. *)

  = VAR LDeclRef : DeclRefTyp

  ; BEGIN
      LDeclRef := NEW ( DeclRefTyp )
    ; LDeclRef ^ . DclSelfDeclNo := DeclNo
    ; LDeclRef ^ . DclParentScopeRef := ParentScopeRef
    ; RETURN LDeclRef 
    END NewDeclRef 

(* A stack of info about a declaration, with possibly multiple identifiers. *)
(* Let's make it a linked stack.  Simpler to implement, and will never
   be very deep.
*) 

; TYPE DeclParseInfoNodeTyp
    = RECORD
        DinLink : DeclParseInfoRefTyp
      ; DinInfo : DeclParseInfoTyp 
      END
; TYPE DeclParseInfoRefTyp = REF DeclParseInfoNodeTyp

; VAR DeclParseInfoStack : DeclParseInfoRefTyp 
; VAR DeclParseInfoStackDepth : INTEGER 

(*EXPORTED*)
; PROCEDURE PushDeclParseInfo ( READONLY Info : DeclParseInfoTyp )
  : INTEGER (* Depth after push. *)  

  = BEGIN
      DeclParseInfoStack
        := NEW ( DeclParseInfoRefTyp
               , DinLink := DeclParseInfoStack
               , DinInfo := Info 
               )
    ; INC ( DeclParseInfoStackDepth )
    ; RETURN DeclParseInfoStackDepth 
    END PushDeclParseInfo
    
(*EXPORTED*) 
; PROCEDURE PopDeclParseInfo ( ) : INTEGER (* Depth before pop. *) 

  = VAR LResult : INTEGER
  ; BEGIN
      IF DeclParseInfoStack = NIL
      THEN
        <* ASSERT DeclParseInfoStackDepth = 0 *>
        RETURN 0 
      ELSE
        LResult := DeclParseInfoStackDepth
      ; <* ASSERT LResult > 0 *>
        DEC ( DeclParseInfoStackDepth ) 
      ; DeclParseInfoStack := DeclParseInfoStack . DinLink
      ; RETURN LResult
      END (*IF*)  
    END PopDeclParseInfo 

(*EXPORTED*) 
; PROCEDURE TopDeclParseInfo ( ) : DeclParseInfoTyp
  (* <Result>.DiKind = DeclKindTyp.DkNull, => stack is empty. *) 

  = BEGIN
      IF DeclParseInfoStack = NIL
      THEN RETURN
        DeclParseInfoTyp
          { DiDeclTok := FM3IntToks . ItkNull
          , DiIdListTok := FM3IntToks . ItkNull
          , DiIdTok := FM3IntToks . ItkNull
          , DiIdSepTok := FM3IntToks . ItkNull
          , DiKind := DeclKindTyp.DkNull
          } 
      ELSE RETURN DeclParseInfoStack . DinInfo 
      END (*IF*) 
   END TopDeclParseInfo 

; BEGIN
    DeclParseInfoStack := NIL
  ; DeclParseInfoStackDepth := 0
(* CHECK: Could there ever be a need to reinitialize this" *) 
  END FM3Decls
.

