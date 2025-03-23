
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT Fmt
; IMPORT TextWr
; IMPORT Wr 

; IMPORT IntRanges 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3Exprs
; FROM FM3Exprs IMPORT ExprRefImage 
; IMPORT FM3Globals 
; IMPORT FM3IntToks
; IMPORT FM3Scopes
; FROM FM3Scopes IMPORT ScopeRefImage
; IMPORT FM3SharedUtils
; IMPORT FM3Utils 
; FROM FM3Utils IMPORT SrcTokImage 
; IMPORT FM3Units
; FROM FM3Utils IMPORT PositionImage
; IMPORT VarArray_Int_Refany

(*EXPORTED*) 
; PROCEDURE DeclKindImage ( Kind : DeclKindTyp ) : TEXT

  = BEGIN 
      CASE Kind OF 
      | DeclKindTyp . DkNull => RETURN "DkNull"
      | DeclKindTyp . DkDuplDecl => RETURN "DkDuplDecl"
      | DeclKindTyp . DkMod => RETURN "DkMod" 
      | DeclKindTyp . DkIntf => RETURN "DkIntf" 
      | DeclKindTyp . DkGenMod => RETURN "DkGenMod"
      | DeclKindTyp . DkGenIntf => RETURN "DkGenIntf"
      | DeclKindTyp . DkExc => RETURN "DkExc"
      | DeclKindTyp . DkType => RETURN "DkType"
      | DeclKindTyp . DkConst => RETURN "DkConst"
      | DeclKindTyp . DkVar => RETURN "DkVar"
      | DeclKindTyp . DkVALUEFormal => RETURN "DkVALUEFormal"
      | DeclKindTyp . DkVARFormal => RETURN "DkVARFormal"
      | DeclKindTyp . DkROFormal => RETURN "DkROFormal"
      | DeclKindTyp . DkRecField => RETURN "DkRecField"
      | DeclKindTyp . DkObjField => RETURN "DkObjField"
      | DeclKindTyp . DkMethod => RETURN "DkMethod"
      | DeclKindTyp . DkProc => RETURN "DkProc"
      | DeclKindTyp . DkWith => RETURN "DkWith"
      | DeclKindTyp . DkFor => RETURN "DkFor"
      | DeclKindTyp . DkExcArg => RETURN "DkExcArg"
      END (*CASE*)
    END DeclKindImage

(*EXPORTED.*)
; PROCEDURE DeclRefImage ( DeclRef : DeclRefTyp ) : TEXT 
  (* DeclNo, REF, and Position. *) 

  = VAR LResult : TEXT

  ; BEGIN (*DeclRefImage*)
      IF DeclRef = NIL THEN RETURN "NIL" END (*IF*)
    ; LResult := FM3SharedUtils . CatArrT
        ( ARRAY OF REFANY
            { "DeclNo " 
            , Fmt . Int ( DeclRef ^ . DclSelfDeclNo )  
            , " at "  
            , FM3SharedUtils . RefanyImage ( DeclRef )  
            , " "
            , FM3Utils . PositionImage ( DeclRef . DclPos )
            }
        ) 
    ; RETURN LResult 
    END DeclRefImage

(*EXPORTED.*)
; PROCEDURE DeclNoImage ( DeclRef : DeclRefTyp )  : TEXT 
  (* Unit-relative/Scope-relative. *)
  
  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LRelDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclNoImage*)
      IF DeclRef = NIL THEN RETURN "<NIL DeclRef>" END (*IF*)
    ; LScopeRef := DeclRef ^ . DclOwningScopeRef
    ; IF LScopeRef = NIL THEN RETURN "<NIL ScopeRef>" END (*IF*)
    ; LDeclNo := DeclRef ^ . DclSelfDeclNo 
    ; LRelDeclNo := LDeclNo - LScopeRef ^ . ScpMinDeclNo
    ; LResult := FM3SharedUtils . CatArrT
        ( ARRAY OF REFANY
            { Fmt . Int ( LDeclNo )  
            , "/"
            , Fmt . Int ( LRelDeclNo )
            }
        )
    ; RETURN LResult 
    END DeclNoImage

(*EXPORTED.*)
; PROCEDURE DeclInfoImage ( DeclRef : DeclRefTyp )  : TEXT 
  (* Unit-relative/Scope-relative atom, position. *)
  
  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LRelDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclInfoImage*)
      IF DeclRef = NIL THEN RETURN "<NIL DeclRef>" END (*IF*)
    ; LScopeRef := DeclRef ^ . DclOwningScopeRef
    ; IF LScopeRef = NIL THEN RETURN "<NIL ScopeRef>" END (*IF*)
    ; LDeclNo := DeclRef ^ . DclSelfDeclNo 
    ; LRelDeclNo := LDeclNo - LScopeRef ^ . ScpMinDeclNo
    ; LResult := FM3SharedUtils . CatArrT
        ( ARRAY OF REFANY
            { Fmt . Int ( LDeclNo )  
            , "/"
            , Fmt . Int ( LRelDeclNo )
            , " Id "
            , AtomImage ( DeclRef )
            , " "
            , PositionImage ( DeclRef ^ . DclPos )
            }
        )
    ; RETURN LResult 
    END DeclInfoImage

; PROCEDURE AtomImage ( DeclRef : DeclRefTyp ) : TEXT

  = VAR LAtom : FM3Base . AtomTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LDict : FM3Atom_OAChars . T
  ; VAR LOACharsRef : REF ARRAY OF CHAR 
  ; VAR LTextWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*AtomImage*)
    (* Sheesh. This is a great example of the kind of code I had hoped to
       mimimize by making this a stream compiler instead of a tree compiler.
       But it does try to prevent crashes, if things are Undone or wrong. 
    *)
      IF DeclRef = NIL THEN RETURN "<NIL DeclRef>" END (*IF*)
    ; LAtom := DeclRef ^ . DclIdAtom
    ; IF LAtom = FM3Base. AtomNull THEN RETURN "<AtomNull>" END (*IF*)
    ; LScopeRef := DeclRef ^ . DclOwningScopeRef
    ; IF LScopeRef = NIL THEN RETURN "<NIL ScopeRef>" END (*IF*)
    ; LUnitRef := LScopeRef ^ . ScpOwningUnitRef 
    ; IF LUnitRef = NIL THEN RETURN "<NIL UnitRef>" END (*IF*)
    ; LDict := LUnitRef ^ . UntIdentAtomDict 
    ; IF LDict = NIL THEN RETURN "<NIL Dict>" END (*IF*)
    ; LOACharsRef := FM3Utils . CharsOfAtom ( LDict , LAtom ) 
    ; IF LOACharsRef = NIL THEN RETURN "<NIL Chars>" END (*IF*)
    ; LTextWrT := TextWr . New ( )
    ; Wr . PutText ( LTextWrT , Fmt . Int ( LAtom ) ) 
    ; Wr . PutText ( LTextWrT , "(\"") 
    ; Wr . PutString ( LTextWrT , LOACharsRef ^ ) 
    ; Wr . PutText ( LTextWrT , "\")") 
    ; LResult := TextWr . ToText ( LTextWrT )
    ; RETURN LResult 
    END AtomImage
      
(*EXPORTED.*)
; PROCEDURE DeclTypImage ( DeclRef : DeclRefTyp ) : TEXT 
  (* Contents of the record. *)  

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT

  ; PROCEDURE PT ( String : TEXT )
    = BEGIN
        Wr . PutText ( LWrT , String )
      END PT

  ; PROCEDURE PTNL ( String : TEXT )
    = BEGIN
        Wr . PutText ( LWrT , String )
      ; Wr . PutText ( LWrT , Wr . EOL )
      END PTNL

  ; BEGIN (*DeclTypImage*)
      IF DeclRef = NIL THEN RETURN "NIL" END (*IF*)
    ; LWrT := TextWr . New ( )
    ; PT("DeclRef at ")          ; PTNL(DeclRefImage(DeclRef))
    ; PT("DclLink =           ") ; PTNL(DeclRefImage(DeclRef^.DclLink))
    ; PT("DclOwningScopeRef = ") ; PTNL(ScopeRefImage(DeclRef^.DclOwningScopeRef))
    ; PT("DclSelfScopeRef =   ") ; PTNL(ScopeRefImage(DeclRef^.DclSelfScopeRef))
    ; PT("DclDefType =        ") ; PTNL(ExprRefImage(DeclRef^.DclDefType))
    ; PT("DclDefValue =       ") ; PTNL(ExprRefImage(DeclRef^.DclDefValue))
    ; PT("DclIdAtom =         ") ; PTNL(AtomImage(DeclRef))
    ; PT("DclIdCt =           ") ; PTNL(Fmt.Int(DeclRef^.DclIdCt))
    ; PT("DclIdNo =           ") ; PTNL(Fmt.Int(DeclRef^.DclIdNo))
    ; PT("DclSelfDeclNo =     ") ; PTNL(DeclNoImage(DeclRef))
    ; PT("DclPos =            ") ; PTNL(PositionImage(DeclRef^.DclPos))
    ; PT("DclStdTok =         ") ; PTNL(SrcTokImage(DeclRef^.DclStdTok))
    ; PT("DclKind =           ") ; PTNL(DeclKindImage(DeclRef^.DclKind))
    ; PT("DclIsUsable =       ") ; PTNL(Fmt.Bool(DeclRef^.DclIsUsable))

    ; LResult := TextWr . ToText ( LWrT ) 
    ; RETURN LResult 
    END DeclTypImage

(*EXPORTED*) 
; PROCEDURE NewDeclMap ( InitDeclCt : FM3Globals . DeclNoTyp ) : DeclMapTyp
  (* One of these per Unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitDeclCt - 1 } )
(*TODO: Maybe, create a decl object for FM3Base.DeclNoNotUsable. *) 
    END NewDeclMap

(*EXPORTED*) 
; PROCEDURE NewDeclRef
    ( OwningScopeRef : FM3Scopes . ScopeRefTyp
    ; DeclNo : FM3Globals . DeclNoTyp
    )
  : DeclRefTyp
  (* Allocate a DeclRef and initialize a couple of fields. *)

  = VAR LDeclRef : DeclRefTyp

  ; BEGIN
      LDeclRef := NEW ( DeclRefTyp )
    ; LDeclRef ^ . DclSelfDeclNo := DeclNo
    ; LDeclRef ^ . DclOwningScopeRef := OwningScopeRef
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

