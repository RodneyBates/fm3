
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT Fmt
; IMPORT Text
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
; IMPORT FM3SrcToks AS Stk
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
      | DeclKindTyp . DkOverride => RETURN "DkOverride"
      | DeclKindTyp . DkProcDecl => RETURN "DkProcDecl"
      | DeclKindTyp . DkProcDef => RETURN "DkProcDef"
      | DeclKindTyp . DkWith => RETURN "DkWith"
      | DeclKindTyp . DkFor => RETURN "DkFor"
      | DeclKindTyp . DkExcArg => RETURN "DkExcArg"
      END (*CASE*)
    END DeclKindImage

(*EXPORTED.*)
; PROCEDURE DeclNoImageOfDeclRef ( DeclRef : DeclRefTyp )  : TEXT 
  (* Unit-relative/Scope-relative. *)
  
  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LRelDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclNoImageOfDeclRef*)
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
    END DeclNoImageOfDeclRef

(*EXPORTED.*)
; PROCEDURE DeclInfoImageOfDeclRef ( DeclRef : DeclRefTyp )  : TEXT 
  (* Unit-relative/Scope-relative atom, position. *)
  
  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LRelDeclNo : FM3Globals . DeclNoTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclInfoImageOfDeclRef*)
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
            , AtomImageOfDeclRef ( DeclRef )
            , " "
            , PositionImage ( DeclRef ^ . DclPos )
            }
        )
    ; RETURN LResult 
    END DeclInfoImageOfDeclRef

; PROCEDURE AtomImageOfDeclRef ( DeclRef : DeclRefTyp ) : TEXT

  = VAR LAtom : FM3Base . AtomTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LDict : FM3Atom_OAChars . T
  ; VAR LOACharsRef : REF ARRAY OF CHAR 
  ; VAR LTextWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*AtomImageOfDeclRef*)
    (* Sheesh. This is a great example of the kind of code I had hoped to
       mimimize by making this a stream compiler instead of a tree compiler.
       But it does try to prevent crashes, if things are undone or wrongly done.
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
    ; Wr . PutText ( LTextWrT , "(\"" ) 
    ; Wr . PutString ( LTextWrT , LOACharsRef ^ ) 
    ; Wr . PutText ( LTextWrT , "\")" ) 
    ; LResult := TextWr . ToText ( LTextWrT )
    ; RETURN LResult 
    END AtomImageOfDeclRef

; VAR GMutex : MUTEX (* Protects GDefaultRef. *) 
; VAR GDefaultRef : DeclRefTyp 

(*EXPORTED.*)
; PROCEDURE DumpDecl
    ( DeclRef : DeclRefTyp
    ; WrT : Wr . T 
    ; DoFields := FALSE
    ; DefaultFields := FALSE
    ) 
  (* DeclNo, REF, and Position. Long => the fields too. *)

  = VAR LResult : TEXT

  ; PROCEDURE DdField ( Name : TEXT ; Value : TEXT ; DefVal : TEXT )

    = BEGIN (*DdField*)
        IF DoFields
        THEN 
          IF DefVal = NIL
             OR DefaultFields  
             OR NOT Text . Equal ( Value , DefVal )
          THEN 
            Wr . PutText ( WrT , "  " ) 
          ; Wr . PutText ( WrT , Name ) 
          ; Wr . PutText ( WrT , " = " ) 
          ; Wr . PutText ( WrT , Value ) 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          END (*IF*) 
        END (*IF*) 
      END DdField

  ; BEGIN (*DumpDecl*)
      IF DeclRef = NIL
      THEN
        Wr . PutText ( WrT , "NIL" )
      ; Wr . PutText ( WrT , Wr . EOL )
      ; RETURN
      END (*IF*)
    ; LOCK GMutex
      DO
        IF GDefaultRef = NIL THEN GDefaultRef := NEW ( DeclRefTyp ) END (*IF*) 
      ; Wr . PutText ( WrT , "DeclNo ") 
      ; Wr . PutText ( WrT , Fmt . Int ( DeclRef ^ . DclSelfDeclNo ) ) 
      ; Wr . PutText ( WrT , " at " ) 
      ; Wr . PutText ( WrT , FM3SharedUtils . RefanyImage ( DeclRef ) ) 
      ; Wr . PutChar ( WrT , ' ' ) 
      ; Wr . PutText ( WrT , DeclInfoImageOfDeclRef ( DeclRef ) )  
      ; Wr . PutText ( WrT , Wr . EOL ) 

      ; IF DoFields
        THEN
          WITH WDecl = DeclRef , WDef = GDefaultRef   
          DO 
            DdField ( "DclLink" , DeclRefImage ( WDecl . DclLink , DoFields := FALSE ) , NIL ) 
          ; DdField ( "DclOwningScopeRef" , ScopeRefImage ( WDecl . DclOwningScopeRef ) , NIL )
          ; DdField ( "DclSelfScopeRef" , ScopeRefImage ( WDecl . DclSelfScopeRef ) , NIL )
          ; DdField ( "DclDefType" , ExprRefImage ( WDecl . DclDefType ) , ExprRefImage ( WDef ^ . DclDefType ) )
          ; DdField ( "DclDefValue" , ExprRefImage ( WDecl . DclDefValue ) , ExprRefImage ( WDef ^ . DclDefValue ) )
          ; DdField ( "DclIdAtom" , AtomImageOfDeclRef ( WDecl ) , AtomImageOfDeclRef ( WDef ) )
          ; DdField ( "DclIdNo" , Fmt . Int ( WDecl . DclIdNo ) , Fmt . Int ( WDef ^ . DclIdNo ) )
          ; DdField ( "DclSelfDeclNo" , DeclNoImageOfDeclRef ( WDecl ) , DeclNoImageOfDeclRef ( WDef  ) )
          ; DdField ( "DclPos" , PositionImage ( WDecl . DclPos ) , PositionImage ( WDef ^ . DclPos ) )
          ; DdField ( "DclStdTok" , Stk . Name ( WDecl . DclStdTok ) , Stk . Name ( WDef ^ . DclStdTok ) )
          ; DdField ( "DclKind" , DeclKindImage ( WDecl . DclKind ) , DeclKindImage ( WDef ^ . DclKind ) ) 
          ; DdField ( "DclIsUsable" , Fmt . Bool ( WDecl . DclIsUsable ) , Fmt . Bool ( WDef ^ . DclIsUsable ) )
          END (*WITH*) 
        END (*IF*) 
      END (*LOCK*) 
    END DumpDecl

(*EXPORTED.*)
; PROCEDURE DeclRefImage
    ( DeclRef : DeclRefTyp ; DoFields := FALSE ; DefaultFields := FALSE ) : TEXT
  (* DeclNo, REF, and Position. Long => the fields too. *)

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclRefImage*)
      LWrT := TextWr . New ( )
    ; DumpDecl ( DeclRef , LWrT , DoFields , DefaultFields )
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END DeclRefImage 

(*EXPORTED.*)
; PROCEDURE DeclRefImageDebug ( DeclRef : DeclRefTyp ) : TEXT
  (* For calling by a debugger. *) 

  = BEGIN
      RETURN
        DeclRefImage ( DeclRef , DoFields := TRUE , DefaultFields := TRUE )
    END DeclRefImageDebug  

(*EXPORTED.*)
; PROCEDURE NewDeclRefListRef ( Ct : INTEGER ) : FM3Globals . DeclRefListRefTyp
  (* With all elements initialized to NIL. *) 

  = VAR LResult : FM3Globals . DeclRefListRefTyp 

  ; BEGIN (*NewDeclRefListRef*)
      LResult := NEW ( FM3Globals . DeclRefListRefTyp , Ct )
    ; FOR RI := FIRST ( LResult ^ ) TO LAST ( LResult ^ )
      DO LResult ^ [ RI ] := NIL 
      END (*FOR*)
    ; RETURN LResult 
    END NewDeclRefListRef

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

(*EXPORTED.*)
; PROCEDURE DeclRefOfDeclNo
    ( DeclNo : FM3Globals . DeclNoTyp
    ; UnitRef : FM3Units . UnitRefTyp := NIL (* NIL means current unit. *)
    )
  : DeclRefTyp
  (* In the current unit. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LDeclMap : DeclMapTyp 
  ; VAR LDeclRef : DeclRefTyp

  ; BEGIN
      IF UnitRef = NIL
      THEN LUnitRef := FM3Units . UnitStackTopRef 
      ELSE LUnitRef := UnitRef
      END (*IF*) 
    ; LDeclMap := LUnitRef ^ . UntDeclMap
    ; IF LDeclMap = NIL THEN RETURN NIL END 
    ; LDeclRef := VarArray_Int_Refany . Fetch ( LDeclMap , DeclNo )
     (*        ^Implied NARROW *)
    ; RETURN LDeclRef  
    END DeclRefOfDeclNo 

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
          , DiKind := DeclKindTyp.DkNull
          } 
      ELSE RETURN DeclParseInfoStack . DinInfo 
      END (*IF*) 
   END TopDeclParseInfo

(*EXPORTED.*)
; BEGIN
    DeclParseInfoStack := NIL
  ; DeclParseInfoStackDepth := 0
(* CHECK: Could there ever be a need to reinitialize this" *) 
  END FM3Decls
.

