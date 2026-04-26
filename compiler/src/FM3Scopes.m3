
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Scopes

; IMPORT Fmt
; IMPORT Text 
; IMPORT TextWr  
; IMPORT Wr 

; IMPORT IntRanges
; IMPORT IntSets
; IMPORT VarArray_Int_Refany

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3Decls 
; FROM FM3Decls IMPORT DeclNoImage
; IMPORT FM3Dict_Int_Int
; IMPORT FM3Exprs 
; IMPORT FM3Globals
; FROM FM3Graph IMPORT ArcSetImage 
; IMPORT FM3Messages
; IMPORT FM3SharedUtils 
; IMPORT FM3Units
; FROM FM3Units IMPORT UnitRefIdImage 
; IMPORT FM3Utils 
; FROM FM3Utils IMPORT PositionImage 
; IMPORT Ranges_Int

; TYPE Skt = ScopeKindTyp 

(*EXPORTED.*)
; PROCEDURE ScopeKindImage ( Kind : ScopeKindTyp ) : TEXT 

  = VAR LResult : TEXT 

  ; BEGIN (*ScopeKindImage*)
      CASE Kind OF
      | Skt . SkNull => LResult := "SkNull"
      | Skt . SkUniverse => LResult := "SkUniverse" 
      | Skt . SkComp => LResult := "SkComp" 
      | Skt . SkUnit => LResult := "SkUnit" 
      | Skt . SkInterface => LResult := "SkInterface" 
      | Skt . SkModule => LResult := "SkModule" 
      | Skt . SkFormals => LResult := "SkFormals" 
      | Skt . SkProcBody => LResult := "SkProcBody" 
      | Skt . SkBlockStmt => LResult := "SkBlockStmt"
      | Skt . SkCompEnv => LResult := "SkCompEnv" 
      | Skt . SkExports => LResult := "SkExports" 
      | Skt . SkEnum => LResult := "SkEnum"
      | Skt . SkRec => LResult := "SkRec"
      | Skt . SkObj => LResult := "SkObj"
      | Skt . SkWith => LResult := "SkWith"
      | Skt . SkTypecase => LResult := "SkTypecase"
      | Skt . SkExcept => LResult := "SkExcept" 
      ELSE LResult := "<UnknownScopeKind>" 
      END (*CASE*)
    ; RETURN LResult 
    END ScopeKindImage


(*EXPORTED*) 
; PROCEDURE NewScopeMap ( ScopeCt : FM3Globals . ScopeNoTyp ) : ScopeMapTyp
  (* One of these per unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , ScopeCt - 1 } ) 
    END NewScopeMap
    
; PROCEDURE IdentSetImage ( IdAtomSet : IntSets . T ; Prefix := "    " )
  : TEXT 

  = PROCEDURE OneIdImage ( Elem : IntSets . ElemT ) : TEXT 
    (* A callback. *) 

    = VAR LElemImage : TEXT
    
    ; BEGIN (*OneIdImage*)
        LElemImage
          := "I" 
             & Fmt . Int ( Elem )
             & "(\""
             & FM3Units . IdAtomText ( Elem ) (* In the current unit. *) 
             & "\")"
       ; RETURN LElemImage 
      END OneIdImage

  ; VAR LElemImagesRef : REF ARRAY OF TEXT 
  ; VAR LResult : TEXT
      
  ; BEGIN (*IdentSetImage*)
      LElemImagesRef := FM3Utils . IntSetElemsImages ( IdAtomSet , OneIdImage )
    ; LResult := FM3Utils . ListImage ( LElemImagesRef ^ , Prefix := Prefix ) 
    ; RETURN LResult 
    END IdentSetImage

; PROCEDURE DeclNoSetImage ( DeclNoSet : IntSets . T ; Prefix := "    " )
  : TEXT 

  = PROCEDURE OneDeclNoImage ( Elem : IntSets . ElemT ) : TEXT 
    (* A callback. *) 

    = VAR LElemImage : TEXT
    
    ; BEGIN (*OneDeclNoImage*)
        LElemImage := FM3Decls . DeclNoImage ( Elem ) 
      ; RETURN LElemImage 
      END OneDeclNoImage

  ; VAR LElemImagesRef : REF ARRAY OF TEXT 
  ; VAR LResult : TEXT
      
  ; BEGIN (*DeclNoSetImage*)
      LElemImagesRef
        := FM3Utils . IntSetElemsImages ( DeclNoSet , OneDeclNoImage )
    ; LResult := FM3Utils . ListImage ( LElemImagesRef ^ , Prefix := Prefix ) 
    ; RETURN LResult 
    END DeclNoSetImage

; PROCEDURE IdDeclDictImage ( Dict : FM3Dict_Int_Int . T ; Prefix := "    " )
  : TEXT 

  = VAR IddRulesImagesRef : REF ARRAY OF TEXT 
      
  ; PROCEDURE OneIdDeclImage
      ( RuleNo : INTEGER
      ; Key : FM3Dict_Int_Int . KeyTyp (* An INTEGER *) 
      ; Value : FM3Dict_Int_Int . ValueTyp (* An INTEGER *) 
      ) 
    (* A callback. *) 

    = VAR LElemImage : TEXT
    
    ; BEGIN (*OneIdDeclImage*)
        LElemImage
          := Fmt . Int ( RuleNo )
             & ": "
             & FM3Units . IdAtomText ( Key ) 
             & " -> " 
             & FM3Decls . DeclNoImage ( Value )
      ; IddRulesImagesRef ^ [ RuleNo ] := LElemImage 
      END OneIdDeclImage

  ; VAR LResult : TEXT
  ; BEGIN (*IdDeclDictImage*)
      IF Dict = NIL THEN RETURN "NIL" END (*IF*) 
    ; IddRulesImagesRef
        := NEW ( REF ARRAY OF TEXT , FM3Dict_Int_Int . Card ( Dict ) )
    ; FM3Dict_Int_Int . ForAllDo ( Dict , OneIdDeclImage ) 
    ; LResult := FM3Utils . ListImage ( IddRulesImagesRef ^ , Prefix := Prefix ) 
    ; RETURN LResult 
    END IdDeclDictImage

; PROCEDURE DeclRefListImage
    ( List : FM3Globals . DeclRefListRefTyp ; Prefix := "    " )
  : TEXT 

  = VAR LImagesRef : REF ARRAY OF TEXT 
  ; VAR LRef : FM3Decls . DeclRefTyp 
  ; VAR LElemImage : TEXT
  ; VAR LResult : TEXT
  ; VAR LCt : INTEGER 
    
  ; BEGIN (*DeclRefListImage*)
      IF List = NIL THEN RETURN "NIL" END (*IF*)
    ; LCt := NUMBER ( List ^ )
    ; IF LCt = 0 THEN RETURN "{ }" END (*IF*) 
    ; LImagesRef := NEW ( REF ARRAY OF TEXT , LCt )
    ; FOR RI := 0 TO LCt - 1
      DO
        LRef := List ^ [ RI ] (* Implied NARROW. *) 
      ; LElemImage := FM3Decls . DeclRefImage ( LRef ) 
      ; LImagesRef ^ [ RI ] := LElemImage 
      END (*FOR*) 
    ; LResult := FM3Utils . ListImage ( LImagesRef ^ , Prefix := Prefix ) 
    ; RETURN LResult 
    END DeclRefListImage

; PROCEDURE DefExprsImage ( Exprs : ARRAY BOOLEAN (*Is value expr*) OF REFANY )
  : TEXT 

  = VAR LResult : TEXT 

  ; BEGIN (*DefExprsImage*)
      LResult
        := Wr . EOL
           & "    " 
           & "FALSE(type) = "
           & FM3Exprs . ExprRefImage ( Exprs [ FALSE ] )
           & Wr . EOL  
           & "    " 
           & "TRUE(value) = "
           & FM3Exprs . ExprRefImage ( Exprs [ TRUE ] ) 
    ; RETURN LResult 
    END DefExprsImage

(*EXPORTED*) 
; PROCEDURE NewScopeRef
    ( OwningUnitRef : FM3Units . UnitRefTyp
    ; ScopeKind : ScopeKindTyp
    ; READONLY Position : FM3Base . tPosition
    )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef Owned by OwningUnitRef. *) 

  = VAR LUnitScopeMap : FM3Base . MapTyp
  ; VAR LScopeRef : ScopeRefTyp
  ; VAR LScopeNo : FM3Globals . ScopeNoTyp
  ; VAR LRange : Ranges_Int . RangeTyp  

  ; BEGIN (*NewScopeRef*) 
      LUnitScopeMap := OwningUnitRef ^ . UntScopeMap 
    ; LRange := VarArray_Int_Refany . TouchedRange ( LUnitScopeMap )
    ; IF Ranges_Int . RangeIsEmpty ( LRange ) 
      THEN LScopeNo := FM3Globals . ScopeNoFirstReal
      ELSE LScopeNo := LRange . Hi + 1
      END (* IF *) 
    ; LScopeRef := NEW ( ScopeRefTyp )
    ; LScopeRef ^ . ScpSelfScopeNo := LScopeNo
    ; LScopeRef ^ . ScpKind := ScopeKind
    ; LScopeRef ^ . ScpPosition := Position
    ; LScopeRef ^ . ScpDeclStackHt := - 1 
    ; LScopeRef ^ . ScpOpenStackHt := - 1 
 
    ; LScopeRef ^ . ScpDeclIdSet := IntSets . Empty ( )
    ; LScopeRef ^ . ScpDeclDict := NIL 
    ; LScopeRef ^ . ScpFormalIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpOwningUnitRef := OwningUnitRef 
    ; LScopeRef ^ . ScpOwningDeclNo := FM3Globals . DeclNoNull
    ; VarArray_Int_Refany . Assign ( LUnitScopeMap , LScopeNo , LScopeRef )
    ; RETURN LScopeRef 
    END NewScopeRef

(*EXPORTED.*)
; PROCEDURE ScopeRefOfScopeNo
    ( ScopeNo : FM3Globals . ScopeNoTyp 
    ; UnitRef : FM3Units . UnitRefTyp := NIL (* NIL means current unit. *)
    )
  : ScopeRefTyp 
   
  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LScopeMap : ScopeMapTyp 
  ; VAR LScopeRef : ScopeRefTyp

  ; BEGIN
      IF UnitRef = NIL
      THEN LUnitRef := FM3Units . UnitStackTopRef 
      ELSE LUnitRef := UnitRef
      END (*IF*) 
    ; LScopeMap := LUnitRef ^ . UntScopeMap
    ; IF LScopeMap = NIL THEN RETURN NIL END 
    ; LScopeRef := VarArray_Int_Refany . Fetch ( LScopeMap , ScopeNo )
      (*        ^Implied NARROW *)
    ; RETURN LScopeRef  
    END ScopeRefOfScopeNo 

(*EXPORTED.*)
; PROCEDURE PushScopeRefDeclsStack ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushDeclScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; IF ScopeRef ^ . ScpDeclStackHt # - 1 
      THEN <* ASSERT FALSE , "Scope repushed on decl scope stack." *>
      END (*IF*) 
    ; IF ScopeDeclStackTopRef = NIL
      THEN ScopeRef ^ . ScpDeclStackHt := 1 
      ELSE ScopeRef ^ . ScpDeclStackHt
             := ScopeDeclStackTopRef ^ . ScpDeclStackHt + 1
      END (*IF*) 
    ; ScopeRef ^ . ScpDeclStackLink := ScopeDeclStackTopRef
    ; ScopeDeclStackTopRef := ScopeRef
    ; INC ( ScopeDeclStackCt ) 
    END PushScopeRefDeclsStack

(*EXPORTED.*)
; PROCEDURE PopScopeRefDeclsStack ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopDeclScope*)
      LPoppedScopeRef := ScopeDeclStackTopRef 
    ; ScopeDeclStackTopRef := LPoppedScopeRef ^ . ScpDeclStackLink
    ; DEC ( ScopeDeclStackCt ) 
    ; <* ASSERT ( ScopeDeclStackTopRef = NIL ) = ( ScopeDeclStackCt = 0 ) *>
      LPoppedScopeRef ^ . ScpDeclStackHt := - 1 
    ; RETURN LPoppedScopeRef
    END PopScopeRefDeclsStack 

(*EXPORTED.*)
; PROCEDURE PruneScopeDeclsStack ( ToDepth : INTEGER := 0 )

  = BEGIN (*PruneScopeDeclsStack*)
      IF ScopeDeclStackCt > ToDepth 
      THEN 
        FM3Messages . FM3LogArrUnit
          ( ARRAY OF REFANY
              { "Scope number "
              , Fmt . Int ( ScopeDeclStackTopRef ^ . ScpSelfScopeNo )
              , " remains on decl scope stack at depth "
              , Fmt . Int ( ScopeDeclStackCt )
              , " when expected down to depth "
              , Fmt . Int ( ToDepth )
              , "." 
              } 
          , ScopeDeclStackTopRef ^ . ScpPosition
          ) 
      ; REPEAT EVAL PopScopeRefDeclsStack ( ) 
      ; UNTIL ScopeDeclStackCt <= ToDepth  
      END (*IF*) 
   END PruneScopeDeclsStack

(*EXPORTED.*)
; PROCEDURE PushScopeRefLookupStack ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushScopeRefLookupStack*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; <* ASSERT ScopeRef ^ . ScpOpenStackHt = - 1 *>
      IF ScopeLookupStackTopRef = NIL
      THEN ScopeRef ^ . ScpOpenStackHt := 1 
      ELSE ScopeRef ^ . ScpOpenStackHt
             := ScopeLookupStackTopRef ^ . ScpOpenStackHt + 1
      END (*IF*) 
    ; ScopeRef ^ . ScpLookupStackLink := ScopeLookupStackTopRef
    ; ScopeLookupStackTopRef := ScopeRef
    ; INC ( ScopeLookupStackCt ) 
    END PushScopeRefLookupStack

(*EXPORTED.*)
; PROCEDURE PopScopeRefLookupStack ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopScopeRefLookup*)
      LPoppedScopeRef := ScopeLookupStackTopRef 
    ; ScopeLookupStackTopRef := LPoppedScopeRef ^ . ScpLookupStackLink
    ; DEC ( ScopeLookupStackCt ) 
    ; <* ASSERT ( ScopeLookupStackTopRef = NIL ) = ( ScopeLookupStackCt = 0 ) *>
      LPoppedScopeRef ^ . ScpOpenStackHt := - 1 
    ; RETURN LPoppedScopeRef
    END PopScopeRefLookupStack (*EXPORTED.*)

(*EXPORTED.*)
; PROCEDURE PruneScopeLookupStack ( ToDepth : INTEGER := 0 )

  = BEGIN (*PruneScopeLookupStack*)
      IF ScopeLookupStackCt > ToDepth 
      THEN 
        FM3Messages . FM3LogArrUnit
          ( ARRAY OF REFANY
              { "Scope number "
              , Fmt . Int ( ScopeLookupStackTopRef ^ . ScpSelfScopeNo )
              , " remains on open scope stack at depth "
              , Fmt . Int ( ScopeLookupStackCt )
              , " when expected down to depth "
              , Fmt . Int ( ToDepth )
              , "." 
              } 
          , ScopeLookupStackTopRef ^ . ScpPosition
          ) 
      ; REPEAT EVAL PopScopeRefLookupStack ( ) 
      ; UNTIL ScopeLookupStackCt <= ToDepth  
      END (*IF*) 
   END PruneScopeLookupStack

(*EXPORTED.*)
; PROCEDURE ScopeNoImageOfScopeRef ( ScopeRef : ScopeRefTyp )  : TEXT 
  (* ScopeNo, Ident spelling, position. *)
  
  = VAR LScopeRef : ScopeRefTyp
  ; VAR LScopeNo : FM3Globals . ScopeNoTyp 
  ; VAR LRelScopeNo : FM3Globals . ScopeNoTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (*ScopeNoImageOfScopeRef*)
      IF ScopeRef = NIL THEN RETURN "<NIL ScopeRef>" END (*IF*)
    ; LScopeNo := ScopeRef ^ . ScpSelfScopeNo 
    ; LResult := Fmt . Int ( LScopeNo )  
    ; RETURN LResult 
    END ScopeNoImageOfScopeRef

(*EXPORTED.*)
; PROCEDURE IdImageOfScopeRef ( ScopeRef : ScopeRefTyp ) : TEXT

  = VAR LIdAtom : FM3Base . AtomTyp
  ; VAR LResult : TEXT 

  ; BEGIN (*IdImageOfScopeRef*)
      IF ScopeRef = NIL THEN RETURN "<NIL ScopeRef>" END (*IF*)
    ; LIdAtom := ScopeRef ^ . ScpIdentAtom
    ; LResult := FM3Utils . IdImageOfAtom ( LIdAtom ) 
    ; RETURN LResult 
    END IdImageOfScopeRef

; VAR GMutex : MUTEX (* Protects GDefaultRef. *)
  (* Just in the unlikely event of multiple threads i here. *) 
; VAR GDefaultRef : ScopeRefTyp 

(*EXPORTED.*)
; PROCEDURE DumpScope
    ( ScopeRef : ScopeRefTyp
    ; WrT : Wr . T 
    ; DoFields := FALSE
    ; DefaultFields := FALSE
    ; Prefix := "" 
    ) 
  (* ScopeNo, REF, and Position. DoFields => the fields too. *)
  (* Will have final NL only if DoFields. *) 

  = VAR LResult : TEXT
  ; VAR LDef : ScopeRefTyp 

  ; PROCEDURE DsField ( Name : TEXT ; Value : TEXT ; DefVal : TEXT )

    = BEGIN (*DsField*)
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
      END DsField

  ; BEGIN (*DumpScope*)
      IF ScopeRef = NIL
      THEN
        Wr . PutText ( WrT , "NIL Scope ref" ) 
      ; Wr . PutText ( WrT , Wr . EOL )
      ; RETURN
      END (*IF*)

    ; Wr . PutText ( WrT , "ScopeNo ") 
    ; Wr . PutText ( WrT , Fmt . Int ( ScopeRef ^ . ScpSelfScopeNo ) ) 
    ; Wr . PutText ( WrT , " at " ) 
    ; Wr . PutText ( WrT , FM3Utils . RefanyImage ( ScopeRef ) ) 
    ; Wr . PutChar ( WrT , ' ' ) 
    ; Wr . PutText ( WrT , IdImageOfScopeRef ( ScopeRef ) )  

    ; IF DoFields
      THEN
        LOCK GMutex
        DO
          IF GDefaultRef = NIL THEN GDefaultRef := NEW ( ScopeRefTyp ) END (*IF*)
        ; LDef := GDefaultRef 
        END (*LOCK*) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; WITH WScope = ScopeRef 
        DO 
          DsField ( "ScpDeclStackLink" , ScopeRefImage ( WScope ^ . ScpDeclStackLink , DoFields := FALSE ) , NIL ) 
        ; DsField ( "ScpLookupStackLink" , ScopeRefImage ( WScope ^ . ScpLookupStackLink , DoFields := FALSE ) , NIL ) 
        ; DsField ( "ScpOwningUnitRef" , UnitRefIdImage ( WScope ^ . ScpOwningUnitRef ) , NIL )
        ; DsField ( "ScpDeclIdSet" , IdentSetImage ( WScope ^ . ScpDeclIdSet ) , IdentSetImage ( LDef ^ . ScpDeclIdSet ) )
        ; DsField ( "ScpFormalIdSet" , IdentSetImage ( WScope ^ . ScpFormalIdSet ) , IdentSetImage ( LDef ^ . ScpFormalIdSet ) )
        ; DsField ( "ScpRefIdSet" , IdentSetImage ( WScope ^ . ScpRefIdSet ) , IdentSetImage ( LDef ^ . ScpRefIdSet ) )
        ; DsField ( "ScpDeclDict" , IdDeclDictImage ( WScope ^ . ScpDeclDict ) , IdDeclDictImage ( LDef ^ . ScpDeclDict ) )
        ; DsField ( "ScpDeclListRef" , DeclRefListImage ( WScope ^ . ScpDeclListRef ) , DeclRefListImage ( LDef ^ . ScpDeclListRef ) )
        ; DsField ( "ScpDeclGraph" , ArcSetImage ( WScope ^ . ScpDeclGraph ) , ArcSetImage ( LDef ^ . ScpDeclGraph ) )
        ; DsField ( "ScpCurDeclRefNoSet" , DeclNoSetImage ( WScope ^ . ScpCurDeclRefNoSet ) , DeclNoSetImage ( LDef ^ . ScpCurDeclRefNoSet ) )
        ; DsField ( "ScpCurDefExprs" , DefExprsImage ( WScope ^ . ScpCurDefExprs ) , DefExprsImage ( LDef ^ . ScpCurDefExprs ) )
        ; DsField ( "ScpIdentAtom" , IdImageOfScopeRef ( WScope ) , IdImageOfScopeRef ( LDef ) ) 
        ; DsField ( "ScpDeclListNo" , Fmt . Int ( WScope ^ . ScpDeclListNo ) , Fmt . Int ( LDef ^ . ScpDeclListNo ) )
        ; DsField ( "ScpMinDeclNo" , DeclNoImage ( WScope ^ . ScpMinDeclNo ) , DeclNoImage ( LDef ^ . ScpMinDeclNo ) )
        ; DsField ( "ScpSelfScopeNo" , Fmt . Int ( WScope ^ . ScpSelfScopeNo ) , Fmt . Int ( LDef ^ . ScpSelfScopeNo ) )
        ; DsField ( "ScpOwningDeclNo" , DeclNoImage ( WScope ^ . ScpOwningDeclNo ) , DeclNoImage ( LDef ^ . ScpOwningDeclNo ) )
        ; DsField ( "ScpDeclStackHt" , Fmt . Int ( WScope ^ . ScpDeclStackHt ) , Fmt . Int ( LDef ^ . ScpDeclStackHt ) )
        ; DsField ( "ScpOpenStackHt" , Fmt . Int ( WScope ^ . ScpOpenStackHt ) , Fmt . Int ( LDef ^ . ScpOpenStackHt ) )
        ; DsField ( "ScpCurDeclExprStackCt" , Fmt . Int ( WScope ^ . ScpCurDeclExprStackCt ) , Fmt . Int ( LDef ^ . ScpCurDeclExprStackCt ) ) 
        ; DsField ( "ScpPosition" , PositionImage ( WScope ^ . ScpPosition ) , PositionImage ( LDef ^ . ScpPosition ) )
        ; DsField ( "ScpKind" , ScopeKindImage ( WScope ^ . ScpKind ) , ScopeKindImage ( LDef ^ . ScpKind ) )
        ; DsField ( "ScpInsideDecl" , Fmt . Bool ( WScope ^ . ScpInsideDecl ) , Fmt . Bool ( LDef ^ . ScpInsideDecl ) )
        ; DsField ( "ScpCurDefIsValue" , Fmt . Bool ( WScope ^ . ScpCurDefIsValue ) , Fmt . Bool ( LDef ^ . ScpCurDefIsValue ) )
        END (*WITH*) 
      END (*IF*) 
    END DumpScope

(*EXPORTED.*)
; PROCEDURE ScopeRefImage
    ( ScopeRef : ScopeRefTyp ; DoFields := FALSE ; DefaultFields := FALSE )
  : TEXT
  (* ScopeNo, REF, and Position. Long => the fields too. *)
  (* Will have final NL only if DoFields. *) 

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*ScopeRefImage*)
      LWrT := TextWr . New ( )
    ; DumpScope ( ScopeRef , LWrT , DoFields , DefaultFields )
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END ScopeRefImage 

(*EXPORTED.*)
; PROCEDURE ScopeRefImageDebug ( ScopeRef : ScopeRefTyp ) : TEXT
  (* For calling by a debugger. *) 

  = BEGIN
      RETURN
        ScopeRefImage ( ScopeRef , DoFields := TRUE , DefaultFields := TRUE )
    END ScopeRefImageDebug  

; BEGIN
    ScopeDeclStackTopRef := NIL
  ; ScopeDeclStackCt := 0 
  ; ScopeLookupStackTopRef := NIL
  ; ScopeLookupStackCt := 0 
  ; GMutex := NEW ( MUTEX ) 
  END FM3Scopes
.

