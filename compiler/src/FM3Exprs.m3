
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Exprs

; IMPORT Fmt
; IMPORT IntRanges
; IMPORT Text 
; IMPORT TextWr 
; IMPORT VarArray_Int_Refany
; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3CLToks AS Clt
; IMPORT FM3CLOptions 
; IMPORT FM3Globals
; IMPORT FM3Messages
; IMPORT FM3Parser 
; IMPORT FM3Scopes
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks
; IMPORT FM3Units 
; IMPORT FM3Utils

(*EXPORTED.*)
; PROCEDURE ExprKindImage ( Kind : ExprKindTyp ) : TEXT 

  = BEGIN (*ExprKindImage*)
      CASE Kind OF
      | ExprKindTyp . EkNull => RETURN "EkNull" 
      | ExprKindTyp . EkLiteral => RETURN "EkLiteral"
(* 
      | ExprKindTyp . EkIntLit => RETURN "EkIntLit" 
      | ExprKindTyp . EkLongintLit => RETURN "EkLongintLit" 
      | ExprKindTyp . EkRealLit => RETURN "EkRealLit" 
      | ExprKindTyp . EkLongLit => RETURN "EkLongLit" 
      | ExprKindTyp . EkExtendedLit => RETURN "EkExtendedLit" 
      | ExprKindTyp . EkCharLit => RETURN "EkCharLit" 
      | ExprKindTyp . EkWideCharLit => RETURN "EkWideCharLit" 
      | ExprKindTyp . EkTextLit => RETURN "EkTextLit" 
      | ExprKindTyp . EkWideTextLit => RETURN "EkWideTextLit"
*)
      | ExprKindTyp . EkIdentRef => RETURN "EkIdentRef" 
      | ExprKindTyp . EkQualIdentRef => RETURN "EkQualIdentRef" 
      | ExprKindTyp . EkRemoteRef => RETURN "EkRemoteRef"  
      | ExprKindTyp . EkEnumType => RETURN "EkEnumType" 
      | ExprKindTyp . EkRecType => RETURN "EkRecType" 
      | ExprKindTyp . EkArrayType => RETURN "EkArrayType" 
      | ExprKindTyp . EkObjType => RETURN "EkObjType" 
      | ExprKindTyp . EkSubrType => RETURN "EkSubrType"  
      | ExprKindTyp . EkRefType => RETURN "EkRefType" 
      | ExprKindTyp . EkType => RETURN "EkType" 
      | ExprKindTyp . EkSupertype => RETURN "EkSupertype" 
      | ExprKindTyp . EkUnop => RETURN "EkUnop" 
      | ExprKindTyp . EkBinop => RETURN "EkBinop" 
      | ExprKindTyp . EkCall => RETURN "EkCall" 
      | ExprKindTyp . EkSubscript => RETURN "EkSubscript" 
      | ExprKindTyp . EkProc => RETURN "EkProc"  
      | ExprKindTyp . EkFunc => RETURN "EkFunc"  
      | ExprKindTyp . EkValue => RETURN "EkValue" 
      | ExprKindTyp . EkBrand => RETURN "EkBrand"  
      | ExprKindTyp . EkConst => RETURN "EkConst"  
      | ExprKindTyp . EkRef => RETURN "EkRef"  
      ELSE RETURN "<Unknown ExprKindImage>"
      END (*CASE*) 
    END ExprKindImage

(* EXPORTED.*) 
; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

  = BEGIN
      CASE Kind OF 
      | ExprKindTyp . EkNull
       => RETURN "<null>" 
      | ExprKindTyp . EkType 
       => RETURN "type" 
      | ExprKindTyp . EkProc 
       => RETURN "procedure" 
      | ExprKindTyp . EkFunc
       => RETURN "function" 
      | ExprKindTyp . EkValue 
       => RETURN "value" 
      | ExprKindTyp . EkConst 
       => RETURN "constant" 
      | ExprKindTyp . EkRef 
       => RETURN "reference" 
      ELSE RETURN "<unknown>"
      END (*CASE *) 
    END ExprKindMessage 

(* EXPORTED.*) 
; PROCEDURE KindSetCard ( KindSet : ExprKindSetTyp ) : INTEGER 

  = VAR LCt : INTEGER

  ; BEGIN
      LCt := 0 
    ; FOR RK := FIRST ( ExprKindTyp ) TO LAST ( ExprKindTyp )
      DO IF RK IN KindSet THEN INC ( LCt ) END (*IF*) 
      END (*FOR*)
    ; RETURN LCt
    END KindSetCard

(* EXPORTED.*) 
; PROCEDURE ExprKindSetMessage ( KindSet : ExprKindSetTyp ) : TEXT

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 
  ; VAR LCt , LNo : INTEGER

  ; BEGIN 
      LCt := KindSetCard ( KindSet ) 
    ; IF LCt = 0 THEN RETURN "<nothing>" END (*IF*) 
    ; LWrT := TextWr . New ( )
    ; FOR RK := FIRST ( ExprKindTyp ) TO LAST ( ExprKindTyp )
      DO IF RK IN KindSet
        THEN
          IF LCt = 1 THEN RETURN "a " & ExprKindMessage ( RK ) END (*IF*)
        ; LNo := ORD ( RK )
        ; IF LNo + 1 <= LCt THEN Wr . PutText ( LWrT , ", " ) END (*IF*) 
        ; IF LNo + 1 = LCt THEN Wr . PutText ( LWrT , "or " ) END (*IF*)
        ; Wr . PutText ( LWrT , ExprKindMessage ( RK ) )
        END (*IF*) 
      END (*FOR*)
    ; LResult := TextWr . ToText ( LWrT)
    ; RETURN LResult
    END ExprKindSetMessage

(* EXPORTED.*) 
; PROCEDURE ExprStateImage ( State : ExprStateTyp ) : TEXT

  = BEGIN 
      CASE State OF 
      | Est . EsUnknown => RETURN "EsUnknown" 
      | Est . EsUnresolved => RETURN "EsUnresolved"
      | Est . EsResolving => RETURN "EsResolving"
      | Est . EsResolved  => RETURN "EsResolved "
      END (*CASE*) 
    END ExprStateImage

; PROCEDURE AppendExprList ( ExprList : ExprListRefTyp ) 

  = BEGIN
      IF ExprList = NIL THEN RETURN END (*IF*)
    ; FOR RI := FIRST ( ExprList ^ ) TO LAST ( ExprList ^ ) 
      DO
        Wr . PutText ( GWrT , GIndentStrings [ ORD ( GDepth MOD 5 = 0 ) ] ) 
      ; Wr . PutChar ( GWrT , '[' )  
      ; Wr . PutText ( GWrT , Fmt . Int ( RI ) ) 
      ; Wr . PutChar ( GWrT , ']' )
      ; Wr . PutText ( GWrT , Wr . EOL ) 
      ; AppendNestedExpr ( ExprList ^ [ RI ] ) 
      ; Wr . PutText ( GWrT , "END" ) 
      ; Wr . PutText ( GWrT , Wr . EOL ) 
    END (*FOR*) 
    END AppendExprList

    
; VAR GWrT : Wr . T (* Sorry for the global. *)
; VAR GDepth : INTEGER
; VAR GExprNosDumped : IntSets . T
; TYPE IndentStringsTyp = ARRAY [ 0 .. 1 ] OF TEXT
; VAR GIndentStrings : IndentStringsTyp 
; CONST IndentBase0
    = ".. 2.. 4.. 6.. 8..10..12..14..16..18..20..22..24..26..28..30..32..34..36..38  "
; CONST IndentBase1 
    = "   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  "

(*EXPORTED.*) 
; PROCEDURE DumpExpr
    ( Expr : ExprTyp ; WrT : Wr . T ; VAR (*IN OUT*) ExprNosDumped : IntSets . T)
  (* PRE: It's not already dumped. *)
  (* PRE: Expr header line already written. *) 

  = BEGIN
      GWrT := WrT 
    ; GDepth := 0 
    ; GExprNosDumped := ExprNosDumped 
    ; Expr . appendDump ( )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; ExprNosDumped := GExprNosDumped
    END DumpExpr

(*EXPORTED.*) 
; PROCEDURE ExprImage ( Expr : ExprTyp ) : TEXT 
  (* For calling from within a debugger. *) 

  = BEGIN
      GWrT := TextWr . New ( )
    ; GDepth := 0
    ; GExprNosDumped := IntSets . Empty ( ) 
    ; Expr . appendDump ( )
    ; Wr . PutText ( GWrT , Wr . EOL ) 
    ; RETURN TextWr . ToText ( GWrT )
    END ExprImage

; PROCEDURE SubtypeComment ( Text : TEXT ) 

  = BEGIN
      Wr . PutText ( GWrT , GIndentStrings [ ORD ( GDepth MOD 5 = 0 ) ] )
    ; Wr . PutText ( GWrT , " (* From subtype " ) 
    ; Wr . PutText ( GWrT , Text ) 
    ; Wr . PutText ( GWrT , ": *)" ) 
    ; Wr . PutText ( GWrT , Wr . EOL ) 
    END SubtypeComment

; PROCEDURE Field ( Name : TEXT ; Value : TEXT ) 

  = BEGIN
      Wr . PutText ( GWrT , GIndentStrings [ ORD ( GDepth MOD 5 = 0 ) ] )
    ; Wr . PutChar ( GWrT , ' ' ) 
    ; Wr . PutText ( GWrT , Name ) 
    ; Wr . PutText ( GWrT , " = " ) 
    ; Wr . PutText ( GWrT , Value ) 
    ; Wr . PutText ( GWrT , Wr . EOL ) 
    END Field

; PROCEDURE AppendNestedExpr ( Expr : ExprTyp ) 

  = VAR LIndentStrings : IndentStringsTyp 

  ; BEGIN
      IF Expr = NIL
      THEN 
        Wr . PutText ( GWrT , Wr . EOL )
      ELSE 
        INC ( GDepth )
      ; LIndentStrings := GIndentStrings 
      ; GIndentStrings [ 0 ]  := Text . Sub ( IndentBase0 , 0 , GDepth * 2 ) 
      ; GIndentStrings [ 1 ]  := Text . Sub ( IndentBase1 , 0 , GDepth * 2 ) 
      ; Expr . appendDump ( )
      ; GIndentStrings := LIndentStrings 
      ; DEC ( GDepth )
      END (*IF*) 
    END AppendNestedExpr

; PROCEDURE NestedField ( Name : TEXT ; Expr : ExprTyp ) 

  = BEGIN
      Wr . PutText ( GWrT , GIndentStrings [ ORD ( GDepth MOD 5 = 0 ) ] )
    ; Wr . PutChar ( GWrT , ' ' ) 
    ; Wr . PutText ( GWrT , Name ) 
    ; Wr . PutText ( GWrT , " = " )
    ; IF Expr = NIL
      THEN
        Wr . PutText ( GWrT , "NIL" )
      ; Wr . PutText ( GWrT , Wr . EOL ) 
      ELSE
        Wr . PutText ( GWrT , "ExprNo " )
      ; Wr . PutText ( GWrT , Fmt . Int ( Expr . ExpSelfExprNo ) )
      ; Wr . PutChar ( GWrT , ' ' )
      ; Wr . PutText ( GWrT , FM3Utils . RefanyImage ( Expr ) ) 
      ; IF IntSets . IsElement ( Expr . ExpSelfExprNo , GExprNosDumped )
        THEN
          Wr . PutText ( GWrT , ", previously displayed." )
        ; Wr . PutText ( GWrT , Wr . EOL ) 
        ELSE
          Wr . PutText ( GWrT , Wr . EOL )  
        ; GExprNosDumped
            := IntSets . Include ( GExprNosDumped , Expr . ExpSelfExprNo )
        ; AppendNestedExpr ( Expr ) 
        ; Wr . PutText ( GWrT , Wr . EOL )  
        END (*IF*) 
      END (*IF*) 
    END NestedField
(*
; PROCEDURE LongHexImage ( Value : LONGINT ) : TEXT 

  = BEGIN
      RETURN
        "16_" & Fmt . Pad
                  ( Fmt . LongUnsigned ( Value , base := 16 )
                  , length := 16
                  , padChar := '0'
                  , align:= Fmt . Align . Right
                  )
    END LongHexImage
*)

; PROCEDURE AtomTypImage ( Value : FM3Base . AtomTyp ) : TEXT

  = VAR LChars : REF ARRAY OF CHAR
  ; VAR LResult : TEXT
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      LFound
        := FM3Atom_OAChars . Key 
             ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
             , Value
             , (*OUT*) LChars
             )
    ; IF LFound
      THEN LResult := Text . FromChars ( LChars ^ )
      ELSE LResult := "<NotFound>"
      END (*IF*)
    ; RETURN LResult 
    END AtomTypImage

; PROCEDURE IntSetsElemImage ( Elem : INTEGER ) : TEXT
  (* Why do I have to wrap Fmt.Int? *) 

  = BEGIN
      RETURN Fmt . Int ( Elem ) 
    END IntSetsElemImage

(*EXPORTED.*)
; PROCEDURE ExprRefImage ( ExprRef : REFANY ) : TEXT 
  (* ExprNo, REF, and Position. *) 

  = VAR LResult : TEXT 

  ; BEGIN (*ExprRefImage*)
      TYPECASE ExprRef OF
      | NULL => RETURN "NIL"
      | ExprTyp ( TExprRef )
      =>  LResult := FM3SharedUtils . CatArrT
            ( ARRAY OF REFANY
                { "ExprNo "
                , Fmt . Int ( TExprRef . ExpSelfExprNo )
                , " at "
                , FM3Utils . RefanyImage ( TExprRef )
                , " "
                , FM3Utils . PositionImage ( TExprRef . ExpPosition )
                }
            ) 
        ; RETURN LResult 
      ELSE RETURN "Not an ExprRef: " & FM3SharedUtils . RefanyImage ( ExprRef )
      END (*TYPECASE*) 
    END ExprRefImage

; REVEAL ExprTyp
    = ExprPublic BRANDED OBJECT OVERRIDES appendDump := ExprAppend END
    
; PROCEDURE ExprAppend ( Expr : ExprTyp )
    = BEGIN
        Field ( "ExpStackLink" , ExprRefImage ( Expr . ExpStackLink ) ) 
      ; NestedField ( "ExpType" , Expr . ExpType ) 
      ; Field ( "ExpRefConstVal" , FM3Utils . RefanyImage ( Expr . ExpRefConstVal ) )  
      ; Field
          ( "ExpScalarConstVal" , Fmt . LongInt ( Expr . ExpScalarConstVal ) )  
      ; Field
          ( "ExpLoTypeInfoRef"
          , "" (* LoTypeInfoRefTypImage ( Expr . ExpLoTypeInfoRef ) *)
          )
      ; Field ( "ExpReachedDeclNoSet"
              , IntSets . Image ( Expr . ExpReachedDeclNoSet , IntSetsElemImage )
              ) 
      ; Field ( "ExpSelfExprNo" , Fmt . Int ( Expr . ExpSelfExprNo ) ) 
      ; Field ( "ExpPosition" , FM3Utils . PositionImage ( Expr . ExpPosition ) )
      ; Field ( "ExpOpcode" , FM3SrcToks . Image ( Expr . ExpOpcode ) )  
      ; Field ( "ExpDownKind" , ExprKindImage ( Expr . ExpDownKind ) ) 
      ; Field ( "ExpUpKind" , ExprKindImage ( Expr . ExpUpKind ) ) 
      ; Field ( "ExpKind" , ExprKindImage ( Expr . ExpKind ) )  
      ; Field ( "ExpState" , ExprStateImage ( Expr . ExpState ) )  
      ; Field ( "ExpIsConst" , Fmt . Bool ( Expr . ExpIsConst ) )  
      ; Field ( "ExpConstValIsKnown" , Fmt . Bool ( Expr . ExpConstValIsKnown ) )
      ; Field ( "ExpIsUsable" , Fmt . Bool ( Expr . ExpIsUsable ) ) 
      ; Field
          ( "ExpIsLegalRecursive" , Fmt . Bool ( Expr . ExpIsLegalRecursive ) )
      ; Field ( "ExpIsDesignator" , Fmt . Bool ( Expr . ExpIsDesignator ) ) 
      ; Field ( "ExpIsWritable" , Fmt . Bool ( Expr . ExpIsWritable ) )  
      ; Field ( "ExpIsPresent" , Fmt . Bool ( Expr . ExpIsPresent ) )  
      ; Field
          ( "ExpRefTypeIsUntraced" , Fmt . Bool ( Expr . ExpRefTypeIsUntraced ) )
      ; Field
          ( "ExpArrayTypeIsOpen" , Fmt . Bool ( Expr . ExpArrayTypeIsOpen ) )
      END ExprAppend

; REVEAL Expr1OpndTyp
    = Expr1OpndPublic BRANDED OBJECT OVERRIDES appendDump := Expr1OpndAppend END

; PROCEDURE Expr1OpndAppend ( Expr : Expr1OpndTyp )
    = BEGIN 
        ExprAppend ( Expr )
      ; SubtypeComment ( "Expr1OpndTyp" )
      ; NestedField ( "ExpOpnd1" , Expr . ExpOpnd1 ) 
      END Expr1OpndAppend

; REVEAL Expr2OpndTyp
    = Expr2OpndPublic BRANDED OBJECT OVERRIDES appendDump := Expr2OpndAppend END

; PROCEDURE Expr2OpndAppend ( Expr : Expr2OpndTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; SubtypeComment ( "Expr2OpndTyp" )
      ; NestedField ( "ExpOpnd2" , Expr . ExpOpnd2 ) 
      END Expr2OpndAppend

; REVEAL Expr3OpndTyp
    = Expr3OpndPublic BRANDED OBJECT OVERRIDES appendDump := Expr3OpndAppend END

; PROCEDURE Expr3OpndAppend ( Expr : Expr3OpndTyp )
    = BEGIN 
        Expr2OpndAppend ( Expr ) 
      ; SubtypeComment ( "Expr3OpndTyp" )
      ; NestedField ( "ExpOpnd3" , Expr . ExpOpnd3 ) 
      END Expr3OpndAppend

; REVEAL ExprMultiOpndTyp
    = ExprMultiOpndPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprMultiOpndAppend END

; PROCEDURE ExprMultiOpndAppend ( Expr : ExprMultiOpndTyp )
    = BEGIN
(*
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprMultiOpndTyp" )
      ; Field ( "ExpOpnds" , "" ) 
      ; AppendExprList ( Expr . ExpOpnds )
*) 
      END ExprMultiOpndAppend

(* Identifier references: *) 
; REVEAL ExprIdentRefTyp
    = ExprIdentRefPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprIdentRefAppend END

; PROCEDURE ExprIdentRefAppend ( Expr : ExprIdentRefTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprIdentRefTyp" )
      ; Field ( "ExpIdentDeclNo" , Fmt . Int ( Expr . ExpIdentDeclNo ) ) 
      END ExprIdentRefAppend (* Not builtin. *) 

; REVEAL ExprRemoteRefTyp
    = ExprRemoteRefPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprRemoteRefAppend END

; PROCEDURE ExprRemoteRefAppend ( Expr : ExprRemoteRefTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprRemoteRefTyp" )
      ; Field ( "ExpRemoteUnitNo" , Fmt . Int ( Expr . ExpRemoteUnitNo ) ) 
      ; Field ( "ExpRemoteDeclNo" , Fmt . Int ( Expr . ExpRemoteDeclNo ) ) 
      END ExprRemoteRefAppend

; REVEAL ExprQualIdDeclNoAtomTyp
    = ExprQualIdDeclNoAtomPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprQualIdDeclNoAtomAppend END

; PROCEDURE ExprQualIdDeclNoAtomAppend ( Expr : ExprQualIdDeclNoAtomTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprQualIdDeclNoAtomTyp" )
      ; Field ( "ExpQualDeclNoLt" , Fmt . Int ( Expr . ExpQualDeclNoLt ) ) 
      ; Field ( "ExpQualIdAtomRt" , AtomTypImage ( Expr . ExpQualIdAtomRt ) ) 
      END ExprQualIdDeclNoAtomAppend

; REVEAL ExprDotTyp
    = ExprDotPublic BRANDED OBJECT OVERRIDES appendDump := ExprDotAppend END

; PROCEDURE ExprDotAppend ( Expr : ExprDotTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprDotTyp" )
      ; Field ( "ExpDotIdAtom" , AtomTypImage ( Expr . ExpDotIdAtom ) ) 
      END ExprDotAppend

(* Either a constant expression or one whose type is of interest. *) 
; REVEAL ExprBinOpTyp
    = ExprBinOpPublic BRANDED OBJECT OVERRIDES appendDump := ExprBinOpAppend END

; PROCEDURE ExprBinOpAppend ( Expr : ExprBinOpTyp )
    = BEGIN 
        Expr2OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprBinOpTyp" )
      ; Field ( "ExpBinOpActualsCt" , Fmt . Int ( Expr . ExpBinOpActualsCt ) )
      ; Field
          ( "ExpBinOpLtOpndKindsAllowed" 
          , ExprKindSetMessage ( Expr . ExpBinOpLtOpndKindsAllowed ) 
          )
      ; Field
          ( "ExpBinOpRtOpndKindsAllowed" 
          , ExprKindSetMessage ( Expr . ExpBinOpRtOpndKindsAllowed ) 
          )
      END ExprBinOpAppend

(* Three or 4 operands: *) 
; REVEAL ExprQuadOpTyp
    = ExprQuadOpPublic BRANDED OBJECT OVERRIDES appendDump := ExprQuadOpAppend END

; PROCEDURE ExprQuadOpAppend ( Expr : ExprQuadOpTyp )
    = BEGIN 
        ExprBinOpAppend ( Expr ) 
      ; SubtypeComment ( "ExprQuadOpTyp" )
      ; NestedField ( "ExpQuadOpOpnd3" , Expr . ExpQuadOpOpnd3 ) 
      ; NestedField ( "ExpQuadOpOpnd4" , Expr . ExpQuadOpOpnd4 ) 
      END ExprQuadOpAppend

; REVEAL ExprArgsObj
    = ExprArgsPublic BRANDED OBJECT OVERRIDES appendDump := ExprArgsAppend END

; PROCEDURE ExprArgsAppend ( Expr : ExprArgsObj )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprArgsObj" )
      ; NestedField ( "ExpArgPrefix" , Expr . ExpArgPrefix ) 
      ; Field ( "ExpArgsList" , "" )
      ; AppendExprList ( Expr . ExpArgsList ) 
      ; Field ( "ExpArgNo" , Fmt . Int ( Expr . ExpArgNo ) )  
      END ExprArgsAppend

; REVEAL ExprReservedIdRefTyp
    = ExprReservedIdRefPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprReservedIdRefAppend END

; PROCEDURE ExprReservedIdRefAppend ( Expr : ExprReservedIdRefTyp )
    = BEGIN
        ExprAppend ( Expr )  
      ; SubtypeComment ( "ExprReservedIdRefTyp" )
      END ExprReservedIdRefAppend

(* Constants: *)

(* Builtin types: *)

; REVEAL ExprIntTypeTyp
    = ExprIntTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprIntTypeAppend END

; PROCEDURE ExprIntTypeAppend ( Expr : ExprIntTypeTyp )
    = BEGIN
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprIntTypeTyp" )
      END ExprIntTypeAppend 

; REVEAL ExprFloatTypeTyp
    = ExprFloatTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprFloatTypeAppend END

; PROCEDURE ExprFloatTypeAppend ( Expr : ExprFloatTypeTyp )
    = BEGIN
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprFloatTypeTyp" )
      END ExprFloatTypeAppend 

(* Type constructors: *)

; REVEAL ExprAddrTypeTyp
    = ExprAddrTypePublic BRANDED OBJECT
      OVERRIDES appendDump := ExprAddrTypeAppend END

; PROCEDURE ExprAddrTypeAppend ( Expr : ExprAddrTypeTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprAddrTypeTyp" )
      ; NestedField ( "ExpAddrReferent" , Expr . ExpAddrReferent ) 
      END ExprAddrTypeAppend (* REF type. *) 

; REVEAL ExprREFTypeTyp
    = ExprREFTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprREFTypeAppend END

; PROCEDURE ExprREFTypeAppend ( Expr : ExprREFTypeTyp )
    = BEGIN 
        Expr2OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprREFTypeTyp" )
      END ExprREFTypeAppend (* REF type. *) 

; REVEAL ExprOpenArrayTypeTyp
    = ExprOpenArrayTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprOpenArrayTypeAppend END

; PROCEDURE ExprOpenArrayTypeAppend ( Expr : ExprOpenArrayTypeTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprOpenArrayTypeTyp" )
      ; NestedField ( "ExpOpenArrayElemType" , Expr . ExpOpenArrayElemType ) 
      END ExprOpenArrayTypeAppend (* REF type. *) 

; REVEAL ExprSubrTypeTyp
    = ExprSubrTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprSubrTypeAppend END

; PROCEDURE ExprSubrTypeAppend ( Expr : ExprSubrTypeTyp )
    = BEGIN 
        Expr3OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprSubrTypeTyp" )
      ; NestedField ( "ExpRangeBase" , Expr . ExpRangeBase ) 
      ; NestedField ( "ExpSubrLo" , Expr . ExpSubrLo ) 
      ; NestedField ( "ExpSubrHi" , Expr . ExpSubrHi ) 
      END ExprSubrTypeAppend (* Subrange *) 

; REVEAL ExprArrayTypeTyp
    = ExprArrayTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprArrayTypeAppend END

; PROCEDURE ExprArrayTypeAppend ( Expr : ExprArrayTypeTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprArrayTypeTyp" )
      ; NestedField ( "ExpDefElmtType" , Expr . ExpDefElmtType ) 
      ; NestedField ( "ExpDefSsType" , Expr . ExpDefSsType ) 
      END ExprArrayTypeAppend

; REVEAL Expr1ScopeTyp
    = Expr1ScopePublic BRANDED OBJECT
        OVERRIDES appendDump := Expr1ScopeAppend END
; PROCEDURE Expr1ScopeAppend ( Expr : Expr1ScopeTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "Expr1ScopeTyp" )
      ; Field ( "ExpScopeRef1"
              , FM3Scopes . ScopeRefImage ( Expr . ExpScopeRef1 )
              ) 
      END Expr1ScopeAppend

; REVEAL ExprRecTypeTyp
    = ExprRecTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprRecTypeAppend END

; PROCEDURE ExprRecTypeAppend ( Expr : ExprRecTypeTyp )
    = BEGIN
        Expr1ScopeAppend ( Expr ) 
      ; SubtypeComment ( "ExprRecTypeTyp" )
      END ExprRecTypeAppend 

; REVEAL ExprEnumTypeTyp
    = ExprEnumTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprEnumTypeAppend END

; PROCEDURE ExprEnumTypeAppend ( Expr : ExprEnumTypeTyp )
    = BEGIN
        Expr1ScopeAppend ( Expr ) 
      ; SubtypeComment ( "ExprEnumTypeTyp" )
      END ExprEnumTypeAppend 

; REVEAL ExprObjTypeTyp
    = ExprObjTypePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprObjTypeAppend END

; PROCEDURE ExprObjTypeAppend ( Expr : ExprObjTypeTyp )
    = BEGIN
        Expr2OpndAppend ( Expr ) 
      ; SubtypeComment ( "ExprObjTypeTyp" )
      ; Field ( "ExpObjOverrides"
              , FM3Utils . RefanyImage ( Expr . ExpObjOverrides )
              )  
      ; Field ( "ExpObjDecls"
              , FM3Scopes . ScopeRefImage ( Expr . ExpObjScopeRef )
              )  
      ; Field ( "ExpObjDecls"
              , FM3Parser . BrandKindImage ( Expr . ExpObjBrandKind )
              )  
      END ExprObjTypeAppend 

(* Constant values: *)
; REVEAL ExprConstValueTyp
    = ExprConstValuePublic BRANDED OBJECT
        OVERRIDES appendDump := ExprConstValueAppend END

; PROCEDURE ExprConstValueAppend ( Expr : ExprConstValueTyp )
    = BEGIN
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprConstValueTyp" )
      END ExprConstValueAppend

(* References in source code: *) 
; REVEAL ExprDeclIdTyp
    = ExprDeclIdPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprDeclIdAppend END

; PROCEDURE ExprDeclIdAppend ( Expr : ExprDeclIdTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprDeclIdTyp" )
      ; Field ( "ExpDefDeclNo" , Fmt . Int ( Expr . ExpDefDeclNo ) )  
      END ExprDeclIdAppend (* Reference to something declared in this unit. *)

; REVEAL ExprExpImpDeclIdTyp
    = ExprExpImpDeclIdPublic BRANDED OBJECT
        OVERRIDES appendDump := ExprExpImpDeclIdAppend END

; PROCEDURE ExprExpImpDeclIdAppend ( Expr : ExprExpImpDeclIdTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; SubtypeComment ( "ExprExpImpDeclIdTyp" )
      ; Field ( "ExpDefIntfUnitNo" , Fmt . Int ( Expr . ExpDefIntfUnitNo ) )  
      ; Field ( "ExpDefIntfDeclNo" , Fmt . Int ( Expr . ExpDefIntfDeclNo ) )   
     END ExprExpImpDeclIdAppend
     (* ^Reference to something declared in another unit. *) 

(* Types in the compiled code, not in the compiler. *)

(* EXPORTED.*) 
; PROCEDURE ResolveNow
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp
  = BEGIN
      IF Expr = NIL THEN RETURN Est . EsUnknown END (*IF*)
    ; CASE Expr . ExpState OF 
      | Est . EsUnresolved  
      => RETURN Expr . resolve ( ExprKind )
      | Est . EsResolving
      => <* ASSERT FALSE , "Illegal recursive declaration." *>
      ELSE RETURN Expr . ExpState
      END (*CASE*) 
    END ResolveNow

(* EXPORTED.*) 
; PROCEDURE ResolveEventually
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp 

  = BEGIN
      IF Expr = NIL THEN RETURN Est . EsUnknown END (*IF*)
    ; IF Expr . ExpState = Est . EsUnresolved  
      THEN RETURN Expr . resolve ( ExprKind )
      ELSE RETURN Expr . ExpState
      END (*IF*) 
    END ResolveEventually 

; PROCEDURE ResolveREFType ( Def : ExprTyp ) : BOOLEAN (* Is resolved. *)
  (* PRE : Def # NIL. *) 

  = BEGIN 
    END ResolveREFType


(*

         FM3Defs . ExpStackTopObj 
         := NEW ( FM3Defs . ExpREFTypeTyp


                , DefLink := FM3Defs . ExpStackTopObj 
                , DefExpKind := FM3Defs . ExpStackTopObj . ExpExpKind 
                )
*)


(* 
           WITH WReferent = FM3Exprs . GetAnPopExprStackTopRef
                , WREFType = FM3Exprs . ExprStackTopRef 
           DO IF NOT WReferent . TgIsUsable
             THEN
               WREFType . TcReferent := NIL
             ELSIF NOT FM3Exprs . IsType ( WReferent )
             THEN
               FM3Messages . Error
                 ( "\"Referent\" of a REF type must be a type. (2.2.7). "
                 , WReferent . TcPosition
                 )
             ; WREFType . TcReferent := NIL
             ELSE
               WREFType . TcReferent := WReferent
             ; WReferent . TgIsUsable := TRUE
             ; WREFType . TcIsLegalRecursive L= TRUE 
             END (*IF*)
           END (*WITH*) 
*)

(*EXPORTED*) 
; PROCEDURE NewExprMap ( InitExprCt : FM3Globals . ExprNoTyp ) : ExprMapTyp
  (* One of these per Unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitExprCt - 1 } )
    END NewExprMap

(*EXPORTED*) 
; PROCEDURE PushExprStack ( NewExpr : ExprTyp )

  = BEGIN
      (* Let's crash on trying to push a NIL. *)
      IF NewExpr . ExpStackHt # 0 (* It's already somewhere on the stack *)  
      THEN <* ASSERT FALSE *>
      END (*IF*) 
    ; IF ExprStackTopObj = NIL
      THEN NewExpr . ExpStackHt := 1
      ELSE NewExpr . ExpStackHt := ExprStackTopObj . ExpStackHt + 1
      END (*IF*)
    ; NewExpr . ExpStackLink := ExprStackTopObj
    ; ExprStackTopObj := NewExpr
    ; INC ( ExprStackCt ) 
    END PushExprStack

; VAR GLastPoppedExpr : ExprTyp 

(*EXPORTED*) 
; PROCEDURE PopExprStack ( ) : ExprTyp 

  = VAR LPoppedExprObj : ExprTyp 

  ; BEGIN
      LPoppedExprObj := ExprStackTopObj
    ; ExprStackTopObj := ExprStackTopObj . ExpStackLink
    ; DEC ( ExprStackCt )
    ; <* ASSERT ( ExprStackTopObj = NIL ) = ( ExprStackCt = 0 ) *> 
      LPoppedExprObj . ExpStackHt := 0
    ; GLastPoppedExpr := LPoppedExprObj
    ; RETURN LPoppedExprObj 
    END PopExprStack

(*EXPORTED.*)
; PROCEDURE PruneExprStack ( ToDepth : INTEGER := 0 )

  = BEGIN (*PruneExprStack*)
      IF ExprStackCt > ToDepth 
      THEN 
        FM3Messages . FM3LogArrUnit
          ( ARRAY OF REFANY
              { "Expression number "
              , Fmt . Int ( ExprStackTopObj . ExpSelfExprNo )
              , " remains on expression stack at depth "
              , Fmt . Int ( ExprStackCt )
              , " when expected down to depth "
              , Fmt . Int ( ToDepth )
              , "." 
              } 
          , ExprStackTopObj . ExpPosition
          ) 
      ; REPEAT EVAL PopExprStack ( ) 
      ; UNTIL ExprStackCt <= ToDepth  
      END (*IF*) 
   END PruneExprStack

(*EXPORTED*) 
; PROCEDURE IsNumericType ( Expr : ExprTyp ) : BOOLEAN 

  = BEGIN
      TYPECASE Expr OF
      | NULL => RETURN FALSE
      | ExprSubrTypeTyp ( TSubr )
      => RETURN IsNumericType ( TSubr . ExpRangeBase )
      | ExprIntTypeTyp
      , ExprAddrTypeTyp (* Can happen in UNSAFE units. *) 
      , ExprFloatTypeTyp
      => RETURN TRUE
      ELSE RETURN FALSE
      END (* TYPECASE*)
    END IsNumericType 

; BEGIN
    GIndentStrings [ 0 ] := "" 
  ; GIndentStrings [ 1 ] := ""
  ; ExprStackTopObj := NIL
  ; ExprStackCt := 0
  END FM3Exprs
.

