
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2026  Rodney M. Bates.                                    *)
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
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Units 
; IMPORT FM3Utils

(*EXPORTED.*)
; PROCEDURE ExprKindImage ( Kind : ExprKindTyp ) : TEXT 

  = BEGIN (*ExprKindImage*)
      CASE Kind OF
      | ExprKindTyp . EkNull => RETURN "EkNull" 
      | ExprKindTyp . EkLiteral => RETURN "EkLiteral"
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
      | ExprKindTyp . EkUnOp => RETURN "EkUnOp" 
      | ExprKindTyp . EkDot => RETURN "EkDot" 
      | ExprKindTyp . EkBinOp => RETURN "EkBinOp" 
      | ExprKindTyp . EkCall => RETURN "EkCall" 
      | ExprKindTyp . EkSubscript => RETURN "EkSubscript" 
      | ExprKindTyp . EkBuiltin => RETURN "EkBuiltin"  
      | ExprKindTyp . EkProc => RETURN "EkProc"  
      | ExprKindTyp . EkFunc => RETURN "EkFunc"  
      | ExprKindTyp . EkValue => RETURN "EkValue" 
      | ExprKindTyp . EkBrand => RETURN "EkBrand"  
      ELSE RETURN "<Unknown ExprKindImage>"
      END (*CASE*) 
    END ExprKindImage

(* EXPORTED.*) 
; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

  = BEGIN
      CASE Kind OF 
      | ExprKindTyp . EkNull    => RETURN "<null>" 
      | ExprKindTyp . EkType    => RETURN "type" 
      | ExprKindTyp . EkProc    => RETURN "procedure" 
      | ExprKindTyp . EkFunc    => RETURN "function" 
      | ExprKindTyp . EkValue   => RETURN "value" 
      | ExprKindTyp . EkRefType => RETURN "reference" 
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

(*EXPORTED.*)
; PROCEDURE NewExprListRef ( Ct : INTEGER ) : ExprListRefTyp
  (* With all elements initialized to NIL. *) 

  = VAR LResult : ExprListRefTyp 

  ; BEGIN (*NewExprListRef*)
      LResult := NEW ( ExprListRefTyp , Ct )
    ; FOR RI := FIRST ( LResult ^ ) TO LAST ( LResult ^ )
      DO LResult ^ [ RI ] := NIL 
      END (*FOR*)
    ; RETURN LResult 
    END NewExprListRef

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
    = " .. 2.. 4.. 6.. 8..10..12..14..16..18..20..22..24..26..28..30..32..34..36..38  "
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

RETURN ; 
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
      THEN
        IF LChars = NIL
        THEN LResult := "<NIL>" 
        ELSE LResult := Text . FromChars ( LChars ^ )
        END (*IF*) 
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
      ; Field ( "ExpRefConstVal" , FM3Utils . RefanyImage ( Expr . ExpRefConstVal ) )  
      ; Field ( "ExpScalarConstVal" , Fmt . LongInt ( Expr . ExpScalarConstVal ) )  
      ; Field ( "ExpHash" , FM3Utils . LongHexImage ( Expr . ExpHash ) )
      ; Field ( "ExpLoTypeInfoRef"
              , "" (* LoTypeInfoRefTypImage ( Expr . ExpLoTypeInfoRef ) *)
              )
      ; Field ( "ExpReachedDeclNoSet"
              , IntSets . Image ( Expr . ExpReachedDeclNoSet , IntSetsElemImage )
              )
      ; Field ( "ExpBuiltinOpLtOpndKindsAllowed" 
              , ExprKindSetMessage ( Expr . ExpBuiltinOpLtOpndKindsAllowed ) 
              )
      ; Field ( "ExpBuiltinOpRtOpndKindsAllowed" 
              , ExprKindSetMessage ( Expr . ExpBuiltinOpRtOpndKindsAllowed ) 
              )
      ; Field ( "ExpArgsList" , "" )
      ; AppendExprList ( Expr . ExpArgsList ) 

      ; Field ( "ExpDeclsListRef"
              , FM3Utils . RefanyImage ( Expr . ExpDeclsListRef )
              )  

      ; Field ( "ExpObjBrandKind"
              , FM3Parser . BrandKindImage ( Expr . ExpObjBrandKind )
              )  
      ; Field ( "ExpScopeRef1"
              , FM3Scopes . ScopeRefImage ( Expr . ExpScopeRef1 )
              ) 


      ; Field ( "ExpIdentDeclNo" , Fmt . Int ( Expr . ExpIdentDeclNo ) ) 
      ; Field ( "ExpRemoteUnitNo" , Fmt . Int ( Expr . ExpRemoteUnitNo ) ) 
      ; Field ( "ExpRemoteDeclNo" , Fmt . Int ( Expr . ExpRemoteDeclNo ) ) 

      ; Field ( "ExpPosition" , FM3Utils . PositionImage ( Expr . ExpPosition ) )
      ; Field ( "ExpOpcode" , FM3SrcToks . Image ( Expr . ExpOpcode ) )  
      ; Field ( "ExpIdAtom" , AtomTypImage ( Expr . ExpIdAtom ) ) 
      ; Field ( "ExpDeclListNo" , Fmt . Int ( Expr . ExpDeclListNo ) )  
      ; Field ( "ExpArgListNo" , Fmt . Int ( Expr . ExpArgListNo ) )  
      ; Field ( "ExpBuiltinOpActualsCt" , Fmt . Int ( Expr . ExpBuiltinOpActualsCt ) )
      ; Field ( "ExpStackHt" , Fmt . Int ( Expr . ExpStackHt ) ) 


      ; Field ( "ExpSelfExprNo" , Fmt . Int ( Expr . ExpSelfExprNo ) ) 
      ; Field ( "ExpDownKind" , ExprKindImage ( Expr . ExpDownKind ) ) 
      ; Field ( "ExpUpKind" , ExprKindImage ( Expr . ExpUpKind ) ) 
      ; Field ( "ExpKind" , ExprKindImage ( Expr . ExpKind ) )  
      ; Field ( "ExpState" , ExprStateImage ( Expr . ExpState ) )  
      ; Field ( "ExpIsConst" , Fmt . Bool ( Expr . ExpIsConst ) )  
      ; Field ( "ExpConstValIsKnown" , Fmt . Bool ( Expr . ExpConstValIsKnown ) )
      ; Field ( "ExpIsUsable" , Fmt . Bool ( Expr . ExpIsUsable ) ) 
      ; Field ( "ExpIsLegalRecursive" , Fmt . Bool ( Expr . ExpIsLegalRecursive ) )
      ; Field ( "ExpIsDesignator" , Fmt . Bool ( Expr . ExpIsDesignator ) ) 
      ; Field ( "ExpIsWritable" , Fmt . Bool ( Expr . ExpIsWritable ) )  
      ; Field ( "ExpIsPresent" , Fmt . Bool ( Expr . ExpIsPresent ) )  
      ; Field ( "ExpIsUntraced" , Fmt . Bool ( Expr . ExpIsUntraced ) )
      ; Field ( "ExpArrayTypeIsOpen" , Fmt . Bool ( Expr . ExpArrayTypeIsOpen ) )

      ; NestedField ( "ExpType" , Expr . ExpType ) 
      ; NestedField ( "ExpOpnd1" , Expr . ExpOpnd1 ) 
      ; NestedField ( "ExpOpnd2" , Expr . ExpOpnd2 ) 
      ; NestedField ( "ExpOpnd3" , Expr . ExpOpnd3 ) 
      ; NestedField ( "ExpOpnd3" , Expr . ExpOpnd3 ) 
      ; NestedField ( "ExpOpnd4" , Expr . ExpOpnd4 ) 
      ; NestedField ( "ExpRangeBase" , Expr . ExpRangeBase ) 
      ; NestedField ( "ExpArgPrefix" , Expr . ExpArgPrefix ) 

      END ExprAppend

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
; PROCEDURE NewExprMap ( InitExprCt : ExprNoTyp ) : ExprMapTyp
  (* One of these per Unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitExprCt - 1 } )
    END NewExprMap

(*EXPORTED.*)
; PROCEDURE ExprRefOfExprNo ( ExprNo : ExprNoTyp ) : ExprTyp
  (* In the current unit. *) 

  = VAR LExprMap : ExprMapTyp 
  ; VAR LExprRef : ExprTyp

  ; BEGIN
      LExprMap := FM3Units . UnitStackTopRef ^ . UntExprMap 
    ; IF LExprMap = NIL THEN RETURN NIL END 
    ; LExprRef := VarArray_Int_Refany . Fetch ( LExprMap , ExprNo )
      (*       ^Implied NARROW *)
    ; RETURN LExprRef  
    END ExprRefOfExprNo 

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
      IF Expr = NIL
      THEN RETURN FALSE
      ELSE 
        CASE Expr . ExpKind OF
        | Ekt . EkSubrType
        => RETURN IsNumericType ( Expr . ExpRangeBase )
        | Ekt . EkBuiltin
        => CASE Expr . ExpOpcode OF
           | Stk . RidCARDINAL
           , Stk . RidEXTENDED
           , Stk . RidINTEGER
           , Stk . RidLONGCARD
           , Stk . RidLONGINT
           , Stk . RidLONGREAL
           , Stk . RidREAL
           => RETURN TRUE
           ELSE RETURN FALSE
           END (* CASE*)
        END (* CASE*)
      END (*IF*) 
    END IsNumericType 

; BEGIN
    GIndentStrings [ 0 ] := "" 
  ; GIndentStrings [ 1 ] := ""
  ; ExprStackTopObj := NIL
  ; ExprStackCt := 0
  END FM3Exprs
.

