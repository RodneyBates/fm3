
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
; IMPORT FM3Globals
; IMPORT FM3SrcToks
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT FM3UnsafeUtils 


(* EXPORTED.*) 
; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

  = BEGIN
      CASE Kind OF 
      | Ekt . EkNull
       => RETURN "<null>" 
      | Ekt . EkType 
       => RETURN "type" 
      | Ekt . EkProc 
       => RETURN "procedure" 
      | Ekt . EkFunc 
       => RETURN "function" 
      | Ekt . EkValue 
       => RETURN "value" 
      | Ekt . EkConst 
       => RETURN "constant" 
      | Ekt . EkRef 
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
    ; LWrT := NEW ( TextWr . T )
    ; Wr . PutText ( LWrT , "one of " ) 
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
    ; FOR RI := FIRST ( ExprList ^) TO LAST ( ExprList ^ ) 
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
; TYPE IndentStringsTyp = ARRAY [ 0 .. 1 ] OF TEXT
; VAR GIndentStrings : IndentStringsTyp 
; CONST IndentBase0
    = "   2   4   6   8  10  12  14  16  18  20  22  24  26  28  30  32  34  36  38  "
; CONST IndentBase1 
    = "   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |  "

(*EXPORTED.*) 
; PROCEDURE ExprImage ( Expr : ExprTyp ) : TEXT 

  = BEGIN
      GWrT := TextWr . New ( )
    ; GDepth := 0 
    ; Expr . appendDump ( )
    ; Wr . PutText ( GWrT , Wr . EOL ) 
    ; RETURN TextWr . ToText ( GWrT )
    END ExprImage

; PROCEDURE Field ( Name : TEXT ; Value : TEXT ) 

  = BEGIN
      Wr . PutText ( GWrT , GIndentStrings [ ORD ( GDepth MOD 5 = 0 ) ] )
    ; Wr . PutText ( GWrT , Name ) 
    ; Wr . PutText ( GWrT , " = " ) 
    ; Wr . PutText ( GWrT , Value ) 
    ; Wr . PutText ( GWrT , Wr . EOL ) 
    END Field

; PROCEDURE AppendNestedExpr ( Expr : ExprTyp ) 

  = VAR LIndentStrings : IndentStringsTyp 

  ; BEGIN
      INC ( GDepth )
    ; LIndentStrings := GIndentStrings 
    ; GIndentStrings [ 0 ]  := Text . Sub ( IndentBase0 , 0 , GDepth * 2 ) 
    ; GIndentStrings [ 1 ]  := Text . Sub ( IndentBase1 , 0 , GDepth * 2 ) 
    ; Expr . appendDump ( )
    ; GIndentStrings := LIndentStrings 
    ; DEC ( GDepth ) 
    END AppendNestedExpr

; PROCEDURE NestedField ( Name : TEXT ; Expr : ExprTyp ) 

  = BEGIN
      Wr . PutText ( GWrT , GIndentStrings [ ORD ( GDepth MOD 5 = 0 ) ] )
    ; Wr . PutText ( GWrT , Name ) 
    ; Wr . PutText ( GWrT , " = " ) 
    ; Wr . PutText ( GWrT , Wr . EOL )
    ; AppendNestedExpr ( Expr ) 
    END NestedField

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

; <*INLINE*> PROCEDURE RefanyImage ( Value : REFANY ) : TEXT

  = BEGIN
      RETURN LongHexImage ( FM3UnsafeUtils . RefanyToLongInt ( Value ) ) 
    END RefanyImage 

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
  (* Why do I have to wrap FMt.Int? *) 

  = BEGIN
      RETURN Fmt . Int ( Elem ) 
    END IntSetsElemImage 

; REVEAL ExprTyp
    = ExprPublic BRANDED OBJECT OVERRIDES appendDump := ExprAppend END
; PROCEDURE ExprAppend ( Expr : ExprTyp )
    = BEGIN
        NestedField ( "ExpStackLink" , Expr . ExpStackLink ) 
      ; NestedField ( "ExpType" , Expr . ExpType ) 
      ; Field ( "ExpRefConstVal" , RefanyImage ( Expr . ExpRefConstVal ) )  
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
      ; Field ( "ExpDownKind" , ExprKindMessage ( Expr . ExpDownKind ) ) 
      ; Field ( "ExpUpKind" , ExprKindMessage ( Expr . ExpUpKind ) ) 
      ; Field ( "ExpKind" , ExprKindMessage ( Expr . ExpKind ) )  
      ; Field ( "ExpState" , ExprStateImage ( Expr . ExpState ) )  
      ; Field ( "ExpIsConst" , Fmt . Bool ( Expr . ExpIsConst ) )  
      ; Field ( "ExpConstValIsKnown" , Fmt . Bool ( Expr . ExpConstValIsKnown ) )
      ; Field ( "ExpIsUsable" , Fmt . Bool ( Expr . ExpIsUsable ) ) 
      ; Field
          ( "ExpIsLegalRecursive" , Fmt . Bool ( Expr . ExpIsLegalRecursive ) )
      ; Field ( "ExpIsDesignator" , Fmt . Bool ( Expr . ExpIsDesignator ) ) 
      ; Field ( "ExpIsWritable" , Fmt . Bool ( Expr . ExpIsWritable ) )  
      END ExprAppend

; REVEAL Expr1OpndTyp
    = Expr1OpndPublic BRANDED OBJECT OVERRIDES appendDump := Expr1OpndAppend END
; PROCEDURE Expr1OpndAppend ( Expr : Expr1OpndTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; NestedField ( "ExpOpnd1" , Expr . ExpOpnd1 ) 
      END Expr1OpndAppend
; REVEAL Expr2OpndTyp
    = Expr2OpndPublic BRANDED OBJECT OVERRIDES appendDump := Expr2OpndAppend END
; PROCEDURE Expr2OpndAppend ( Expr : Expr2OpndTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; NestedField ( "ExpOpnd2" , Expr . ExpOpnd2 ) 
      END Expr2OpndAppend

; REVEAL Expr3OpndTyp
    = Expr3OpndPublic BRANDED OBJECT OVERRIDES appendDump := Expr3OpndAppend END
; PROCEDURE Expr3OpndAppend ( Expr : Expr3OpndTyp )
    = BEGIN 
        Expr2OpndAppend ( Expr ) 
      ; NestedField ( "ExpOpnd3" , Expr . ExpOpnd3 ) 
      END Expr3OpndAppend

; REVEAL ExprMultiOpndTyp
    = ExprMultiOpndPublic BRANDED OBJECT OVERRIDES appendDump := ExprMultiOpndAppend END
; PROCEDURE ExprMultiOpndAppend ( Expr : ExprMultiOpndTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpOpnds" , "" ) 
      ; AppendExprList ( Expr . ExpOpnds ) 
      END ExprMultiOpndAppend

(* Identifier references: *) 
; REVEAL ExprIdentRefTyp
    = ExprIdentRefPublic BRANDED OBJECT OVERRIDES appendDump := ExprIdentRefAppend END
; PROCEDURE ExprIdentRefAppend ( Expr : ExprIdentRefTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpIdentDeclNo" , Fmt . Int ( Expr . ExpIdentDeclNo ) ) 
      END ExprIdentRefAppend (* Not builtin. *) 

; REVEAL ExprRemoteRefTyp
    = ExprRemoteRefPublic BRANDED OBJECT OVERRIDES appendDump := ExprRemoteRefAppend END
; PROCEDURE ExprRemoteRefAppend ( Expr : ExprRemoteRefTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpRemoteUnitNo" , Fmt . Int ( Expr . ExpRemoteUnitNo ) ) 
      ; Field ( "ExpRemoteDeclNo" , Fmt . Int ( Expr . ExpRemoteDeclNo ) ) 
      END ExprRemoteRefAppend

; REVEAL ExprQualIdDeclNoAtomTyp
    = ExprQualIdDeclNoAtomPublic BRANDED OBJECT OVERRIDES appendDump := ExprQualIdDeclNoAtomAppend END
; PROCEDURE ExprQualIdDeclNoAtomAppend ( Expr : ExprQualIdDeclNoAtomTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpQualDeclNoLt" , Fmt . Int ( Expr . ExpQualDeclNoLt ) ) 
      ; Field ( "ExpQualIdAtomRt" , AtomTypImage ( Expr . ExpQualIdAtomRt ) ) 
      END ExprQualIdDeclNoAtomAppend

; REVEAL ExprDotTyp
    = ExprDotPublic BRANDED OBJECT OVERRIDES appendDump := ExprDotAppend END
; PROCEDURE ExprDotAppend ( Expr : ExprDotTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; Field ( "ExpDotIdAtom" , AtomTypImage ( Expr . ExpDotIdAtom ) ) 
      END ExprDotAppend

(* Either a constant expression or one whose type is of interest. *) 
; REVEAL ExprBinOpTyp
    = ExprBinOpPublic BRANDED OBJECT OVERRIDES appendDump := ExprBinOpAppend END
; PROCEDURE ExprBinOpAppend ( Expr : ExprBinOpTyp )
    = BEGIN 
        Expr2OpndAppend ( Expr ) 
      ; Field ( "ExpBinOpOp" , Fmt . Int ( Expr . ExpBinOpOp ) ) 
      END ExprBinOpAppend

; REVEAL ExprCallTyp
    = ExprCallPublic BRANDED OBJECT OVERRIDES appendDump := ExprCallAppend END
; PROCEDURE ExprCallAppend ( Expr : ExprCallTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; NestedField ( "ExpCallProc" , Expr . ExpCallProc ) 
      ; Field ( "ExpActualsList" , "" )
      ; AppendExprList ( Expr . ExpActualsList ) 
      ; Field ( "ExpActualNo" , Fmt . Int ( Expr . ExpActualNo ) )  
      END ExprCallAppend

; REVEAL ExprReservedIdRefTyp
    = ExprReservedIdRefPublic BRANDED OBJECT OVERRIDES appendDump := ExprReservedIdRefAppend END
; PROCEDURE ExprReservedIdRefAppend ( Expr : ExprReservedIdRefTyp )
    = BEGIN
        ExprAppend ( Expr )  
      END ExprReservedIdRefAppend

(* Constants: *)

(* Builtin types: *)

; REVEAL ExprIntTypeTyp
    = ExprIntTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprIntTypeAppend END
; PROCEDURE ExprIntTypeAppend ( Expr : ExprIntTypeTyp )
    = BEGIN
        ExprAppend ( Expr ) 
      END ExprIntTypeAppend 

; REVEAL ExprFloatTypeTyp
    = ExprFloatTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprFloatTypeAppend END
; PROCEDURE ExprFloatTypeAppend ( Expr : ExprFloatTypeTyp )
    = BEGIN
        ExprAppend ( Expr ) 
      END ExprFloatTypeAppend 

(* Type constructors: *)

; REVEAL ExprAddrTypeTyp
    = ExprAddrTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprAddrTypeAppend END
; PROCEDURE ExprAddrTypeAppend ( Expr : ExprAddrTypeTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; NestedField ( "ExpAddrReferent" , Expr . ExpAddrReferent ) 
      END ExprAddrTypeAppend (* REF type. *) 

; REVEAL ExprREFTypeTyp
    = ExprREFTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprREFTypeAppend END
; PROCEDURE ExprREFTypeAppend ( Expr : ExprREFTypeTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; NestedField ( "ExpREFReferent" , Expr . ExpREFReferent ) 
      END ExprREFTypeAppend (* REF type. *) 

; REVEAL ExprOpenArrayTypeTyp
    = ExprOpenArrayTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprOpenArrayTypeAppend END
; PROCEDURE ExprOpenArrayTypeAppend ( Expr : ExprOpenArrayTypeTyp )
    = BEGIN 
        Expr1OpndAppend ( Expr ) 
      ; NestedField ( "ExpOpenArrayElemType" , Expr . ExpOpenArrayElemType ) 
      END ExprOpenArrayTypeAppend (* REF type. *) 

; REVEAL ExprSubrTypeTyp
    = ExprSubrTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprSubrTypeAppend END
; PROCEDURE ExprSubrTypeAppend ( Expr : ExprSubrTypeTyp )
    = BEGIN 
        Expr3OpndAppend ( Expr ) 
      ; NestedField ( "ExpRangeBase" , Expr . ExpRangeBase ) 
      ; NestedField ( "ExpSubrLo" , Expr . ExpSubrLo ) 
      ; NestedField ( "ExpSubrHi" , Expr . ExpSubrHi ) 
      END ExprSubrTypeAppend (* Subrange *) 

; REVEAL ExprArrayTypeTyp
    = ExprArrayTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprArrayTypeAppend END
; PROCEDURE ExprArrayTypeAppend ( Expr : ExprArrayTypeTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; NestedField ( "ExpDefElmtType" , Expr . ExpDefElmtType ) 
      ; NestedField ( "ExpDefSsType" , Expr . ExpDefSsType ) 
      END ExprArrayTypeAppend

; REVEAL Expr1ScopeTyp
    = Expr1ScopePublic BRANDED OBJECT OVERRIDES appendDump := Expr1ScopeAppend END
; PROCEDURE Expr1ScopeAppend ( Expr : Expr1ScopeTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpScopeRef1" , "" (* ScopeRefTypImage ( Expr . ExpScopeRef1 ) *) ) 
      END Expr1ScopeAppend

; REVEAL ExprRecTypeTyp
    = ExprRecTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprRecTypeAppend END
; PROCEDURE ExprRecTypeAppend ( Expr : ExprRecTypeTyp )
    = BEGIN
        Expr1ScopeAppend ( Expr ) 
      END ExprRecTypeAppend 

; REVEAL ExprEnumTypeTyp
    = ExprEnumTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprEnumTypeAppend END
; PROCEDURE ExprEnumTypeAppend ( Expr : ExprEnumTypeTyp )
    = BEGIN
        Expr1ScopeAppend ( Expr ) 
      END ExprEnumTypeAppend 

; REVEAL ExprObjTypeTyp
    = ExprObjTypePublic BRANDED OBJECT OVERRIDES appendDump := ExprObjTypeAppend END
; PROCEDURE ExprObjTypeAppend ( Expr : ExprObjTypeTyp )
    = BEGIN
        Expr1ScopeAppend ( Expr ) 
      ; Field ( "ExpObjMethods" , "" (* ScopeRefTypImage ( Expr . ExpObjMethods ) *) )  
      END ExprObjTypeAppend 

(* Constant values: *)
; REVEAL ExprConstValueTyp
    = ExprConstValuePublic BRANDED OBJECT OVERRIDES appendDump := ExprConstValueAppend END
; PROCEDURE ExprConstValueAppend ( Expr : ExprConstValueTyp )
    = BEGIN
        ExprAppend ( Expr ) 
      END ExprConstValueAppend

(* References: *) 
; REVEAL ExprDeclIdTyp
    = ExprDeclIdPublic BRANDED OBJECT OVERRIDES appendDump := ExprDeclIdAppend END
; PROCEDURE ExprDeclIdAppend ( Expr : ExprDeclIdTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpDefDeclNo" , Fmt . Int ( Expr . ExpDefDeclNo ) )  
      END ExprDeclIdAppend (* Reference to something declared in this unit. *)

; REVEAL ExprExpImpDeclIdTyp
    = ExprExpImpDeclIdPublic BRANDED OBJECT OVERRIDES appendDump := ExprExpImpDeclIdAppend END
; PROCEDURE ExprExpImpDeclIdAppend ( Expr : ExprExpImpDeclIdTyp )
    = BEGIN 
        ExprAppend ( Expr ) 
      ; Field ( "ExpDefIntfUnitNo" , Fmt . Int ( Expr . ExpDefIntfUnitNo ) )  
      ; Field ( "ExpDefIntfDeclNo" , Fmt . Int ( Expr . ExpDefIntfDeclNo ) )   
     END ExprExpImpDeclIdAppend (* Reference to something declared in another unit. *) 

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
                , WREFType = FM3Exprs . TopExprStackTopRef 
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
      NewExpr . ExpStackLink := ExprStackTopObj
    ; ExprStackTopObj := NewExpr 
    END PushExprStack 

(*EXPORTED*) 
; PROCEDURE PopExprStack ( ) : ExprTyp 

  = VAR LResult : ExprTyp 

  ; BEGIN
      LResult := ExprStackTopObj
    ; IF LResult # NIL
      THEN ExprStackTopObj := ExprStackTopObj . ExpStackLink
      END (*IF*) 
    ; RETURN LResult 
    END PopExprStack

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
  END FM3Exprs
.

