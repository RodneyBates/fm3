
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Exprs

(* An expression, as meant here, can be a part of what a declaration defines
   and can sometimes contain named references to other declarations.  Within
   a single scope, these can refer to each other left-to-right, right-to-left,
   and cyclically.  This can require an arbitrary mix of jumping leftward
   and rigthward.  So we build in-memory, linked data structure for them.
*)

; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Base
; FROM   FM3Base IMPORT tPosition 
; IMPORT FM3Globals
; IMPORT FM3LoTypes 
; IMPORT FM3Scopes
; IMPORT FM3SrcToks

; TYPE OpcodeTyp = FM3Base .OpcodeTyp 

(* Types in the compiled code, not in the compiler. *)

; TYPE RelKindTyp = { RkEqual , RkSubtype }

(* In-memory data structure for types and constant expressions.
   These may be anonymous or named by a local or [ex|im]port identifier. 
*)

; TYPE ExprKindTyp (* What kind of definition are we expanding? *) 
   = { EkNull 
     , EkType 
     , EkProc 
     , EkFunc 
     , EkValue 
     , EkConst (* A Subcategory of EkValue *)     
     , EkRef 
     }
; TYPE Ekt = ExprKindTyp

; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

; TYPE ExprKindSetTyp = SET OF ExprKindTyp

; TYPE EkSetTyp = ExprKindSetTyp

; CONST EkSetValue = EkSetTyp { Ekt . EkValue } 

; CONST EkSetType = EkSetTyp { Ekt . EkType } 

; CONST EkSetConst = EkSetTyp { Ekt . EkConst } 

; CONST EkSetTypeOrValue = EkSetTyp { Ekt . EkValue , Ekt . EkValue } 

; PROCEDURE KindSetCard ( KindSet : ExprKindSetTyp ) : INTEGER 

; PROCEDURE ExprKindSetMessage ( KindSet : ExprKindSetTyp ) : TEXT 

; TYPE ExprStateTyp
    = { EsUnknown 
      , EsUnresolved
      , EsResolving
      , EsResolved
      }

; PROCEDURE ExprStateImage ( State : ExprStateTyp ) : TEXT 

; TYPE Est = ExprStateTyp

; TYPE ExprListRefTyp = REF ARRAY OF ExprTyp

; PROCEDURE ExprRefImage ( ExprRef : REFANY ) : TEXT 
  (* ExprNo, REF, and Position. *) 

; PROCEDURE DumpExpr
    ( Expr : ExprTyp ; WrT : Wr . T ; VAR (*IN OUT*) ExprNosDumped : IntSets . T )
;
 PROCEDURE ExprImage ( Expr : ExprTyp ) : TEXT 
 (* Contents of the object. *) 
; PROCEDURE ResolveNow
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp

; PROCEDURE ResolveEventually
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp

; CONST ExprBrand = "ExprTyp0.1"
; REVEAL FM3Globals . ExprTyp = ExprPublic BRANDED ExprBrand OBJECT END  

; TYPE ExprTyp <: ExprPublic
; TYPE ExprPublic
    = OBJECT
        ExpStackLink : ExprTyp := NIL
        (* Deeper on stack is parent expression.*)
        (* NIL in root expression of a tree. *) 
      ; ExpType : ExprTyp := NIL
      ; ExpRefConstVal : REFANY := NIL
      ; ExpScalarConstVal : LONGINT := 0L
      ; ExpLoTypeInfoRef : FM3LoTypes . LoTypeInfoRefTyp := NIL
      ; ExpReachedDeclNoSet : IntSets . T
        (* ^DeclNos of ids declared in the same open scope and reached on
            paths that do not allow recursive declaration .
        *) 
      ; ExpSelfExprNo : INTEGER (* < 0 for builtin ops. *) 
      ; ExpPosition : tPosition := FM3Base . PositionNull
      ; ExpOpcode : OpcodeTyp := FM3SrcToks . RidNull
      ; ExpDownKind := Ekt . EkNull (* Inherited. *) 
      ; ExpUpKind := Ekt . EkNull (* Synthesized. *) 
      ; ExpKind : ExprKindTyp := Ekt . EkNull
      ; ExpState : ExprStateTyp := Est . EsUnresolved
      ; ExpIsConst : BOOLEAN := FALSE
      ; ExpConstValIsKnown : BOOLEAN := FALSE
      ; ExpIsUsable : BOOLEAN := TRUE
      ; ExpIsLegalRecursive : BOOLEAN := FALSE
        (* ^Self or any containing def could make it legal. *) 
      ; ExpIsDesignator : BOOLEAN := FALSE
      ; ExpIsWritable : BOOLEAN := FALSE
      ; ExpIsPresent : BOOLEAN := TRUE  
      ; ExpRefTypeIsUntraced : BOOLEAN := FALSE   
      METHODS
        appendDump ( )  
      ; resolve ( ExprKind : ExprKindTyp ) : ExprStateTyp (* final. *) 
      END

(* Add some operand links. *)
; TYPE Expr1OpndTyp <: Expr1OpndPublic
; TYPE Expr1OpndPublic
    =  ExprTyp OBJECT
         ExpOpnd1 : ExprTyp (* Left operand when binary. *) 
      END 

; TYPE Expr2OpndTyp <: Expr2OpndPublic 
; TYPE Expr2OpndPublic
    =  Expr1OpndTyp OBJECT
         ExpOpnd2 : ExprTyp (* Right Operand when binary. *) 
     END 

; TYPE Expr3OpndTyp <: Expr3OpndPublic 
; TYPE Expr3OpndPublic
    =  Expr2OpndTyp OBJECT
         ExpOpnd3 : ExprTyp 
     END 

; TYPE ExprMultiOpndTyp <: ExprMultiOpndPublic 
; TYPE ExprMultiOpndPublic
    =  ExprTyp OBJECT
         ExpOpnds : ExprListRefTyp  
     END 

(* Identifier references: *) 
; TYPE ExprIdentRefTyp <: ExprIdentRefPublic 
; TYPE ExprIdentRefPublic (* Not builtin. *) 
    = ExprTyp OBJECT
        ExpIdentDeclNo : FM3Globals . DeclNoTyp
        (* ABS ( ExpIdentDeclNo < 0 ) is builtin opcode. *)
      END 

; TYPE ExprRemoteRefTyp <: ExprRemoteRefPublic 
; TYPE ExprRemoteRefPublic
    = ExprTyp OBJECT
        ExpRemoteUnitNo : FM3Globals . UnitNoTyp 
      ; ExpRemoteDeclNo : FM3Globals . DeclNoTyp 
      END 

; TYPE ExprQualIdDeclNoAtomTyp <: ExprQualIdDeclNoAtomPublic 
; TYPE ExprQualIdDeclNoAtomPublic
    = ExprTyp OBJECT
        ExpQualDeclNoLt : FM3Globals . DeclNoTyp 
      ; ExpQualIdAtomRt : FM3Base . AtomTyp 
      END

; TYPE ExprDotTyp <: ExprDotPublic 
; TYPE ExprDotPublic
    = Expr1OpndTyp OBJECT
        ExpDotIdAtom : FM3Base . AtomTyp
      END

(* Either a constant expression or one whose type is of interest. *) 
; TYPE ExprBinOpTyp <: ExprBinOpPublic 
; TYPE ExprBinOpPublic
    = Expr2OpndTyp OBJECT
        ExpBinOpActualsCt : INTEGER 
      ; ExpBinOpLtOpndKindsAllowed := ExprKindSetTyp { } 
      ; ExpBinOpRtOpndKindsAllowed := ExprKindSetTyp { }
        (* This can denote a unary operator, in which case we use this
           type with 2nd operand fields just going unused.
        *) 
      END

; TYPE ExprQuadOpTyp <: ExprQuadOpPublic 
; TYPE ExprQuadOpPublic
    = ExprBinOpTyp OBJECT
        ExpQuadOpOpnd3 : ExprTyp 
      ; ExpQuadOpOpnd4 : ExprTyp 
        (* This can denote a ternary operator, in which case we use this
           type with ExpQuadOpOpnd4 field just going unused.
        *) 
      END

(* ExprArgsObj is used both for a call (with actuals) and a subscripted array.
   "Arg" is used in the names to refer to either.
*)
; TYPE ExprArgsObj <: ExprArgsPublic 
; TYPE ExprArgsPublic
    = ExprTyp OBJECT
        ExpArgPrefix : ExprTyp 
      ; ExpArgsList : ExprListRefTyp 
      ; ExpArgNo : INTEGER (* # of actuals still to be linked in. *) 
      END

; TYPE ExprReservedIdRefTyp <: ExprReservedIdRefPublic 
; TYPE ExprReservedIdRefPublic
    = ExprTyp OBJECT
        (* Use the ExpOpcode field. *)  
      END 

(* Constants: *)

(* Builtin types: *)

; TYPE ExprIntTypeTyp <: ExprIntTypePublic 
; TYPE ExprIntTypePublic = ExprTyp OBJECT END

; TYPE ExprFloatTypeTyp <: ExprFloatTypePublic 
; TYPE ExprFloatTypePublic = ExprTyp OBJECT END

(* Type constructors: *)

; TYPE ExprAddrTypeTyp <: ExprAddrTypePublic 
; TYPE ExprAddrTypePublic (* Simple address type. *) 
    = Expr1OpndTyp OBJECT
        ExpAddrReferent : ExprTyp := NIL (* Redundant to Opnd1? *) 
      END

; TYPE ExprREFTypeTyp <: ExprREFTypePublic 
; TYPE ExprREFTypePublic (* REF type. *) 
    = Expr2OpndTyp OBJECT
        (* ExpOpnd1 is brand expression. *) 
        (* ExpOpnd2 is referent type or supertype. *) 
        ExpREFIsUntraced : BOOLEAN 
      END

; TYPE ExprOpenArrayTypeTyp <: ExprOpenArrayTypePublic 
; TYPE ExprOpenArrayTypePublic (* REF type. *) 
    = Expr1OpndTyp OBJECT
        ExpOpenArrayElemType : ExprTyp := NIL (* Redundant to Opnd1? *)
      END

; TYPE ExprSubrTypeTyp <: ExprSubrTypePublic 
; TYPE ExprSubrTypePublic (* Subrange *) 
    = Expr3OpndTyp OBJECT
        ExpRangeBase : ExprTyp := NIL 
      ; ExpSubrLo : ExprTyp := NIL 
      ; ExpSubrHi : ExprTyp := NIL 
      END

; TYPE ExprArrayTypeTyp <: ExprArrayTypePublic 
; TYPE ExprArrayTypePublic
    = ExprTyp OBJECT
        ExpDefElmtType : ExprTyp 
      ; ExpDefSsType : ExprTyp (* NIL means open array. *)
      END

; TYPE Expr1ScopeTyp <: Expr1ScopePublic 
; TYPE Expr1ScopePublic
    = ExprTyp OBJECT
        ExpScopeRef1 : FM3Scopes . ScopeRefTyp 
      END 

; TYPE ExprRecTypeTyp <: ExprRecTypePublic 
; TYPE ExprRecTypePublic 
    = Expr1ScopeTyp OBJECT END 

; TYPE ExprEnumTypeTyp <: ExprEnumTypePublic 
; TYPE ExprEnumTypePublic 
    = Expr1ScopeTyp OBJECT END 

; TYPE ExprObjTypeTyp <: ExprObjTypePublic 
; TYPE ExprObjTypePublic 
    = Expr1ScopeTyp OBJECT
        ExpObjMethods : FM3Scopes . ScopeRefTyp 
      END 

(* Constant values: *)
; TYPE ExprConstValueTyp <: ExprConstValuePublic 
; TYPE ExprConstValuePublic
    = ExprTyp OBJECT END
(*CHECK: Any need for this? ExpIsConst should suffice. *) 

(* References: *) 
; TYPE ExprDeclIdTyp <: ExprDeclIdPublic 
; TYPE ExprDeclIdPublic (* Reference to something declared in this unit. *)
    = ExprTyp OBJECT
        ExpDefDeclNo : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
      END 

; TYPE ExprExpImpDeclIdTyp <: ExprExpImpDeclIdPublic 
; TYPE ExprExpImpDeclIdPublic (* Reference to something declared in another unit. *) 
    = ExprTyp OBJECT
        ExpDefIntfUnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull
      ; ExpDefIntfDeclNo : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
      END

(* Expression operations: *)
(* Either a constant expression or one whose type is of interest. *) 

; TYPE ExprMapTyp = FM3Base . MapTyp
    (* Map ExprNoTyp to ExprTyp. One of these per Unit. *)

; PROCEDURE NewExprMap ( InitExprCt : FM3Globals . ExprNoTyp ) : ExprMapTyp 

(* Stack of definitions refs. *)

(* Let's just make this one a linked stack. *) 
; VAR ExprStackTopObj : ExprTyp := NIL
; VAR ExprTopDeclNo : INTEGER := 0
(* INVARIANT: (ExprStackTopObj = NIL) = (ExprTopDeclNo = 0). *)

; PROCEDURE PushExprStack ( NewExpr : ExprTyp )

; PROCEDURE PopExprStack ( ) : ExprTyp 

; PROCEDURE IsNumericType ( Expr : ExprTyp ) : BOOLEAN 

; END FM3Exprs
.

