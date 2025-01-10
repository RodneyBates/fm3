
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
     , EkValue
     , EkRef 
     , EkFunc     
     , EkConst    
     , EkProc     
     }
; TYPE Ekt = ExprKindTyp

; TYPE EkSetTyp = SET OF ExprKindTyp 

; TYPE ExprStateTyp
    = { EsUnknown 
      , EsUnresolved
      , EsResolving
      , EsResolved
      }

; TYPE Est = ExprStateTyp

; TYPE ExprListRefTyp = REF ARRAY OF ExprTyp

; TYPE ExprTyp
    = OBJECT
        ExpStackLink : ExprTyp := NIL
        (* Deeper on stack is parent expression.*)
        (* NIL in root expression of a tree. *) 
      ; ExpType : ExprTyp := NIL
      ; ExpRefConstVal : REFANY := NIL
      ; ExpScalarConstVal : LONGINT 
      ; ExpLoTypeInfoRef : FM3LoTypes . LoTypeInfoRefTyp := NIL
      ; ExpReachedDeclNoSet : IntSets . T
        (* ^DeclNos of ids declared in the same open scope and reached on
            paths that do not allow recursive declaration .
        *) 
      ; ExpSelfExprNo : FM3Globals . ExprNoTyp
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
      METHODS
        resolve ( ExprKind : ExprKindTyp ) : ExprStateTyp (* final. *) 
      END

; PROCEDURE ResolveNow
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp

; PROCEDURE ResolveEventually
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp

(* Add some operand links. *)
; TYPE Expr1OpndPublic
    = ExprTyp OBJECT
         ExpOpnd1 : ExprTyp (* Left operand when binary. *) 
      END 
; TYPE Expr1OpndTyp <: Expr1OpndPublic

; TYPE Expr2OpndTyp
    <: Expr1OpndTyp OBJECT
         ExpOpnd2 : ExprTyp (* Right Operand when binary. *) 
     END 

; TYPE Expr3OpndTyp
    <: Expr2OpndTyp OBJECT
         ExpOpnd3 : ExprTyp 
     END 

; TYPE ExprMultiOpndTyp
    <: ExprTyp OBJECT
         ExpOpnds : ExprListRefTyp  
     END 

(* Identifier references: *) 
; TYPE ExprIdentReference (* Not builtin. *) 
    = ExprTyp OBJECT
        ExpIdentDeclNo : FM3Globals . DeclNoTyp
        (* ABS ( ExpIdentDeclNo < 0 ) is builtin opcode. *)
      END 

; TYPE ExprRemoteRef
    = ExprTyp OBJECT
        ExpRemoteUnitNo : FM3Globals . UnitNoTyp 
      ; ExpRemoteDeclNo : FM3Globals . DeclNoTyp 
      END 

; TYPE ExprQualIdDeclNoAtomTyp
    = ExprTyp OBJECT
        ExpQualDeclNoLt : FM3Globals . DeclNoTyp 
      ; ExpQualIdAtomRt : FM3Base . AtomTyp 
      END

; TYPE ExprDot
    = Expr1OpndTyp OBJECT
        ExpDotIdAtom : FM3Base . AtomTyp
      END

(* Either a constant expression or one whose type is of interest. *) 
; TYPE ExprBinOpTyp
    = Expr2OpndTyp OBJECT
        ExpBinOpOp : INTEGER 
        (* Certain values from FM3SrcToks.i3 or FM3LongToks.i3.
           can be a unary operator, in which case we use this type
           with the 2nd operand field just going unused.
        *) 
      END

; TYPE ExprCallTyp
    = ExprTyp OBJECT
        ExpCallProc : ExprTyp 
      ; ExpActualsRef : REF ARRAY OF ExprTyp
      END 

(* Constants: *)

(* Builtin types: *)

; TYPE ExprIntTypeTyp = ExprTyp OBJECT END

; TYPE ExprFloatTypeTyp = ExprTyp OBJECT END

(* Type constructors: *)

; TYPE ExprAddrTypeTyp (* REF type. *) 
    = Expr1OpndTyp OBJECT
        ExpAddrReferent : ExprTyp := NIL (* Redundant to Opnd1? *) 
      END

; TYPE ExprREFTypeTyp (* REF type. *) 
    = Expr1OpndTyp OBJECT
        ExpREFReferent : ExprTyp := NIL (* Redundant to Opnd1? *) 
      END

; TYPE ExprOpenArrayTypeTyp (* REF type. *) 
    = Expr1OpndTyp OBJECT
        ExpOpenArrayElemType : ExprTyp := NIL (* Redundant to Opnd1? *)
      END

; TYPE ExprSubrTypeTyp (* Subrange *) 
    = Expr3OpndTyp OBJECT
        ExpRangeBase : ExprTyp := NIL 
      ; ExpSubrLo : ExprTyp := NIL 
      ; ExpSubrHi : ExprTyp := NIL 
      END

; TYPE ExprArrayTypeTyp
    = ExprTyp OBJECT
        DefElmtType : ExprTyp 
      ; DefSsType : ExprTyp (* NIL means open array. *)
      END

; TYPE Expr1ScopeTyp
    = ExprTyp OBJECT
        ExpScopeRef1 : FM3Scopes . ScopeRefTyp 
      END 

; TYPE ExprRecTypeTyp 
    = Expr1ScopeTyp OBJECT END 

; TYPE ExprEnumTypeTyp 
    = Expr1ScopeTyp OBJECT END 

; TYPE ExprObjTypeTyp 
    = Expr1ScopeTyp OBJECT
        ExpObjMethods : FM3Scopes . ScopeRefTyp 
      END 

(* Constant values: *)
; TYPE ExprConstValue
    = ExprTyp OBJECT END
(*CHECK: Any need for this? ExpIsConst should suffice. *) 

(* References: *) 
; TYPE ExprDeclIdTyp (* Reference to something declared in this unit. *)
    = ExprTyp OBJECT
        ExpDefDeclNo : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
      END 

; TYPE ExprExpImpDeclIdTyp (* Reference to something declared in another unit. *) 
    = ExprTyp OBJECT
        DefIntfUnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull
      ; DefIntfDeclNo : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
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

