
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

   Remember that in Modula3, programmer-written constructs that define types
   are expressions along with value-computing expressions, and each can contain
   one of the other.  So they are all part of what's called "expressions".

   Some expressions contain things that are not strictly expressions, but are
   needed as components, e.g. field lists. also data structure for them is
   defined here.
*)

; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Base
; FROM   FM3Base IMPORT tPosition
; IMPORT FM3Globals
; IMPORT FM3LoTypes
; IMPORT FM3Parser 
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
     , EkReservedxxx
(* TODO: ^Split this into several. *) 
     , EkLiteral
(*
     , EkLongintLit
     , EkRealLit
     , EkLongLit
     , EkExtendedLit 
     , EkCharLit
     , EkWideCharLit
     , EkTextLit
     , EkWideTextLit
*)     
     , EkIdentRef
     , EkQualIdentRef
     , EkReservedIdent 
     , EkRemoteRef 
     , EkEnumType
     , EkRecType
     , EkArrayType
     , EkObjType
     , EkSubrType (* Subrange *) 
     , EkRefType
     , EkType
     , EkSupertype
     , EkDot
     , EkUnOp
     , EkBinOp
     , EkCall
     , EkSubscript

     , EkIntType
     , EkAddrType
     , EkFloatType 
     
     , EkProc 
     , EkFunc 
     , EkValue
     , EkBrand 
     , EkConst (* A Subcategory of EkValue *)     
     , EkRef 
     }
; TYPE Ekt = ExprKindTyp

; PROCEDURE ExprKindImage ( Kind : ExprKindTyp ) : TEXT 

; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

; TYPE ExprKindSetTyp = SET OF ExprKindTyp

; TYPE EkSetTyp = ExprKindSetTyp

; CONST EkSetValue = EkSetTyp { Ekt . EkValue } 

; CONST EkSetType = EkSetTyp { Ekt . EkType } 

; CONST EkSetConst = EkSetTyp { Ekt . EkConst } 

; CONST EkSetBrand
    = EkSetTyp { Ekt . EkBrand , Ekt . EkValue , Ekt . EkConst } 

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
    ( Expr : ExprTyp
    ; WrT : Wr . T
    ; VAR (*IN OUT*) ExprNosDumped : IntSets . T
    )
    
; PROCEDURE ExprImage ( Expr : ExprTyp ) : TEXT 
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
      ; ExpOpnd1 : ExprTyp (* Left operand when binary. *)
      ; ExpOpnd2 : ExprTyp (* Right Operand when binary. *) 
      ; ExpOpnd3 : ExprTyp 
      ; ExpQuadOpOpnd3 : ExprTyp 
      ; ExpQuadOpOpnd4 : ExprTyp 
        (* This can denote a ternary operator, in which case we use this
           type with ExpQuadOpOpnd4 field just going unused.
        *) 
      ; ExpBinOpLtOpndKindsAllowed := ExprKindSetTyp { } 
      ; ExpBinOpRtOpndKindsAllowed := ExprKindSetTyp { }
        (* This can denote a unary operator, in which case we use this
           type with 2nd operand fields just going unused.
        *) 
      ; ExpAddrReferent : ExprTyp := NIL (* Redundant to Opnd1? *) 
      ; ExpOpenArrayElemType : ExprTyp := NIL (* Redundant to Opnd1? *)
      ; ExpRangeBase : ExprTyp := NIL 
      ; ExpSubrLo : ExprTyp := NIL 
      ; ExpSubrHi : ExprTyp := NIL 
      ; ExpDefElmtType : ExprTyp 
      ; ExpDefSsType : ExprTyp (* NIL means open array. *)
     (* ExpOpnd1 is supertype, non-NIL even if defaulted. *)
     (* ExpOpnd2 is brand, NIL if no BRANDED. *) 
      ; ExpObjOverrides : REFANY (* What do we need here? *) 
      ; ExpObjScopeRef : FM3Scopes . ScopeRefTyp 
      ; ExpObjBrandKind : FM3Parser . BrandKindTyp 
      ; ExpScopeRef1 : FM3Scopes . ScopeRefTyp 
      ; ExpArgPrefix : ExprTyp 
      ; ExpArgsList : ExprListRefTyp


      ; ExpQualDeclNoLt : FM3Globals . DeclNoTyp 
      ; ExpDefDeclNo : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
      ; ExpQualIdAtomRt : FM3Base . AtomTyp 
      ; ExpIdentDeclNo : FM3Globals . DeclNoTyp
        (* ^ABS ( ExpIdentDeclNo < 0 ) is builtin opcode. *)
      ; ExpRemoteUnitNo : FM3Globals . UnitNoTyp 
      ; ExpRemoteDeclNo : FM3Globals . DeclNoTyp 
      ; ExpDefIntfUnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull
      ; ExpDefIntfDeclNo : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull
      ; ExpPosition : tPosition := FM3Base . PositionNull
      ; ExpOpcode : OpcodeTyp := FM3SrcToks . RidNull
      ; ExpDotIdAtom : FM3Base . AtomTyp
      ; ExpArgNo : INTEGER (* # of actuals still to be linked in. *) 
      ; ExpBinOpActualsCt : INTEGER 
      ; ExpStackHt : INTEGER := 0  
      ; ExpSelfExprNo : INTEGER (* < 0 for builtin ops. *)



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
      ; ExpArrayTypeIsOpen : BOOLEAN := FALSE
      ; ExpREFIsUntraced : BOOLEAN 


      METHODS
        appendDump ( )  
      ; resolve ( ExprKind : ExprKindTyp ) : ExprStateTyp (* final. *) 
      END


(* Expression operations: *)
(* Either a constant expression or one whose type is of interest. *) 

; TYPE ExprMapTyp = FM3Base . MapTyp
    (* Map ExprNoTyp to ExprTyp. One of these per Unit. *)

; PROCEDURE NewExprMap ( InitExprCt : FM3Globals . ExprNoTyp ) : ExprMapTyp 

(* Stack of definitions refs. *)

(* Let's just make this one a linked stack. *) 
; VAR ExprStackTopObj : ExprTyp := NIL
; VAR ExprStackCt : INTEGER := 0
; VAR ExprTopDeclNo : INTEGER := 0
(* INVARIANT: (ExprStackTopObj = NIL) = (ExprTopDeclNo = 0). *)

; PROCEDURE PushExprStack ( NewExpr : ExprTyp )

; PROCEDURE PopExprStack ( ) : ExprTyp 

; PROCEDURE PruneExprStack ( ToDepth : INTEGER := 0 )

; PROCEDURE IsNumericType ( Expr : ExprTyp ) : BOOLEAN 

; END FM3Exprs
.

