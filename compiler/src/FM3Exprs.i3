
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Exprs

(* In-memory data structure for types and constant expressions.
   These may be anonymous or named by a local or [ex|im]port identifier. 
*)

(* An expression, as meant here, can be a part of what a declaration defines
   and can sometimes contain named references to other declarations.  Within
   a single scope, these can refer to each other left-to-right, right-to-left,
   and cyclically.  This can require an arbitrary mix of jumping leftward
   and rigthward.  So we build in-memory, linked data structure for them.

   Remember that in Modula3, programmer-written constructs that define types
   are expressions along with value-computing expressions, and each can contain
   one of the other.  So they are all part of what's called "expressions".

   Some ExprTyps contain things that are not strictly expressions, but are
   needed as components, e.g. field lists. also data structure for them is
   defined here.
*)

(*  Expression equality and terminology:
   - Two expressions are *identical* if both refer to the same instance
     of an expression subtree.
   - An expression is *equality-eligible if it is free of errors and
     defines a type or a value that is constant, not necessarily in
     the Modula-3 sense of constant.  An ineligible expression is
     called *distinct*.
   - Two expressions are *equal* if they are equality-eligible and
     structurally equal to eac other.
   - Equal expressions can be treated as interchangeable.  A group of
     interchangable expressions has a single *representative* that
     stands for them all.  Such a group need not be maximal. Field
     ExpRepExprNo denotes an expression's representative, which can be
     itself. An eligible expression starts out its own representative
     in its own singleton group until some equalities might be discovered.
   - Rather than eagerly looking for equalities and maximal groups,
     expressions are put into groups lazily, as a side-effect of successful
     required equality checks, for use in future equality checks.  Some
     equalities may never need to be noticed.
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
; IMPORT FM3Utils 

; TYPE OpcodeTyp = FM3Base .OpcodeTyp 

(* Types in the compiled code, not in the compiler. *)

; TYPE RelKindTyp = { RkEqual , RkSubtype }

(* Footnotes on ExprKnds:
     (1) Field assigned when ExprTyp record created.
     (2) Assigned during resolve, after bottom-up resolve of children.
     (12) Assigned on creation of record that is created already resolved. 

(* Fields relevant to all ExprKinds: *) 
     (1) ExpPosition. ExpSelfExprNo, and ExpKind always
     (1) ExpRepExprNo < ExprNoFirstReal or =ExpSelfExprNo
         May change when expr is found equal to another. 
*) 

; TYPE ExprKindTyp (* What kind of definition are we expanding? *) 
   = { EkNull

(* Fields relevant to specific expr kinds" *) 
     , EkLiteral      (* (1)ExpScalarConstVal
                         (1)ExpLoTypeInfoRef  
                         (1)ExpOpcode 
                         (1)ExpHash 
                         (1)ExpIsConst=TRUE  
                         (1)ExpConstValIsKnown=TRUE  
                         (1)ExpIsLegalRecursive=TRUE
                         (1)ExpState=EsResolved
                      *)
     , EkIdentRef     (*(1)ExpDotIdAtom*) 
     , EkQualIdentRef (*(1)ExpIdentDeclNo*) 
     , EkRemoteRef    (*(1)ExpRemoteUnitNo, ExpRemoteDeclNo*) 
     
     , EkEnumType     (*(1)ExpScope1*)
     , EkRecType      (*(1)ExpScope1*)
     , EkObjType      (*(1)ExpOpcode=StkRwOBJECT.*)
                      (*(1)ExpScope1.*)
                      (*(1)ExpBrandKind.*)
                      (*(2)Opnd1 is supertype. Opnd2 is brand.*)
     , EkArrayType    (*(1)ExpArrayTypeIsOpen,*)
                      (*(1)ExpOpcode=StkRwARRAY.(Needed?)*) 
                      (*(2)Opnd1 is subscript type, Opnd2 is element type.*)
     , EkSubrType     (* Subrange *)
                      (*(1)Opnd1 is lower bound. Opnd2 is upper bound. *) 
     , EkRefType      (*(1)ExpIsUntraced.*)
                      (*(1)ExpOpcode=StkRwREF(Needed?).*)
                      (*(2)Opnd1 is brand. Opnd2 is referent type.*) 
     , EkSupertype    (*Used only for an absent supertype of REF and OBJECT.*)
                      (*(1)ExpIsLegalRecursive=TRUE, ExpIsPresent=FALSE,
                           ExpState=EsResolved.
                      *)
     , EkBrand
                      (*(1)ExpIsLegalRecursive=TRUE, ExpIsPresent,
                           ExpState=EsResolved.
                      *)
     
     , EkBuiltin      (*Distinguished by ExpOpcode.*)

     , EkDot          (*(1)ExpDotIdtom.*)
                      (*(1)ExpOpnd1: [EkQualIdentRef|EkRemoteRef].*) 
     , EkUnOp         (*(1)ExpOpcode.*)
                      (*(2)ExpOpnd1.*) 
     , EkBinOp        (*(1)ExpOpcode.*)
                      (*(2)ExpOpnd1, ExpOpnd2.*) 
     , EkCall         (*(1)ExpArgsList, ExpArgNo.*) 
                      (*(1)ExpRepExprNo=ExpExprNoDistinct*)
     , EkSubscript    (*(1)ExpArgsList, ExpArgNo.*)
                      (*(1)ExpRepExprNo=ExpExprNoDistinct*)

     , EkProc 
     , EkFunc

     , EkType  (* Probably replace by a set of the above. *) 
     , EkValue (* Probably replace by a set of the above. *)
     }
     
; TYPE Ekt = ExprKindTyp

; PROCEDURE ExprKindImage ( Kind : ExprKindTyp ) : TEXT 

; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

; TYPE ExprKindSetTyp = SET OF ExprKindTyp

; TYPE EkSetTyp = ExprKindSetTyp

; CONST EkSetValue = EkSetTyp { Ekt . EkValue } 

; CONST EkSetType = EkSetTyp { Ekt . EkType }

; CONST EkSetUniquableTypes = EkSetTyp 
       { Ekt . EkEnumType
       , Ekt . EkRecType
       , Ekt . EkArrayType
       , Ekt . EkObjType
       , Ekt . EkSubrType
       , Ekt . EkRefType
       , Ekt . EkSupertype
       (* There will never be >1 identical expr of kind EkBuiltin,
          since these are hard coded, thus not programmer-definable.
       *)
       } 

; CONST EkSetPossiblyConstants = EkSetTyp 
       { Ekt . EkLiteral
       , Ekt . EkDot
       , Ekt . EkUnOp 
       , Ekt . EkBinOp 
       , Ekt . EkCall
       , Ekt . EkSubscript 
       , Ekt . EkBrand
       } 

; CONST EkSetBrand
    = EkSetTyp { Ekt . EkBrand , Ekt . EkValue } 

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

; TYPE ExprNoTyp = INTEGER
; CONST ExprNoNull = 0
; CONST ExprNoFirstReal = 1

(* Special ExprNo values used in field ExpRepExprNo: *) 
; CONST RepExprNoSingleton = - 2
; CONST RepExprNoDistinct = -1
  (* ExpRepExprNo = RepExprNoSingleton means there will never be another expr
     structurally equal to this one (because it is not definable by code being
     compiled, created only by internal compiler initialization code.
     
     ExpRepExprNo = RepExprNoDistinct means each expression instance is unique
     even if structurally equal to others. E.g. a value expression computed
     at runtime. 

     ExpRepExprNo >= ExprNoFirstReal denotes a representative expr node of a
     class of Exprs that are all structurally and equal and interchangeable.
     E.g., type expressions and value expressions that have constant value.
  *)

; CONST ExprNoMax = LAST ( ExprNoTyp )

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
      ; ExpHash : FM3Utils . HashTyp := FM3Base . HashNull
      ; ExpOpnd1 : ExprTyp (* Left operand when binary. *)
      ; ExpOpnd2 : ExprTyp (* Right Operand when binary. *) 
      ; ExpOpnd3 : ExprTyp 
      ; ExpOpnd4 : ExprTyp

      ; ExpBuiltinOpLtOpndKindsAllowed := ExprKindSetTyp { } 
      ; ExpBuiltinOpRtOpndKindsAllowed := ExprKindSetTyp { }
        (* This can denote a unary operator, in which case we use this
           type with 2nd operand fields just going unused.
        *) 
      ; ExpRangeBase : ExprTyp := NIL 
     (* ExpOpnd1 is supertype, non-NIL even if defaulted. *)
     (* ExpOpnd2 is brand, NIL if no BRANDED. *) 
      ; ExpObjOverrides : FM3Globals . DeclRefListTyp
      ; ExpObjBrandKind : FM3Parser . BrandKindTyp 
      ; ExpScopeRef1 : FM3Scopes . ScopeRefTyp 
      ; ExpArgPrefix : ExprTyp (* Array of subscript or proc of call. *) 
      ; ExpArgsList : ExprListRefTyp

      ; ExpIdentDeclNo : FM3Globals . DeclNoTyp
        (* ^ABS ( ExpIdentDeclNo < 0 ) is builtin opcode. *)
      ; ExpRemoteUnitNo : FM3Globals . UnitNoTyp
      ; ExpRemoteDeclNo : FM3Globals . DeclNoTyp
      ; ExpPosition : tPosition := FM3Base . PositionNull
      ; ExpOpcode : OpcodeTyp := FM3SrcToks . RidNull
      ; ExpDotIdAtom : FM3Base . AtomTyp
      ; ExpArgNo : INTEGER (* # of actuals still to be linked in. *)
      ; ExpBuiltinOpActualsCt : INTEGER
      ; ExpStackHt : INTEGER := 0
      ; ExpSelfExprNo : ExprNoTyp (* < 0 for builtin ops. *)
      ; ExpRepExprNo : ExprNoTyp := ExprNoNull 

      ; ExpDownKind := Ekt . EkNull (* Inherited. *) 
      ; ExpUpKind := Ekt . EkNull (* Synthesized. *) 
      ; ExpKind : ExprKindTyp := Ekt . EkNull
      ; ExpState : ExprStateTyp := Est . EsUnresolved

      ; ExpIsConst : BOOLEAN := FALSE
      ; ExpConstValIsKnown : BOOLEAN := FALSE
(* TODO ^ Can't we just use ExpState # Est . EsUnresolved for this? *)  
      ; ExpIsUsable : BOOLEAN := TRUE
      ; ExpIsLegalRecursive : BOOLEAN := FALSE
        (* ^Self or any containing def could make it legal. *) 
      ; ExpIsDesignator : BOOLEAN := FALSE
      ; ExpIsWritable : BOOLEAN := FALSE
      ; ExpIsPresent : BOOLEAN := TRUE  
      ; ExpArrayTypeIsOpen : BOOLEAN := FALSE
      ; ExpIsUntraced : BOOLEAN 

      METHODS
        appendDump ( )  
      ; resolve ( ExprKind : ExprKindTyp ) : ExprStateTyp (* final. *) 
      END

(* Expression operations: *)
(* Either a constant expression or one whose type is of interest. *) 

; TYPE ExprMapTyp = FM3Base . MapTyp
    (* Map ExprNoTyp to ExprTyp. One of these per Unit. *)

; PROCEDURE NewExprMap ( InitExprCt : ExprNoTyp ) : ExprMapTyp 

; PROCEDURE ExprRefOfExprNo ( ExprNo : ExprNoTyp ) : ExprTyp
  (* In the current unit. *) 

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

