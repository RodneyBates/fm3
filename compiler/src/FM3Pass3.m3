
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Pass3

; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT IntSets 
; IMPORT OSError
; IMPORT Pathname
; IMPORT TextWr 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT RangeUtils 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 
; IMPORT VarArray_Int_ExpImpProxy 
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base 
; FROM   FM3Base IMPORT PositionNull , tPosition 
; IMPORT FM3CLArgs
; IMPORT FM3CLOptions  
; IMPORT FM3Compile
; IMPORT FM3Compress
; FROM   FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Exprs  
; IMPORT FM3Dict_Int_Int
; IMPORT FM3ExpImp
; IMPORT FM3ExpImpProxy
; IMPORT FM3Pass2 
; IMPORT FM3RTFailures 
; IMPORT FM3Globals
; FROM   FM3Globals IMPORT P2RdBack 
; FROM   FM3Globals IMPORT P3RdBack 
; IMPORT FM3Graph
; IMPORT FM3CTIntArith 
; IMPORT FM3IntToks AS Itk
; IMPORT FM3SrcToks AS Stk
; FROM   FM3StreamUtils
    IMPORT GetBwdInt , GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo 
; IMPORT FM3Messages 
; FROM   FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Predefs
; IMPORT FM3Scopes
; IMPORT FM3SharedUtils
; IMPORT FM3Target
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT RdBackFile

; TYPE Ekt = FM3Exprs . ExprKindTyp 
; TYPE Est = FM3Exprs . ExprStateTyp 
; TYPE Ust = FM3Units . UnitStateTyp
; TYPE Dkt = FM3Decls . DeclKindTyp 

; PROCEDURE PutBwdP3 ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd for pass 3 output file. 
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN (*PutBwdP3*) 
      <* ASSERT RdBack = FM3Globals . P3RdBack *> 
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0 
      THEN (* We are skipping output. *) RETURN
      END (*IF*) 
    ; TRY
        FM3Compress . PutBwd ( RdBack , ValueL ) 
      EXCEPT OSError . E ( EMsg )
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to write to readback file \""
               , RdBackFile . FileName ( RdBack )
               , "\", " 
               , FM3Messages . AtomListToOSError ( EMsg ) , "."  
               }
           ) 
      END (*EXCEPT*) 
    END PutBwdP3

; PROCEDURE SkipLt ( SkipNo : INTEGER )
  (* pass 3, initial (left) end of a range of conditionally skipped tokens. *)

  = BEGIN 
      WITH WSkipNoStack = FM3Globals . SkipNoStack
      DO VarArray_Int_Int . Assign
          ( WSkipNoStack
          , VarArray_Int_Int . TouchedRange ( WSkipNoStack ) . Hi + 1
          , SkipNo
          )
         (* ^Effectively a push, where touched Hi is stack Ss. *)
      (* And discard the ItkSkipLt token. *)
      END (*WITH*)
    END SkipLt 

 ; PROCEDURE SkipRt ( SkipNo : INTEGER )
  (* pass 3 final (right) end of a range of conditionally skipped tokens. *)

  = BEGIN 
      WITH WSkipNoStack = FM3Globals . SkipNoStack
      , WSkipRange = VarArray_Int_Int . TouchedRange ( WSkipNoStack )
      DO
        <* ASSERT WSkipRange . Hi > 0 *>
        <* ASSERT 
             VarArray_Int_Int . Fetch ( WSkipNoStack , WSkipRange . Hi )
             = SkipNo
        *>
        VarArray_Int_Int . Project (* Pop SkipNoStack. *) 
          ( WSkipNoStack , RangeUtils . TrimHi ( WSkipRange ) )
     (* And discard the ItkSkipRt token. *) 
      END (*WITH*)
    END SkipRt

; PROCEDURE DiscardOperands
    ( OpndCt : [ 0 .. 6 ] ; FromRdBack : RdBackFile . T )
    
  = BEGIN
      (* The obvious loop is unrolled. *) 
      IF OpndCt >= 1 
      THEN EVAL FM3Compress . GetBwd ( FromRdBack )
      ; IF OpndCt >= 2
        THEN EVAL FM3Compress . GetBwd ( FromRdBack )
        ; IF OpndCt >= 3
          THEN EVAL FM3Compress . GetBwd ( FromRdBack )
          ; IF OpndCt >= 4
            THEN EVAL FM3Compress . GetBwd ( FromRdBack )
            ; IF OpndCt >= 5
              THEN EVAL FM3Compress . GetBwd ( FromRdBack )
              ; IF OpndCt >= 6
                THEN EVAL FM3Compress . GetBwd ( FromRdBack )
                ; <* ASSERT OpndCt < 7 *> 
                END (*IF*) 
              END (*IF*) 
            END (*IF*) 
          END (*IF*) 
        END (*IF*) 
      END (*IF*) 
    END DiscardOperands 

; <*UNUSED*> PROCEDURE CopyOperandsReverse

    ( OpndCt : [ 0 .. 6 ] ; FromRdBack , ToRdBack : RdBackFile . T )
  (* Copy operands, up to 6. This is could be confusing.  One-at-a-time pop,
     push temporally reverses them left-to-right.  But if the caller is copying
     to a RdBack that is to be read in the opposite direction, that will
     implicitly reverse them, for net same temporal order when the next pass
     reads them. 
  *)
  
  = BEGIN
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
         > 0 
      THEN DiscardOperands ( OpndCt , FromRdBack )
      ELSE (* Actually copy. *) 
      (* The obvious loop is unrolled. *) 
        IF OpndCt >= 1 
        THEN FM3Compress . PutBwd
               ( ToRdBack , FM3Compress . GetBwd ( FromRdBack ) ) 
        ; IF OpndCt >= 2
          THEN FM3Compress . PutBwd
                 ( ToRdBack , FM3Compress . GetBwd ( FromRdBack ) )
          ; IF OpndCt >= 3
            THEN FM3Compress . PutBwd
                   ( ToRdBack , FM3Compress . GetBwd ( FromRdBack ) )
            ; IF OpndCt >= 4
              THEN FM3Compress . PutBwd
                     ( ToRdBack , FM3Compress . GetBwd ( FromRdBack ) )
              ; IF OpndCt >= 5
                THEN FM3Compress . PutBwd
                       ( ToRdBack , FM3Compress . GetBwd ( FromRdBack ) )
                ; IF OpndCt >= 6
                  THEN FM3Compress . PutBwd
                         ( ToRdBack , FM3Compress . GetBwd ( FromRdBack ) )
                  ; <* ASSERT OpndCt < 7 *> 
                  END (*IF*) 
                END (*IF*) 
              END (*IF*) 
            END (*IF*) 
          END (*IF*) 
        END (*IF*) 
      END (*IF*)
    END CopyOperandsReverse

; PROCEDURE CopyOperandsInOrder
    ( OpndCt : [ 0 .. 6 ] ; FromRdBack , ToRdBack : RdBackFile . T )
  (* This is tricky.  Pop, push reverses them, but this procedure does
     a compensating reverse, for net same order. 
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*CopyOperandsInOrder*)
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
         > 0 
      THEN DiscardOperands ( OpndCt , FromRdBack ) 
      ELSE (* Actually copy, with net reversal. *) 
        (* The obvious loop is unrolled, not so obviously. *) 
        IF OpndCt >= 1 
        THEN
          LOpnd1 := FM3Compress . GetBwd ( FromRdBack )
        ; IF OpndCt >= 2
          THEN LOpnd2 := FM3Compress . GetBwd ( FromRdBack )
          ; IF OpndCt >= 3
            THEN LOpnd3 := FM3Compress . GetBwd ( FromRdBack )
            ; IF OpndCt >= 4
              THEN LOpnd4 := FM3Compress . GetBwd ( FromRdBack )
              ; IF OpndCt >= 5
                THEN LOpnd5 := FM3Compress . GetBwd ( FromRdBack )
                ; IF OpndCt >= 6
                  THEN LOpnd6 := FM3Compress . GetBwd ( FromRdBack )
                  ; FM3Compress . PutBwd ( ToRdBack , LOpnd6 ) 
                  END (*IF*) 
                ; FM3Compress . PutBwd ( ToRdBack , LOpnd5 ) 
                END (*IF*) 
              ; FM3Compress . PutBwd ( ToRdBack , LOpnd4 ) 
              END (*IF*) 
            ; FM3Compress . PutBwd ( ToRdBack , LOpnd3 ) 
            END (*IF*) 
          ; FM3Compress . PutBwd ( ToRdBack , LOpnd2 ) 
          END (*IF*)
        ; FM3Compress . PutBwd ( ToRdBack , LOpnd1 ) 
        END (*IF*)
      END (*IF*) 
    END CopyOperandsInOrder

; PROCEDURE GetTokCode
    ( LMPass2Depth : LONGINT ) : Itk . TokTyp  
  (* Return a token code.  Callers will
     read its varying complement of arguments.
  *) 

  = VAR LTokenL  : LONGINT 
  ; VAR LToken : Itk . TokTyp
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPass2RdBack : RdBackFile . T 

  ; BEGIN (* GetTokCode *) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; LPass2RdBack := LUnitRef ^ . UntPass2OutRdBack 
    ; LMPass2Depth := MAX ( LMPass2Depth , LUnitRef ^ . UntPass2OutEmptyCoord )
    ; LOOP (* Thru' a sequence of SkipRt & SkipLt tokens plus one other. *)  
        IF RdBackFile . LengthL ( LPass2RdBack ) <= LMPass2Depth
        THEN (* Done with the entire file. *)
          RETURN Itk . ItkBOF
        END (*IF*)
      ; LTokenL := FM3Compress . GetBwd ( LPass2RdBack )
      ; LToken := VAL ( LTokenL , Itk . TokTyp ) 
      (* ItkSkip[Lt|Rt] pairs are inserted by pass 2 and acted-on in pass 3.*)
      ; IF LToken = Itk . ItkSkipLt
        THEN (* Handle ItkSkipLt here, so multiple token handlers needn't. *) 
            SkipLt ( GetBwdInt ( LPass2RdBack) ) 
          (* and loop. *) 
        ELSE (* Return unskipped token. *)
          RETURN LToken 
        END (*IF*)
      END (*LOOP*)
    END GetTokCode 

(*EXPORTED*)
; PROCEDURE Pass3Tokens ( LMPass2Depth : LONGINT )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LToken : Itk . TokTyp 

  ; BEGIN (* Pass3Tokens *) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; LMPass2Depth := MAX ( LMPass2Depth , LUnitRef ^ . UntPass2OutEmptyCoord )
    ; <* ASSERT GetBwd ( P2RdBack ) = VAL ( Itk . ItkBOF , LONGINT ) *> 
      <* ASSERT GetBwd ( P2RdBack ) = VAL ( Itk . ItkLeftEnd , LONGINT )*> 
      LOOP
        LToken := GetTokCode ( LMPass2Depth )
      ; IF LToken = Itk . ItkEOF
        THEN EXIT
        ELSE HandleTok ( LToken )
        END (*IF*)
      (* And loop. *) 
      END (*LOOP*)
    END Pass3Tokens
(*
; PROCEDURE AssertExprStack1 ( VAR (*OUT*) Top : FM3Exprs . ExprTyp )

  = BEGIN
      Top := FM3Exprs . ExprStackTopObj
(*    ; <* ASSERT Top # NIL *>
      <* ASSERT Top . ExpStackLink = NIL *>
*) 
    END AssertExprStack1 

; PROCEDURE AssertExprStackGE1 ( VAR (*OUT*) Top : FM3Exprs . ExprTyp )

  = BEGIN
      Top := FM3Exprs . ExprStackTopObj
    ; <* ASSERT Top # NIL *>
    END AssertExprStackGE1 

; PROCEDURE AssertExprStack2 ( VAR (*OUT*) Top , Second : FM3Exprs . ExprTyp )

  = BEGIN
      Top := FM3Exprs . ExprStackTopObj
    ; <* ASSERT Top # NIL *>
      Second := Top . ExpStackLink 
    ; <* ASSERT Top # NIL *>
      <* ASSERT Second . ExpStackLink = NIL *>
    END AssertExprStack2 

; PROCEDURE AssertExprStackGE2 ( VAR (*OUT*) Top , Second : FM3Exprs . ExprTyp )

  = BEGIN
      Top := FM3Exprs . ExprStackTopObj
    ; <* ASSERT Top # NIL *>
      Second := Top . ExpStackLink 
    END AssertExprStackGE2

; PROCEDURE PopExpr1 ( ) 

  = BEGIN
      <* ASSERT FM3Exprs . PopExprStack ( ) # NIL *>  
      <* ASSERT FM3Exprs . ExprStackTopObj = NIL *>  
    END PopExpr1 

; PROCEDURE PopExpr2 ( ) 

  = BEGIN
      <* ASSERT FM3Exprs . PopExprStack ( ) # NIL *>  
      <* ASSERT FM3Exprs . PopExprStack ( ) # NIL *>  
      <* ASSERT FM3Exprs . ExprStackTopObj = NIL *>  
    END PopExpr2 

; PROCEDURE PopExprGE2 ( ) 

  = BEGIN
      <* ASSERT FM3Exprs . PopExprStack ( ) # NIL *>  
<* ASSERT FM3Exprs . PopExprStack ( ) # NIL *>  
    END PopExprGE2 
*)

; PROCEDURE PushExprIgnore ( READONLY Position : tPosition )
  (* Push an expression node with only a position.
     Could be legally absent or unusable on account of some static error.
  *) 

  = VAR LNewObj : FM3Exprs . ExprTyp 

  ; BEGIN 
      LNewObj := NEW ( FM3Exprs . ExprTyp )
    ; LNewObj . ExpPosition := Position
    ; FM3Exprs . PushExprStack ( LNewObj ) 
    END PushExprIgnore

; REVEAL FM3Exprs . Expr1OpndTyp
   = FM3Exprs . Expr1OpndPublic BRANDED "FM3Exprs.Expr1OpndTyp"
     OBJECT OVERRIDES resolve := ResolveOperand
     END 

; PROCEDURE ResolveOperand
    ( Expr1Opnd : FM3Exprs . Expr1OpndTyp
    ; ExprKind : FM3Exprs . ExprKindTyp
    )
  : FM3Exprs . ExprStateTyp
  = BEGIN
    END ResolveOperand 

; PROCEDURE DefExprLt ( NewExprObj : FM3Exprs . ExprTyp )
  (* PRE: NOT Skipping. *)
  (* Expressions that are contained in definitions. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  
  ; BEGIN
      LUnitRef := FM3Units . UnitStackTopRef
    ; LScopeRef := FM3Scopes . DeclScopeStackTopRef
    ; NewExprObj . ExpSelfExprNo 
        := VarArray_Int_Refany . TouchedRange ( LUnitRef ^ . UntExprMap ) . Hi
           + 1
    ; VarArray_Int_Refany . Assign
        ( LUnitRef ^ . UntExprMap , NewExprObj . ExpSelfExprNo , NewExprObj )
    ; WITH WCurDef
           = LScopeRef ^ . ScpCurDefExprs [ LScopeRef . ScpCurDefIsValue ]
      DO IF WCurDef = NIL
        THEN (* NewExprObj is root of expression tree. *)
          WCurDef := NewExprObj
        ; PutBwdP3
            ( P3RdBack , VAL ( NewExprObj . ExpPosition . Column , LONGINT ) )
        ; PutBwdP3
            ( P3RdBack , VAL ( NewExprObj . ExpPosition . Line , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( NewExprObj . ExpSelfExprNo , LONGINT ) )
        ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkDefTopExprNo , LONGINT ) )
        ELSE (* Inherit some things from parent expr node. *) 
          TYPECASE FM3Exprs . ExprStackTopObj OF 
          | NULL => <* ASSERT FALSE , "Orphan expr node" *> 
          | FM3Exprs . ExprTyp ( TParentExpr ) 
          =>  NewExprObj . ExpDownKind := TParentExpr . ExpDownKind
            ; NewExprObj . ExpIsLegalRecursive
                := NewExprObj . ExpIsLegalRecursive
                   OR TParentExpr . ExpIsLegalRecursive

          ELSE <* ASSERT FALSE , "Orphan expr node" *>
          END (*TYPECASE*)
        END (*IF*)
      END (*WITH*) 
    ; FM3Exprs . PushExprStack ( NewExprObj )
    END DefExprLt 

; PROCEDURE DefExprRt ( )
  (* PRE NOT Skipping. *)
  (* Expressions that are contained in definitions. *) 

  = BEGIN 
(* TODO: Anything? *) 
    END DefExprRt

; PROCEDURE HandleTok ( Token : Itk . TokTyp ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR HtPass2RdBack : RdBackFile . T 
  ; VAR HtPass3RdBack : RdBackFile . T
  ; VAR LLongInt : LONGINT 
  ; VAR LScopeNo : FM3Globals . ScopeNoTyp
  ; VAR LPosition : tPosition
  ; VAR HtSkipping : BOOLEAN

  ; PROCEDURE HtIntLit ( LoTypeNo : FM3LoTypes . LoTypeNoTyp  )
    (* Both INTEGER and LONGINT. *)

    = VAR LLongInt : LONGINT 
    ; VAR LPosition : tPosition

    ; BEGIN
        LLongInt := GetBwd ( HtPass2RdBack )
      ; LPosition := GetBwdPos ( HtPass2RdBack )
      ; IF NOT HtSkipping 
        THEN
          IF AreInsideADecl ( )
          THEN 
            WITH LExpr
              = NEW ( FM3Exprs . ExprConstValue , ExpUpKind := Ekt . EkValue ) 
            DO 
              LExpr . ExpScalarConstVal := LLongInt
            ; LExpr . ExpLoTypeInfoRef
                := VarArray_Int_Refany . Fetch
                     ( FM3LoTypes . LoTypeMap , LoTypeNo )  
            ; LExpr . ExpPosition := LPosition
            ; LExpr . ExpUpKind := Ekt . EkValue
            ; LExpr . ExpIsConst := TRUE 
            ; LExpr . ExpIsLegalRecursive := TRUE
            ; DefExprLt ( LExpr )
            END (*WITH*)
          ELSE
(* DECIDE: Do we want just one token code with a LoTyp operand here? *) 
            PutBwdP3 ( HtPass3RdBack , VAL ( LPosition . Column , LONGINT ) ) 
          ; PutBwdP3 ( HtPass3RdBack , VAL ( LPosition . Line , LONGINT ) ) 
          ; PutBwdP3 ( HtPass3RdBack , LLongInt )
          ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )
          END (*IF*) 
        END (*IF*)
      END HtIntLit 

  ; PROCEDURE HtExprLt ( NewExpr : FM3Exprs . ExprTyp )

    = BEGIN 
        WITH WPosition = GetBwdPos ( HtPass2RdBack )
        DO IF HtSkipping 
          THEN (* NewExpr becomes immediate garbage. *) 
          ELSE 
            NewExpr . ExpPosition := WPosition
          ; DefExprLt ( NewExpr )
          END (*IF*)
        END (*WITH*)
      END HtExprLt 

  ; PROCEDURE HtExprOpnd1  ( )
      (* PRE: TOS is 1st (LM) operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp

    ; BEGIN 
        WITH WPosition = GetBwdPos ( HtPass2RdBack )
        DO IF NOT HtSkipping 
          THEN
            LOpnd := FM3Exprs . PopExprStack ( )
          ; TYPECASE FM3Exprs . ExprStackTopObj
            OF NULL => <*ASSERT FALSE*>
            | FM3Exprs . Expr1OpndTyp ( TParentExpr ) 
            => <*ASSERT WPosition = TParentExpr . ExpPosition *>
               TParentExpr . ExpOpnd1 := LOpnd
            ELSE <*ASSERT FALSE*>
            END (*TYPECASE*)
          ; DefExprRt ( ) 
          END (*IF*) 
        END (*WITH*)
      END HtExprOpnd1 

  ; PROCEDURE HtExprOpnd2  ( )
      (* PRE: TOS is 2nd operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp

    ; BEGIN 
        WITH WPosition = GetBwdPos ( HtPass2RdBack )
        DO IF NOT HtSkipping 
          THEN
            LOpnd := FM3Exprs . PopExprStack ( )
          ; TYPECASE FM3Exprs . ExprStackTopObj
            OF NULL => <*ASSERT FALSE*>
            | FM3Exprs . Expr2OpndTyp ( TParentExpr ) 
            => <*ASSERT WPosition = TParentExpr . ExpPosition *>
               TParentExpr . ExpOpnd2 := LOpnd
            ELSE <*ASSERT FALSE*>
            END (*TYPECASE*)
          ; DefExprRt ( ) 
          END (*IF*) 
        END (*WITH*)
      END HtExprOpnd2

  ; PROCEDURE HtPassTokenThru ( )

    = BEGIN 
        IF HtSkipping 
        THEN
          DiscardOperands
            ( FM3Utils . TokenOpndCt ( Token )
            , HtPass2RdBack
            ) 
        ELSE 
          CopyOperandsInOrder
            ( FM3Utils . TokenOpndCt ( Token )
            , HtPass2RdBack 
            , HtPass3RdBack
            ) 
        ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )
        END (*IF*) 
      END HtPassTokenThru 

  ; PROCEDURE HtMaybePassTokenThru ( ) : BOOLEAN (* Handled it somehow. *)
    (* Use this only for writing to pass 3 output. *) 

    = BEGIN 
        IF HtSkipping 
        THEN
          DiscardOperands
            ( FM3Utils . TokenOpndCt ( Token )
            , HtPass2RdBack
            ) 
        ; RETURN TRUE  
        ELSE 
          IF NOT FM3Scopes . DeclScopeStackTopRef ^ . ScpInsideDecl 
          THEN (* Not inside a decl-defining expression. *) 
            CopyOperandsInOrder
              ( FM3Utils . TokenOpndCt ( Token )
              , HtPass2RdBack 
              , HtPass3RdBack
              ) 
          ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )
          ; RETURN TRUE
          ELSE (* Caller must handle in its own way. *)
            RETURN FALSE 
          END (*IF*)
        END (*IF*) 
      END HtMaybePassTokenThru 

  ; PROCEDURE HtReverseVariableValues
      ( MaybeSkip : BOOLEAN (* ToRdBack is conditional on SkipNoStack. *) )
    (* Reverse copy a variable number (given by the first operand. of values,
       with a copy of their count before and after.
    *) 

    = VAR LCountRt : LONGINT
    ; VAR LCountLt : LONGINT
    ; VAR LValuesRef : REF ARRAY OF LONGINT 
    ; VAR LCount : INTEGER

    ; BEGIN
        IF MaybeSkip AND HtSkipping 
        THEN
        ELSE 
          LCountLt := FM3Compress . GetBwd ( HtPass2RdBack )
        ; PutBwdP3 ( HtPass3RdBack , LCountLt )
        ; LCount := VAL ( LCountLt , INTEGER )
        ; LValuesRef := NEW ( REF ARRAY OF LONGINT , LCount )
        ; FOR RI := 0 TO LCount - 1
          DO LValuesRef ^ [ RI ] := FM3Compress . GetBwd ( HtPass2RdBack )
          END (*FOR*) 
        ; FOR RI := LCount - 1 TO 0 BY - 1 
          DO PutBwdP3 ( HtPass3RdBack , LValuesRef ^ [ RI ] )
          END (*FOR*)
        ; LValuesRef := NIL 
        ; LCountRt := FM3Compress . GetBwd ( HtPass2RdBack )
        ; <*ASSERT LCountRt = LCountLt *>
          PutBwdP3 ( HtPass3RdBack , LCountRt )
        END (*IF*) 
      END HtReverseVariableValues 

  ; BEGIN (*HandleTok*) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; HtPass2RdBack := LUnitRef ^ . UntPass2OutRdBack 
    ; HtPass3RdBack := LUnitRef ^ . UntPass3OutRdBack
    ; HtSkipping
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
    ; CASE Token OF

      | Itk . ItkScopeEmpty 
      =>  LScopeNo := GetBwdScopeNo ( HtPass2RdBack )
(* TODO: Anything here? *) 

      | Itk . ItkDeclScopeLt 
      =>  LScopeNo := GetBwdScopeNo ( HtPass2RdBack )
        ; FM3Scopes . PushDeclScopeRef
            ( FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) )

      | Itk . ItkDeclScopeRt 
      =>  LScopeNo := GetBwdScopeNo ( HtPass2RdBack )
        ; <* ASSERT FM3Scopes . PopDeclScopeRef ( ) ^ . ScpSelfScopeNo
             = LScopeNo
          *>

      | Itk . ItkOpenScopeLt 
      =>  LScopeNo := GetBwdScopeNo ( HtPass2RdBack ) 
        ; LScopeRef := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
        ; FM3Scopes . PushOpenScopeRef ( LScopeRef )
        ; OpenScopeLt ( LScopeRef ) 

      | Itk . ItkOpenScopeRt  
      =>  LScopeNo := GetBwdScopeNo ( HtPass2RdBack ) 
        ; LScopeRef := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
        ;  OpenScopeRt ( LScopeRef )
  
      | Itk . ItkOpenDeclListLt
      =>  FM3Scopes . OpenScopeStackTopRef ^ . ScpInsideDecl := TRUE 
        ; HtPassTokenThru ( ) 

      | Itk . ItkOpenDeclListRt
      =>  FM3Scopes . OpenScopeStackTopRef ^ . ScpInsideDecl := FALSE 
        ; HtPassTokenThru ( ) 

      | Itk . ItkConstDeclLt 
      , Itk . ItkVarDeclLt 
      , Itk . ItkVALUEFormalLt
      , Itk . ItkVARFormalLt
      , Itk . ItkROFormalLt
      , Itk . ItkFieldDeclLt (* Of either record or object. *) 
      =>  FM3Scopes . DeclScopeStackTopRef ^.  ScpCurDeclRefNoSet
            := IntSets . Empty ( )
        ; FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefIsValue := TRUE
            (* ^ Value def coming up. *)
        ; HtPassTokenThru ( )

      | Itk . ItkConstDeclValue 
      , Itk . ItkVarDeclValue 
      , Itk . ItkVALUEFormalValue
      , Itk . ItkVARFormalValue
      , Itk . ItkROFormalValue
      , Itk . ItkFieldDeclValue (* Of either record or object. *) 
      =>  EVAL FM3Exprs . PopExprStack ( ) 
        ; FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefIsValue := FALSE
            (* ^Now type def coming up. *)
        ; HtPassTokenThru ( )

      | Itk . ItkConstDeclType 
      , Itk . ItkVarDeclType 
      , Itk . ItkVALUEFormalType
      , Itk . ItkVARFormalType
      , Itk . ItkROFormalType
      , Itk . ItkFieldDeclType (* Of either record or object. *) 
      =>  EVAL  FM3Exprs . PopExprStack ( )  
        ; FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefIsValue := TRUE
            (* ^Done with type, value is next. *) 
        ; HtPassTokenThru ( )

      | Itk . ItkConstDeclRt 
      , Itk . ItkVarDeclRt 
      , Itk . ItkVALUEFormalRt
      , Itk . ItkVARFormalRt
      , Itk . ItkROFormalRt
      , Itk . ItkFieldDeclRt (* Of either record or object. *) 
      =>  HtPassTokenThru ( )

      | Itk . ItkTypeDeclLt
      , Itk . ItkFullRevealLt 
      , Itk . ItkPartialRevealLt
      =>  FM3Scopes . DeclScopeStackTopRef ^.  ScpCurDeclRefNoSet
            := IntSets . Empty ( ) 
        ; FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefIsValue := FALSE
            (* ^ These have only a type def. *) 
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclType
      , Itk . ItkFullRevealType 
      , Itk . ItkPartialRevealSubtype
      =>  EVAL FM3Exprs . PopExprStack ( )  
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclRt
      , Itk . ItkFullRevealRt 
      , Itk . ItkPartialRevealRt
      =>  HtPassTokenThru ( )

      | Itk . ItkMethodDeclRt
      =>  (* One subtree to pop. *)
          EVAL FM3Exprs . PopExprStack ( )  
        ; HtPassTokenThru ( )

      | Itk . ItkDeclTypeAbsent 
      , Itk . ItkDeclValAbsent
      , Itk . ItkFormalTypeAbsent
      , Itk . ItkFormalExprAbsent 
      =>  LPosition := GetBwdPos ( HtPass2RdBack )
        ; IF NOT HtSkipping 
          THEN
            PushExprIgnore ( LPosition ) 
          ; PutBwdP3 ( HtPass3RdBack , VAL ( LPosition . Column , LONGINT ) ) 
          ; PutBwdP3 ( HtPass3RdBack , VAL ( LPosition . Line , LONGINT ) ) 
          ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )
          END (*IF*) 

(* CONSISTIFY: For some of these, fetch the operands inside the called proc. *) 
      | Itk . ItkDuplDeclId
      => EVAL DuplDeclIdL2R ( )

      | Itk . ItkDeclId
      => EVAL DeclIdL2R ( )

(* FIXME: We now use different tokens for different declkinds, eg.
    ItkVALUEFormalIdListElem.  But is that necessary? *) 

      | Itk . ItkIdRefAtom
      => IdentRefL2R ( )

      | Itk . ItkQualIdAtoms 
      => QualIdentL2R ( HtPass2RdBack )

      | Itk . ItkBlockLt
      => LScopeNo := GetBwdScopeNo ( HtPass2RdBack ) 
     (* Doesn't exist, probably ItkBlock[LR]t will disappear.

     *) 
        ; CopyOperandsInOrder
           ( 2 (*Position*) , HtPass2RdBack , HtPass3RdBack )
        ; PutBwdP3 ( HtPass3RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP3 ( HtPass3RdBack , VAL ( Itk . ItkBlockLt , LONGINT ) )

(* ItkBlock[RL]t will probably disappear. *) 
      | Itk . ItkBlockRt 
      => CopyOperandsInOrder
           ( FM3Utils . TokenOpndCt ( Token ) 
           , HtPass2RdBack
           , HtPass3RdBack
           )
        ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )

      | Itk . ItkIntLit
      => HtIntLit ( FM3LoTypes . LoTypeNoInt ) 

      | Itk . ItkLongIntLit
      => HtIntLit ( FM3LoTypes . LoTypeNoLong ) 

      | Itk . ItkTextLitLt
      , Itk . ItkWideTextLitLt
      => CopyOperandsInOrder
           ( 3 (* Atom, position. *) , HtPass2RdBack , HtPass3RdBack )
        ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )
        ; HtReverseVariableValues ( MaybeSkip := TRUE ) 

      | Itk . ItkTextLitRt
      , Itk . ItkWideTextLitRt
      => CopyOperandsInOrder
           ( 3 (* Atom, position. *) , P2RdBack , P3RdBack )
        ; PutBwdP3 ( HtPass3RdBack , VAL ( Token , LONGINT ) )

      (* Enumeration type: *) 
(* FIXME: Some of these need to copy the token and arguments. *)   
      | Itk . ItkEnumTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( HtPass2RdBack ) (* Field count. *) 
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprEnumTypeTyp , ExpUpKind := Ekt . EkType ) ) 

      | Itk . ItkEnumTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( HtPass2RdBack ) (* Field count. *) 

      (* Record type: *) 
      | Itk . ItkRecTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( HtPass2RdBack ) (* Field count. *) 
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprRecTypeTyp , ExpUpKind := Ekt . EkType ) )
(* Copy token? *) 

      | Itk . ItkRecTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( HtPass2RdBack ) (* Field count. *)
        ; EVAL GetBwdPos ( HtPass2RdBack )
(* Copy token? *) 

      (* REF type: *) 
      | Itk . ItkREFTypeLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprREFTypeTyp
                  , ExpUpKind := Ekt . EkType
                  , ExpIsLegalRecursive := TRUE
                  )
            ) 

      | Itk . ItkREFTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd1 ( ) 

      (* Open array type: *) 
      | Itk . ItkOpenArrayTypeLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprOpenArrayTypeTyp , ExpUpKind := Ekt . EkValue) )

      | Itk . ItkOpenArrayTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd1 ( ) 

      (* Subrange type: *) 
      | Itk . ItkSubrTypeLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprSubrTypeTyp , ExpUpKind := Ekt . EkType ) ) 

      | Itk . ItkSubrTypeDotDot 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 

      | Itk . ItkSubrTypeRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*)
        ; HtExprOpnd2 ( )

      (* Unary operators: *) 
      | Itk . ItkUnaryOpLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*)
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprBinOpTyp
                  , ExpBinOpOp := GetBwdInt ( HtPass2RdBack )
                  )
            ) 
 
      | Itk . ItkUnaryOpRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd1 ( ) (* Only operand. *)

      (* Binary Operators: *) 
      | Itk . ItkBinaryOpLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprLt
            ( NEW ( FM3Exprs . ExprBinOpTyp
                  , ExpBinOpOp := GetBwdInt ( HtPass2RdBack )
                  )
            )

            TRY
              LConst
                := FM3CTIntArith . BinOp
                     ( LOpnd1 , LOpnd2 , LOpcode , IsConst := )
            ; EVAL FM3Exprs . PopExprStack ( )
            ; LNewExpr := NewScalarConst ( ) 
            ; FM3Exprs . PushExprStack ( LNewEexpr )
            EXCEPT FM3CTIntRITH . ArithError
            => 
            END (* EXCEPT *) 




      | Itk . ItkBinaryOpOpCode 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd2 ( ) (* Right operand. *) 

      | Itk . ItkBinaryOpRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd1 ( ) (* Left operand. *)

      ELSE (* No special pass3 handling. *)
        HtPassTokenThru ( ) 
      END (*CASE*)
    END HandleTok

(* Right-to-left scope handling.  Call sites read the Itk and its operands,
   and pass the operands in. *) 

; PROCEDURE LookupDeclNoInScope
    ( READONLY Scope : FM3Scopes . ScopeTyp ; IdAtom : FM3Base . AtomTyp ) 
  : FM3Globals . DeclNoTyp 
  (* PRE: IdAtom is in Scope's dictionary. *) 

  = VAR LDeclNoInt : INTEGER
  ; VAR LResult : FM3Globals . DeclNoTyp 
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupDeclNoInScope*)
      LResult := FM3Globals . DeclNoNull
    ; IF IdAtom > 0 THEN  
        TRY
          LFound 
            := FM3Dict_Int_Int . LookupFixed
                 ( Scope . ScpDeclDict
                 , IdAtom
                 , FM3Base . HashNull
                 , (*OUT*) LDeclNoInt
                 )
        ; IF LFound
          THEN LResult := VAL ( LDeclNoInt , FM3Globals . DeclNoTyp )
          END (*IF*) 
        EXCEPT FM3Dict_Int_Int . Error ( <*UNUSED*> EMsg )
        => LResult := FM3Globals . DeclNoNull 
        ELSE LResult := FM3Globals . DeclNoNull 
        END (*EXCEPT*)
      END (*IF*) 
    ; RETURN LResult 
    END LookupDeclNoInScope

; PROCEDURE LookupAtomExpImp
    ( IdAtom : FM3Base . AtomTyp
    ; VAR (*OUT*) UnitNo : FM3Globals . UnitNoTyp
    ; VAR (*OUT*) DeclNo : FM3Globals . DeclNoTyp
    )
  : BOOLEAN (* It's present. *) 
  (* Look in the current unit.  *)

  = VAR LExpImpProxy : FM3ExpImpProxy . T

  ; BEGIN
      IF IntSets . IsElement
           ( IdAtom , FM3Units . UnitStackTopRef ^ . UntExpImpIdSet ) 
      THEN (* It's [ex|im]ported. *) 
        LExpImpProxy
          := VarArray_Int_ExpImpProxy . Fetch
               ( FM3Units . UnitStackTopRef ^ . UntExpImpMap , IdAtom )
      ; UnitNo := LExpImpProxy . EipUnitNo 
      ; DeclNo := LExpImpProxy . EipDeclNo
      ; RETURN TRUE
      ELSE
        UnitNo := FM3Globals . UnitNoNull
      ; DeclNo := FM3Globals . DeclNoNull 
      ; RETURN FALSE 
      END (*IF*)
    END LookupAtomExpImp 

; PROCEDURE OpenScopeLt ( ScopeRef : FM3Scopes . ScopeRefTyp ) 

  = VAR LScopeNo : FM3Scopes . ScopeNoTyp
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LDeclRef : FM3Decls . DeclRefTyp 

  ; BEGIN (* OpenScopeLt *) 
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi <= 0
      THEN (* Not skipping. *) 
        FM3Scopes . PushOpenScopeRef ( ScopeRef )
      ; PutBwdP3 ( P3RdBack , VAL ( ScopeRef ^ . ScpSelfScopeNo , LONGINT ) ) 
      ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkOpenScopeLt, LONGINT ) )
      ; LUnitRef := FM3Units . UnitStackTopRef
      ; FOR RDeclNo := ScopeRef . ScpMinDeclNo TO ScopeRef . ScpDeclCt - 1
        DO
          LDeclRef (* Implied NARROW *) 
            := VarArray_Int_Refany . Fetch ( LUnitRef ^ . UntDeclMap , RDeclNo )
        ; IF LDeclRef ^ . DclIsUsable
          THEN
            <* ASSERT FM3Exprs . ResolveNow
                        ( LDeclRef ^ . DclDefType , Ekt . EkType  )
                      = Est . EsResolved
            *>
            <* ASSERT FM3Exprs . ResolveNow
                        ( LDeclRef ^ . DclDefValue , Ekt . EkValue ) 
                      = Est . EsResolved
            *>
          END (*IF*) 
        END (*FOR*) 
      END (*IF*)
    END OpenScopeLt 

; PROCEDURE OpenScopeRt ( ScopeRef : FM3Scopes . ScopeRefTyp ) 

  = VAR LScopeNo : FM3Scopes . ScopeNoTyp

  ; BEGIN
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi <= 0
      THEN (* Not skipping. *) 
          LScopeNo := ScopeRef ^ . ScpSelfScopeNo  
        ; PutBwdP3 ( P3RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkOpenScopeRt, LONGINT ) )
        ; <* ASSERT FM3Scopes . PopOpenScopeRef ( ) = ScopeRef *>
    END (*IF*)
  END OpenScopeRt

; PROCEDURE ResolveNot ( NotExpr : FM3Exprs  . Expr1OpndTyp )

  = BEGIN
      WITH WOpnd = NotExpr . ExpOpnd1
      DO 
        EVAL FM3Exprs . ResolveNow ( WOpnd , Ekt . EkValue )
      ; IF WOpnd . ExpOpcode = Stk . RidBOOLEAN
        THEN
          NotExpr . ExpType := FM3Predefs . BoolType  
        ; NotExpr . ExpKind := WOpnd . ExpKind
        ; NotExpr . ExpIsUsable := TRUE 
        ; NotExpr . ExpIsLegalRecursive := TRUE
        ; NotExpr . ExpIsDesignator := FALSE 
        ; NotExpr . ExpIsWritable := FALSE 
        ; NotExpr . ExpState := Est . EsResolved
        ; IF WOpnd . ExpIsConst
          THEN
            NotExpr . ExpIsConst := TRUE 
          ; NotExpr . ExpScalarConstVal
              := FM3CTIntArith . Not ( WOpnd . ExpScalarConstVal ) 
          ELSE NotExpr . ExpIsConst := FALSE
          END (*IF*) 
        ELSE 
          FM3Messages  . ErrorArr
            ( ARRAY OF REFANY 
                { "Operand of \"NOT\"  must have type BOOLEAN." }
            , WOpnd . ExpPosition  
            ) 
        ; NotExpr . ExpIsUsable := FALSE 
        END (*IF*) 
      END (*WITH*)
    END ResolveNot

; PROCEDURE ResolveBoolBinOp
    ( OpExpr : FM3Exprs . Expr2OpndTyp
    ; Opcode : FM3Exprs . OpcodeTyp
    ; ReqdConstPos : tPosition
    )

  = VAR LOk : BOOLEAN := FALSE
  ; VAR LExprState1 , LExprState2 : FM3Exprs . ExprStateTyp

  ; BEGIN
      TYPECASE OpExpr OF
      | NULL => RETURN
      ELSE RETURN 
      END (*TYPECASE*)
    ; WITH WOpnd1= OpExpr . ExpOpnd1
           , WOpnd2= OpExpr . ExpOpnd2
      DO 
        LExprState1 := FM3Exprs . ResolveNow ( WOpnd1 , WOpnd1 . ExpKind )
      ; LExprState2 := FM3Exprs . ResolveNow ( WOpnd2 , WOpnd2 . ExpKind )
      ; IF NOT WOpnd1 . ExpIsUsable
        THEN LOk := FALSE 
        ELSIF WOpnd1 . ExpOpcode # Stk . RidBOOLEAN
        THEN 
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY 
                { "Left operand of \""
                , FM3SrcToks . Image ( Opcode )
                , "\" must have type BOOLEAN." 
                }
            , WOpnd1 . ExpPosition 
            ) 
        ; LOk := FALSE
        END (*IF*)  

      ; IF NOT WOpnd2 . ExpIsUsable
        THEN LOk := FALSE 
        ELSIF WOpnd2 . ExpOpcode # Stk . RidBOOLEAN
        THEN 
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY 
                { "Right operand of \""
                , FM3SrcToks . Image ( Opcode )
                , "\" must have type BOOLEAN." 
                }
            , WOpnd2 . ExpPosition 
            ) 
        ; LOk := FALSE
        END (*IF*)

      ; OpExpr . ExpIsUsable := LOk 
      ; IF LOk THEN 
          OpExpr . ExpType := FM3Predefs . BoolType
        ; OpExpr . ExpOpcode := Stk . RidBOOLEAN 
        ; OpExpr . ExpKind := Ekt . EkValue 
        ; OpExpr . ExpIsUsable := TRUE 
        ; OpExpr . ExpIsLegalRecursive := TRUE
        ; OpExpr . ExpIsDesignator := FALSE 
        ; OpExpr . ExpIsWritable := FALSE 
        ; OpExpr . ExpState := Est . EsResolved

        ; IF WOpnd1 . ExpIsConst AND WOpnd2 . ExpIsConst
          THEN
            OpExpr . ExpIsConst := TRUE
          ; IF Opcode = Stk . StkRwOR
            THEN 
              OpExpr . ExpScalarConstVal
                := FM3CTIntArith . Or
                     ( WOpnd1 . ExpScalarConstVal
                     , WOpnd2 . ExpScalarConstVal
                     )
            ELSE 
              OpExpr . ExpScalarConstVal
                := FM3CTIntArith . And
                     ( WOpnd1 . ExpScalarConstVal
                     , WOpnd2 . ExpScalarConstVal
                     )
            END (*IF*) 
          ELSIF ReqdConstPos # PositionNull 
          THEN
            FM3Messages . ErrorArr
              ( ARRAY OF REFANY
                  { "Expression at "
                  , FM3Utils . PositionImage ( ReqdConstPos )
                  , " must be constant/ "
                  }
                , OpExpr . ExpPosition 
              )
          ELSE OpExpr . ExpIsConst := FALSE
          END (*IF*) 
        END (*IF*) 
      END (*WITH*)
    END ResolveBoolBinOp

; PROCEDURE ResolveNumBinOp
    ( OpExpr : FM3Exprs . Expr2OpndTyp
    ; Opcode : FM3Exprs . OpcodeTyp
    ; ReqdConstPos : tPosition 
    )

  = VAR LOk : BOOLEAN := FALSE
  ; VAR LLeftAssignableType : FM3Exprs . ExprTyp 
  ; VAR LRightAssignableType : FM3Exprs . ExprTyp
  ; VAR LExprState1 : FM3Exprs . ExprStateTyp 
  ; VAR LExprState2 : FM3Exprs . ExprStateTyp 

  ; BEGIN
      WITH WOpnd1= OpExpr . ExpOpnd1
           , WOpnd2= OpExpr . ExpOpnd2
      DO 
        LExprState1 := FM3Exprs . ResolveNow ( WOpnd1 , WOpnd1 . ExpKind )
      ; LExprState2 := FM3Exprs . ResolveNow ( WOpnd2 , WOpnd2 . ExpKind )
      ; IF NOT WOpnd1 . ExpIsUsable
        THEN LOk := FALSE 
        ELSIF NOT FM3Exprs . IsNumType ( WOpnd1 . ExpType )
        THEN 
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY 
                { "Left operand of \""
                , FM3SrcToks . Image ( Opcode )
                , "\" must have a numeric type." 
                }
            , WOpnd1 . ExpPosition 
            ) 
        ; LOk := FALSE
        ELSE LLeftAssignableType := MaxAssignableType ( WOpnd1 . ExpType ) 
        END (*IF*)  

      ; IF NOT WOpnd2 . ExpIsUsable
        THEN LOk := FALSE 
        ELSIF NOT IsNumType ( WOpnd2 . ExpType )
        THEN 
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY 
                { "Right operand of \""
                , FM3Predefs . Image ( Opcode )
                , "\" must have a numeric type." 
                }
            , WOpnd2 . ExpPosition 
            ) 
        ; LOk := FALSE
        ELSE LRightAssignableType
             := FM3TypeUtils . MaxAssignableType ( WOpnd2 . ExpType ) 
        END (*IF*)

      ; IF NOT FM3TypeUtils . EqualTypes
                 ( LLeftAssignableType , LRightAssignableType )
        THEN
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Operands of "
                , FM3Predefs . Image ( Opcode )
                , " must be assignable to a common type." 
                }
           , OpExpr . ExpPosition
           ) 
        ; LOk := FALSE
        END (*IF*) 
      ; OpExpr . ExpIsUsable := LOk
      ; IF LOk
        THEN 
          OpExpr . ExpType := LLeftAssignableType
        ; OpExpr . ExpKind := Ekt . EkValue
        ; OpExpr . ExpIsUsable := TRUE 
        ; OpExpr . ExpIsLegalRecursive := TRUE
        ; OpExpr . ExpIsDesignator := FALSE 
        ; OpExpr . ExpIsWritable := FALSE 
        ; OpExpr . ExpState := Est . EsResolved

        ; IF WOpnd1 . ExpIsConst AND WOpnd2 . ExpIsConst
          THEN
            OpExpr . ExpIsConst := TRUE
          ; IF Opcode = FM3SrcToks . StkRwOR
            THEN 
              OpExpr . ExpScalarConstVal
                := FM3CTIntArith . Or
                     ( WOpnd1 . ExpScalarConstVal
                     , WOpnd2 . ExpScalarConstVal
                     )
            ELSE 
              OpExpr . ExpScalarConstVal
                := FM3CTIntArith . And
                     ( WOpnd1 . ExpScalarConstVal
                     , WOpnd2 . ExpScalarConstVal
                     )
            END (*IF*) 
          ELSIF NOT OpExpr . ExpIsConst AND ReqdConstPos # PositionNull 
          THEN
            FM3Messages . ErrorArr
              ( ARRAY OF REFANY
                  { "Expression at "
                  , FM3Utils . PositionImage ( ReqdConstPos )
                  , " must be constant/ "
                  }
              , OpExpr . ExpPosition 
              )
          ; OpExpr . ExpIsUsable := FALSE
          ELSE OpExpr . ExpIsConst := FALSE
          END (*IF*) 
        END (*IF*) 
      END (*WITH*)
    END ResolveNumBinOp

; PROCEDURE Resolve1Opnd
    ( Expr : FM3Exprs . Expr1OpndTyp ; ExprKind : FM3Exprs . ExprKindTyp )

  = VAR LExprState : FM3Exprs . ExprStateTyp

  ; BEGIN
      WITH WOpnd = Expr . ExpOpnd1
      DO 
        FM3Exprs . ResolveNow ( WOpnd , ExprKind )
      ; CASE Expr . ExpOpcode OF
        | Stk . StkRwNOT
        => ResolveNot ( Expr , WOpnd )
        | Stk . StkUnaryPlus 
        , Stk . StkUnaryMinus 
        => LExprState := Expr . ExpOpnd1 . resolve ( ExprKind )  
        END (*CASE*)
      END (*WITH*)
    END Resolve1Opnd










; PROCEDURE LookupAtomInOpenScopes
    ( IdAtom : FM3Base . AtomTyp ) : FM3Globals . DeclNoTyp  
  (* In nearest enclosing open scope on open scope stack. *) 

  = VAR LScopeRef : FM3Scopes . ScopeRefTyp 
  ; VAR LDeclNoInt : INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupAtomInOpenScopes*)
      LScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; LOOP
        IF LScopeRef = NIL THEN RETURN FM3Globals . DeclNoNull END (*IF*) 
      ; IF NOT LScopeRef ^ . ScpKind IN FM3Scopes . ScopeKindSetOpen
           (* ^Can this happen? *) 
        THEN (* Skip over this scope. *)
          LScopeRef := LScopeRef ^ . ScpOpenScopeStackLink
        ELSIF IntSets . IsElement ( IdAtom , LScopeRef ^ . ScpDeclIdSet )
        THEN (* It's declared in this scope. *) 
          TRY 
            LFound
              := FM3Dict_Int_Int . LookupFixed
                   ( LScopeRef ^ . ScpDeclDict
                   , IdAtom
                   , FM3Base . HashNull
                   , (*OUT*) LDeclNoInt 
                   )
          EXCEPT FM3Dict_Int_Int . Error ( <*UNUSED*> EMsg )
          => LFound := FALSE
          ELSE LFound := FALSE
          END (*EXCEPT*)
        ; <* ASSERT LFound *>
          RETURN VAL ( LDeclNoInt , FM3Globals . DeclNoTyp ) 
        ELSE (* Try the next outer scope. *) 
          LScopeRef := LScopeRef ^ . ScpOpenScopeStackLink
        END (*IF*) 
      END (*LOOP*) 
    END LookupAtomInOpenScopes

; PROCEDURE DuplDeclIdL2R ( )
  : FM3Globals . DeclNoTyp
  (* Append a temporary, pseudo-decl node to the linked list rooted at
     the decl number.  The position of the original declaration of
     the ident, which is needed for the error message, is not known yet.
  *) 

  = VAR DdiAtom : FM3Base . AtomTyp
  ; VAR DdiPosition : tPosition

  ; PROCEDURE Visit ( DeclNoI : INTEGER ; VAR (* IN OUT *) DeclRefany : REFANY )
    (* A callback. *) 
    (* PRE: DeclNoI IN FM3Globals . DeclNoTyp *)  
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
 
    = VAR LOldDeclRef : FM3Decls . DeclRefTyp  
    ; VAR LNewDeclRef : FM3Decls . DeclRefTyp  

    ; BEGIN
      (* Create a new decl object with DclKind DkDuplDecl and put it on
         the front of a linked list rooted at DeclRefAny.
      *) 
        LOldDeclRef := DeclRefany (* Implicit NARROW. *) 
      ; LNewDeclRef
          := FM3Decls . NewDeclRef
               ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
      ; LNewDeclRef . DclLink := LOldDeclRef 
      ; LNewDeclRef . DclSelfScopeRef
          := FM3Scopes . DeclScopeStackTopRef (* Why not? *)
      ; LNewDeclRef . DclIdAtom := DdiAtom 
      ; LNewDeclRef . DclPos := DdiPosition 
      ; LNewDeclRef . DclKind := FM3Decls . DeclKindTyp . DkDuplDecl
      (* Let's not put it into the map. 
      ; VarArray_Int_Refany . Assign
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap
          , (* Implicit NARROW. *) DeclNoI
          , LNewDeclRef
          )
      *) 
      END Visit

  ; BEGIN (* DuplDeclIdL2R *) 
      VAR LDeclNo : FM3Globals . DeclNoTyp
    ; BEGIN (* Block. *)
        DdiAtom := GetBwdAtom ( P3RdBack )
      ; DdiPosition := GetBwdPos ( P3RdBack )
      ; LDeclNo
          := LookupDeclNoInScope
               ( FM3Scopes . DeclScopeStackTopRef ^ , DdiAtom )
      ; <*ASSERT LDeclNo # FM3Globals . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdL2R
    
; PROCEDURE DeclIdL2R ( ) : FM3Globals . DeclNoTyp
  (* ^This will be the only decl of DeclIdAtom in its scope. *) 

  = VAR DidAtom : FM3Base . AtomTyp
  ; VAR DidOpenDeclNo : FM3Globals . DeclNoTyp
  ; VAR DidPosition : tPosition
  ; VAR DidDeclKind : FM3Decls . DeclKindTyp

  ; PROCEDURE DidVisitRefNo ( RefNoI : INTEGER ) 
    (* A callback. *) 

    = VAR LScopeRef : FM3Scopes . ScopeRefTyp

    ; BEGIN
        LScopeRef := FM3Scopes . DeclScopeStackTopRef 
      ; FM3Graph . AddArc
          ( (*IN OUT*) LScopeRef ^ . ScpDeclGraph
          , DidOpenDeclNo - LScopeRef ^ . ScpMinDeclNo
            (* ^Bias to zero in graph. *) 
          , RefNoI - LScopeRef ^ . ScpMinDeclNo
          )
      END DidVisitRefNo 

  ; PROCEDURE DidVisitDecl
      ( DeclNoI : INTEGER ; VAR (* IN OUT *) DeclRefany : REFANY )
    (* A callback. *) 
    (* PRE: DeclNoI IN FM3Globals . DeclNoTyp *) 
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *)
    
    = VAR LDeclRef : FM3Decls . DeclRefTyp
    ; VAR LIdentText : TEXT

    ; BEGIN (* DidVisitDecl *)
        IF DeclRefany # NIL (* Some duplicate decls of DeclNoI also exist? *) 
        THEN (* Dispense with them with error messages. *) 
          LDeclRef := DeclRefany (* Implied NARROW. *)
        ; LIdentText := FM3Units . TextOfIdAtom ( DidAtom ) 
        ; WHILE LDeclRef # NIL
          DO
            FM3Messages . ErrorArr
              ( ARRAY OF REFANY
                  { "Duplicate declaration of \""
                  , LIdentText
                  , "\", ignored, original at "
                  , FM3Utils . PositionImage ( DidPosition )
                  , "." 
                  }
              , LDeclRef . DclPos 
              )
          ; LDeclRef := LDeclRef ^ . DclLink
          END (*WHILE*) 
        END (*IF*)

      (* Now handle the original/only declaration. *) 
      ; LDeclRef
          := FM3Decls . NewDeclRef
               ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
      ; LDeclRef ^ . DclLink := NIL 
      ; LDeclRef ^ . DclSelfScopeRef := FM3Scopes . DeclScopeStackTopRef 
(* TODO: Set the reverse link ScpOwningDeclNo. *) 
      ; LDeclRef ^ . DclIdAtom := DidAtom
      ; LDeclRef ^ . DclPos := DidPosition 
      ; LDeclRef ^ . DclKind := DidDeclKind
      ; LDeclRef ^ . DclDefValue
          := FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefExprs [ TRUE ]  
      ; LDeclRef ^ . DclDefType 
          := FM3Scopes . DeclScopeStackTopRef ^. ScpCurDefExprs [ FALSE ] 
          
      ; CASE DidDeclKind OF
        | Dkt . DkVar
        =>  <* ASSERT FM3Scopes . DeclScopeStackTopRef ^. ScpKind
                      IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkConst
        =>  <* ASSERT FM3Scopes . DeclScopeStackTopRef ^. ScpKind
                      IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkType                        
        , Dkt . DkMethod
        , Dkt . DkProc
        , Dkt . DkReveal 
        =>  <* ASSERT FM3Scopes . DeclScopeStackTopRef ^. ScpKind
                      IN FM3Scopes . ScopeKindSetOpen
            *>
            (* These have a required type on the expression stack. *)
            <* ASSERT LDeclRef ^. DclDefType # NIL *> 

        | Dkt . DkVALUEFormal
        , Dkt . DkVARFormal
        , Dkt . DkROFormal
        , Dkt . DkRecField
        , Dkt . DkObjField
        =>  <* ASSERT NOT FM3Scopes . DeclScopeStackTopRef ^. ScpKind
                          IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkExc
        , Dkt . DkEnumLit
        =>  <* ASSERT NOT FM3Scopes . DeclScopeStackTopRef ^. ScpKind
                          IN FM3Scopes . ScopeKindSetOpen
            *>
(* COMPLETEME *)
            
        | Dkt . DkNull
        , Dkt . DkDuplDecl
        , Dkt . DkMod 
        , Dkt . DkIntf 
        , Dkt . DkGenMod
        , Dkt . DkGenIntf
        , Dkt . DkExports (* EXPORTS list is declaration-ish. *) 
        , Dkt . DkWith
        , Dkt . DkFor
        , Dkt . DkExcArg
        => <* ASSERT FALSE *>
        END (*CASE*)
      ; IF FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
           IN FM3Scopes . ScopeKindSetOpen 
        THEN
          IntSets . ForAllDo (* All the relevant references to declared Ident. *)
            ( FM3Scopes . OpenScopeStackTopRef ^ . ScpCurDeclRefNoSet
            , DidVisitRefNo
            )
        END (*IF*) 
      ; DeclRefany := LDeclRef 
      END DidVisitDecl

  ; BEGIN (*DeclIdL2R*)
      DidDeclKind := GetBwdDeclKind ( P3RdBack )
    ; DidAtom := GetBwdAtom ( P3RdBack )
    ; DidPosition := GetBwdPos ( P3RdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN FM3Globals . DeclNoNull 
      END (*IF*) 

    ; DidOpenDeclNo
        := LookupDeclNoInScope
             ( FM3Scopes . DeclScopeStackTopRef ^ , DidAtom ) 
    ; <*ASSERT DidOpenDeclNo # FM3Globals . DeclNoNull *>
      VarArray_Int_Refany . CallbackWithElem 
        ( FM3Units . UnitStackTopRef ^ . UntDeclMap
        , DidOpenDeclNo
        , DidVisitDecl
        )
    ; PutBwdP3 ( P3RdBack , VAL ( DidPosition . Column , LONGINT ) ) 
    ; PutBwdP3 ( P3RdBack , VAL ( DidPosition . Line , LONGINT ) ) 
    ; PutBwdP3 ( P3RdBack , VAL ( DidOpenDeclNo , LONGINT ) ) 
    ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
    ; RETURN DidOpenDeclNo
    END DeclIdL2R

; PROCEDURE BadIdentMessage
    ( Msg : TEXT
    ; IdentAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ) 

  = BEGIN 
      FM3Messages . ErrorArr
         ( ARRAY OF REFANY 
             { Msg
             , " \""
             , FM3Units . TextOfIdAtom ( IdentAtom )
             , "\", (2.1)." 
             }
         , Position 
         )
    END BadIdentMessage

; PROCEDURE PutNotUsable 
    ( IdentRefAtom : FM3Base . AtomTyp ; READONLY Position : FM3Base . tPosition )

  = BEGIN
      IF AreInsideADecl ( )
      THEN PushExprIgnore ( Position )
      ELSE 
        PutBwdP3 ( P3RdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwdP3 ( P3RdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwdP3 ( P3RdBack , VAL ( IdentRefAtom , LONGINT ) ) 
      ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkIdRefAtomNotUsable , LONGINT ) )
      END (*IF*) 
    END PutNotUsable

; <*INLINE*> PROCEDURE AreInsideADecl ( ) : BOOLEAN

  = VAR LOpenScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      LOpenScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; IF LOpenScopeRef = NIL THEN RETURN FALSE END (*IF*)
    ; RETURN LOpenScopeRef ^ . ScpInsideDecl  
    END AreInsideADecl

; PROCEDURE CheckRecursiveRef ( RefDeclNo : FM3Globals . DeclNoTyp )

  = VAR LOpenScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LScopeMinDeclNo : FM3Globals . DeclNoTyp 

  ; BEGIN
      IF RefDeclNo < 0 THEN RETURN END (*IF*) 
    ; LOpenScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; IF LOpenScopeRef = NIL THEN RETURN END (*IF*)
    ; LScopeMinDeclNo := LOpenScopeRef . ScpMinDeclNo
    ; IF RefDeclNo < LScopeMinDeclNo THEN RETURN END (*IF*) 
    ; IF RefDeclNo >= LScopeMinDeclNo + LOpenScopeRef ^ . ScpDeclCt
      THEN RETURN
      END (*IF*)
    (* The ref is to the current open scope.  Would it be legal in a recursion? *) 
    ; TYPECASE FM3Exprs . ExprStackTopObj OF 
      NULL => 
      | FM3Exprs . ExprTyp ( TRefExpr ) 
      => IF NOT TRefExpr . ExpIsLegalRecursive
         THEN 
           WITH WRefIdNoSet
                = FM3Scopes . OpenScopeStackTopRef ^ . ScpCurDeclRefNoSet
           DO WRefIdNoSet := IntSets . Include ( WRefIdNoSet , RefDeclNo ) 
           END (*WITH*)
         END (*IF*)
      ELSE 
      END (*TYPECASE*) 
    END CheckRecursiveRef

; PROCEDURE ReservedIdL2R ( )

  = VAR LReservedId : Stk . TokTyp 
  ; VAR LPosition : FM3Base . tPosition 

  ; BEGIN
      LReservedId := GetBwdAtom ( P3RdBack )
    ; LPosition := GetBwdPos ( P3RdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN 
      END (*IF*)
    ; CASE LReservedId OF

      (* Functions with one parameter: *) 
      | Stk . RidABS 
      , Stk . RidADR 
      , Stk . RidADRSIZE 
      , Stk . RidBITSIZE 
      , Stk . RidBYTESIZE 
      , Stk . RidCEILING 
      , Stk . RidFIRST 
      , Stk . RidFLOAT 
      , Stk . RidFLOOR 
      , Stk . RidISTYPE 
      , Stk . RidLAST 
      , Stk . RidNUMBER 
      , Stk . RidORD 
      , Stk . RidROUND 
      , Stk . RidTRUNC 
      , Stk . RidTYPECODE 
      => 

      (* Functions with two parameters: *)
      | Stk . RidLOOPHOLE 
      , Stk . RidMAX 
      , Stk . RidMIN 
      , Stk . RidNARROW
      , Stk . RidVAL
      => 

      (* Functions with a variable number of parameters: *)
      | Stk . RidNEW 
      , Stk . RidSUBARRAY 
      => 
      
      (* Constants: *) 
      | Stk . RidFALSE 
      , Stk . RidNIL 
      , Stk . RidTRUE 
      => 

      (* Types: *)  
      | Stk . RidADDRESS 
      , Stk . RidBOOLEAN 
      , Stk . RidCARDINAL 
      , Stk . RidCHAR 
      , Stk . RidEXTENDED 
      , Stk . RidINTEGER 
      , Stk . RidLONGCARD 
      , Stk . RidLONGINT 
      , Stk . RidLONGREAL 
      , Stk . RidMUTEX 
      , Stk . RidNULL 
      , Stk . RidREAL 
      , Stk . RidREFANY 
      , Stk . RidTEXT 
      , Stk . RidWIDECHAR 
      , Stk . RidROOT 
      , Stk . RidUNTRACEDROOT 
      => 

      (* Procedures: *) 
      | Stk . RidDEC 
      , Stk . RidDISPOSE 
      , Stk . RidINC 
      => 

      END (*CASE*) 
    END ReservedIdL2R 

; PROCEDURE IdentRefL2R ( )
  (* PRE: The ident is not followed by dot Ident. (The parser has gone
          to some trouble to ensure this.)
  *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LExpImpUnitRef : FM3Units . UnitRefTyp
  ; VAR LPass2RdBack : RdBackFile . T 
  ; VAR LIdentRefAtom : FM3Base . AtomTyp 
  ; VAR LUnitNo : FM3Globals . UnitNoTyp
  ; VAR LRefDeclNo : FM3Globals . DeclNoTyp
  ; VAR LPosition : FM3Base . tPosition 
  ; VAR LIntfNameChars : FM3Atom_OAChars . KeyTyp
  ; VAR LIsUsable : BOOLEAN 

  ; BEGIN (*IdentRefL2R*)
      LUnitRef := FM3Units . UnitStackTopRef
    ; LPass2RdBack := LUnitRef ^ . UntPass2OutRdBack 
    ; LIdentRefAtom := GetBwdAtom ( LPass2RdBack )
    ; LPosition := GetBwdPos ( LPass2RdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN 
      END (*IF*)
      
    (* Look for a reference to a decl in an enclosing* open scope. *) 
    ; IF LIdentRefAtom < 0
      THEN LRefDeclNo := LIdentRefAtom
      ELSE LRefDeclNo := LookupAtomInOpenScopes ( LIdentRefAtom )
      END (*IF*) 
    ; IF LRefDeclNo # FM3Globals . DeclNoNull 
      THEN 
        IF AreInsideADecl ( )
(* CHECK: Is this precluded by syntactic context? *) 
        THEN (* Create an ExprIdentReference node. *) 
          CheckRecursiveRef ( LRefDeclNo )
        ; WITH WExpr
               = NEW ( FM3Exprs . ExprIdentReference , ExpUpKind := Ekt . EkRef )
          DO
            WExpr . ExpIdentDeclNo := LRefDeclNo 
          ; WExpr . ExpPosition := LPosition
          ; WExpr . ExpUpKind := Ekt . EkRef
          ; WExpr . ExpIsUsable := TRUE
          ; DefExprLt ( WExpr )
          END (*WITH*)
        ELSE (* Change to a reference token with DeclNo instead of Atom. *)
          PutBwdP3 ( P3RdBack , VAL ( LPosition . Column , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LPosition . Line , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LRefDeclNo , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )
        END (*IF*)

      (* Look for something [ex|im]ported. *) 
      ELSIF LookupAtomExpImp
              ( LIdentRefAtom , (*OUT*) LUnitNo , (*OUT*) LRefDeclNo )
      THEN (* Export or import is present. *) 
        IF LUnitNo = FM3Globals . UnitNoNull (* But not usable. *) 
        THEN LIsUsable := FALSE
(* CHECK: ^v Which of these ways denoting unusability can happen? *) 
        ELSE
          LExpImpUnitRef (* Implicit NARROW. *) 
            := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNo )
        ; LIsUsable := LExpImpUnitRef ^ . UntState # Ust . UsNotUsable
        END (*IF*)
      ; IF LIsUsable
        THEN (* LExpImpUnitRef names a usable interface. *)
          IF LRefDeclNo = FM3Globals . DeclNoNull
          THEN (* Interface name w/o a selection--illegal. *) 
            IF NOT FM3Atom_OAChars . Key
                     ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                     , LIdentRefAtom
                     , (*OUT*) LIntfNameChars
                     )
            THEN LIntfNameChars := NIL
            END (*IF*) 
          ; FM3Messages . ErrorArr
              ( ARRAY OF REFANY
                  { "No declaration within imported interface \""
                  , LIntfNameChars
                  , "\" is selected. (2.6.3)."
                  }
              , LPosition
              ) 
          ; PutNotUsable( LIdentRefAtom , LPosition ) 
          ELSE (* A remote decl brought in by EXPORTS or FROM I IMPORT. *)
            IF AreInsideADecl ( )   
            THEN (* Create an ExprIdNo node. *) 
              WITH WExpr = NEW ( FM3Exprs . ExprRemoteRef )
              DO 
                WExpr . ExpRemoteUnitNo := LUnitNo 
              ; WExpr . ExpRemoteDeclNo := LRefDeclNo 
              ; WExpr . ExpPosition := LPosition
              ; WExpr . ExpUpKind := Ekt . EkRef
              ; WExpr . ExpIsUsable := TRUE
              ; DefExprLt ( WExpr )
              END (*WITH*)
            ELSE (* Emit a token. *)  
            (* Read the following backwards: *) 
              PutBwdP3 ( P3RdBack , VAL ( LPosition . Column , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LPosition . Line , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LRefDeclNo , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LUnitNo , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkExpImpRef , LONGINT ) )
            END (*IF*) 
          END (*IF*)
        ELSE (* It was already unusable. *) 
          PutNotUsable( LIdentRefAtom , LPosition ) 
        END (*IF*) 
      ELSE (* Undeclared. *) 
        BadIdentMessage ( "Undeclared identifier" , LIdentRefAtom , LPosition )
      ; PutNotUsable( LIdentRefAtom , LPosition ) 
      END (*IF*)
    ; DefExprRt ( ) 
    END IdentRefL2R

; PROCEDURE QualIdentL2R ( Pass2RdBack : RdBackFile . T )
  (* (NON)PRE: No operands have been read. *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp 
  ; VAR LUnitNoRt : FM3Globals . UnitNoTyp
  ; VAR LRefDeclNoRt : FM3Globals . DeclNoTyp
  ; VAR LRefDeclNo : FM3Globals . DeclNoTyp
  ; VAR LAtomRt , LAtomLt : FM3Base . AtomTyp
  ; VAR LRemoteAtom : FM3Base . AtomTyp
  ; VAR LRemoteDeclNoInt : INTEGER 
  ; VAR LPosRt , LPosLt : FM3Base . tPosition

  ; BEGIN (*QualIdentL2R*)
      LAtomRt := GetBwdAtom ( Pass2RdBack ) 
    ; LAtomLt := GetBwdAtom ( Pass2RdBack ) 
    ; LPosRt := GetBwdPos ( Pass2RdBack ) 
    ; LPosLt := GetBwdPos ( Pass2RdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN 
      END (*IF*) 
      (* Look for a left reference to a decl in an enclosing open scope. *) 
    ; LRefDeclNoRt := LookupAtomInOpenScopes ( LAtomRt )
    ; IF LRefDeclNoRt # FM3Globals . DeclNoNull
      THEN (* Rt names a local declaration, not an interface. *)
        IF AreInsideADecl ( ) 
        THEN (* Create an ExprIdNo node. *) 
          CheckRecursiveRef ( LRefDeclNo )
        ; WITH WDotExpr = NEW ( FM3Exprs . ExprDot )
               , WRtExpr = NEW ( FM3Exprs . ExprIdentReference )
          DO 
            WDotExpr . ExpOpnd1 := WRtExpr 
          ; WDotExpr . ExpDotIdAtom := LAtomLt
          ; WDotExpr . ExpPosition := LPosLt
          ; WDotExpr . ExpUpKind := Ekt . EkRef
          ; WDotExpr . ExpIsUsable := TRUE
          ; DefExprLt ( WDotExpr )
          ; WRtExpr . ExpIdentDeclNo := LRefDeclNoRt 
          ; WRtExpr . ExpPosition := LPosRt
          ; WRtExpr . ExpUpKind := Ekt . EkRef
          ; WRtExpr . ExpIsUsable := TRUE
          ; DefExprLt ( WRtExpr )
          END (*WITH*)
        ; EVAL FM3Exprs . PopExprStack ( ) (* The expr *) 
        ELSE (* Emit tokens for a dot Id applied to a DeclNo Id reference. *)

        (* Turn the qualident into separate Id ref and dot Id. *)
          PutBwdP3 ( P3RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LAtomLt , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )

        ; PutBwdP3 ( P3RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LRefDeclNoRt , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )

        ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( LAtomLt , LONGINT ) ) 
        ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )
        END (*IF*) 

      (* Look for something [ex|im]ported. *) 
      ELSIF LookupAtomExpImp
              ( LAtomRt , (*OUT*) LUnitNoRt , (*OUT*) LRefDeclNoRt )
      THEN (* Rt ident is [ex|im]ported. *)  
        IF LUnitNoRt = FM3Globals . UnitNoNull
        THEN (* Unusable. *) 
          PutNotUsable( LAtomRt , LPosRt ) 
        ELSIF LRefDeclNoRt = FM3Globals . DeclNoNull
        THEN (* Rt names an imported interface. *)
          LIntfUnitRef (*Implicit NARROW*) 
            := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNoRt )
        ; <* ASSERT LIntfUnitRef # NIL *>
          LRemoteAtom
            := FM3Compile . ConvertIdentAtom
                 ( LAtomLt
                 , FromUnitRef := FM3Units . UnitStackTopRef
                 , ToUnitRef := LIntfUnitRef
                 )
        ; IF IntSets . IsElement
               ( LRemoteAtom , LIntfUnitRef ^ . UntExpImpIdSet )
          THEN (* Lt ident is imported into the remote interface,
                  not transitively importable.
               *)
            IF NOT FM3Atom_OAChars . Key
                     ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                     , LAtomLt 
                     , (*OUT*) LIdentChars
                     )
            THEN LIdentChars := NIL
            END (*IF*) 
          ; FM3Messages . ErrorArr
              ( ARRAY OF REFANY
                  { "Interface \""
                  , LIntfUnitRef ^ . UntUnitIdent
                  , "\" does not declare \""
                  , LIdentChars 
                  , "\" (2.6.3)"
                  , FM3ExpImp . NonTransitiveNote 
                  }
              , LPosLt
              )
          ; PutNotUsable ( LAtomLt , LPosLt )
          ELSIF IntSets . IsElement
                  ( LRemoteAtom
                  , LIntfUnitRef ^ . UntScopeRef ^ . ScpDeclIdSet
                  )
          THEN (* Right ident is declared in LIntfUnitRef. *) 
            <* ASSERT
                 FM3Dict_Int_Int . LookupFixed 
                   ( LIntfUnitRef ^ . UntScopeRef ^ . ScpDeclDict
                   , LRemoteAtom
                   , FM3Base . HashNull
                   , (*OUT*) LRemoteDeclNoInt 
                   )
            *>
            IF AreInsideADecl ( )
            THEN
(* TODO: Handle a Word.*, etc. builtin reference. *) 
              WITH WExpr = NEW ( FM3Exprs . ExprRemoteRef )
              DO 
                WExpr . ExpRemoteUnitNo := LIntfUnitRef ^ . UntSelfUnitNo 
              ; WExpr . ExpRemoteDeclNo := LRemoteDeclNoInt 
              ; WExpr . ExpPosition := LPosRt
              ; WExpr . ExpUpKind := Ekt . EkRef
              ; WExpr . ExpIsUsable := TRUE
              ; DefExprLt ( WExpr )
              END (*WITH*)
            ELSE 
              PutBwdP3 ( P3RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP3 ( P3RdBack , VAL ( LRemoteDeclNoInt , LONGINT ) ) 
            ; PutBwdP3
                ( P3RdBack , VAL ( LIntfUnitRef ^ . UntSelfUnitNo , LONGINT ) ) 
            ; PutBwdP3
                ( P3RdBack , VAL ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )
            END (*IF*) 
          ELSE (* Right ident is not known in left-named interface. *)
            IF NOT FM3Atom_OAChars . Key
                     ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                     , LAtomLt 
                     , (*OUT*) LIdentChars
                     )
            THEN LIdentChars := NIL
            END (*IF*) 
          ; FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Interface \""
                , LIntfUnitRef ^ . UntUnitIdent
                , "\" does not declare \""
                , LIdentChars 
                , "\" (2.6.3)"
                }
            , LPosLt
            )
          ; PutNotUsable ( LAtomLt , LPosLt ) 
          END (*IF*)
        ELSE (* Left Ident by itself denotes a remote decl, brought in
                by EXPORTS or FROM-IMPORT.
             *)
          IF AreInsideADecl ( )
          THEN 
          (* Turn it into separate QualId ref and dot Id *) 
            WITH WDotExpr = NEW ( FM3Exprs . ExprDot )
            , WRtExpr = NEW ( FM3Exprs . ExprRemoteRef )
            DO 
              WDotExpr . ExpDotIdAtom := LAtomLt
            ; WDotExpr . ExpOpnd1 := WRtExpr 
            ; WDotExpr . ExpPosition := LPosLt
            ; WDotExpr . ExpUpKind := Ekt . EkRef
            ; WDotExpr . ExpIsUsable := TRUE
            ; DefExprLt ( WDotExpr ) (* Which pushes. *) 
            ; WRtExpr . ExpRemoteUnitNo := LUnitNoRt 
            ; WRtExpr . ExpRemoteDeclNo := LRefDeclNoRt 
            ; WRtExpr . ExpPosition := LPosRt
            ; WRtExpr . ExpUpKind := Ekt . EkRef
            ; WRtExpr . ExpIsUsable := TRUE
            ; DefExprLt ( WRtExpr ) (* Which pushes. *)
            END (*WITH*)
          ; EVAL FM3Exprs . PopExprStack ( ) (* The RtExpr. *) 
          ELSE 
            PutBwdP3 ( P3RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LAtomLt , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )

          ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LRefDeclNoRt , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LUnitNoRt , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL
              ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )

          ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( LAtomLt , LONGINT ) ) 
          ; PutBwdP3 ( P3RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )
          END (*IF*) 
        ; RETURN 
        END (*IF*)
      ELSE (* Undeclared. *)
        BadIdentMessage ( "Undeclared identifier", LAtomRt , LPosRt ) 
      ; PutNotUsable ( LAtomLt , LPosLt ) 
      END (*IF*)
    END QualIdentL2R

(***********************************************************
    
*********************************************************************)

(*EXPORTED*) 
; PROCEDURE RunPass3 ( ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp  

  ; BEGIN (*RunPass3*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; InitPass3 ( LUnitRef ) 
    ; TranslatePass3 ( LUnitRef ) 
    ; FinishPass3 ( LUnitRef ) 
    END RunPass3

; PROCEDURE DisAsmPass3
    ( UnitRef : FM3Units . UnitRefTyp ; DoEarlierPasses : BOOLEAN )

  = BEGIN (*DisAsmPass3*)
      IF NOT FM3CLOptions . PassNo3 IN UnitRef ^ . UntPassNosDisAsmed 
      THEN (* Disassembly file is not already written. *) 
        FM3Compile . DisAsmPassFile
          ( UnitRef , FM3Globals . Pass3OutSuffix , L2R := TRUE )
      ; FM3CLOptions . InclPassNo
          ( UnitRef ^ . UntPassNosDisAsmed , FM3CLOptions . PassNo3 ) 
      END (*IF*) 
    ; IF DoEarlierPasses
      THEN
        FM3Pass2 . DisAsmPass2 ( UnitRef , DoEarlierPasses := TRUE )
      END (*IF*) 
    END DisAsmPass3

; PROCEDURE InitPass3 ( UnitRef : FM3Units . UnitRefTyp )

  = VAR LPass3FileFullName : TEXT
  
  ; BEGIN (*InitPass3*)

    (* Create the pass 3 output file. *)
      TRY (*EXCEPT*)
        UnitRef ^ . UntPass3OutSimpleName
          := Pathname . Join
               ( NIL 
               , UnitRef ^ . UntSrcFileSimpleName
               , FM3Globals . Pass3OutSuffix
               ) 
      ; LPass3FileFullName
          := Pathname . Join
               ( UnitRef ^ . UntBuildDirPath
               , UnitRef ^ . UntPass3OutSimpleName 
               , NIL 
               )
      ; UnitRef ^ . UntPass3OutRdBack
          := RdBackFile . Create ( LPass3FileFullName , Truncate := TRUE )
      EXCEPT
      | OSError . E ( EMsg ) 
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to create pass 3 output file \""
               , LPass3FileFullName 
               , "\": "
               , FM3Messages . AtomListToOSError ( EMsg)
               , "."
               } 
           ) 
      END (*EXCEPT*)

    (* Write pass 3 output file initial tokens. *)
    ; PutBwdP3
        ( UnitRef ^ . UntPass3OutRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwdP3
        ( UnitRef ^ . UntPass3OutRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; UnitRef ^ . UntPass3OutEmptyCoord
        := RdBackFile . LengthL ( UnitRef ^ . UntPass3OutRdBack )  
    END InitPass3

; PROCEDURE TranslatePass3 ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN (*TranslatePass3*)
  
    (* Write the Pass3 RdBack. *)
      TRY 
        Pass3Tokens ( UnitRef ^ . UntPass3OutEmptyCoord )

      (* Write final successful pass 3 output tokens. *)
      ; PutBwdP3
          ( UnitRef ^ . UntPass3OutRdBack , VAL ( Itk . ItkRightEnd , LONGINT ) )
      ; PutBwdP3
          ( UnitRef ^ . UntPass3OutRdBack , VAL ( Itk . ItkEOF , LONGINT ) )

      (* Prepare for possible disassembly later. *) 
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass3OutSuffix
          , UnitRef ^ . UntPass3OutRdBack
          )
        (*^ This copy may be used by disassembly called for by command-line
            option, a later pass failure, or not at all. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE (* pass 3 failed. *) 

      (* Disassemble what there is of the failed file. *)
        PutBwdP3
          ( UnitRef ^ . UntPass3OutRdBack
          , VAL ( Itk . ItkRightEndIncomplete , LONGINT ) 
          )
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass3OutSuffix
          , UnitRef ^ . UntPass3OutRdBack
          )
      ; DisAsmPass3 ( UnitRef , DoEarlierPasses := TRUE )

      ; FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure writing pass 3 output at depth "
               , Fmt . LongInt
                   ( RdBackFile . LengthL ( UnitRef ^ . UntPass3OutRdBack ) )
               , FM3Messages . NLIndent 
               , "Exception "
               , FM3RTFailures . ExcNameFromAddr
                   ( Compiler . ThisException ( ) )
               , ","
               , FM3Messages . NLIndent 
               , "raised at "
               , FM3RTFailures . ActivationLocationFromAddr
                   ( Compiler . ThisException ( ) )
               , "."
               }
           )
      END (*EXCEPT *)
    END TranslatePass3
  
; PROCEDURE FinishPass3 ( UnitRef : FM3Units . UnitRefTyp )

  = VAR LPass3FullFileName : TEXT 
  ; VAR LLengthImage : TEXT
  ; VAR LLengthL : LONGINT 

  ; BEGIN (*FinishPass3*)
    (* Close pass 3. *) 
      UnitRef ^ . UntPass3Result := 0 
      
    (* Finish with and close pass 2 output. *)
    ; LLengthL := RdBackFile . LengthL ( UnitRef ^ . UntPass2OutRdBack )
    ; RdBackFile . Close 
        (  UnitRef ^ . UntPass2OutRdBack , - 1L (* Leave full length. *) )
    ; IF LLengthL # UnitRef ^ . UntPass2OutEmptyCoord 
      THEN
        UnitRef ^ . UntPass3Result := FM3CLArgs . CcPass2OutNotEmpty  
      ; DisAsmPass3 ( UnitRef , DoEarlierPasses := TRUE )
      ; LLengthImage := FM3Base . Int64Image ( LLengthL ) 
      ; FatalArr
          ( ARRAY OF REFANY
              { "Pass 2 output file final size = "
              , LLengthImage
              , ", should be "
              , FM3Base . Int64Image ( UnitRef ^ . UntPass2OutEmptyCoord )
              , "."
              }
          )
      END (*IF*)

(* This is now deferred until compile cleanup: 
    ; IF NOT FM3Base . PassNo2 IN FM3CLArgs . PassNosToKeep 
      THEN 
        FM3SharedUtils . DeleteFile
          ( Pathname . Join
              ( UnitRef ^ . UntBuildDirPath 
              , UnitRef ^ . UntPass2OutSimpleName
              , NIL 
              )
          ) 
      END (*IF*)
*) 

    (* Report size and maybe disassemble pass 3 output file. *) 
    ; LPass3FullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPass3OutSimpleName
             , NIL
             )
    ; UnitRef ^ . UntMaxPass3OutLength
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPass3OutRdBack )
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "pass 3 output file "
            , UnitRef ^ . UntPass3OutSimpleName
            , " has "
            , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPass3OutLength )
            , " bytes."
            } 
        )
    ; IF FM3CLOptions . PassNo3 IN FM3CLOptions . PassNosToDisAsm 
      THEN DisAsmPass3 ( UnitRef , DoEarlierPasses := FALSE )
      END (*IF*)

    (* Finish with the skip stack. *) 
    ; <* ASSERT
           VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi 
           = UnitRef ^ . UntSkipStackBase 
      *> 
    END FinishPass3

; BEGIN (*FM3Pass3*)

END FM3Pass3
.

