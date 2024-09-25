
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Pass2

; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT IntSets 
; IMPORT OSError
; IMPORT Pathname
; IMPORT Thread 
; IMPORT UniRd

; IMPORT RangeUtils 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 
; IMPORT VarArray_Int_ExpImpProxy 
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base 
; FROM FM3Base IMPORT tPosition 
; IMPORT FM3CLArgs
; IMPORT FM3CLOptions  
; IMPORT FM3Compile
; IMPORT FM3Compress
; FROM FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Exprs  
; IMPORT FM3Dict_Int_Int
; IMPORT FM3ExpImp
; IMPORT FM3ExpImpProxy 
; IMPORT FM3RTFailures 
; IMPORT FM3Globals
; IMPORT FM3Graph 
; IMPORT FM3IntToks AS Itk
; FROM FM3StreamUtils
    IMPORT GetBwdInt , GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT RdBackFile 

; TYPE Ekt = FM3Exprs . ExprKindTyp 
; TYPE Ust = FM3Units . UnitStateTyp
; TYPE Lot = FM3Base . LoTypeTyp 

; CONST PosImage = FM3Utils . PositionImage

; PROCEDURE PutBwdP2 ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd for pass 2 output file. 
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN (*PutBwdP2*) 
      <* ASSERT RdBack = FM3Globals . P2RdBack *> 
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
    END PutBwdP2

; PROCEDURE PutBwdPatch ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd for patch stack 
     Catch OSError.E.
  *)

  = BEGIN (*PutBwdPatch*) 
      <* ASSERT RdBack = FM3Globals . PatchRdBack *> 
      TRY
        FM3Compress . PutBwd ( RdBack , ValueL ) 
      EXCEPT OSError . E ( EMsg )
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to write to patch file \""
               , RdBackFile . FileName ( RdBack )
               , "\", " 
               , FM3Messages . AtomListToOSError ( EMsg ) , "."  
               }
           ) 
      END (*EXCEPT*) 
    END PutBwdPatch

; PROCEDURE SkipRt ( SkipNo : INTEGER )
  (* Pass 2, initial (right) end of a range of conditionally skipped tokens. *)

  = BEGIN 
      WITH WSkipNoStack = FM3Globals . SkipNoStack
      DO VarArray_Int_Int . Assign
          ( WSkipNoStack
          , VarArray_Int_Int . TouchedRange ( WSkipNoStack ) . Hi + 1
          , SkipNo
          )
         (* ^Effectively a push, where touched Hi is stack Ss. *)
      (* And discard the ItkSkipRt token. *)
      END (*WITH*)
    END SkipRt 

; PROCEDURE SkipLt ( SkipNo : INTEGER )
  (* Pass 2 final (left) end of a range of conditionally skipped tokens. *)

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
     (* And discard the ItkSkipLt token. *) 
      END (*WITH*)
    END SkipLt

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

    ( OpndCt : [ 0 .. 6 ] 
    ; FromRdBack , ToRdBack : RdBackFile . T
    ; MaybeSkip : BOOLEAN (* ToRdBack is conditional on SkipNoStack. *) 
    )
  (* Copy operands, up to 6. This is could be confusing.  One-at-a-time pop,
     push temporally reverses them left-to-right.  But if the caller is copying
     to a RdBack that is to be read in the opposite direction, that will
     implicitly reverse them, for net same temporal order when the next pass
     reads them. 
  *)
  
  = BEGIN
      IF MaybeSkip
         AND VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
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
    ( OpndCt : [ 0 .. 6 ] 
    ; FromRdBack , ToRdBack : RdBackFile . T
    ; MaybeSkip : BOOLEAN (* ToRdBack is conditional on SkipNoStack. *) 
    )
  (* This is tricky.  Pop, push reverses them, but this procedure does
     a compensating reverse, for net same order. 
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*CopyOperandsInOrder*)
<* ASSERT MaybeSkip = ( ToRdBack = FM3Globals . P2RdBack ) *> 
      IF MaybeSkip
         AND VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
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

; TYPE TokResultTyp
    = RECORD
        TrRdBack : RdBackFile . T (* From which to get its operands. *) 
      ; TrTok : Itk . TokTyp (* Just the Itk code.*) 
      END 

; PROCEDURE GetTokCode
    ( LMPass1Depth : LONGINT ; VAR (*OUT*) Result : TokResultTyp )
  (* Return a token code and the RdBack it came from.  Callers will
     get its varying complement of arguments, from the RdBack returned.
  *) 

  = VAR LPass1Coord : LONGINT
  ; VAR LPatchStackTopCoord : LONGINT
  ; VAR LTokenL  : LONGINT 
  ; VAR LPatchedTokenL : LONGINT 
  ; VAR LToken : Itk . TokTyp
  ; VAR LPatchedToken : Itk . TokTyp
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPass1RdBack : RdBackFile . T 
  ; VAR LPatchRdBack : RdBackFile . T

  ; BEGIN (* GetTokCode *) 
      LUnitRef := FM3Units . UnitStackTopRef
    (* ItkSkip[Lt|Rt] pairs are inserted during pass 1 and acted-on in pass 2.*)
    ; LPass1RdBack := LUnitRef . UntPass1OutRdBack 
    ; LPatchRdBack := LUnitRef . UntPatchStackRdBack 
    ; LMPass1Depth := MAX ( LMPass1Depth , LUnitRef . UntPass1OutEmptyCoord )

    ; LOOP (* Thru' a sequence of SkipRt & SkipLt tokens plus one other. *)  
        LPass1Coord := RdBackFile . LengthL ( LPass1RdBack )
      ; LPatchStackTopCoord := FM3Compress . GetBwd ( LPatchRdBack ) 
      ; IF LPass1Coord <= LMPass1Depth
           (* ^Nothing more to read from the Pass1 file. *) 
           AND RdBackFile . LengthL ( LPatchRdBack )
               <= LUnitRef ^ . UntPatchStackEmptyCoord
           (* ^ Nothing more to pop off Patch stack. *) 
        THEN (* Done with the entire file. *)
          <* ASSERT 
               RdBackFile . LengthL ( LPatchRdBack )
               = LUnitRef ^ . UntPatchStackEmptyCoord 
          *>
          PutBwdPatch ( LPatchRdBack , LPatchStackTopCoord )
            (* ^Push the current patch coordinate back on patch stack, just
               for stack consistency. *)
        ; Result . TrRdBack := NIL
        ; Result . TrTok := Itk . ItkBOF
        ; RETURN 
        END (*IF*)
        
      (* More to be done.  One of three possible actions. *)
      (* Check first for an already patched token on top of the patch stack. *)
      ; IF LPass1Coord <= LPatchStackTopCoord
       THEN (* Give caller the token off the patch stack. *) 
          <*ASSERT
            LPass1Coord = LPatchStackTopCoord
            (* ^Haven't missed a patch stack token. *)
          *> 
          LPatchedTokenL := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchedToken := VAL ( LPatchedTokenL , Itk . TokTyp ) 
        ; IF LPatchedToken = Itk . ItkSkipLt
          THEN (* ItkSkipLt will come only from the patch stack.  Handle it
                  here, so multiple token handlers don't have to.
               *)
            SkipLt ( GetBwdInt ( FM3Globals . PatchRdBack ) )
            (* And loop. *)
          ELSE 
            Result . TrRdBack := LPatchRdBack
          ; Result . TrTok := LPatchedToken
          ; RETURN 
          END (*IF*) 

        ELSE (* Look at the top token on the input file. *) 
          PutBwdPatch ( LPatchRdBack , LPatchStackTopCoord )
            (* ^Push the current patch coordinate back on patch stack. *)
        ; LTokenL := FM3Compress . GetBwd ( LPass1RdBack )
        ; LToken := VAL ( LTokenL , Itk . TokTyp ) 
        ; IF IntSets . IsElement ( LToken , FM3SharedGlobals . GTokSetPatch )
          THEN

          (* Move this token from the input file to the patch stack. *)
          (* Also change the token code to its already-patched counterpart. *) 
          (* Reading RtoL toward BOF.  Writing RtoL towards EOF/TOS. *) 
            LPatchStackTopCoord := FM3Compress . GetBwd ( LPass1RdBack )
          ; CopyOperandsInOrder
              ( FM3Utils . TokenOpndCt ( LToken ) 
              , LPass1RdBack
              , LPatchRdBack
              , MaybeSkip := FALSE
              )
          ; LPatchedToken := LToken - Itk . LtToPatch
(* FIXME: The patch operation can apply to any non-Rt token.  I think
          the necessary bias is always the same as LtToPatch, but check
          this and then use a better name for it.
*) 
          ; PutBwdPatch ( LPatchRdBack , VAL ( LPatchedToken , LONGINT ) )
          ; PutBwdPatch ( LPatchRdBack , LPatchStackTopCoord )
          (* And loop. *)

          ELSIF LToken = Itk . ItkSkipRt
          THEN (* ItkSkipRt will come only from the Pass1 RdBack.  Handle it
                  here, so multiple token handlers don't have to.
               *) 
            SkipRt ( GetBwdInt ( LPass1RdBack) ) 
          (* and loop. *) 
          ELSE (* Return it directly. *)
            Result . TrRdBack := LPass1RdBack
          ; Result . TrTok := LToken
          ; RETURN 
          END (*IF*) 
        END (*IF*)
      END (*LOOP*)
    END GetTokCode 

(*EXPORTED*)
; PROCEDURE Pass2Tokens ( LMPass1Depth : LONGINT )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPass1RdBack : RdBackFile . T 
  ; VAR LPass2RdBack : RdBackFile . T 
  ; VAR LTokResult : TokResultTyp 

  ; BEGIN (* Pass2Tokens *) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; LPass1RdBack := LUnitRef . UntPass1OutRdBack 
    ; LPass2RdBack := LUnitRef . UntPass2OutRdBack
    ; LMPass1Depth := MAX ( LMPass1Depth , LUnitRef . UntPass1OutEmptyCoord )
    ; <* ASSERT GetBwd ( LPass1RdBack ) = VAL ( Itk . ItkEOF , LONGINT ) *> 
      <* ASSERT GetBwd ( LPass1RdBack ) = VAL ( Itk . ItkRightEnd , LONGINT )*> 

      LOOP
        GetTokCode ( LMPass1Depth , (*OUT*) LTokResult )
      ; IF LTokResult . TrTok = Itk . ItkBOF
        THEN EXIT
        ELSE HandleTok ( LTokResult )
        END (*IF*)
      (* And loop. *) 
      END (*LOOP*)
    END Pass2Tokens

; PROCEDURE ExprAbsent ( )

  = VAR LNewObj : FM3Exprs . ExprTyp

  ; BEGIN
      LNewObj := NEW ( FM3Exprs . ExprTyp )  
    ; LNewObj . ExpLink := FM3Exprs . ExprStackTopObj  
    ; FM3Exprs . ExprStackTopObj := LNewObj  
    END ExprAbsent  

; PROCEDURE DefExprRt ( NewExprObj : FM3Exprs . ExprCommon )
  (* PRE: NOT Skipping. *)
  (* Expressions that are contained in definitions. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp 
  
  ; BEGIN
      LUnitRef := FM3Units . UnitStackTopRef 
    ; LScopeRef := FM3Scopes . DeclScopeStackTopRef
    ; IF LScopeRef ^ . ScpCurExprObj = NIL 
      THEN
        LScopeRef ^ . ScpCurExprObj := NewExprObj
      ; LScopeRef ^ . ScpCurDefRefDeclNoSet := IntSets . Empty ( ) 
      ELSE
        TYPECASE NewExprObj . ExpLink OF
        | NULL => 
        | FM3Exprs . ExprCommon ( TDeeperExpr ) 
        => NewExprObj . ExpKind := TDeeperExpr . ExpKind
        ELSE
        END (*TYPECASE*) 
      END (*IF*) 
    ; NewExprObj . ExpIsLegalRecursive := FALSE 
    ; NewExprObj . ExpSelfExprNo 
        := VarArray_Int_Refany . TouchedRange ( LUnitRef ^ . UntExprMap ) . Hi
           + 1
    ; VarArray_Int_Refany . Assign
        ( LUnitRef . UntExprMap , NewExprObj . ExpSelfExprNo , NewExprObj )
    ; NewExprObj . ExpLink := FM3Exprs . ExprStackTopObj  
    ; FM3Exprs . ExprStackTopObj := NewExprObj  
    END DefExprRt 

; PROCEDURE DefExprLt ( )
  (* PRE NOT Skipping. *)
  (* Expressions that are contained in definitions. *) 

  = BEGIN 
(* TODO: Anything? *) 
    END DefExprLt

; PROCEDURE HandleTok ( READONLY TokResult : TokResultTyp ) 

  = VAR HtDeclNo : FM3Base . DeclNoTyp 

  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR HtPass1RdBack : RdBackFile . T 
  ; VAR HtPass2RdBack : RdBackFile . T
  ; VAR LLongInt : LONGINT 
  ; VAR LScopeNo : FM3Base . ScopeNoTyp
  ; VAR LCount : INTEGER 
  ; VAR LPosition : tPosition
  ; VAR HtSkipping : BOOLEAN

  ; PROCEDURE HtIntLit ( LoType : Lot )
    (* Both INTEGER and LONGINT. *)

    = VAR LLongInt : LONGINT 
    ; VAR LPosition : tPosition

    ; BEGIN
        LLongInt := GetBwd ( TokResult . TrRdBack )
      ; LPosition := GetBwdPos ( TokResult . TrRdBack )
      ; IF NOT HtSkipping 
        THEN
          IF AreInsideADecl ( )
          THEN 
            WITH LExpr = NEW ( FM3Exprs . ExprConstValue )
            DO 
              DefExprRt ( LExpr )
            ; LExpr . ExpValueL := LLongInt
            ; LExpr . ExpValueLoType := LoType  
            ; LExpr . ExpPosition := LPosition
            ; LExpr . ExpKind := Ekt . EkConst
            ; LExpr . ExpIsLegalRecursive := TRUE
            END (*WITH*)
          ELSE
  (* DECIDE: Do we want just one token code with a LoTyp operand here? *) 
            PutBwdP2 ( HtPass2RdBack , VAL ( LPosition . Column , LONGINT ) ) 
          ; PutBwdP2 ( HtPass2RdBack , VAL ( LPosition . Line , LONGINT ) ) 
          ; PutBwdP2 ( HtPass2RdBack , LLongInt )
          ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )
          END (*IF*) 
        END (*IF*)
      END HtIntLit 

  ; PROCEDURE HtExprRt ( NewExpr : FM3Exprs . ExprCommon )

    = BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
        DO IF HtSkipping 
          THEN (* NewExpr becomes immediate garbage. *) 
          ELSE 
            DefExprRt ( NewExpr )
          ; NewExpr . ExpPosition := WPosition
          ; NewExpr . ExpKind := Ekt . EkType
(* TODO:      ^Figure this out. *) 
          END (*IF*)
        END (*WITH*)
      END HtExprRt 

  ; PROCEDURE HtExprOpnd1  ( )
      (* PRE: TOS is 1st (LM) operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp

    ; BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
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
          ; DefExprLt ( ) 
          END (*IF*) 
        END (*WITH*)
      END HtExprOpnd1 

  ; PROCEDURE HtExprOpnd2  ( )
      (* PRE: TOS is 2nd operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp

    ; BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
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
          ; DefExprLt ( ) 
          END (*IF*) 
        END (*WITH*)
      END HtExprOpnd2

  ; PROCEDURE HtScopeOpnd1  ( )
      (* 1st (LM) operand of an expr node and it is a scope. *) 
      (* PRE: Top of scope stack is 1st scope operand.
              Top of expr stack is containing type. *) 
    = VAR LScopeRef : FM3Scopes . ScopeRefTyp

    ; BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
        DO IF NOT HtSkipping 
          THEN
            LScopeRef := FM3Scopes . DeclScopeStackTopRef
          ; TYPECASE FM3Exprs . ExprStackTopObj
            OF NULL => <*ASSERT FALSE*>
            | FM3Exprs . Expr1ScopeTyp ( TParentExpr ) 
            => <*ASSERT WPosition = TParentExpr . ExpPosition *>
               TParentExpr . ExpScopeRef1 := LScopeRef
            ELSE <*ASSERT FALSE*>
            END (*TYPECASE*)
          END (*IF*) 
        END (*WITH*)
      END HtScopeOpnd1 

  ; <*INLINE*> PROCEDURE HtPassTokenThru ( )

    = BEGIN 
        CopyOperandsInOrder
          ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
          , TokResult . TrRdBack 
          , HtPass2RdBack
          , MaybeSkip := TRUE 
          ) 
      ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )
      END HtPassTokenThru 

  ; <*INLINE*> PROCEDURE HtMaybePassTokenThru ( ) : BOOLEAN (* Handled it. *)
    (* Use this only for writing to pass 2 output. *) 

    = BEGIN 
        <* ASSERT HtPass2RdBack = FM3Globals . P2RdBack *> 
        IF HtSkipping 
        THEN
          DiscardOperands
            ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
            , TokResult . TrRdBack
            ) 
        ; RETURN TRUE  
        ELSE 
          IF FM3Scopes . DeclScopeStackTopRef ^ . ScpCurExprObj = NIL
          THEN (* Not inside a decl-defining expression. *) 
            CopyOperandsInOrder
              ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
              , TokResult . TrRdBack 
              , HtPass2RdBack
              , MaybeSkip := TRUE 
              ) 
          ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )
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

    = VAR LCountLt : LONGINT
    ; VAR LCountRt : LONGINT
    ; VAR LValuesRef : REF ARRAY OF LONGINT 
    ; VAR LCount : INTEGER

    ; BEGIN
        IF MaybeSkip AND HtSkipping 
        THEN
        ELSE 
          LCountRt := FM3Compress . GetBwd ( HtPass1RdBack )
        ; PutBwdP2 ( HtPass2RdBack , LCountRt )
        ; LCount := VAL ( LCountRt , INTEGER )
        ; LValuesRef := NEW ( REF ARRAY OF LONGINT , LCount )
        ; FOR RI := 0 TO LCount - 1
          DO LValuesRef ^ [ RI ] := FM3Compress . GetBwd ( HtPass1RdBack )
          END (*FOR*) 
        ; FOR RI := LCount - 1 TO 0 BY - 1 
          DO PutBwdP2 ( HtPass2RdBack , LValuesRef ^ [ RI ] )
          END (*FOR*)
        ; LValuesRef := NIL 
        ; LCountLt := FM3Compress . GetBwd ( HtPass1RdBack )
        ; <*ASSERT LCountLt = LCountRt *>
          PutBwdP2 ( HtPass2RdBack , LCountLt )
        END (*IF*) 
      END HtReverseVariableValues 

    ; PROCEDURE HtVisitRefNo ( RefDeclNo : INTEGER )  

      = BEGIN
          FM3Graph . AddArc
            ( (*IN OUT*) FM3Scopes . LookupScopeStackTopRef . ScpDeclGraph
            , HtDeclNo
            , RefDeclNo
            )
        END HtVisitRefNo 

  ; BEGIN (*HandleTok*) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; HtPass1RdBack := LUnitRef . UntPass1OutRdBack 
    ; HtPass2RdBack := LUnitRef . UntPass2OutRdBack
    ; HtSkipping
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
    ; CASE TokResult . TrTok OF

      | Itk . ItkScopeEmpty 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack )
(* TODO: Anything here? *) 

      | Itk . ItkDeclScopeRt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack )
        ; FM3Scopes . PushDeclScopeRef
            ( FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) )

      | Itk . ItkDeclScopeLt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
        ; <* ASSERT FM3Scopes . PopDeclScopeRef ( )
                    = FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
          *>

      | Itk . ItkLookupScopeRt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
        ; LScopeRef := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
        ; LScopeRef ^ . ScpDeclGraph
            := FM3Graph . NewEmpty ( MaxNodeCt := LScopeRef ^ . ScpDeclCt ) 
        ; FM3Scopes . PushLookupScopeRef ( LScopeRef ) 

      | Itk . ItkLookupScopeLt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
        ; <* ASSERT FM3Scopes . PopLookupScopeRef ( )
                    = FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
          *>

      | Itk . ItkBlockDeclListRt
      , Itk . ItkBlockDeclListSep 
      , Itk . ItkBlockDeclListLt 
      , Itk . ItkFieldDeclListRt
      , Itk . ItkFieldDeclListSep 
      , Itk . ItkFieldDeclListLt 
      =>  LCount := GetBwdInt ( TokResult . TrRdBack ) 
        ; CopyOperandsInOrder
           ( 2 (*Position*)
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( LCount , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

      | Itk . ItkTypeDeclRt 
      , Itk . ItkVarDeclRt 
      , Itk . ItkConstDeclRt 
      , Itk . ItkExceptionDeclRt 
      , Itk . ItkFullRevealRt 
      , Itk . ItkPartialRevealRt 
      =>  FM3Scopes . DeclScopeStackTopRef ^ . ScpCurExprObj := NIL
        ; HtPassTokenThru ( )

      | Itk . ItkDeclTypeAbsent 
      , Itk . ItkDeclValAbsent
      , Itk . ItkFormalTypeAbsent
      , Itk . ItkFormalExprAbsent 
      =>  ExprAbsent ( )
        ; HtPassTokenThru ( ) 

      | Itk . ItkFieldDeclVal
      , Itk . ItkFieldDeclType
(* FIXME: What about Formals? *)
      =>  EVAL FM3Exprs . PopExprStack ( ) (* Garbits. *) 
        ; HtPassTokenThru ( ) 

      | Itk . ItkDeclNo
      =>  HtDeclNo := GetBwdInt ( TokResult . TrRdBack ) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IntSets . ForAllDo
            ( FM3Scopes . LookupScopeStackTopRef ^ . ScpCurDefRefDeclNoSet
            , HtVisitRefNo
            ) 

(* CONSISTIFY: For some of these, fetch the operands inside the called proc. *) 
      | Itk . ItkDuplDeclId
      => EVAL DuplDeclIdR2L ( TokResult )

      | Itk . ItkDeclId
      => EVAL DeclIdR2L ( TokResult )

(* FIXME: We now use different tokens for different declkinds, eg.
    ItkVALUEFormalIdListElem.  But is that necessary? *) 

      | Itk . ItkIdRefAtom 
      => IdentRefR2L ( TokResult )

      | Itk . ItkQualIdAtoms 
      => QualIdentR2L ( TokResult . TrRdBack )

      | Itk . ItkBlockRt
      => LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
     (* Doesn't exist, probably ItkBlock[LR]t will disappear.

     *) 
        ; CopyOperandsInOrder
           ( 2 (*Position*)
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( Itk . ItkBlockRt , LONGINT ) )

(* ItkBlock[RL]t will probably disappear. *) 
      | Itk . ItkBlockLt 
      => CopyOperandsInOrder
           ( FM3Utils . TokenOpndCt ( TokResult . TrTok ) 
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

      | Itk . ItkIntLit
      => HtIntLit ( Lot . LotInt ) 

      | Itk . ItkLongIntLit
      => HtIntLit ( Lot . LotLongInt ) 

      | Itk . ItkTextLitRt
      , Itk . ItkWideTextLitRt
      => CopyOperandsInOrder
           ( 3 (* Atom, position. *)
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )
        ; HtReverseVariableValues ( MaybeSkip := TRUE ) 

      | Itk . ItkTextLitLt
      , Itk . ItkWideTextLitLt
      => CopyOperandsInOrder
           ( 3 (* Atom, position. *)
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

      (* Enumeration type: *) 
(* FIXME: Some of these need to copy the token and arguments. *)   
      | Itk . ItkEnumTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
        ; HtExprRt ( NEW ( FM3Exprs . ExprEnumTypeTyp ) ) 

      | Itk . ItkEnumTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
        ; HtScopeOpnd1 ( ) 

      (* Record type: *) 
      | Itk . ItkRecTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
        ; HtExprRt ( NEW ( FM3Exprs . ExprRecTypeTyp ) )
(* Copy token? *) 

      | Itk . ItkRecTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
        ; HtScopeOpnd1 ( ) 
(* Copy token? *) 

      (* REF type: *) 
      | Itk . ItkREFTypeRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprRt ( NEW ( FM3Exprs . ExprREFTypeTyp ) ) 

      | Itk . ItkREFTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd1 ( ) 

      (* Subrange type: *) 
      | Itk . ItkSubrTypeRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprRt ( NEW ( FM3Exprs . ExprSubrTypeTyp ) ) 

      | Itk . ItkSubrTypeDotDot 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 

      | Itk . ItkSubrTypeLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*)
        ; HtExprOpnd2 ( )

      ELSE (* No special pass2 handling. *)
        HtPassTokenThru ( ) 
      END (*CASE*)
    END HandleTok
    
(* Right-to-left scope handling.  Call sites read the Itk and its operands,
   and pass the operands in. *) 

; PROCEDURE LookupDeclNoInScope
    ( READONLY Scope : FM3Scopes . ScopeTyp ; IdAtom : FM3Base . AtomTyp ) 
  : FM3Base . DeclNoTyp 
  (* PRE: IdAtom is in Scope's dictionary. *) 

  = VAR LDeclNoInt : INTEGER
  ; VAR LResult : FM3Base . DeclNoTyp 
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupDeclNoInScope*)
      LResult := FM3Base . DeclNoNull
    ; TRY
        LFound 
          := FM3Dict_Int_Int . LookupFixed
               ( Scope . ScpDeclDict
               , IdAtom
               , FM3Base . HashNull
               , (*OUT*) LDeclNoInt
               )
      ; IF LFound
        THEN LResult := VAL ( LDeclNoInt , FM3Base . DeclNoTyp )
        END (*IF*) 
      EXCEPT FM3Dict_Int_Int . Error ( <*UNUSED*> EMsg )
      => 
      ELSE 
      END (*EXCEPT*)
    ; RETURN LResult 
    END LookupDeclNoInScope

; PROCEDURE LookupExpImp
    ( IdAtom : FM3Base . AtomTyp
    ; VAR (*OUT*) UnitNo : FM3Base . UnitNoTyp
    ; VAR (*OUT*) DeclNo : FM3Base . DeclNoTyp
    )
  : BOOLEAN (* It's present. *) 
  (* Look in the current unit.  *) 

  = VAR LExpImpProxy : FM3ExpImpProxy . T

  ; BEGIN
      IF IntSets . IsElement
           ( IdAtom , FM3Units . UnitStackTopRef . UntExpImpIdSet ) 
      THEN (* It's [ex|im]ported. *) 
        LExpImpProxy
          := VarArray_Int_ExpImpProxy . Fetch
               ( FM3Units . UnitStackTopRef ^ . UntExpImpMap , IdAtom )
      ; UnitNo := LExpImpProxy . EipUnitNo 
      ; DeclNo := LExpImpProxy . EipDeclNo
      ; RETURN TRUE
      ELSE
        UnitNo := FM3Base . UnitNoNull
      ; DeclNo := FM3Base . DeclNoNull 
      ; RETURN FALSE 
      END (*IF*)
    END LookupExpImp 

; PROCEDURE LookupBlockRef ( IdAtom : FM3Base . AtomTyp ) : FM3Base . DeclNoTyp  
  (* In nearest enclosing (block) scope on lookup stack. *) 

  = VAR LScopeRef : FM3Scopes . ScopeRefTyp 
  ; VAR LDeclNoInt : INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupBlockRef*)
      LScopeRef := FM3Scopes . LookupScopeStackTopRef
    ; LOOP
        IF LScopeRef = NIL THEN RETURN FM3Base . DeclNoNull END (*IF*) 
      ; IF NOT LScopeRef ^ . ScpKind IN FM3Scopes . ScopeKindSetBlock
           (* ^Can this happen? *) 
        THEN (* Skip over this scope. *)
          LScopeRef := LScopeRef ^ . ScpLookupStackLink
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
          RETURN VAL ( LDeclNoInt , FM3Base . DeclNoTyp ) 
        ELSE (* Try the next outer scope. *) 
          LScopeRef := LScopeRef ^ . ScpLookupStackLink
        END (*IF*) 
      END (*LOOP*) 
    END LookupBlockRef

; PROCEDURE DuplDeclIdR2L ( READONLY TokResult : TokResultTyp )
  : FM3Base . DeclNoTyp
  (* Append a temporary, pseudo-decl node to the linked list rooted at
     the decl number.  The position of the original declaration of
     the ident, which is needed for the error message, is not known yet.
  *) 

  = VAR DdiAtom : FM3Base . AtomTyp
  ; VAR DdiPosition : tPosition

  ; PROCEDURE Visit ( DeclNoI : INTEGER ; VAR (* IN OUT *) DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *)  
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

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block. *)
        DdiAtom := GetBwdAtom ( TokResult . TrRdBack )
      ; DdiPosition := GetBwdPos ( TokResult . TrRdBack )
      ; LDeclNo
          := LookupDeclNoInScope
               ( FM3Scopes . DeclScopeStackTopRef ^ , DdiAtom )
      ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdR2L
    
; PROCEDURE DeclIdR2L ( READONLY TokResult : TokResultTyp ) : FM3Base . DeclNoTyp
  (* ^This will be the only decl of DeclIdAtom in the current scope. *) 

  = VAR DidAtom : FM3Base . AtomTyp
  ; VAR DidPosition : tPosition
  ; VAR DidDeclKind : FM3Decls . DeclKindTyp

  ; PROCEDURE VisitDecl
      ( DeclNoI : INTEGER ; VAR (* IN OUT *) DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *) 
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *)
    
    = VAR LDeclRef : FM3Decls . DeclRefTyp
    ; VAR LIdentText : TEXT

    ; BEGIN (* VisitDecl *)
        IF DeclRefany # NIL (* Some duplicate decls of DeclNoI also exist? *) 
        THEN (* Dispense with them with error messages. *) 
          LDeclRef := DeclRefany (* Implied NARROW. *)
        ; LIdentText := FM3Units . TextOfIdAtom ( DidAtom ) 
        ; WHILE LDeclRef # NIL
          DO
            ErrorArr
              ( ARRAY OF REFANY
                  { "Duplicate declaration of \""
                  , LIdentText
                  , "\", ignored, original at "
                  , PosImage ( DidPosition )
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
      ; DeclRefany := LDeclRef 
      END VisitDecl

  ; BEGIN (*DeclIdR2L*)
      DidDeclKind := GetBwdDeclKind ( TokResult . TrRdBack )
    ; DidAtom := GetBwdAtom ( TokResult . TrRdBack )
    ; DidPosition := GetBwdPos ( TokResult . TrRdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    ; VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block *)
        WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        DO 
          LDeclNo
            := LookupDeclNoInScope
                 ( FM3Scopes . DeclScopeStackTopRef ^ , DidAtom ) 
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          VarArray_Int_Refany . CallbackWithElem 
            ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , VisitDecl )
        ; PutBwdP2 ( Wp2RdBack , VAL ( DidPosition . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( DidPosition . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
        ; RETURN LDeclNo
        END (*WITH*) 
      END (* Block *) 
    END DeclIdR2L

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
    ( IdentRefAtom : FM3Base . AtomTyp
    ; READONLY Position : FM3Base . tPosition
    )

  = BEGIN
      IF AreInsideADecl ( )
      THEN ExprAbsent ( )
      ELSE 
        WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        DO 
        (* Read the following backwards: *) 
          PutBwdP2 ( Wp2RdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( IdentRefAtom , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdRefAtomNotUsable , LONGINT ) )
        END (*WITH*)
      END (*IF*) 
    END PutNotUsable

; <*INLINE*> PROCEDURE AreInsideADecl ( ) : BOOLEAN

  = VAR LOpenScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      LOpenScopeRef := FM3Scopes . LookupScopeStackTopRef
    ; IF LOpenScopeRef = NIL THEN RETURN FALSE END (*IF*)
    ; RETURN LOpenScopeRef ^ . ScpCurExprObj # NIL 
    END AreInsideADecl

; PROCEDURE CheckRecursiveRef ( RefDeclNo : FM3Base . DeclNoTyp )

  = VAR LOpenScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LScopeMinDeclNo : FM3Base . DeclNoTyp 

  ; BEGIN
      LOpenScopeRef := FM3Scopes . LookupScopeStackTopRef
    ; IF LOpenScopeRef = NIL THEN RETURN END (*IF*)
    ; LScopeMinDeclNo := LOpenScopeRef . ScpMinDeclNo
    ; IF RefDeclNo < LScopeMinDeclNo THEN RETURN END (*IF*) 
    ; IF RefDeclNo >= LScopeMinDeclNo + LOpenScopeRef ^ . ScpDeclCt
      THEN RETURN
      END (*IF*)
    (* It's recursive, now is it legal? *) 
    ; TYPECASE FM3Exprs . ExprStackTopObj OF 
      NULL => 
      | FM3Exprs . ExprCommon ( TRefExpr ) 
      => IF NOT TRefExpr . ExpIsLegalRecursive
         THEN 
           WITH WRefIdNoSet
                = FM3Scopes . LookupScopeStackTopRef ^ . ScpCurDefRefDeclNoSet
           DO WRefIdNoSet := IntSets . Include ( WRefIdNoSet , RefDeclNo ) 
           END (*WITH*)
         END (*IF*)
      ELSE 
      END (*TYPECASE*) 
    END CheckRecursiveRef 

; PROCEDURE IdentRefR2L ( TokResult : TokResultTyp )
  (* PRE: The ident is not followed by dot Ident. (The parser has gone
          to some trouble to ensure this.)
  *) 

  = VAR LExpImpUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentRefAtom : FM3Base . AtomTyp 
  ; VAR LUnitNo : FM3Base . UnitNoTyp
  ; VAR LRefDeclNo : FM3Base . DeclNoTyp
  ; VAR LPosition : FM3Base . tPosition 
  ; VAR LIntfNameChars : FM3Atom_OAChars . KeyTyp
  ; VAR LIsUsable : BOOLEAN 

  ; BEGIN (*IdentRefR2L*)
      LIdentRefAtom := GetBwdAtom ( TokResult . TrRdBack )
    ; LPosition := GetBwdPos ( TokResult . TrRdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN 
      END (*IF*)
    ; WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
      
      (* Look for a reference to a decl in an enclosing* open scope. *) 
        LRefDeclNo := LookupBlockRef ( LIdentRefAtom )
      ; IF LRefDeclNo # FM3Base . DeclNoNull 
        THEN 
          IF AreInsideADecl ( ) 
          THEN (* Create an ExprIdNo node. *) 
            CheckRecursiveRef ( LRefDeclNo )
          ; WITH WExpr = NEW ( FM3Exprs . ExprRefDeclNo )
            DO 
              DefExprRt ( WExpr )
            ; WExpr . ExpDeclNo := LRefDeclNo 
            ; WExpr . ExpPosition := LPosition
            ; WExpr . ExpKind := Ekt . EkRef
            ; WExpr . ExpIsUsable := TRUE
            END (*WITH*)
          ELSE (* Change to a reference token with DeclNo instead of Atom. *)
            PutBwdP2 ( Wp2RdBack , VAL ( LPosition . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosition . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LRefDeclNo , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )
          END (*IF*)
          
        (* Look for something [ex|im]ported. *) 
        ELSIF LookupExpImp
                ( LIdentRefAtom , (*OUT*) LUnitNo , (*OUT*) LRefDeclNo )
        THEN (* Export or import is present. *) 
          IF LUnitNo = FM3Base . UnitNoNull (* But not usable. *) 
          THEN LIsUsable := FALSE
(* CHECK: ^v Which of these ways denoting unusability can happen? *) 
          ELSE
            LExpImpUnitRef (* Implicit NARROW. *) 
              := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNo )
          ; LIsUsable := LExpImpUnitRef ^ . UntState # Ust . UsNotUsable
          END (*IF*)
        ; IF LIsUsable
          THEN (* LExpImpUnitRef names an interface. *)
            IF LRefDeclNo = FM3Base . DeclNoNull
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
                  DefExprRt ( WExpr )
                ; WExpr . ExpRemoteUnitNo := LUnitNo 
                ; WExpr . ExpRemoteDeclNo := LRefDeclNo 
                ; WExpr . ExpPosition := LPosition
                ; WExpr . ExpKind := Ekt . EkRef
                ; WExpr . ExpIsUsable := TRUE
                END (*WITH*)
              ELSE (* Emit a token. *)  
              (* Read the following backwards: *) 
                PutBwdP2 ( Wp2RdBack , VAL ( LPosition . Column , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosition . Line , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LRefDeclNo , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LUnitNo , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExpImpRef , LONGINT ) )
              END (*IF*) 
            END (*IF*)
          ELSE (* It was already unusable. *) 
            PutNotUsable( LIdentRefAtom , LPosition ) 
          END (*IF*) 
        ELSE (* Undeclared. *) 
          BadIdentMessage ( "Undeclared identifier" , LIdentRefAtom , LPosition )
        ; PutNotUsable( LIdentRefAtom , LPosition ) 
        END (*IF*)
      END (*WITH*)
    ; DefExprLt ( ) 
    END IdentRefR2L

; PROCEDURE QualIdentR2L ( Pass1RdBack : RdBackFile . T )
  (* (NON)PRE: No operands have been read. *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp 
  ; VAR LUnitNoLt : FM3Base . UnitNoTyp
  ; VAR LRefDeclNoLt : FM3Base . DeclNoTyp
  ; VAR LRefDeclNo : FM3Base . DeclNoTyp
  ; VAR LAtomLt , LAtomRt : FM3Base . AtomTyp
  ; VAR LIntfAtomRt : FM3Base . AtomTyp
  ; VAR LIntfDeclNoInt : INTEGER 
  ; VAR LPosLt , LPosRt : FM3Base . tPosition

  ; BEGIN (*QualIdentR2L*)
      LAtomLt := GetBwdAtom ( Pass1RdBack ) 
    ; LAtomRt := GetBwdAtom ( Pass1RdBack ) 
    ; LPosLt := GetBwdPos ( Pass1RdBack ) 
    ; LPosRt := GetBwdPos ( Pass1RdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN 
      END (*IF*) 
    ; WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
      
      (* Look for a left reference to a decl in an enclosing open scope. *) 
        LRefDeclNoLt := LookupBlockRef ( LAtomLt )
      ; IF LRefDeclNoLt # FM3Base . DeclNoNull
        THEN (* Lt names a local declaration, not an interface. *)
          IF AreInsideADecl ( ) 
          THEN (* Create an ExprIdNo node. *) 
            CheckRecursiveRef ( LRefDeclNo )
          ; WITH WDotExpr = NEW ( FM3Exprs . ExprDot )
                 , WLtExpr = NEW ( FM3Exprs . ExprRefDeclNo )
            DO 
              DefExprRt ( WDotExpr )
            ; WDotExpr . ExpOpnd1 := WLtExpr 
            ; WDotExpr . ExpDotIdAtom := LAtomRt
            ; WDotExpr . ExpPosition := LPosRt
            ; WDotExpr . ExpKind := Ekt . EkRef
            ; WDotExpr . ExpIsUsable := TRUE
            ; DefExprRt ( WLtExpr )
            ; WLtExpr . ExpDeclNo := LRefDeclNoLt 
            ; WLtExpr . ExpPosition := LPosLt
            ; WLtExpr . ExpKind := Ekt . EkRef
            ; WLtExpr . ExpIsUsable := TRUE
            END (*WITH*)
          ; EVAL FM3Exprs . PopExprStack ( ) (* The expr **) 
          ELSE (* Emit tokens for a dot Id applied to a DeclNo Id reference. *)

          (* Turn the qualident into separate Id ref and dot Id. *)
            PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )

          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LRefDeclNoLt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )

          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )
          END (*IF*) 

        (* Look for something [ex|im]ported. *) 
        ELSIF LookupExpImp ( LAtomLt , (*OUT*) LUnitNoLt , (*OUT*) LRefDeclNoLt )
        THEN (* Lt ident is [ex|im]ported. *)  
          IF LUnitNoLt = FM3Base . UnitNoNull
          THEN (* Unusable. *) 
            PutNotUsable( LAtomLt , LPosLt ) 
          ELSIF LRefDeclNoLt = FM3Base . DeclNoNull
          THEN (* Lt names an imported interface. *)
            LIntfUnitRef (*Implicit NARROW*) 
              := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNoLt )
          ; <* ASSERT LIntfUnitRef # NIL *>
            LIntfAtomRt
              := FM3Compile . ConvertIdentAtom
                   ( LAtomRt
                   , FromUnitRef := FM3Units . UnitStackTopRef
                   , ToUnitRef := LIntfUnitRef
                   )
          ; IF IntSets . IsElement
                 ( LIntfAtomRt , LIntfUnitRef ^ . UntExpImpIdSet )
            THEN (* Rt ident is imported into the remote interface,
                    not transitively importable.
                 *)
              IF NOT FM3Atom_OAChars . Key
                       ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                       , LAtomRt 
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
                , LPosRt
                )
            ; PutNotUsable ( LAtomRt , LPosRt )
            ELSIF IntSets . IsElement
                    ( LIntfAtomRt
                    , LIntfUnitRef ^ . UntScopeRef ^ . ScpDeclIdSet
                    )
            THEN (* Right ident is declared in LIntfUnitRef. *) 
              <* ASSERT
                   FM3Dict_Int_Int . LookupFixed 
                     ( LIntfUnitRef ^ . UntScopeRef ^ . ScpDeclDict
                     , LIntfAtomRt
                     , FM3Base . HashNull
                     , (*OUT*) LIntfDeclNoInt 
                     )
              *>
              IF AreInsideADecl ( )
              THEN 
                WITH WExpr = NEW ( FM3Exprs . ExprRemoteRef )
                DO 
                  DefExprRt ( WExpr )
                ; WExpr . ExpRemoteUnitNo := LIntfUnitRef ^ . UntSelfUnitNo 
                ; WExpr . ExpRemoteDeclNo := LIntfDeclNoInt 
                ; WExpr . ExpPosition := LPosLt
                ; WExpr . ExpKind := Ekt . EkRef
                ; WExpr . ExpIsUsable := TRUE
                END (*WITH*)
              ELSE 
                PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LIntfDeclNoInt , LONGINT ) ) 
              ; PutBwdP2
                  ( Wp2RdBack , VAL ( LIntfUnitRef ^ . UntSelfUnitNo , LONGINT ) ) 
              ; PutBwdP2
                  ( Wp2RdBack , VAL ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )
              END (*IF*) 
            ELSE (* Right ident is not known in left-named interface. *)
              IF NOT FM3Atom_OAChars . Key
                       ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                       , LAtomRt 
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
              , LPosRt
              )
            ; PutNotUsable ( LAtomRt , LPosRt ) 
            END (*IF*)
          ELSE (* Left Ident by itself denotes a remote decl, brought in
                  by EXPORTS or FROM-IMPORT.
               *)
            IF AreInsideADecl ( )
            THEN 
              WITH WDotExpr = NEW ( FM3Exprs . ExprDot )
              , WLtExpr = NEW ( FM3Exprs . ExprRemoteRef )
              DO 
                DefExprRt ( WDotExpr ) (* Which pushes. *) 
              ; WDotExpr . ExpDotIdAtom := LAtomRt
              ; WDotExpr . ExpOpnd1 := WLtExpr 
              ; WDotExpr . ExpPosition := LPosRt
              ; WDotExpr . ExpKind := Ekt . EkRef
              ; WDotExpr . ExpIsUsable := TRUE
              ; DefExprRt ( WLtExpr ) (* Which pushes. *)
              ; WLtExpr . ExpRemoteUnitNo := LUnitNoLt 
              ; WLtExpr . ExpRemoteDeclNo := LRefDeclNoLt 
              ; WLtExpr . ExpPosition := LPosLt
              ; WLtExpr . ExpKind := Ekt . EkRef
              ; WLtExpr . ExpIsUsable := TRUE
              END (*WITH*)
            ; EVAL FM3Exprs . PopExprStack ( ) (* The LtExpr. *) 
            ELSE 
            (* Turn it into separate QualId ref and dot Id *) 
              PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )

            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LRefDeclNoLt , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LUnitNoLt , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL
                ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )

            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )
            END (*IF*) 
          ; RETURN 
          END (*IF*)
        ELSE (* Undeclared. *)
          BadIdentMessage ( "Undeclared identifier", LAtomLt , LPosLt ) 
        ; PutNotUsable ( LAtomRt , LPosRt ) 
        END (*IF*)
      END (*WITH*) 
    END QualIdentR2L

(*EXPORTED*) 
; PROCEDURE RunPass2 ( ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp  

  ; BEGIN (*RunPass2*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; InitPass2 ( LUnitRef ) 
    ; TranslatePass2 ( LUnitRef ) 
    ; FinishPass2 ( LUnitRef ) 
    END RunPass2

; PROCEDURE DisAsmPass2
    ( UnitRef : FM3Units . UnitRefTyp ; DoEarlierPasses : BOOLEAN )

  = BEGIN (*DisAsmPass2*)
      IF NOT FM3CLOptions . PassNo2 IN UnitRef ^ . UntPassNosDisAsmed 
      THEN (* Disassembly file is not already written. *) 
        FM3Compile . DisAsmPassFile
          ( UnitRef , FM3Globals . Pass2OutSuffix , L2R := TRUE )
      ; FM3CLOptions . InclPassNo
          ( UnitRef ^ . UntPassNosDisAsmed , FM3CLOptions . PassNo2 ) 
      END (*IF*) 
    ; IF DoEarlierPasses
      THEN
(* This is unnecessary, as pass 1 has no earlier pass.  It is here in comment
   to remind whoever to do this in passes later than pass 2.
 
        FM3Pass1 . DisAsmPass1 ( DoEarlierPasses := TRUE )
*) 
      END (*IF*) 
    END DisAsmPass2

; PROCEDURE InitPass2 ( UnitRef : FM3Units . UnitRefTyp )

  = VAR LPass2FileFullName : TEXT
  
  ; BEGIN (*InitPass2*)

    (* Create the pass 2 output file. *)
      TRY (*EXCEPT*)
        UnitRef ^ . UntPass2OutSimpleName
          := Pathname . Join
               ( NIL 
               , UnitRef ^ . UntSrcFileSimpleName
               , FM3Globals . Pass2OutSuffix
               ) 
      ; LPass2FileFullName
          := Pathname . Join
               ( UnitRef ^ . UntBuildDirPath
               , UnitRef ^ . UntPass2OutSimpleName 
               , NIL 
               )
      ; UnitRef ^ . UntPass2OutRdBack
          := RdBackFile . Create ( LPass2FileFullName , Truncate := TRUE )
      ; FM3Globals . P2RdBack := UnitRef ^ . UntPass2OutRdBack
        (* ^Cache for faster access. *) 
      EXCEPT
      | OSError . E ( EMsg ) 
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to create pass 2 output file \""
               , LPass2FileFullName 
               , "\": "
               , FM3Messages . AtomListToOSError ( EMsg)
               , "."
               } 
           ) 
      END (*EXCEPT*)

    (* Write pass 2 output file initial tokens. *)
    ; PutBwdP2
        ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwdP2
        ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkRightEnd , LONGINT ) )
    ; UnitRef ^ . UntPass2OutEmptyCoord
        := RdBackFile . LengthL ( UnitRef ^ . UntPass2OutRdBack )  
    END InitPass2

; PROCEDURE TranslatePass2 ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN (*TranslatePass2*)
  
    (* Write the Pass2 RdBack. *)
      TRY 
        Pass2Tokens ( UnitRef ^ . UntPass2OutEmptyCoord )

      (* Write final successful pass 2 output tokens. *)
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkEOF , LONGINT ) )

      (* Prepare for possible disassembly later. *) 
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass2OutSuffix
          , UnitRef ^ . UntPass2OutRdBack
          )
        (*^ This copy may be used by disassembly called for by command-line
            option, a later pass failure, or not at all. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE (* Pass 2 failed. *) 

      (* Disassemble what there is of the failed file. *)
        PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack
          , VAL ( Itk . ItkLeftEndIncomplete , LONGINT ) 
          )
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass2OutSuffix
          , UnitRef ^ . UntPass2OutRdBack
          )
      ; DisAsmPass2 ( UnitRef , DoEarlierPasses := TRUE )

      ; FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure writing pass 2 output at depth "
               , Fmt . LongInt
                   ( RdBackFile . LengthL ( UnitRef ^ . UntPass2OutRdBack ) )
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
    END TranslatePass2
  
; PROCEDURE FinishPass2 ( UnitRef : FM3Units . UnitRefTyp )

  = VAR LPatchFullFileName : TEXT
  ; VAR LPass2FullFileName : TEXT 
  ; VAR LLengthImage : TEXT
  ; VAR LLengthL : LONGINT 
  ; VAR LCoordSentinalL : LONGINT 

  ; BEGIN (*FinishPass2*)
    (* Close pass 2. *) 
      UnitRef . UntPass2Result := 0 
    ; EVAL FM3Scanner . PopState ( )
(* TODO^ Maybe do this elsewhere. *) 

    (* Finish with and close patch stack. *)
    ; UnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPatchStackRdBack )
    ; LCoordSentinalL := FM3Compress . GetBwd ( UnitRef ^ . UntPatchStackRdBack )
    ; <* ASSERT LCoordSentinalL = FM3Globals . PatchSackEmptySentinal 
                , "Mismatched coordinate sentinal."
      *>
      LLengthL := RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack )
    ; RdBackFile . Close (  UnitRef ^ . UntPatchStackRdBack , 0L )
      (* No point in keeping the patch stack.  It has pogo-sticked and 
         now should be devoid of significant content. *)
    ; LPatchFullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPatchStackSimpleName
             , NIL
             )
    ; FM3SharedUtils . DeleteFile ( LPatchFullFileName )

    ; FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Patch stack "
              , UnitRef ^ . UntPatchStackSimpleName
              , " peak size = "
              , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPatchStackDepth )
              , " bytes."
            } 
        )
    ; LLengthImage := FM3Base . Int64Image ( LLengthL ) 
    ; IF LLengthL # UnitRef ^ . UntPatchStackEmptyCoord 
      THEN
        UnitRef . UntPass2Result := FM3CLArgs . CcPatchStackNotEmpty  
      ; DisAsmPass2 ( UnitRef , DoEarlierPasses := TRUE )
      ; LLengthImage := FM3Base . Int64Image ( LLengthL ) 
      ; FM3Messages . FatalArr
          ( ARRAY OF REFANY
              { "Patch stack ending size = "
              , LLengthImage
              , " bytes, should be "  
              , FM3Base . Int64Image ( UnitRef ^ . UntPatchStackEmptyCoord )
              , "." 
              } 
          )
      END (*IF*)
      
    (* Finish with and close pass 1 output. *)
    ; LLengthL := RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack )
    ; RdBackFile . Close 
        (  UnitRef ^ . UntPass1OutRdBack , - 1L (* Leave full length. *) )
    ; IF LLengthL # UnitRef ^ . UntPass1OutEmptyCoord 
      THEN
        UnitRef . UntPass2Result := FM3CLArgs . CcPass1OutNotEmpty  
      ; DisAsmPass2 ( UnitRef , DoEarlierPasses := TRUE )
      ; LLengthImage := FM3Base . Int64Image ( LLengthL ) 
      ; FatalArr
          ( ARRAY OF REFANY
              { "Pass 1 output file final size = "
              , LLengthImage
              , ", should be "
              , FM3Base . Int64Image ( UnitRef ^ . UntPass1OutEmptyCoord )
              , "."
              }
          )
      END (*IF*)

(* This is now deferred until compile cleanup: 
    ; IF NOT FM3Base . PassNo1 IN FM3CLArgs . PassNosToKeep 
      THEN 
        FM3SharedUtils . DeleteFile
          ( Pathname . Join
              ( UnitRef ^ . UntBuildDirPath 
              , UnitRef ^ . UntPass1OutSimpleName
              , NIL 
              )
          ) 
      END (*IF*)
*) 

    (* Report size and maybe disassemble pass 2 output file. *) 
    ; LPass2FullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPass2OutSimpleName
             , NIL
             )
    ; UnitRef ^ . UntMaxPass2OutLength
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPass2OutRdBack )
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Pass 2 output file "
            , UnitRef ^ . UntPass2OutSimpleName
            , " has "
            , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPass2OutLength )
            , " bytes."
            } 
        )
    ; IF FM3CLOptions . PassNo2 IN FM3CLOptions . PassNosToDisAsm 
      THEN DisAsmPass2 ( UnitRef , DoEarlierPasses := FALSE )
      END (*IF*)

    (* Close the source file. *) 
    ; UniRd . Close ( UnitRef ^ . UntSrcUniRd ) 

    (* Finish with the skip stack. *) 
    ; <* ASSERT
           VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi 
           = UnitRef ^ . UntSkipStackBase 
      *> 
    END FinishPass2

; BEGIN (*FM3Pass2*)


END FM3Pass2
.

