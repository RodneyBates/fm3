(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Pass2

; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT IntSets 
; IMPORT OSError
; IMPORT Pathname
; IMPORT TextWr 
; IMPORT Thread 
; IMPORT UniRd
; IMPORT Wr 

; IMPORT RangeUtils 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 
; IMPORT VarArray_Int_ExpImpProxy 
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base 
; FROM   FM3Base IMPORT tPosition
; IMPORT FM3Builtins 
; IMPORT FM3CLArgs
; IMPORT FM3CLOptions  
; IMPORT FM3Compile
; IMPORT FM3Compress
; FROM   FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Exprs  
; FROM FM3Exprs IMPORT Est   
; IMPORT FM3Dict_Int_Int
; IMPORT FM3ExpImp
; IMPORT FM3ExpImpProxy 
; IMPORT FM3RTFailures 
; IMPORT FM3Globals
; FROM   FM3Globals IMPORT P2RdBack 
; IMPORT FM3Graph 
; IMPORT FM3IntToks AS Itk
; IMPORT FM3LoTypes
; IMPORT FM3Pass1
; IMPORT FM3ReservedIds
; IMPORT FM3SrcToks 
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Std 
; FROM   FM3StreamUtils
    IMPORT GetBwdInt , GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo 
; IMPORT FM3Messages 
; FROM   FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT RdBackFile

; TYPE Ekt = FM3Exprs . ExprKindTyp
; TYPE EkSetTyp = FM3Exprs . EkSetTyp 
; TYPE Ust = FM3Units . UnitStateTyp
; TYPE Dkt = FM3Decls . DeclKindTyp 

; PROCEDURE PutBwdP2 ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd for pass 2 output file. 
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN (*PutBwdP2*) 
      <* ASSERT RdBack = P2RdBack *>
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
(*TODO: Derive operand count from Token code. *) 
    
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
(*TODO: Derive operand count from Token code. *) 

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
(*TODO: Derive operand count from Token code. *) 
  (* This is tricky.  Pop, push reverses them, but this procedure does
     a compensating reverse, for net same order. 
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*CopyOperandsInOrder*)
<* ASSERT MaybeSkip = ( ToRdBack = P2RdBack ) *> 
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
    ; LPass1RdBack := LUnitRef ^ . UntPass1OutRdBack 
    ; LPatchRdBack := LUnitRef ^ . UntPatchStackRdBack 
    ; LMPass1Depth := MAX ( LMPass1Depth , LUnitRef ^ . UntPass1OutEmptyCoord )

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
          <* ASSERT LPass1Coord = LPatchStackTopCoord
             , "Missed a patch stack token."
          *>
          LPatchedTokenL := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchedToken := VAL ( LPatchedTokenL , INTEGER (*Itk . TokTyp*) )
        ; IF LPatchedToken = Itk . ItkSkipLt
          THEN (* ItkSkipLt will come only from the patch stack.  Handle it
                  here, so multiple token handlers don't have to.
               *)
            SkipLt ( GetBwdInt ( FM3Globals . PatchRdBack ) )
            (* And loop. *)
; IF LPatchedToken = 1109
  THEN LToken := 1109 + 0
  END 
          ELSIF (* Skipping? *) 
            VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
          THEN (* Skip it and loop. *) 
            DiscardOperands
              ( FM3Utils . TokenOpndCt ( LPatchedToken ) , LPatchRdBack ) 
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
; IF LToken = 1109
  THEN LToken := 1109 + 0
  END 
          ELSIF 
            VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
          THEN (* Skip it and loop. *) 
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
    ; LPass1RdBack := LUnitRef ^ . UntPass1OutRdBack 
    ; LPass2RdBack := LUnitRef ^ . UntPass2OutRdBack
    ; LMPass1Depth := MAX ( LMPass1Depth , LUnitRef ^ . UntPass1OutEmptyCoord )
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
    ; LNewObj . ExpIsUsable := FALSE 
    ; FM3Exprs . PushExprStack ( LNewObj ) 
    END PushExprIgnore 

; PROCEDURE DefExprRt ( NewExprObj : FM3Exprs . ExprTyp )
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
        ; PutBwdP2
            ( P2RdBack , VAL ( NewExprObj . ExpPosition . Column , LONGINT ) )
        ; PutBwdP2
            ( P2RdBack , VAL ( NewExprObj . ExpPosition . Line , LONGINT ) ) 
        ; PutBwdP2 ( P2RdBack , VAL ( NewExprObj . ExpSelfExprNo , LONGINT ) )
        ; PutBwdP2 ( P2RdBack , VAL ( Itk . ItkDefTopExprNo , LONGINT ) )
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
    END DefExprRt 

;  PROCEDURE WrongKindMsg
     ( Operand , Opcode , Expected : TEXT ; Position : tPosition )

  = BEGIN
      FM3Messages . ErrorArr
        ( ARRAY OF REFANY
            { Operand , " of \"" , Opcode , "\" must be " , Expected , "." }
        , Position
        )
    END WrongKindMsg

; PROCEDURE DefExprLt ( )
  (* PRE NOT Skipping. *)
  (* Expressions that are contained in definitions. *) 

  = BEGIN 
(* TODO: Anything? *) 
    END DefExprLt

; PROCEDURE UnaryOp ( Opcode : FM3Exprs . OpcodeTyp )
  (* PRE: Not skipping, *) 

  = VAR LUnOpExpr : FM3Exprs . Expr2OpndTyp

  ; BEGIN
      LUnOpExpr
        := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . Expr2OpndTyp )
    ; IF NOT LUnOpExpr . ExpOpnd1 . ExpUpKind
             IN EkSetTyp { Ekt . EkValue , Ekt . EkConst }
      THEN
        WrongKindMsg
          ( "Parameter"
          , FM3SrcToks . Image ( Opcode ) 
          , "a value expression"
          , LUnOpExpr . ExpPosition
          )
      ; LUnOpExpr . ExpIsUsable := FALSE
      ELSE  
        LUnOpExpr . ExpIsConst := LUnOpExpr . ExpOpnd1 . ExpIsConst
      ; LUnOpExpr . ExpReachedDeclNoSet
          := LUnOpExpr . ExpOpnd1 . ExpReachedDeclNoSet 
      END (*IF*) 
    END UnaryOp
      
; PROCEDURE BinaryOp ( Opcode : FM3Exprs . OpcodeTyp )
  (* PRE: Not skipping, *) 

  = VAR LBinOpExpr : FM3Exprs . Expr2OpndTyp
  ; VAR LOpnd1 , LOpnd2 : FM3Exprs . ExprTyp 

  ; BEGIN
      LBinOpExpr
        := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . Expr2OpndTyp )
    ; LOpnd1 := LBinOpExpr . ExpOpnd1 
    ; LOpnd2 := LBinOpExpr . ExpOpnd2 
    ; IF NOT LOpnd1 . ExpUpKind
             IN EkSetTyp { Ekt . EkValue , Ekt . EkConst }
      THEN
        WrongKindMsg
          ( "Left parameter"
          , FM3SrcToks . Image ( Opcode ) 
          , "a value expression"
          , LOpnd1 . ExpPosition
          )
      ; LBinOpExpr . ExpIsUsable :=FALSE
      END (*IF*) 
    ; IF NOT LOpnd2 . ExpUpKind
             IN EkSetTyp { Ekt . EkValue , Ekt . EkConst } 
      THEN
        WrongKindMsg
          ( "Right parameter"
          , FM3SrcToks . Image ( Opcode ) 
          , "a value expression"
          , LOpnd2 . ExpPosition
          )
      ; LBinOpExpr . ExpIsUsable := FALSE
      END (*IF*)
    ; IF LBinOpExpr . ExpIsUsable
      THEN
        LBinOpExpr . ExpIsConst
          := LOpnd1 . ExpIsConst
               AND LOpnd2 . ExpIsConst
               AND IntSets . IsElement
                     ( LBinOpExpr . ExpOpcode , FM3Std . TwoParamSet )
               AND IntSets . IsElement
                     ( LBinOpExpr . ExpOpcode , FM3Std . WordLongQualifierSet )
      ; LBinOpExpr . ExpReachedDeclNoSet
          := IntSets . Union
               ( LOpnd1 . ExpReachedDeclNoSet , LOpnd2 . ExpReachedDeclNoSet )
      END (*IF*) 
    END BinaryOp

; PROCEDURE DeclType ( DeclKindTag : TEXT ) 
  (* PRE: The type expression exists and is on the expression stack. *) 

  = VAR  LTypeExpr : FM3Exprs . ExprTyp 

  ; BEGIN
      LTypeExpr := FM3Exprs . PopExprStack ( )
    ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ FALSE ] := LTypeExpr
    ; IF NOT LTypeExpr . ExpIsPresent THEN RETURN END (*IF*) 
    ; IF NOT LTypeExpr . ExpIsUsable THEN RETURN END (*IF*) 
    ; IF FALSE
(* FIXME:         LTypeExpr could be an identifier referring to a decl farther
                  to the left, whose (the decl's) kind we can't get here.
                  This check will have to be done in Pass3
*) 
         AND LTypeExpr . ExpUpKind # Ekt . EkType 
      THEN
        FM3Messages . ErrorArr 
          ( ARRAY OF REFANY
              { "Type of " 
              , DeclKindTag 
              , " declaration must be a type expression." 
              } 
          , LTypeExpr . ExpPosition
          )
      ; LTypeExpr . ExpIsUsable := FALSE
      ; LTypeExpr . ExpIsLegalRecursive := TRUE (* Necessary?*)
      ; FM3Exprs . ExprStackTopObj . ExpIsUsable := FALSE 
      END (*IF*)
    END DeclType

; PROCEDURE DeclValue ( DeclKindTag : TEXT ; MustBeConst : BOOLEAN ) 
  (* PRE: The value expression exists and is on the expression stack. *) 

  = VAR  LValueExpr : FM3Exprs . ExprTyp 

  ; BEGIN
      LValueExpr := FM3Exprs . PopExprStack ( )
    ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ TRUE ] := LValueExpr
    ; IF NOT LValueExpr . ExpIsPresent THEN RETURN END (*IF*) 
    ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE 
      (* ^Type is coming up next. *) 
    ; IF NOT LValueExpr . ExpIsUsable THEN RETURN END (*IF*) 
    ; IF FALSE 
(* FIXME:         LValusExpr could be an identifier referring to a decl farther
                  to the left, whose (the decl's) kind we can't get here.
                  This check will have to be done in Pass3
*)
         AND LValueExpr . ExpUpKind # Ekt . EkValue
      THEN
        FM3Messages . ErrorArr 
          ( ARRAY OF REFANY
              { DeclKindTag , " declaration must be a value expression." } 
          , LValueExpr . ExpPosition
          )
      ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE  
      ELSIF FALSE
(* FIXME Same problem as above.*) 
            AND MustBeConst AND NOT LValueExpr . ExpIsConst
      THEN
        FM3Messages . ErrorArr 
          ( ARRAY OF REFANY
              { DeclKindTag , " declaration must be constant." }
          , LValueExpr . ExpPosition
          )
      ELSE RETURN 
      END (*IF*)
    ; LValueExpr . ExpIsUsable := FALSE
    ; LValueExpr . ExpIsLegalRecursive := TRUE (* Necessary?*)
    ; FM3Exprs . ExprStackTopObj . ExpIsUsable := FALSE
    END DeclValue 

; PROCEDURE InsideDecl ( ) : BOOLEAN 

  = BEGIN
      RETURN TRUE
    END InsideDecl

; PROCEDURE HandleTok ( READONLY TokResult : TokResultTyp ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR HtPass1RdBack : RdBackFile . T 
  ; VAR HtPass2RdBack : RdBackFile . T
  ; VAR LNewExpr , LValueExpr , LProcExpr , LListElem : FM3Exprs . ExprTyp
  ; VAR LCallExpr : FM3Exprs . ExprCallTyp
  ; VAR LLongInt : LONGINT 
  ; VAR LScopeNo : FM3Globals . ScopeNoTyp
  ; VAR LOpcode : FM3SrcToks . TokTyp
  ; VAR LCt : INTEGER 
  ; VAR LPosition : tPosition
  ; VAR HtSkipping : BOOLEAN
  ; VAR LBool : BOOLEAN

  ; PROCEDURE HtIntLit ( LoTypeNo : FM3LoTypes . LoTypeNoTyp )
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
            WITH LExpr = NEW ( FM3Exprs . ExprConstValueTyp ) 
            DO 
              LExpr . ExpScalarConstVal := LLongInt
            ; LExpr . ExpLoTypeInfoRef
                := VarArray_Int_Refany . Fetch
                     ( FM3LoTypes . LoTypeMap , LoTypeNo )  
            ; LExpr . ExpPosition := LPosition
            ; LExpr . ExpUpKind := Ekt . EkValue 
            ; LExpr . ExpIsConst := TRUE 
            ; LExpr . ExpConstValIsKnown := TRUE 
            ; LExpr . ExpIsLegalRecursive := TRUE
            ; LExpr . ExpState := Est . EsResolved
            ; DefExprRt ( LExpr )
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

  ; PROCEDURE HtExprRt ( NewExpr : FM3Exprs . ExprTyp )

    = BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
        DO IF HtSkipping 
          THEN (* NewExpr becomes immediate garbage. *) 
          ELSE 
            NewExpr . ExpPosition := WPosition
          ; DefExprRt ( NewExpr )
          END (*IF*)
        END (*WITH*)
      END HtExprRt 

  ; PROCEDURE HtExprOpnd1 ( )
      (* PRE: TOS is 1st (LM) operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp
    ; VAR LParentExpr : FM3Exprs . Expr1OpndTyp 

    ; BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
        DO IF NOT HtSkipping 
          THEN
            LOpnd := FM3Exprs . PopExprStack ( )
          ; LParentExpr
              := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . Expr1OpndTyp ) 
          ; LParentExpr . ExpOpnd1 := LOpnd
          ; DefExprLt ( ) 
          END (*IF*) 
        END (*WITH*)
      END HtExprOpnd1 

  ; PROCEDURE HtExprOpnd2  ( )
      (* PRE: TOS is 2nd operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp
    ; VAR LParentExpr : FM3Exprs . Expr2OpndTyp 

    ; BEGIN 
        WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
        DO IF NOT HtSkipping 
          THEN
            LOpnd := FM3Exprs . PopExprStack ( )
          ; LParentExpr
              := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . Expr2OpndTyp ) 
          ; LParentExpr . ExpOpnd2 := LOpnd
          ; DefExprLt ( ) 
          END (*IF*) 
        END (*WITH*)
      END HtExprOpnd2

  ; <*INLINE*> PROCEDURE IntUnionSelf
      ( VAR (*IN OUT*) Self : IntSets . T ; Opnd1 , Opnd2 : IntSets . T )  

    = VAR LResult : IntSets . T 

    ; BEGIN
        LResult := IntSets . Union ( Self , Opnd1 ) 
      ; LResult := IntSets . Union ( LResult , Opnd2 )
      ; Self := LResult 
      END IntUnionSelf

  ; PROCEDURE HtPassTokenThru ( )

    = BEGIN 
        IF HtSkipping 
        THEN
          DiscardOperands
            ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
            , TokResult . TrRdBack
            ) 
        ELSE 
          CopyOperandsInOrder
            ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
            , TokResult . TrRdBack 
            , HtPass2RdBack
            , MaybeSkip := TRUE 
            ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )
        END (*IF*) 
      END HtPassTokenThru 

  ; PROCEDURE HtMaybePassTokenThru ( ) : BOOLEAN (* Handled it somehow. *)
    (* Use this only for writing to pass 2 output. *) 

    = BEGIN 
        IF HtSkipping 
        THEN
          DiscardOperands
            ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
            , TokResult . TrRdBack
            ) 
        ; RETURN TRUE  
        ELSE 
          IF NOT FM3Scopes . OpenScopeStackTopRef ^ . ScpInsideDecl 
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

  ; BEGIN (*HandleTok*) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; HtPass1RdBack := LUnitRef ^ . UntPass1OutRdBack 
    ; HtPass2RdBack := LUnitRef ^ . UntPass2OutRdBack
    ; HtSkipping
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0

; IF TokResult . TrTok = 1079
  THEN HtSkipping := FALSE 
  END 


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
        ; <* ASSERT FM3Scopes . PopDeclScopeRef ( ) ^ . ScpSelfScopeNo
             = LScopeNo
          *>

      | Itk . ItkOpenScopeRt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
        ; LScopeRef := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
        ; LScopeRef ^ . ScpDeclGraph
            := FM3Graph . NewEmpty ( MaxNodeCt := LScopeRef ^ . ScpDeclCt ) 
        ; FM3Scopes . PushOpenScopeRef ( LScopeRef ) 

      | Itk . ItkOpenScopeLt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack )
        ; OpenScopeLt ( LScopeNo ) 

      | Itk . ItkOpenDeclListRt
      =>  FM3Scopes . OpenScopeStackTopRef ^ . ScpInsideDecl := TRUE 
        ; HtPassTokenThru ( ) 

      | Itk . ItkOpenDeclListLt
      =>  FM3Scopes . OpenScopeStackTopRef ^ . ScpInsideDecl := FALSE 
        ; HtPassTokenThru ( ) 

      | Itk . ItkConstDeclRt 
      , Itk . ItkVarDeclRt 
      , Itk . ItkFieldDeclRt (* Of either record or object. *) 
      , Itk . ItkVALUEFormalRt
      , Itk . ItkVARFormalRt
      , Itk . ItkROFormalRt
      =>  FM3Scopes . DeclScopeStackTopRef ^ .  ScpCurDeclRefNoSet
            := IntSets . Empty ( )
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := TRUE
            (* ^Value def coming up next. *)
        ; HtPassTokenThru ( )

      | Itk . ItkConstDeclValue (* Parser ensures this exists, no boolean arg. *)
      =>  DeclValue ( "Value of CONST" , MustBeConst := TRUE )
        ; DiscardOperands ( 2 , TokResult . TrRdBack )
      
      | Itk . ItkFieldDeclValue (* Of either record or object. *)
      => DeclValue ( "Default value of field" , MustBeConst := FALSE )
        ; DiscardOperands ( 2 , TokResult . TrRdBack )

      | Itk . ItkVarDeclValue 
      => DeclValue ( "Default value of variable" , MustBeConst := FALSE )  
        ; DiscardOperands ( 2 , TokResult . TrRdBack )
         
      | Itk . ItkVALUEFormalValue
      , Itk . ItkROFormalValue
      => DeclValue ( "Default value of formal" , MustBeConst := FALSE ) 
        ; DiscardOperands ( 2 , TokResult . TrRdBack )

      | Itk . ItkVARFormalValue
      =>  <* ASSERT NOT FM3Exprs . ExprStackTopObj . ExpIsPresent *> 
          FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE
        ; DiscardOperands ( 2 , TokResult . TrRdBack )

       | Itk . ItkDeclTypeAbsent
       , Itk . ItkDeclValAbsent
       =>  LPosition := GetBwdPos ( TokResult . TrRdBack )
         ; FM3Exprs . PushExprStack
             ( NEW ( FM3Exprs . ExprTyp , ExpIsPresent := FALSE ) ) 
         ; IF NOT HtSkipping 
           THEN
             PushExprIgnore ( LPosition ) 
           ; PutBwdP2 ( HtPass2RdBack , VAL ( LPosition . Column , LONGINT ) ) 
           ; PutBwdP2 ( HtPass2RdBack , VAL ( LPosition . Line , LONGINT ) ) 
           ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )
           END (*IF*) 

       | Itk . ItkConstDeclType
       =>  DeclType ( "CONST" )
         ; DiscardOperands ( 2 , TokResult . TrRdBack )

       | Itk . ItkFieldDeclType (* Of either record or object. *)
       =>  DeclType ( "field" )
         ; DiscardOperands ( 2 , TokResult . TrRdBack )

       | Itk . ItkVarDeclType 
       =>  DeclType ( "variable" )  
         ; DiscardOperands ( 2 , TokResult . TrRdBack )

      | Itk . ItkVALUEFormalType
      , Itk . ItkROFormalType
      , Itk . ItkVARFormalType
       =>  DeclType ( "formal" ) 
         ; DiscardOperands ( 2 , TokResult . TrRdBack )


      | Itk . ItkConstDeclLt 
      , Itk . ItkVarDeclLt 
      , Itk . ItkVALUEFormalLt
      , Itk . ItkVARFormalLt
      , Itk . ItkROFormalLt
      , Itk . ItkFieldDeclLt (* Of either record or object. *) 
      =>  HtPassTokenThru ( )

      | Itk . ItkTypeDeclRt
      , Itk . ItkFullRevealRt 
      , Itk . ItkPartialRevealRt
      =>  FM3Scopes . DeclScopeStackTopRef ^ .  ScpCurDeclRefNoSet
            := IntSets . Empty ( ) 
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE
            (* ^ TYPE Decl has only a type def. *) 
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclType
      , Itk . ItkFullRevealType 
      , Itk . ItkPartialRevealSubtype
      =>  EVAL FM3Exprs . PopExprStack ( )  
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclLt
      , Itk . ItkFullRevealLt 
      , Itk . ItkPartialRevealLt
      =>  HtPassTokenThru ( )

      | Itk . ItkMethodDeclLt
      =>  (* One subtree to pop. *)
          EVAL FM3Exprs . PopExprStack ( )  
        ; HtPassTokenThru ( )

(* CONSISTIFY: For some of these, fetch the operands inside the called proc. *) 
      | Itk . ItkDuplDeclId
      => EVAL DuplDeclIdR2L ( TokResult )

      | Itk . ItkDeclId
      => EVAL DeclIdR2L ( TokResult )

(* FIXME: We now use different tokens for different declkinds, eg.
    ItkVALUEFormalIdListElem.  But is that necessary? *)

      | Itk . ItkReservedIdRef
      =>  IF InsideDecl ()
          THEN
            LNewExpr
              := NEW ( FM3Exprs . ExprReservedIdRefTyp ) 
          ; LNewExpr . ExpOpcode := GetBwdAtom ( TokResult . TrRdBack )
          ; LNewExpr . ExpPosition := GetBwdPos ( TokResult . TrRdBack )
          ; LNewExpr . ExpIsLegalRecursive := TRUE
          ; LNewExpr . ExpUpKind
              := FM3Builtins . OpExprKind ( LNewExpr . ExpOpcode ) 
          (* Other properties computed later. *) 
          ; DefExprRt ( LNewExpr )
          ELSE HtPassTokenThru ( ) 
          END (*IF*) 

      | Itk . ItkIdRefAtom
      => IdentRefR2L ( TokResult )

      | Itk . ItkIdRefAtomNotUsable 
      =>  LNewExpr := NEW ( FM3Exprs . ExprTyp ) 
        ; LNewExpr . ExpOpcode := GetBwdAtom ( TokResult . TrRdBack )
        ; LNewExpr . ExpPosition := GetBwdPos ( TokResult . TrRdBack )
        ; LNewExpr . ExpIsLegalRecursive := TRUE
        ; LNewExpr . ExpUpKind := Ekt . EkNull
        ; DefExprRt ( LNewExpr )

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
      => HtIntLit ( FM3LoTypes . LoTypeNoInt ) 

      | Itk . ItkLongIntLit
      => HtIntLit ( FM3LoTypes . LoTypeNoLong ) 

      | Itk . ItkTextLitRt
      , Itk . ItkWideTextLitRt
      =>  LCt := GetBwdInt ( TokResult . TrRdBack ) (* ^Atom, actually. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IF NOT HtSkipping 
          THEN
            DefExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpUpKind := Ekt . EkValue
                    , ExpScalarConstVal := VAL ( LCt , LONGINT ) 
                    , ExpOpcode := TokResult . TrTok 
                    , ExpPosition := LPosition 
                    )
              )
          END (*IF*) 

      | Itk . ItkTextLitLt
      , Itk . ItkWideTextLitLt
      => DiscardOperands ( 3 (* Atom, position. *) , TokResult . TrRdBack ) 

      (* Enumeration type: *) 
(* FIXME: Some of these need to copy the token and arguments. *)   
      | Itk . ItkEnumTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
        ; HtExprRt
            ( NEW ( FM3Exprs . ExprEnumTypeTyp , ExpUpKind := Ekt . EkType ) ) 

      | Itk . ItkEnumTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 

      (* Record type: *) 
      | Itk . ItkRecTypeRt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
        ; HtExprRt
            ( NEW ( FM3Exprs . ExprRecTypeTyp , ExpUpKind := Ekt . EkType ) )
(* Copy token? *) 

      | Itk . ItkRecTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *)
        ; EVAL GetBwdPos ( TokResult . TrRdBack )
(* Copy token? *)

      (* Brands: *)

      | Itk . ItkBrandAbsent
      =>  HtExprRt
            ( NEW ( FM3Exprs . ExprTyp
                  , ExpUpKind := Ekt . EkNull
                  , ExpIsLegalRecursive := TRUE
                  , ExpIsPresent := FALSE 
                  )
            ) 

      | Itk . ItkBrandAnon
      =>  WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
          DO IF NOT HtSkipping 
          THEN 
            LNewExpr
              := FM3Builtins . BuiltinExpr
                   ( FM3SrcToks . StkRTUniqueBrand , WPosition ) 
          ; DefExprRt ( LNewExpr )
          END (*IF*)
        END (*WITH*)

      (* An explicit brand is just an expression. *) 

      (* REF type: *) 
      | Itk . ItkREFTypeRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*)
        ; LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN ) 
        ; HtExprRt
            ( NEW ( FM3Exprs . Expr2OpndTyp
                  , ExpUpKind := Ekt . EkType
                  , ExpIsLegalRecursive := TRUE
                  , ExpRefTypeIsUntraced := LBool
                  , ExpOpcode := Stk . StkRwREF
                  , ExpRefTypeIsUntraced := LBool
                  (* ExpOpnd2 will later be referent. *) 
                  (* ExpOpnd1 will later be brand. *) 
                  )
            ) 

      | Itk . ItkREFTypeReferent 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd2 ( ) (* Referent *)

      | Itk . ItkREFTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN ) 
        ; HtExprOpnd1 ( ) (* Brand *)
        ; SynthIsUsable2 ( FM3Exprs . PopExprStack ( ) (* The REF Type. *) )

      (* Open array type: *) 
      | Itk . ItkOpenArrayTypeRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprRt
            ( NEW ( FM3Exprs . ExprOpenArrayTypeTyp
                  , ExpUpKind := Ekt . EkValue
                  )
            )

      | Itk . ItkOpenArrayTypeLt
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd1 ( ) 

      (* Subrange type: *) 
      | Itk . ItkSubrTypeRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprRt
            ( NEW ( FM3Exprs . ExprSubrTypeTyp , ExpUpKind := Ekt . EkType ) ) 

      | Itk . ItkSubrTypeEllipsis 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprOpnd2 ( )

      | Itk . ItkSubrTypeLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*)
        ; HtExprOpnd1 ( )

      (* Unary operators: *) 
      | Itk . ItkUnaryOpRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*)
        ; HtExprRt
            ( NEW ( FM3Exprs . ExprBinOpTyp
                  , ExpOpcode := GetBwdInt ( TokResult . TrRdBack )
                  )
            ) 
 
      | Itk . ItkUnaryOpLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LOpcode := GetBwdInt ( TokResult . TrRdBack ) 
        ; HtExprOpnd1 ( ) (* Only operand. *)
        ; IF NOT HtSkipping THEN UnaryOp ( LOpcode ) END (*IF*)

      (* Binary Operators: *) 
      | Itk . ItkBinaryOpRt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; HtExprRt
            ( NEW ( FM3Exprs . ExprBinOpTyp
                  , ExpOpcode := GetBwdInt ( TokResult . TrRdBack )
                  )
            )

      | Itk . ItkBinaryOpOperator
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LOpcode := GetBwdInt ( TokResult . TrRdBack ) (* Opcode. *) 
        ; HtExprOpnd2 ( ) (* Right operand. *)

      | Itk . ItkBinaryOpLt 
      =>  IF HtMaybePassTokenThru ( ) THEN RETURN END (*IF*) 
        ; LOpcode := GetBwdInt ( TokResult . TrRdBack ) 
        ; HtExprOpnd1 ( ) (* Left operand. *)
        ; IF NOT HtSkipping THEN BinaryOp ( LOpcode ) END (*IF*)

      | Itk . ItkCallRt
      =>  LCt := GetBwdInt ( TokResult . TrRdBack ) (* Actuals count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; LCallExpr := NEW ( FM3Exprs . ExprCallTyp )
        ; LCallExpr . ExpActualsList
            := NEW ( REF ARRAY OF FM3Exprs . ExprTyp , LCt )
        ; LCallExpr . ExpActualNo := LCt
        ; LCallExpr . ExpPosition := LPosition
        ; FM3Exprs . PushExprStack ( LCallExpr )

      | Itk . ItkActualsListRt
       => LCt := GetBwdInt ( TokResult . TrRdBack ) (* Actuals count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IF LCt > 0
          THEN 
            LCallExpr 
              := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprCallTyp )
          ; <* ASSERT LCt = LCallExpr . ExpActualNo *>
          END (*IF*) 

      | Itk . ItkActualsListSep
       => LCt := GetBwdInt ( TokResult . TrRdBack ) (* Actuals count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; LListElem := FM3Exprs . PopExprStack ( )
        ; LCallExpr 
            := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprCallTyp )
        ; DEC ( LCallExpr . ExpActualNo )  
        ; LCallExpr . ExpActualsList ^ [ LCallExpr . ExpActualNo ] := LListElem

      | Itk . ItkActualsListLt
       => LCt := GetBwdInt ( TokResult . TrRdBack ) (* Actuals count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IF LCt > 0
          THEN 
            LListElem := FM3Exprs . PopExprStack ( ) (* LM actual. *) 
          ; LCallExpr 
              := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprCallTyp )
          ; <* ASSERT LCallExpr . ExpActualNo = 1 *> 
            DEC ( LCallExpr . ExpActualNo )
          ; LCallExpr . ExpActualsList ^ [ LCallExpr . ExpActualNo ] := LListElem
          END (*IF*) 

      | Itk . ItkCallLt
      =>  LCt := GetBwdInt ( TokResult . TrRdBack )
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; LProcExpr := FM3Exprs . PopExprStack ( )
        ; LCallExpr
            := NARROW ( FM3Exprs . PopExprStack ( ) , FM3Exprs . ExprCallTyp )
        ; <* ASSERT LPosition = LCallExpr . ExpPosition *>
          <* ASSERT LCallExpr . ExpActualNo = 0 *>
          LCallExpr . ExpCallProc := LProcExpr 
        ; FM3Exprs . PushExprStack ( MaybeConvertCallToOperator ( LCallExpr ) )
        ; PutBwdP2
            ( HtPass2RdBack , VAL ( LCallExpr . ExpSelfExprNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( Itk . ItkExprTyp , LONGINT ) )

      ELSE (* No special pass2 handling. *)
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

; PROCEDURE OpenScopeLt ( ScopeNo : FM3Globals . ScopeNoTyp ) 

  = VAR OslScopeRef : FM3Scopes . ScopeRefTyp

  ; PROCEDURE VisitSCC
      ( READONLY SCC : ARRAY OF INTEGER
                             (* ^ DeclNo, biased relative to the scope. *) )
    (* A callback. *) 

    = VAR LWrT : Wr . T 
    ; VAR LDeclRef0 , LDeclRefn : FM3Decls . DeclRefTyp
    ; VAR LDeclNo : FM3Globals . DeclNoTyp 
    ; VAR LIdentCt : INTEGER

    ; BEGIN
        LIdentCt := NUMBER ( SCC ) 
      ; IF LIdentCt < 1 THEN RETURN END (*IF*)
      ; LWrT := TextWr . New (  )
      ; Wr . PutText ( LWrT , "Illegal recursive declaration" )
      ; IF LIdentCt > 1 THEN Wr . PutChar ( LWrT , 's' ) END (*IF*) 
      ; Wr . PutText ( LWrT , ": \"" )
      ; LDeclNo := SCC [ 0 ] + OslScopeRef ^ . ScpMinDeclNo (* Remove bias. *) 
      ; LDeclRef0 (* Implicit NARROW. *) 
          := VarArray_Int_Refany . Fetch
               ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo )
      ; FM3Utils . PutOACharsWr
          ( LWrT
          , FM3Utils . CharsOfAtom
              ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
              , LDeclRef0 ^ . DclIdAtom
              )
          ) 
      ; Wr . PutChar ( LWrT , '\"' ) 
      ; LDeclRef0 ^ . DclIsUsable := FALSE 
      ; IF LIdentCt > 1
        THEN
          Wr . PutText ( LWrT , ", also involving")
        ; FOR RI := 1 TO LIdentCt - 1
          DO 
            Wr . PutText ( LWrT , FM3Messages . NLIndent )
          ; Wr . PutChar ( LWrT , '\"' )
          ; LDeclNo := SCC [ RI ] + OslScopeRef ^ . ScpMinDeclNo (* Remove bias. *) 
          ; LDeclRefn (* Implicit NARROW. *) 
              := VarArray_Int_Refany . Fetch
                   ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo )  
          ; FM3Utils . PutOACharsWr
              ( LWrT
              , FM3Utils . CharsOfAtom
                  ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                  , LDeclRefn ^ . DclIdAtom
                  )
              ) 
          ; Wr . PutText ( LWrT , "\" " ) 
          ; Wr . PutText ( LWrT , FM3Utils . PositionImage ( LDeclRefn . DclPos ) )
          ; LDeclRefn ^ . DclIsUsable := FALSE 
          END (*FOR*) 
        END (*IF*) 
      ; FM3Messages . ErrorArr
          ( ARRAY OF REFANY { TextWr . ToText ( LWrT ) } , LDeclRef0 . DclPos ) 
      END VisitSCC

  ; BEGIN (* OpenScopeLt *) 
      OslScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi <= 0
      THEN (* Not skipping. *) 
        FM3Graph . SCCs ( OslScopeRef ^ . ScpDeclGraph , VisitSCC ) 
      ; <* ASSERT FM3Scopes . PopOpenScopeRef ( )
                  = FM3Scopes . ScopeRefOfScopeNo ( ScopeNo )
        *>
      END (*IF*)
    ; WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
      DO 
        PutBwdP2 ( Wp2RdBack , VAL ( ScopeNo , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkOpenScopeLt, LONGINT ) )
      END (*WITH*) 
    END OpenScopeLt 

; PROCEDURE LookupAtomInOpenScopes
    ( IdAtom : FM3Base . AtomTyp ) : FM3Globals . DeclNoTyp  
  (* In innermost enclosing open scope on open scope stack. *) 

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

; PROCEDURE DuplDeclIdR2L ( READONLY TokResult : TokResultTyp )
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

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Globals . DeclNoTyp
    ; BEGIN (* Block. *)
        DdiAtom := GetBwdAtom ( TokResult . TrRdBack )
      ; DdiPosition := GetBwdPos ( TokResult . TrRdBack )
      ; LDeclNo
          := LookupDeclNoInScope
               ( FM3Scopes . DeclScopeStackTopRef ^ , DdiAtom )
      ; <*ASSERT LDeclNo # FM3Globals . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdR2L
    
; PROCEDURE DeclIdR2L ( READONLY TokResult : TokResultTyp )
  : FM3Globals . DeclNoTyp
  (* ^This will be the only decl of DeclIdAtom in its scope. *) 

  = VAR DidAtom : FM3Base . AtomTyp
  ; VAR DidStdTok : FM3SrcToks . TokTyp 
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
            ErrorArr
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
          := FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ TRUE ]  
      ; LDeclRef ^ . DclDefType 
          := FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ FALSE ]
      ; LDeclRef ^ . DclStdTok := DidStdTok 
          
      ; CASE DidDeclKind OF
        | Dkt . DkVar
        =>  <* ASSERT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                      IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkConst
        =>  <* ASSERT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                      IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkType                        
        , Dkt . DkProc
        , Dkt . DkReveal 
        =>  <* ASSERT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                      IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkVALUEFormal
        , Dkt . DkVARFormal
        , Dkt . DkROFormal
        , Dkt . DkRecField
        , Dkt . DkObjField
        , Dkt . DkMethod
        =>  <* ASSERT NOT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                          IN FM3Scopes . ScopeKindSetOpen
            *>

        | Dkt . DkExc
        , Dkt . DkEnumLit
        =>  <* ASSERT NOT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
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

  ; BEGIN (*DeclIdR2L*)
      DidDeclKind := GetBwdDeclKind ( TokResult . TrRdBack )
    ; DidAtom := GetBwdAtom ( TokResult . TrRdBack )
    ; DidStdTok := GetBwdInt ( TokResult . TrRdBack )
    ; DidPosition := GetBwdPos ( TokResult . TrRdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN (* We are skipping output. *) RETURN FM3Globals . DeclNoNull 
      END (*IF*) 

    ; WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
      DO 
        DidOpenDeclNo
          := LookupDeclNoInScope
               ( FM3Scopes . DeclScopeStackTopRef ^ , DidAtom ) 
      ; <*ASSERT DidOpenDeclNo # FM3Globals . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem 
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap
          , DidOpenDeclNo
          , DidVisitDecl
          )
      ; PutBwdP2 ( Wp2RdBack , VAL ( DidPosition . Column , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( DidPosition . Line , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( DidOpenDeclNo , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
      ; RETURN DidOpenDeclNo
      END (*WITH*) 
    END DeclIdR2L

; PROCEDURE SynthIsUsable1 ( Parent : FM3Exprs . Expr1OpndTyp )

  = BEGIN (*SynthIsUsable1*)
      Parent . ExpIsUsable
        := Parent . ExpIsUsable AND Parent . ExpOpnd1 . ExpIsUsable 
    END SynthIsUsable1        

; PROCEDURE SynthIsUsable2 ( Parent : FM3Exprs . Expr2OpndTyp )

  = BEGIN (*SynthIsUsable2*)
      Parent . ExpIsUsable
        := Parent . ExpIsUsable AND Parent . ExpOpnd2 . ExpIsUsable 
    END SynthIsUsable2        

; PROCEDURE SynthIsUsable12 ( Parent : FM3Exprs . Expr2OpndTyp )

  = BEGIN (*SynthIsUsable12*)
      Parent . ExpIsUsable
        := Parent . ExpIsUsable
           AND Parent . ExpOpnd1 . ExpIsUsable 
           AND Parent . ExpOpnd2 . ExpIsUsable 
    END SynthIsUsable12     

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
        WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        DO 
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

; PROCEDURE ReservedIdR2L ( READONLY TokResult : TokResultTyp )

  = VAR LReservedId : Stk . TokTyp 
  ; VAR LPosition : FM3Base . tPosition 

  ; BEGIN
      LReservedId := GetBwdAtom ( TokResult . TrRdBack )
    ; LPosition := GetBwdPos ( TokResult . TrRdBack )
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
    END ReservedIdR2L

; PROCEDURE CheckOpndKind
    ( OpndExpr : FM3Exprs . ExprTyp
    ; Opcode : FM3SrcToks . TokTyp 
    ; OpndTag : TEXT 
    ; AllowedKinds : FM3Exprs . ExprKindSetTyp 
    )
  : BOOLEAN (* Check is OK. *) 

  = BEGIN
      IF OpndExpr = NIL THEN RETURN TRUE END (*IF*) 
    ; IF NOT OpndExpr . ExpUpKind IN AllowedKinds
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { OpndTag
              , " of "
              , FM3SrcToks . Image ( Opcode )
              , " must be "
              , FM3Exprs . ExprKindSetMessage ( AllowedKinds ) 
              , "." 
              }
          , OpndExpr . ExpPosition 
          )
      ; RETURN FALSE
      ELSE RETURN TRUE
      END (*IF*) 
    END CheckOpndKind

; PROCEDURE DisambiguateStdDeclTok
     ( UnitTok , OrigDeclTok : FM3SrcToks . TokTyp ) : FM3SrcToks . TokTyp
  (* Make decl tokens that occur in >1 standard interface unique. *)
  (* OrigDeclTok denotes an identifier declared in more that one standard
     interface.  Replace it with one that is unique. *) 

  = BEGIN
      IF FM3SrcToks . StkMinWordLong <= OrigDeclTok
         AND OrigDeclTok <= FM3SrcToks . StkMaxWordLong
         AND UnitTok = FM3SrcToks . StkPdLong
      THEN
        (*The only noticed cases of namesake decls in std interfaces. *) 
        RETURN
          OrigDeclTok - FM3SrcToks . StkMinWordLong +  FM3SrcToks . StkMinLong
      ELSE RETURN OrigDeclTok
      END (*IF*) 
    END DisambiguateStdDeclTok

; PROCEDURE CheckOperation ( OpExpr : FM3Exprs . ExprTyp ) : BOOLEAN (* OK *) 

  = VAR LOK : BOOLEAN

  ; BEGIN
      TYPECASE OpExpr OF
      | NULL => RETURN TRUE
      
      | FM3Exprs . ExprBinOpTyp ( TOpExpr )
      =>  LOK := TRUE
        ; IF NOT CheckOpndKind
                   ( TOpExpr . ExpOpnd1
                   , TOpExpr . ExpOpcode 
                   , "Left parameter"
                   , TOpExpr . ExpBinOpLtOpndKindsAllowed
                   )
          THEN LOK := FALSE
          END (*IF*)

        ; IF TOpExpr . ExpBinOpActualsCt > 1
             AND  NOT CheckOpndKind
                        ( TOpExpr . ExpOpnd2 
                        , TOpExpr . ExpOpcode
                        , "Second parameter"
                        , TOpExpr . ExpBinOpRtOpndKindsAllowed
                        )
          THEN LOK := FALSE
          END (*IF*)

        ; TYPECASE OpExpr OF
          | NULL =>
          
          | FM3Exprs . ExprQuadOpTyp ( TQuadOpExpr )
          =>  IF NOT CheckOpndKind
                      ( TQuadOpExpr . ExpQuadOpOpnd3 
                      , TQuadOpExpr . ExpOpcode
                      , "Third parameter"
                      , FM3Exprs . EkSetValue 
                      )
              THEN LOK := FALSE
              END (*IF*)
            ; IF TOpExpr . ExpBinOpActualsCt > 3
                 AND NOT CheckOpndKind
                           ( TQuadOpExpr . ExpQuadOpOpnd4 
                           , TQuadOpExpr . ExpOpcode
                           , "Forth parameter"
                           , FM3Exprs . EkSetValue 
                           )
              THEN LOK := FALSE
              END (*IF*)

          ELSE
          END (*TYPECASE*) 

        ; IF NOT LOK
          THEN
            OpExpr . ExpIsUsable := FALSE
          ; RETURN FALSE 
          ELSE RETURN TRUE
          END (*IF*)
      ELSE RETURN TRUE 
      END (*TYPECASE*) 
    END CheckOperation 

; PROCEDURE MaybeConvertCallToOperator ( OrigExpr : FM3Exprs . ExprTyp )
  : FM3Exprs . ExprTyp
  (* If conditions are met, return an ExpBinOp-rooted subtree that is the
     conversion of OrigExpr.
  *) 

  = VAR LCallExpr : FM3Exprs . ExprCallTyp 
  ; VAR LOpnd1 , LOpnd2 : FM3Exprs . ExprTyp 
  ; VAR LNewExpr : FM3Exprs . ExprBinOpTyp
  ; VAR LPluralSuffix : TEXT
  ; VAR LActualsCt : INTEGER
  ; VAR LUnitTok , LDeclTok : FM3SrcToks . TokTyp

  ; BEGIN
      IF NOT OrigExpr . ExpIsUsable THEN RETURN OrigExpr END (*IF*)
    ; TYPECASE OrigExpr OF
      | NULL => RETURN NIL
      | FM3Exprs . ExprCallTyp ( TCallExpr )
      =>  LCallExpr := TCallExpr 
      ELSE RETURN OrigExpr
      END (*TYPECASE*) 
    ; IF LCallExpr . ExpActualsList = NIL THEN RETURN OrigExpr END (*IF*)
    ; LActualsCt := NUMBER ( LCallExpr . ExpActualsList ^ )
    ; TYPECASE LCallExpr . ExpCallProc OF
    
      | NULL => <* ASSERT FALSE , "NIL ExpCallProc of ExprCallTyp." *> 

      | FM3Exprs . ExprReservedIdRefTyp ( TReservedIdRef )  
       => LUnitTok := FM3Base . TokNull 
        ; LDeclTok := TReservedIdRef . ExpOpcode  

      | FM3Exprs . ExprIdentRefTyp ( TExprIdentRef )  
       => GetStdToks 
            ( FM3Units . UnitStackTopRef ^ . UntSelfUnitNo 
            , TExprIdentRef . ExpIdentDeclNo 
            , (*OUT*) LUnitTok 
            , (*OUT*) LDeclTok 
            ) 

      | FM3Exprs . ExprRemoteRefTyp ( TExprRemoteRef ) 
       => GetStdToks 
            ( TExprRemoteRef . ExpRemoteUnitNo 
            , TExprRemoteRef . ExpRemoteDeclNo 
            , (*OUT*) LUnitTok 
            , (*OUT*) LDeclTok 
            ) 

      ELSE (* Called proc is not a named builtin. *) RETURN OrigExpr 
      END (* TYPECASE *)  
    ; IF LUnitTok = FM3Base . TokNull AND LDeclTok = FM3Base . TokNull 
      THEN (* Also not a call on anything builtin. *) RETURN OrigExpr
      END (*IF*) 

    ; LDeclTok := DisambiguateStdDeclTok ( LUnitTok , LDeclTok ) 
      (* ^LDeclTok now belongs to only one unit . *) 
    ; IF NOT FM3Builtins . IsOperationTok ( LDeclTok ) 
      THEN (* It's not a callable builtin (type, constant, etc.) . *) 
        FM3Messages . ErrorArr 
          ( ARRAY OF REFANY 
              { FM3SrcToks . Image ( LDeclTok ) , " is not callable." }
          , LCallExpr . ExpCallProc . ExpPosition 
          ) 
      ; OrigExpr . ExpIsUsable := FALSE 
      ; RETURN OrigExpr 
      END (*IF*)

    ; LNewExpr
        := FM3Builtins . BuiltinExpr
             ( LDeclTok , LCallExpr . ExpCallProc . ExpPosition ) 
    ; IF LNewExpr . ExpBinOpActualsCt = FM3Builtins . ActualsCtAtLeastOne  
         (* NEW is the only case here. *)
      THEN IF  LActualsCt < 1
        THEN 
          FM3Messages . ErrorArr 
            ( ARRAY OF REFANY 
                { "NEW requires one or more actual parameters." }
            , LCallExpr . ExpPosition 
            ) 
        ; LNewExpr . ExpIsUsable := FALSE
        ; RETURN LCallExpr
        END (*IF*) 
      ELSIF LActualsCt # LNewExpr . ExpBinOpActualsCt 
      THEN (* Wrong number of actual parameters. *) 
        IF LNewExpr . ExpBinOpActualsCt = 1 THEN LPluralSuffix := "."  
        ELSE LPluralSuffix := "s." 
        END (*IF*)
      ; FM3Messages . ErrorArr 
          ( ARRAY OF REFANY 
              { FM3SrcToks . Image ( LDeclTok )
              , " requires "
              , Fmt . Int ( LNewExpr . ExpBinOpActualsCt )
              , " actual parameter"
              , LPluralSuffix
              }
          , LCallExpr . ExpPosition 
          ) 
      ; LNewExpr . ExpIsUsable := FALSE 
      ; RETURN LNewExpr
      END (*IF*)

    ; LNewExpr . ExpPosition := LCallExpr . ExpPosition 
    ; LOpnd1 := LCallExpr . ExpActualsList ^ [ 0 ]
    ; LNewExpr . ExpOpnd1 := LOpnd1
    ; LNewExpr . ExpIsUsable := LCallExpr . ExpIsUsable AND LOpnd1 . ExpIsUsable
    ; IF LActualsCt = 1
      THEN
        LNewExpr . ExpReachedDeclNoSet := LOpnd1 . ExpReachedDeclNoSet 
      ELSE
        LOpnd2 := LCallExpr . ExpActualsList ^ [ 1 ]
      ; LNewExpr . ExpOpnd2 := LOpnd2
      ; LNewExpr . ExpIsUsable := LNewExpr . ExpIsUsable AND LOpnd2 . ExpIsUsable
      ; LNewExpr . ExpReachedDeclNoSet
        := IntSets . Union
             ( LOpnd1 . ExpReachedDeclNoSet 
             , LOpnd2 . ExpReachedDeclNoSet
             )
      END (*IF*) 

    ; EVAL CheckOperation ( LNewExpr )
    ; RETURN LNewExpr
    END MaybeConvertCallToOperator

; PROCEDURE InterfaceExpr
    ( DeclStdTok : FM3SrcToks . TokTyp
    ; IsCall : BOOLEAN (* Relevant only if it turns out to a procedure. *)
    )
  : FM3Exprs . ExprTyp

  = VAR I : INTEGER

  ; BEGIN
      CASE DeclStdTok OF
      | FM3SrcToks . StkPdPlus
      =>  IF NOT IsCall THEN RETURN NIL END (*IF*)
      
      | FM3SrcToks . StkPdMinus
      => IF NOT IsCall THEN RETURN NIL END (*IF*)
      
      ELSE RETURN NIL
      END (*CASE*)
    ; RETURN NIL  
    END InterfaceExpr

; PROCEDURE MaybeStdDeclRef
    ( Expr : FM3Exprs . ExprTyp
    ; UnitNo : FM3Globals . UnitNoTyp
    ; DeclNo : FM3Globals . DeclNoTyp
    ; IsCall : BOOLEAN (* Relevant only if it turns out to a procedure. *)
    )
  : FM3Exprs . ExprTyp (* NIL if not a decl in a standard interface. *) 
  (* Return an expression for UnitNo/DeclNo if it's in a standard interface. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LDeclRef : FM3Decls . DeclRefTyp
  ; VAR LUnitStdTok : FM3SrcToks . TokTyp 
  ; VAR LDeclStdTok : FM3SrcToks . TokTyp 
 
  ; BEGIN
      IF UnitNo = FM3Globals . UnitNoNull THEN RETURN NIL END (*IF*)
    ; IF DeclNo = FM3Globals . DeclNoNull THEN RETURN NIL END (*IF*)  
    ; LUnitRef := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , UnitNo )
    ; IF LUnitRef = NIL THEN RETURN NIL END (*IF*)
    ; LUnitStdTok := LUnitRef . UntStdTok
    ; IF LUnitStdTok = FM3Base . TokNull THEN RETURN NIL END (*IF*)
    ; LDeclRef := VarArray_Int_Refany . Fetch ( LUnitRef . UntDeclMap , DeclNo ) 
    ; IF LDeclRef = NIL THEN RETURN NIL END (*IF*)
    ; LDeclStdTok := LDeclRef . DclStdTok
    ; IF LDeclStdTok = FM3Base . TokNull THEN RETURN NIL END (*IF*)
    ; CASE LUnitStdTok OF
      | FM3SrcToks . StkPdWord
      => RETURN
           InterfaceExpr
             ( LDeclStdTok
             , IsCall
             )


(*
        ; Expr
           ( Expr
           , LDeclStdTok
           , IsCall
           )
*) 
      | FM3SrcToks . StkPdLong
      => RETURN InterfaceExpr
           ( LDeclStdTok
           , IsCall
           )

(* COMPLETEME: Add more standard units *)
      ELSE RETURN NIL
      END (*CASE*)
    END MaybeStdDeclRef

; PROCEDURE GetStdToks 
    ( UnitNo : FM3Globals . UnitNoTyp
    ; DeclNo :FM3Globals . DeclNoTyp
    ; VAR (*OUT*) UnitTok : FM3SrcToks . TokTyp 
    ; VAR (*OUT*) DeclTok : FM3SrcToks . TokTyp 
    )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LDeclRef : FM3Decls . DeclRefTyp

  ; BEGIN
      UnitTok := FM3Base . TokNull 
    ; DeclTok := FM3Base . TokNull 
    ; IF UnitNo = FM3Globals . UnitNoNull THEN RETURN END (*IF*)
    ; LUnitRef := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , UnitNo )
    ; IF LUnitRef = NIL THEN RETURN END (*IF*)
    ; IF DeclNo = FM3Globals . DeclNoNull THEN RETURN END (*IF*)  
    ; LDeclRef := VarArray_Int_Refany . Fetch ( LUnitRef . UntDeclMap , DeclNo ) 
    ; IF LDeclRef = NIL THEN RETURN END (*IF*)
    ; IF LUnitRef ^. UntHasStdUnitPragma
      THEN UnitTok := LUnitRef . UntStdTok
      END (*IF*) 
    ; DeclTok := LDeclRef . DclStdTok 
    END GetStdToks 

; PROCEDURE IdentRefR2L ( READONLY TokResult : TokResultTyp )
  (* PRE: The ident is not followed by dot Ident. (The parser has gone
          to some trouble to ensure this.)
     PRE: The atom does not denote a reserved ident. 
  *) 

  = VAR LExpImpUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentRefAtom : FM3Base . AtomTyp
  ; VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LUnitNo : FM3Globals . UnitNoTyp
  ; VAR LRefDeclNo : FM3Globals . DeclNoTyp
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
        LRefDeclNo := LookupAtomInOpenScopes ( LIdentRefAtom )
      ; IF LRefDeclNo # FM3Globals . DeclNoNull 
        THEN
          LUnitRef := FM3Units . UnitStackTopRef 
        ; IF AreInsideADecl ( )
          THEN (* Create an ExprIdentRefTyp node. *) 
            CheckRecursiveRef ( LRefDeclNo )
          ; WITH WExpr = NEW ( FM3Exprs . ExprIdentRefTyp )
            DO
              WExpr . ExpIdentDeclNo := LRefDeclNo 
            ; WExpr . ExpPosition := LPosition
            ; WExpr . ExpUpKind := Ekt . EkRef
            ; DefExprRt ( WExpr )
            END (*WITH*)
          ELSE (* Change to a reference token with DeclNo instead of Atom. *)
            PutBwdP2 ( Wp2RdBack , VAL ( LPosition . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosition . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LRefDeclNo , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdentRefWDeclNo , LONGINT ) )
          END (*IF*)
          
        (* Look for something [ex|im]ported. *) 
        ELSIF LookupAtomExpImp
                ( LIdentRefAtom , (*OUT*) LUnitNo , (*OUT*) LRefDeclNo )
(* CHECK: Negative Decl No => reserved ident. *) 
        THEN (* Export or import is present. *) 
          IF LUnitNo = FM3Globals . UnitNoNull (* But not usable. *) 
          THEN LIsUsable := FALSE
(* CHECK: ^v Which of these ways denoting unusability can happen? *) 
          ELSE
            LExpImpUnitRef (* Implicit NARROW. *) 
              := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNo )
          ; LIsUsable := LExpImpUnitRef ^ . UntState # Ust . UsNotUsable
(* Consistify: Two ways of denoting nonusability. *) 
          END (*IF*)
        ; IF LIsUsable
          THEN (* LExpImpUnitRef names a usable interface. *)
            IF LRefDeclNo = FM3Globals . DeclNoNull
            THEN (* Interface name w/o a selection--illegal. *) 
              IF NOT FM3Atom_OAChars . Key
                       ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                       , LIdentRefAtom
(* CHECK: Negative atom => reserved ident. *) 
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
                WITH WExpr = NEW ( FM3Exprs . ExprRemoteRefTyp )
                DO 
                  WExpr . ExpRemoteUnitNo := LUnitNo 
                ; WExpr . ExpRemoteDeclNo := LRefDeclNo 
                ; WExpr . ExpPosition := LPosition
                ; WExpr . ExpUpKind := Ekt . EkRef
                ; DefExprRt ( WExpr )
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
  (* (NON)PRE: No operands have been read.
     PRE Neither atom denotes a reserved ident.
  *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp 
  ; VAR LUnitNoLt : FM3Globals . UnitNoTyp
  ; VAR LRefDeclNoLt : FM3Globals . DeclNoTyp
  ; VAR LRefDeclNo : FM3Globals . DeclNoTyp
  ; VAR LAtomLt , LAtomRt : FM3Base . AtomTyp
  ; VAR LRemoteAtom : FM3Base . AtomTyp
  ; VAR LRemoteDeclNoInt : INTEGER 
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
        LRefDeclNoLt := LookupAtomInOpenScopes ( LAtomLt )
      ; IF LRefDeclNoLt # FM3Globals . DeclNoNull
        THEN (* Lt names a local declaration, not an interface. *)
          IF AreInsideADecl ( ) 
          THEN (* Create an ExprIdNo node. *) 
            CheckRecursiveRef ( LRefDeclNo )
          ; WITH WDotExpr = NEW ( FM3Exprs . ExprDotTyp )
                 , WLtExpr = NEW ( FM3Exprs . ExprIdentRefTyp )
            DO 
              WDotExpr . ExpOpnd1 := WLtExpr 
            ; WDotExpr . ExpDotIdAtom := LAtomRt
            ; WDotExpr . ExpPosition := LPosRt
            ; WDotExpr . ExpUpKind := Ekt . EkRef
            ; DefExprRt ( WDotExpr )
            ; WLtExpr . ExpIdentDeclNo := LRefDeclNoLt 
            ; WLtExpr . ExpPosition := LPosLt
            ; WLtExpr . ExpUpKind := Ekt . EkRef
            ; DefExprRt ( WLtExpr )
            END (*WITH*)
          ; EVAL FM3Exprs . PopExprStack ( ) (* WLtExpr *) 
          ELSE (* Emit tokens for a dot Id applied to a DeclNo Id reference. *)

          (* Turn the qualident into separate Id ref and dot Id. *)
            PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )

          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LRefDeclNoLt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdentRefWDeclNo , LONGINT ) )

          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )
          END (*IF*) 

        (* Look for something [ex|im]ported. *) 
        ELSIF LookupAtomExpImp
                ( LAtomLt , (*OUT*) LUnitNoLt , (*OUT*) LRefDeclNoLt )
        THEN (* Lt ident is [ex|im]ported. *)  
          IF LUnitNoLt = FM3Globals . UnitNoNull
          THEN (* Lt is Undeclared. *)
            BadIdentMessage
              ( "Undeclared identifier" , LAtomLt , LPosLt )
          ; PutNotUsable( LAtomLt , LPosLt ) 
          ELSIF LRefDeclNoLt = FM3Globals . DeclNoNull
          THEN (* Lt names an imported interface with no dot selection. *)
            LIntfUnitRef (*Implicit NARROW*) 
              := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNoLt )
          ; <* ASSERT LIntfUnitRef # NIL *>
            LRemoteAtom
              := FM3Compile . ConvertIdentAtom
                   ( LAtomRt
                   , FromUnitRef := FM3Units . UnitStackTopRef
                   , ToUnitRef := LIntfUnitRef
                   )
          ; IF IntSets . IsElement
                 ( LRemoteAtom , LIntfUnitRef ^ . UntExpImpIdSet )
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
                WITH WExpr = NEW ( FM3Exprs . ExprRemoteRefTyp )
                DO 
                  WExpr . ExpRemoteUnitNo := LIntfUnitRef ^ . UntSelfUnitNo 
                ; WExpr . ExpRemoteDeclNo := LRemoteDeclNoInt 
                ; WExpr . ExpPosition := LPosLt
                ; WExpr . ExpUpKind := Ekt . EkRef
                ; DefExprRt ( WExpr )
                END (*WITH*)
              ELSE 
                PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
              ; PutBwdP2 ( Wp2RdBack , VAL ( LRemoteDeclNoInt , LONGINT ) ) 
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
            (* Turn it into separate QualId ref and dot Id *) 
              WITH WDotExpr = NEW ( FM3Exprs . ExprDotTyp )
              , WLtExpr = NEW ( FM3Exprs . ExprRemoteRefTyp )
              DO 
                WDotExpr . ExpDotIdAtom := LAtomRt
              ; WDotExpr . ExpOpnd1 := WLtExpr 
              ; WDotExpr . ExpPosition := LPosRt
              ; WDotExpr . ExpUpKind := Ekt . EkRef
              ; DefExprRt ( WDotExpr ) (* Which pushes. *) 
              ; WLtExpr . ExpRemoteUnitNo := LUnitNoLt 
              ; WLtExpr . ExpRemoteDeclNo := LRefDeclNoLt 
              ; WLtExpr . ExpPosition := LPosLt
              ; WLtExpr . ExpUpKind := Ekt . EkRef
              ; DefExprRt ( WLtExpr ) (* Which pushes. *)
              END (*WITH*)
            ; EVAL FM3Exprs . PopExprStack ( ) (* The WLtExpr. *) 
            ELSE 
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

(*EXPORTED*)
; PROCEDURE DisAsmPass2
    ( UnitRef : FM3Units . UnitRefTyp ; DoEarlierPasses : BOOLEAN )

  = BEGIN (*DisAsmPass2*)
      IF NOT FM3CLOptions . PassNo2 IN UnitRef ^ . UntPassNosDisAsmed 
      THEN (* Disassembly file is not already written. *)
        TRY 
          FM3Compile . DisAsmPassFile
            ( UnitRef , FM3Globals . Pass2OutSuffix , L2R := TRUE )
          EXCEPT
          | RdBackFile . BOF
          => DoEarlierPasses := NOT DoEarlierPasses
          END (*EXCEPT*)
          
      ; FM3CLOptions . InclPassNo
          ( UnitRef ^ . UntPassNosDisAsmed , FM3CLOptions . PassNo2 ) 
      END (*IF*) 
    ; IF DoEarlierPasses
      THEN FM3Pass1 . DisAsmPass1 (UnitRef )
      END (*IF*) 
    END DisAsmPass2

(*EXPORTED*)
; PROCEDURE DumpExprsPass2
    ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN (*DumpExprsPass2*)
      IF NOT FM3CLOptions . PassNo2 IN UnitRef ^ . UntPassNosDumped 
      THEN (* Dump file is not already written. *) 
        FM3Compile . DumpPassExprs
          ( UnitRef , FM3Globals . Pass2OutSuffix )
      ; FM3CLOptions . InclPassNo
          ( UnitRef ^ . UntPassNosDumped , FM3CLOptions . PassNo2 ) 
      END (*IF*) 
    END DumpExprsPass2

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
      ; FM3Units . CacheTopUnitValues ( )
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
      UnitRef ^ . UntPass2Result := 0 
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
        UnitRef ^ . UntPass2Result := FM3CLArgs . CcPatchStackNotEmpty  
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
        UnitRef ^ . UntPass2Result := FM3CLArgs . CcPass1OutNotEmpty  
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

    ; IF FM3CLOptions . PassNo2 IN FM3CLOptions . PassNosToDumpExprs 
      THEN FM3Compile . DumpPassExprs ( UnitRef , FM3Globals . Pass2OutSuffix )
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

