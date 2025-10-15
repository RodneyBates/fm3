
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
; IMPORT IntIntVarArray AS VarArray_Int_Int (* Use FM3's naming convention. *) 
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
; FROM   FM3Exprs IMPORT Est   
; IMPORT FM3Dict_Int_Int
; IMPORT FM3ExpImp
; IMPORT FM3ExpImpProxy 
; IMPORT FM3Globals
; FROM   FM3Globals IMPORT P2RdBack 
; IMPORT FM3Graph 
; IMPORT FM3IntToks AS Itk
; IMPORT FM3LoTypes
; IMPORT FM3Parser 
; IMPORT FM3Patch 
; IMPORT FM3Pass1
; IMPORT FM3ReservedIds
; IMPORT FM3RTFailures 
; IMPORT FM3SrcToks 
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Std 
; FROM   FM3StreamUtils
    IMPORT GetBwdInt , GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo
           , GetBwdBool , GetBwdBrandKind 
; IMPORT FM3Messages 
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT RdBackFile

; TYPE Ekt = FM3Exprs . ExprKindTyp
; TYPE EkSetTyp = FM3Exprs . EkSetTyp
; TYPE Skt = FM3Scopes . ScopeKindTyp 
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

; IF ValueL = 11L
  THEN ValueL := 11L
  END 
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

; TYPE TokResultTyp
    = RECORD
        TrRdBack : RdBackFile . T (* From which to get its operands. *) 
      ; TrTok : Itk . TokTyp (* Just the Itk code.*) 
      END 

; PROCEDURE GetTokCode
    ( LMPass1Coord : LONGINT ; VAR (*OUT*) Result : TokResultTyp )
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
    ; LMPass1Coord := MAX ( LMPass1Coord , LUnitRef ^ . UntPass1OutEmptyCoord )

    ; LOOP (* Thru' a sequence of SkipRt & SkipLt tokens plus one other. *)  
        LPass1Coord := RdBackFile . LengthL ( LPass1RdBack )
      ; LPatchStackTopCoord := FM3Compress . GetBwd ( LPatchRdBack ) 
      ; IF LPass1Coord <= LMPass1Coord
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
        ; Result . TrRdBack:= NIL
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
            FM3Patch . SkipLt ( GetBwdInt ( FM3Globals . PatchRdBack ) )
            (* And loop. *)
          ELSIF (* Skipping? *) 
           VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
          THEN (* Skip it and loop. *) 
            FM3Patch . DiscardOperands
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
          ; FM3Patch . CopyOperandsInOrder
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
            FM3Patch . SkipRt ( GetBwdInt ( LPass1RdBack) ) 
          (* and loop. *)
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
; PROCEDURE Pass2Tokens ( LMPass1Coord : LONGINT )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPass1RdBack : RdBackFile . T 
  ; VAR LTokResult : TokResultTyp 

  ; BEGIN (* Pass2Tokens *) 
      LUnitRef := FM3Units . UnitStackTopRef
    ; LPass1RdBack := LUnitRef ^ . UntPass1OutRdBack 
    ; LMPass1Coord := MAX ( LMPass1Coord , LUnitRef ^ . UntPass1OutEmptyCoord )
    ; <* ASSERT GetBwd ( LPass1RdBack ) = VAL ( Itk . ItkEOF , LONGINT ) *> 
      <* ASSERT GetBwd ( LPass1RdBack ) = VAL ( Itk . ItkRightEnd , LONGINT )*> 

      LOOP
        FM3Patch . GetTokCode ( LMPass1Coord , (*OUT*) LTokResult )
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
    ; LNewObj . ExpKind := Ekt . EkNull 
    ; LNewObj . ExpPosition := Position
    ; LNewObj . ExpIsUsable := FALSE 
    ; FM3Exprs . PushExprStack ( LNewObj ) 
    END PushExprIgnore 

; PROCEDURE DefExprRt ( NewExprObj : FM3Exprs . ExprTyp )
  (* PRE: NOT Skipping. *)
  (* A value- or type-expression that is contained in a definition. *) 

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
           = LScopeRef ^ . ScpCurDefExprs [ LScopeRef ^ . ScpCurDefIsValue ]
      DO IF WCurDef  = NIL
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
          | NULL => 
          | FM3Exprs . ExprTyp ( TParentExpr ) 
          =>  NewExprObj . ExpDownKind := TParentExpr . ExpDownKind
            ; NewExprObj . ExpIsLegalRecursive
                := NewExprObj . ExpIsLegalRecursive
                   OR TParentExpr . ExpIsLegalRecursive
          ELSE 
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

; PROCEDURE DefExprLt ( ExprObj : FM3Exprs . ExprTyp )
  (* PRE NOT Skipping. *)
  (* Expressions that are contained in definitions. *) 

  = VAR LScopeRef : FM3Scopes .ScopeRefTyp 

  ; BEGIN 
      (* Don't pop it. Something else wants it. *) 
      LScopeRef := FM3Scopes . DeclScopeStackTopRef
    END DefExprLt

; PROCEDURE UnaryOp ( Opcode : FM3Exprs . OpcodeTyp )
  (* PRE: Not skipping, *) 

  = VAR LUnOpExpr : FM3Exprs . ExprTyp

  ; BEGIN
      LUnOpExpr
        := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
    ; IF NOT LUnOpExpr . ExpOpnd1 . ExpUpKind
             IN EkSetTyp { Ekt . EkValue , Ekt . EkConst }
      THEN
        WrongKindMsg
          ( "Operand"
          , FM3SrcToks . Image ( Opcode ) 
          , "a value expression"
          , LUnOpExpr . ExpPosition
          )
      ; LUnOpExpr . ExpIsUsable := FALSE
      ELSE
        LUnOpExpr . ExpKind := Ekt . EkUnOp
      ; LUnOpExpr . ExpKind := FM3Builtins . OpExprKind ( Opcode )  
      ; LUnOpExpr . ExpUpKind := Ekt . EkValue 
      ; LUnOpExpr . ExpIsConst := LUnOpExpr . ExpOpnd1 . ExpIsConst
      ; LUnOpExpr . ExpReachedDeclNoSet
          := LUnOpExpr . ExpOpnd1 . ExpReachedDeclNoSet 
      END (*IF*) 
    END UnaryOp
      
; PROCEDURE BinaryOp ( Opcode : FM3Exprs . OpcodeTyp )
  (* PRE: Not skipping, *) 

  = VAR LBinOpExpr : FM3Exprs . ExprTyp
  ; VAR LOpnd1 , LOpnd2 : FM3Exprs . ExprTyp 

  ; BEGIN
      LBinOpExpr
        := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
    ; LOpnd1 := LBinOpExpr . ExpOpnd1 
    ; LOpnd2 := LBinOpExpr . ExpOpnd2 
    ; IF NOT LOpnd1 . ExpUpKind
             IN EkSetTyp { Ekt . EkValue , Ekt . EkConst }
      THEN
        WrongKindMsg
          ( "Left operand"
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
          ( "Right Operand"
          , FM3SrcToks . Image ( Opcode ) 
          , "a value expression"
          , LOpnd2 . ExpPosition
          )
      ; LBinOpExpr . ExpIsUsable := FALSE
      END (*IF*)
    ; IF LBinOpExpr . ExpIsUsable
      THEN
        LBinOpExpr . ExpKind := Ekt . EkBinOp
      ; LBinOpExpr . ExpKind := FM3Builtins . OpExprKind ( Opcode )  
      ; LBinOpExpr . ExpUpKind := Ekt . EkValue 
      ; LBinOpExpr . ExpIsConst
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
      END (*IF*)
    END DeclType

; PROCEDURE DeclValue ( DeclKindTag : TEXT ; MustBeConst : BOOLEAN ) 
  (* PRE: The value expression exists and is on the expression stack. *) 

  = VAR  LValueExpr : FM3Exprs . ExprTyp 

  ; BEGIN
      LValueExpr := FM3Exprs . PopExprStack ( )
    ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ TRUE ] := LValueExpr
    ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE 
      (* ^Type will be coming up next. *) 
    ; IF NOT LValueExpr . ExpIsPresent THEN RETURN END (*IF*) 
    ; IF NOT LValueExpr . ExpIsUsable THEN RETURN END (*IF*) 
    ; IF FALSE 
(* FIXME:         LValueExpr could be an identifier referring to a decl farther
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
        (* ^Type def is next. *) 
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
    (* There was a problem. *) 
    ; LValueExpr . ExpIsUsable := FALSE
    ; LValueExpr . ExpIsLegalRecursive := TRUE (* Necessary?*)
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
  ; VAR LNewExpr , LValueExpr , LPrefixExpr , LListElem : FM3Exprs . ExprTyp
  ; VAR LArgsExpr : FM3Exprs . ExprTyp
  ; VAR LLongInt: LONGINT 
  ; VAR LScopeNo : FM3Globals . ScopeNoTyp
  ; VAR LOpcode : FM3SrcToks . TokTyp
  ; VAR LCt : INTEGER 
  ; VAR LPosition : tPosition
  ; VAR LBrandKind : FM3Parser . BrandKindTyp 
  ; VAR HtSkipping : BOOLEAN
  ; VAR LBool : BOOLEAN

  ; PROCEDURE HtCallOrSubscript ( Kind : FM3Exprs . ExprKindTyp )

    = VAR LArgsExpr : FM3Exprs . ExprTyp

    ; BEGIN 
        LCt := GetBwdInt ( TokResult . TrRdBack ) (* Args/Ss count. *) 
      ; LPosition := GetBwdPos ( TokResult . TrRdBack )
      ; LArgsExpr := NEW ( FM3Exprs . ExprTyp )
      ; LArgsExpr . ExpKind := Kind
      ; LArgsExpr . ExpUpKind := Ekt . EkValue 
      ; LArgsExpr . ExpArgsList := NEW ( FM3Exprs . ExprListRefTyp , LCt )
      ; LArgsExpr . ExpArgNo := LCt
      ; LArgsExpr . ExpPosition := LPosition
      ; FM3Exprs . PushExprStack ( LArgsExpr )
      END HtCallOrSubscript  

  ; PROCEDURE HtLiteral ( )

    = VAR LLongInt : LONGINT 
    ; VAR LOpcode : INTEGER
    ; VAR LLoTypeNo : INTEGER
    ; VAR LPosition : tPosition

    ; BEGIN
        LOpcode := VAL ( GetBwd ( TokResult . TrRdBack ), INTEGER )
      ; CASE LOpcode OF
        | Stk . StkIntLit
        , Stk . StkBasedLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoInt  

        | Stk . StkLongIntLit
        , Stk . StkLongBasedLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoLong 

        | Stk . StkCharLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoU8 

        | Stk . StkWideCharLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoU16 

        | Stk . StkRealLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoReal  

        | Stk . StkLongRealLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoLongReal  

        | Stk . StkExtendedLit
        => LLoTypeNo := FM3LoTypes . LoTypeNoExtended  

        | Stk . StkTextLit
        , Stk . StkWideTextLit 
        => LLoTypeNo := FM3LoTypes . LoTypeNoAddr  

        END (*CASE*) 
      ; LLongInt := GetBwd ( TokResult . TrRdBack )
      ; LPosition := GetBwdPos ( TokResult . TrRdBack )
      ; IF NOT HtSkipping 
        THEN
          IF AreInsideADecl ( )
          THEN 
            WITH LExpr = NEW ( FM3Exprs . ExprTyp ) 
            DO 
              LExpr . ExpScalarConstVal := LLongInt
            ; LExpr . ExpLoTypeInfoRef
                := VarArray_Int_Refany . Fetch
                     ( FM3LoTypes . LoTypeMap , LLoTypeNo )  
            ; LExpr . ExpPosition := LPosition
            ; LExpr . ExpKind := Ekt . EkLiteral
            ; LExpr . ExpOpcode := LOpcode 
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
      END HtLiteral 

  ; PROCEDURE HtNumericLit ( LoTypeNo : FM3LoTypes . LoTypeNoTyp )
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
            WITH LExpr = NEW ( FM3Exprs . ExprTyp ) 
            DO 
              LExpr . ExpScalarConstVal := LLongInt
            ; LExpr . ExpLoTypeInfoRef
                := VarArray_Int_Refany . Fetch
                     ( FM3LoTypes . LoTypeMap , LoTypeNo )  
            ; LExpr . ExpPosition := LPosition
            ; LExpr . ExpKind := Ekt . EkLiteral  
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
      END HtNumericLit 

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
    ; VAR LParentExpr : FM3Exprs . ExprTyp
    ; VAR LPosition : tPosition


    ; BEGIN 
        LPosition := GetBwdPos ( TokResult . TrRdBack )
      ; IF NOT HtSkipping 
        THEN
          LOpnd := FM3Exprs . PopExprStack ( )
        ; LParentExpr
            := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
        ; LParentExpr . ExpOpnd1 := LOpnd
        ; DefExprLt ( LParentExpr ) 
        END (*IF*) 
      END HtExprOpnd1 

  ; PROCEDURE HtExprOpnd2 ( )
      (* PRE: TOS is 2nd operand, TOS-1 is parent. *) 
    = VAR LOpnd : FM3Exprs . ExprTyp
    ; VAR LParentExpr : FM3Exprs . ExprTyp 

    ; BEGIN 
        EVAL GetBwdPos ( TokResult . TrRdBack )
      ; IF NOT HtSkipping 
        THEN
          LOpnd := FM3Exprs . PopExprStack ( )
        ; LParentExpr
            := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
        ; LParentExpr . ExpOpnd2 := LOpnd
        ; DefExprLt ( LParentExpr ) 
        END (*IF*) 
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
          FM3Patch . DiscardOperands
            ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
            , TokResult . TrRdBack
            ) 
        ELSE 
          FM3Patch . CopyOperandsInOrder
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
          FM3Patch . DiscardOperands
            ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
            , TokResult . TrRdBack
            ) 
        ; RETURN TRUE 
        ELSE 
          IF
 FALSE AND   NOT FM3Scopes . OpenScopeStackTopRef ^ . ScpInsideDecl 
          THEN (* Not inside a decl-defining expression. *) 
            FM3Patch . CopyOperandsInOrder
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

    ; CASE TokResult . TrTok OF     

      | Itk . ItkScopeEmpty 
      =>  HtPassTokenThru ( ) 

      | Itk . ItkDeclScopeRt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack )
        ; FM3Scopes . PushDeclScopeRef
            ( FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

      | Itk . ItkDeclScopeLt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack )
        ; LScopeRef := FM3Scopes . PopDeclScopeRef ( ) 
        ; IF LScopeRef ^ . ScpSelfScopeNo # LScopeNo
          THEN <* ASSERT FALSE *> 
          END (*IF*)
        ; PutBwdP2 ( HtPass2RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

      | Itk . ItkOpenScopeRt 
      =>  LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
        ; LScopeRef := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
        ; LScopeRef ^ . ScpDeclGraph
            := FM3Graph . NewEmpty ( MaxNodeCt := LScopeRef ^ . ScpDeclCt ) 
        ; FM3Scopes . PushOpenScopeRef ( LScopeRef ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

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
      =>  FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := TRUE
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDeclExprStackCt 
            := FM3Exprs . ExprStackCt 
        ; HtPassTokenThru ( )

      | Itk . ItkVALUEFormalRt
      , Itk . ItkVARFormalRt
      , Itk . ItkROFormalRt
      =>  FM3Scopes . DeclScopeStackTopRef ^ .  ScpCurDeclRefNoSet
            := IntSets . Empty ( )
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := TRUE
            (* ^Value def coming up next, R2L. *)
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDeclExprStackCt
            := FM3Exprs . ExprStackCt 
        ; HtPassTokenThru ( )

      | Itk . ItkConstDeclValue (* Parser ensures this exists, no boolean arg. *)
      =>  DeclValue ( "Value of CONST" , MustBeConst := TRUE )
        ; HtPassTokenThru ( )
      
      | Itk . ItkFieldDeclValue (* Of either record or object. *)
      => DeclValue ( "Default value of field" , MustBeConst := FALSE )
        ; HtPassTokenThru ( )

      | Itk . ItkVarDeclValue 
      =>  DeclValue ( "Default value of variable" , MustBeConst := FALSE )  
        ; HtPassTokenThru ( )
         
      | Itk . ItkVALUEFormalValue
      , Itk . ItkROFormalValue
      =>  DeclValue ( "Default value of formal" , MustBeConst := FALSE ) 
        ; HtPassTokenThru ( )

      | Itk . ItkVARFormalValue
      =>  LValueExpr := FM3Exprs . PopExprStack ( ) 
        ; <* ASSERT NOT LValueExpr . ExpIsPresent *> 
          FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE 
          (* ^Type is coming up next. *) 
        ; HtPassTokenThru ( )

      | Itk . ItkDeclTypeAbsent
      , Itk . ItkDeclValAbsent
      , Itk . ItkSubscriptTypeAbsent
      =>  LPosition := GetBwdPos ( TokResult . TrRdBack )
          (* An especially dumb ExprRef, just to keep expr stack consistent. *)
        ; FM3Exprs . PushExprStack
            ( NEW ( FM3Exprs . ExprTyp
                  , ExpKind := Ekt . EkNull 
                  , ExpIsPresent := FALSE
                  , ExpPosition := LPosition
                  )
            )
        ; PutBwdP2
            ( P2RdBack , VAL ( LPosition . Column , LONGINT ) )
        ; PutBwdP2
            ( P2RdBack , VAL ( LPosition . Line , LONGINT ) ) 
        ; PutBwdP2 ( P2RdBack , VAL ( TokResult. TrTok , LONGINT ) )

      | Itk . ItkConstDeclType
      =>  DeclType ( "CONST" )
        ; HtPassTokenThru ( )

      | Itk . ItkFieldDeclType (* Of either record or object. *)
      =>  DeclType ( "field" )
        ; HtPassTokenThru ( )

      | Itk . ItkVarDeclType 
      =>  DeclType ( "variable" )  
        ; HtPassTokenThru ( )

      | Itk . ItkVALUEFormalType
      , Itk . ItkROFormalType
      , Itk . ItkVARFormalType
      =>  DeclType ( "formal" ) 
        ; HtPassTokenThru ( )

      | Itk . ItkConstDeclLt 
      , Itk . ItkVarDeclLt 
      , Itk . ItkVALUEFormalLt
      , Itk . ItkVARFormalLt
      , Itk . ItkROFormalLt
      , Itk . ItkFieldDeclLt (* Of either record or object. *)
      
      =>  FM3Exprs . PruneExprStack
            ( ToDepth
                := FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDeclExprStackCt
            ) 
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclRt
      , Itk . ItkFullRevealRt 
      , Itk . ItkPartialRevealRt
      =>  FM3Scopes . DeclScopeStackTopRef ^ .  ScpCurDeclRefNoSet
            := IntSets . Empty ( ) 
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs
            := ARRAY BOOLEAN OF REFANY { NIL , .. }
        ; FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefIsValue := FALSE
            (* ^ TYPE Decl and revelation have only a type def. *) 
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclType
      , Itk . ItkFullRevealType 
      , Itk . ItkPartialRevealSubtype
      =>  DeclType ( "type" ) 
        ; HtPassTokenThru ( )

      | Itk . ItkTypeDeclLt
      , Itk . ItkFullRevealLt 
      , Itk . ItkPartialRevealLt
      =>  HtPassTokenThru ( )

      | Itk . ItkMethodDeclLt
      =>  HtPassTokenThru ( )

(* CONSISTIFY: For some of these, fetch the operands inside the called proc. *) 
      | Itk . ItkDuplDeclId
      => EVAL DuplDeclIdR2L ( TokResult )

      | Itk . ItkDeclId
      => EVAL DeclIdR2L ( TokResult )

(* FIXME: We currently use different tokens for different declkinds, eg.
    ItkVALUEFormalIdListElem.  But is that necessary? *)

      | Itk . ItkReservedIdRef
      =>  IF InsideDecl ( )
          THEN
            LNewExpr := NEW ( FM3Exprs . ExprTyp ) 
          ; LNewExpr . ExpOpcode := GetBwdAtom ( TokResult . TrRdBack )
          ; LNewExpr . ExpPosition := GetBwdPos ( TokResult . TrRdBack )
          ; LNewExpr . ExpIsLegalRecursive := TRUE
          ; LNewExpr . ExpKind := Ekt . EkReservedIdent  
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
        ; LNewExpr . ExpKind := Ekt . EkNull
        ; LNewExpr . ExpUpKind := Ekt . EkNull
        ; LNewExpr . ExpIsUsable := FALSE 
        ; DefExprRt ( LNewExpr )

      | Itk . ItkQualIdAtoms 
      => QualIdentR2L ( TokResult . TrRdBack )

      | Itk . ItkAnonBlockRt
      , Itk . ItkProcBlockRt
      => LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
     (* Doesn't exist, probably ItkBlock[LR]t will disappear.

     *) 
        ; FM3Patch . CopyOperandsInOrder
           ( 2 (*Position*)
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( LScopeNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

(* ItkBlock[RL]t will probably disappear. *) 
      | Itk . ItkProcBlockLt 
      , Itk . ItkAnonBlockLt 
      => FM3Patch . CopyOperandsInOrder
           ( FM3Utils . TokenOpndCt ( TokResult . TrTok ) 
           , TokResult . TrRdBack
           , HtPass2RdBack
           , MaybeSkip := TRUE 
           )
        ; PutBwdP2 ( HtPass2RdBack , VAL ( TokResult . TrTok , LONGINT ) )

      | Itk . ItkLiteral
      => HtLiteral ( ) 
(*      

      | Itk . ItkIntLit
      , Itk . ItkBasedLit
      => HtLiteral ( FM3LoTypes . LoTypeNoInt ) 

      | Itk . ItkLongIntLit
      , Itk . ItkLongBasedLit
      => HtLiteral ( FM3LoTypes . LoTypeNoLong )

      | Itk . ItkCharLit
      => HtLiteral ( FM3LoTypes . LoTypeNoU8 )

      | Itk . ItkWideCharLit
      => HtLiteral ( FM3LoTypes . LoTypeNoU16 )

      | Itk . ItkRealLit
      => HtLiteral ( FM3LoTypes . LoTypeNoReal ) 

      | Itk . ItkLongRealLit
      => HtLiteral ( FM3LoTypes . LoTypeNoLongReal ) 

      | Itk . ItkExtendedLit
      => HtLiteral ( FM3LoTypes . LoTypeNoExtended ) 

      | Itk . ItkTextLitRt
      , Itk . ItkWideTextLitRt 
      => HtLiteral ( FM3LoTypes . LoTypeNoAddr ) 

      | Itk . ItkTextLitRt
      , Itk . ItkWideTextLitRt
      =>  LLongInt
            := FM3Compress . GetBwd ( TokResult . TrRdBack ) (* ^Atom. *)  
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IF NOT HtSkipping 
          THEN
            DefExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkLiteral
                    , ExpUpKind := Ekt . EkValue
                    , ExpScalarConstVal := LLongInt
                    (* No base in an Expr? *) 
                    , ExpOpcode := TokResult . TrTok 
                    , ExpPosition := LPosition 
                    )
              )
          END (*IF*) 
      | Itk . ItkTextLitLt
      , Itk . ItkWideTextLitLt
      => FM3Patch . DiscardOperands
           ( 3 (* Atom, position. *) , TokResult . TrRdBack ) 
*)
      (* Enumeration type: *) 
(* FIXME: Some of these need to copy the token and arguments. *)   
      | Itk . ItkEnumLitListRt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN  
            LCt := GetBwdInt ( TokResult . TrRdBack ) (* Enum lit count. *)
          ; LScopeRef := FM3Scopes . DeclScopeStackTopRef
          ; IF LScopeRef = NIL
            OR LScopeRef . ScpKind # Skt . SkEnum
            THEN <* ASSERT FALSE , "No enum decl scope at left end." *>
            END (*IF*) 
          ; HtExprRt (* Which will get and store the position. *) 
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkEnumType 
                    , ExpUpKind := Ekt . EkEnumType 
                    , ExpScopeRef1 := LScopeRef 
                    )
              ) 
          END (*IF*)

      | Itk . ItkEnumLitListLt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            LCt := GetBwdInt ( TokResult . TrRdBack ) (* Field count. *)
          ; LPosition := GetBwdPos ( TokResult . TrRdBack )
          END (*IF*)  

      (* Record type: *) 
      | Itk . ItkRecTypeRt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *) 
          ; HtExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkRecType
                    , ExpUpKind := Ekt . EkType
                    , ExpScopeRef1 := FM3Scopes . DeclScopeStackTopRef
                    )
              )
(* Copy token? *)
          END (*IF*) 

      | Itk . ItkRecTypeLt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN  
            LLongInt := GetBwd ( TokResult . TrRdBack ) (* Field count. *)
          ; LPosition := GetBwdPos ( TokResult . TrRdBack )
(* Copy token? *)
          END (*IF*)

      (* Brands: *)

      | Itk . ItkBrandAbsent
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LPosition := GetBwdPos ( TokResult . TrRdBack )
            (* There's always an expression for a brand, even if it's absent. *)
          ; LNewExpr 
              := NEW ( FM3Exprs . ExprTyp
                     , ExpKind := Ekt . EkBrand
                     , ExpUpKind := Ekt . EkBrand
                     , ExpIsLegalRecursive := TRUE
                     , ExpIsPresent := FALSE
                     , ExpPosition := LPosition 
                     )
          ; DefExprRt ( LNewExpr )
          END (*IF*)

      | Itk . ItkBrandAnon
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
            DO
              LNewExpr
                := FM3Builtins . BuiltinExpr
                     ( FM3SrcToks . StkRTUniqueBrand , WPosition ) 
            ; DefExprRt ( LNewExpr )
            END (*WITH*)
          END (*IF*)

      | Itk . ItkBrandExplicitRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
            DO
              LNewExpr
                := NEW ( FM3Exprs . ExprTyp
                       , ExpKind := Ekt . EkBrand
                       , ExpUpKind := Ekt . EkBrand
                       , ExpIsLegalRecursive := TRUE
                       , ExpIsPresent := TRUE  
                       , ExpPosition := WPosition 
                       ) 
         (* ; DefExprRt ( LNewExpr ) *)
            END (*WITH*)
          END (*IF*)

      | Itk . ItkBrandExplicitLt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            EVAL GetBwdPos ( TokResult . TrRdBack )
          END (*IF*)

      (* REF type: *) 
      | Itk . ItkREFTypeRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN ) 
          ; HtExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkRefType
                    , ExpUpKind := Ekt . EkType
                    , ExpIsLegalRecursive := TRUE
                    , ExpRefTypeIsUntraced := LBool
                    , ExpOpcode := Stk . StkRwREF
                    , ExpRefTypeIsUntraced := LBool
                    (* ExpOpnd2 will later be referent. *) 
                    (* ExpOpnd1 will later be brand. *) 
                    )
              )
          END (*IF*)

      | Itk . ItkREFTypeReferent 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            HtExprOpnd2 ( ) (* Referent *)
          END (*IF*) 

      | Itk . ItkREFTypeLt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN )
          ; IF FM3Exprs . ExprStackTopObj . ExpUpKind # Ekt . EkSupertype
            THEN <* ASSERT FALSE , "Ref type has no absent supertype" *>
            END (*IF*)
          ; EVAL FM3Exprs . PopExprStack ( )
            (* ^Of REF type, a supertype  is bogus.  Ignore it. *)
          ; IF NOT FM3Exprs . ExprStackTopObj . ExpUpKind
                   IN FM3Exprs . EkSetBrand 
            THEN <* ASSERT FALSE , "Ref type has no brand expr. " *>
            END (*IF*)
          ; HtExprOpnd1 ( ) (* Brand *)
          ; SynthIsUsable2 ( FM3Exprs . ExprStackTopObj (* The REF Type. *) )
          END (*IF*) 

      (* Used in both OBJECT type and REF type. *)
      | Itk . ItkSupertypeAbsent
      =>  (* No supertype given in source code.  Could turn out a REF type *)
          IF NOT HtMaybePassTokenThru ( )
          THEN
            WITH WPosition = GetBwdPos ( TokResult . TrRdBack )
            DO
              LNewExpr 
                := NEW ( FM3Exprs . ExprTyp
                       , ExpKind := Ekt . EkSupertype
                       , ExpUpKind := Ekt . EkSupertype
                       , ExpIsLegalRecursive := TRUE
                       , ExpIsPresent := FALSE  
                       , ExpPosition := WPosition 
                       ) 
            ; DefExprRt ( LNewExpr )
            END (*WITH*)
          END (*IF*) 

      (* OBJECT type: *)

      | Itk . ItkOverrideRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            LPosition := GetBwdPos ( TokResult . TrRdBack )
          END (*IF*)
          
      | Itk . ItkOverrideEquals 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            LPosition := GetBwdPos ( TokResult . TrRdBack )
          ; EVAL FM3Exprs . PopExprStack ( ) (*The proc id. *) 
          END (*IF*)
          
      | Itk . ItkOverrideIdAtom  
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            EVAL GetBwdAtom ( TokResult . TrRdBack ) 
          ; LPosition := GetBwdPos ( TokResult . TrRdBack )
          END (*IF*)
          
      | Itk . ItkOverrideLt  
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            LPosition := GetBwdPos ( TokResult . TrRdBack )
          END (*IF*)
          
      | Itk . ItkObjTypeRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            VAR LExpr : FM3Exprs . ExprTyp
          ; BEGIN 
              LBrandKind := GetBwdBrandKind ( TokResult . TrRdBack )
            ; LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack ) 
            ; LExpr
                := NEW ( FM3Exprs . ExprTyp
                       , ExpKind := Ekt . EkObjType
                       , ExpUpKind := Ekt . EkType
                       , ExpIsLegalRecursive := TRUE
                       , ExpOpcode := Stk . StkRwOBJECT 
                       , ExpOpnd1 := NIL (* Supertype. *) 
                       , ExpOpnd2 := NIL (* Brand. *)
                       , ExpObjOverrides := NIL
                       , ExpObjBrandKind := LBrandKind
                       , ExpObjScopeRef
                           := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
                       )
            ; LExpr . ExpObjScopeRef
                := FM3Scopes . ScopeRefOfScopeNo ( LScopeNo )
            ; <* ASSERT
                   LExpr . ExpObjScopeRef = FM3Scopes . DeclScopeStackTopRef
              *>
              HtExprRt ( LExpr ) (* Consumes position and stores in LExpr. *) 
            END (* Block. *) 
          END (*IF*)
          
      | Itk . ItkObjTypeLt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            EVAL GetBwdBrandKind ( TokResult . TrRdBack )
          ; LScopeNo := GetBwdScopeNo ( TokResult . TrRdBack )
          (* The object type decl scope was popped by ItkDeclScopeLt. *) 
          ; EVAL GetBwdPos ( TokResult . TrRdBack )
          END (*IF*)
          
      | Itk . ItkSupertypeLt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            HtExprOpnd1 ( ) (* Supertype *) 
          END (*IF*)
          
      | Itk . ItkSupertypeRt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN (* Finish the brand: *)
            HtExprOpnd2 ( ) (* Brand *)
          END (*IF*)
          
      (* Array type: *) 
      | Itk . ItkArrayTypeRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN  
            LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN ) 
          ; HtExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkArrayType
                    , ExpUpKind := Ekt . EkType
                    , ExpOpcode := FM3SrcToks.StkRwARRAY
                    , ExpArrayTypeIsOpen := LBool 
                    )
              )
          END (*IF*)
            
      | Itk . ItkArrayTypeElmt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN ) 
          ; HtExprOpnd2 ( )
          END (*IF*) 

      | Itk . ItkArrayTypeLt
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LBool := VAL ( GetBwd ( TokResult . TrRdBack ) , BOOLEAN ) 
          ; HtExprOpnd1 ( )
          END (*IF*) 

      (* Subrange type: *) 
      | Itk . ItkSubrTypeRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            HtExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkSubrType
                    , ExpUpKind := Ekt . EkType
                    )
              )
          END (*IF*) 

      | Itk . ItkSubrTypeEllipsis 
      =>  IF NOT HtMaybePassTokenThru ( ) THEN  HtExprOpnd2 ( )  END (*IF*)

      | Itk . ItkSubrTypeLt 
      =>  IF NOT HtMaybePassTokenThru ( ) THEN HtExprOpnd1 ( )  END (*IF*)

      (* Unary operators: *) 
      | Itk . ItkUnaryOpRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN
            HtExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpKind := Ekt . EkUnOp
                    , ExpOpcode := GetBwdInt ( TokResult . TrRdBack )
                    )
              )
          END (*IF*)
 
      | Itk . ItkUnaryOpLt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN  
            LOpcode := GetBwdInt ( TokResult . TrRdBack ) 
          ; HtExprOpnd1 ( ) (* The only operand. *)
          ; IF NOT HtSkipping THEN UnaryOp ( LOpcode ) END (*IF*)
          END (*IF*) 

      (* Binary Operators: *) 
      | Itk . ItkBinaryOpRt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            HtExprRt
              ( NEW ( FM3Exprs . ExprTyp
                    , ExpOpcode := GetBwdInt ( TokResult . TrRdBack )
                    , ExpKind := Ekt . EkBinOp
                    )
              )
          END (*IF*) 

      | Itk . ItkBinaryOpOperator
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN 
            LOpcode := GetBwdInt ( TokResult . TrRdBack ) (* Opcode. *) 
          ; HtExprOpnd2 ( ) (* Right operand. *)
          END (*IF*) 

      | Itk . ItkBinaryOpLt 
      =>  IF NOT HtMaybePassTokenThru ( )
          THEN  
            LOpcode := GetBwdInt ( TokResult . TrRdBack ) 
          ; HtExprOpnd1 ( ) (* Left operand. *)
          ; IF NOT HtSkipping THEN BinaryOp ( LOpcode ) END (*IF*)
          END (*IF*) 

      | Itk . ItkCallRt
      => HtCallOrSubscript ( Ekt . EkCall )
      
      | Itk . ItkSubscriptRt 
      => HtCallOrSubscript ( Ekt . EkSubscript ) 

      | Itk . ItkActualsListRt
      , Itk . ItkSubscriptsPlusListRt 
       => LCt := GetBwdInt ( TokResult . TrRdBack ) (* Args count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IF LCt > 0
          THEN 
            LArgsExpr 
              := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
          ; <* ASSERT LCt = LArgsExpr . ExpArgNo *>
          END (*IF*) 

      | Itk . ItkActualsListSep
      , Itk . ItkSubscriptsPlusListSep
       => LCt := GetBwdInt ( TokResult . TrRdBack ) (* Args count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; LListElem := FM3Exprs . PopExprStack ( ) 
        ; LArgsExpr 
            := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
        ; DEC ( LArgsExpr . ExpArgNo )  
        ; LArgsExpr . ExpArgsList ^ [ LArgsExpr . ExpArgNo ] := LListElem

   (* | Itk . ItkEnumLitListSep *)
      
      | Itk . ItkActualsListLt
      , Itk . ItkSubscriptsPlusListLt
       => LCt := GetBwdInt ( TokResult . TrRdBack ) (* Args count. *) 
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; IF LCt > 0
          THEN 
            LListElem := FM3Exprs . PopExprStack ( ) (* LM actual. *) 
          ; LArgsExpr 
              := NARROW ( FM3Exprs . ExprStackTopObj , FM3Exprs . ExprTyp )
          ; <* ASSERT LArgsExpr . ExpArgNo = 1 *> 
            DEC ( LArgsExpr . ExpArgNo )
          ; LArgsExpr . ExpArgsList ^ [ LArgsExpr . ExpArgNo ] := LListElem
          END (*IF*) 

      | Itk . ItkCallLt
      , Itk . ItkSubscriptLt 
      =>  LCt := GetBwdInt ( TokResult . TrRdBack )
        ; LPosition := GetBwdPos ( TokResult . TrRdBack )
        ; LPrefixExpr := FM3Exprs . PopExprStack ( ) (* Procedure or array *) 
        ; LArgsExpr
            := NARROW ( FM3Exprs . PopExprStack ( ) , FM3Exprs . ExprTyp )
        ; <* ASSERT LPosition = LArgsExpr . ExpPosition *>
          <* ASSERT LArgsExpr . ExpArgNo = 0 *>
          LArgsExpr . ExpArgPrefix := LPrefixExpr
        ; IF TokResult . TrTok = Itk . ItkCallLt
          THEN LNewExpr := MaybeConvertCallToOperator ( LArgsExpr )
          ELSE LNewExpr := LArgsExpr 
          END (*IF*) 
        ; FM3Exprs . PushExprStack ( LNewExpr )
        ; PutBwdP2
            ( HtPass2RdBack , VAL ( LNewExpr . ExpSelfExprNo , LONGINT ) ) 
        ; PutBwdP2 ( HtPass2RdBack , VAL ( Itk . ItkExprTyp , LONGINT ) )

      | Itk . ItkInterfaceRt
      =>  LUnitRef ^ . UntDeclScopeStackBaseCt := FM3Scopes . DeclScopeStackCt 
        ; LUnitRef ^ . UntOpenScopeStackBaseCt := FM3Scopes . OpenScopeStackCt 
        ; HtPassTokenThru ( )
        
      | Itk . ItkInterfaceLt
      =>  FM3Scopes . PruneDeclScopeStack
            ( LUnitRef ^ . UntDeclScopeStackBaseCt ) 
        ; FM3Scopes . PruneOpenScopeStack
            ( LUnitRef ^ . UntOpenScopeStackBaseCt )
        ; HtPassTokenThru ( )

      (* Discard these tokens: *)
      | Itk . ItkEnumTypeRt
      , Itk . ItkEnumTypeLt 
      => FM3Patch . DiscardOperands
           ( FM3Utils . TokenOpndCt ( TokResult . TrTok )
           , TokResult . TrRdBack
           ) 


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
          ; LDeclNo
              := SCC [ RI ] + OslScopeRef ^ . ScpMinDeclNo (* Remove bias. *) 
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
          ; Wr . PutText
              ( LWrT , FM3Utils . PositionImage ( LDeclRefn . DclPos ) )
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
  ; VAR DdiOrigScopeRef : FM3Scopes . ScopeRefTyp 
  ; VAR DdiOrigScopeNo : FM3Scopes . ScopeNoTyp 

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
          := FM3Decls . NewDeclRef ( DdiOrigScopeRef , DeclNoI )
      ; LNewDeclRef . DclLink := LOldDeclRef 
      ; LNewDeclRef . DclSelfScopeRef := DdiOrigScopeRef (* Why not? *)
      ; LNewDeclRef . DclIdAtom := DdiAtom 
      ; LNewDeclRef . DclPos := DdiPosition 
      ; LNewDeclRef . DclKind := FM3Decls . DeclKindTyp . DkDuplDecl
      ; VarArray_Int_Refany . Assign
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap
          , (* Implicit NARROW. *) DeclNoI
          , LNewDeclRef
          )
      END Visit

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Globals . DeclNoTyp
    ; BEGIN (* Block. *)
        DdiAtom := GetBwdAtom ( TokResult . TrRdBack )
      ; DdiOrigScopeNo := GetBwdInt ( TokResult . TrRdBack )
      ; DdiOrigScopeRef := FM3Scopes . ScopeRefOfScopeNo ( DdiOrigScopeNo ) 
      ; DdiPosition := GetBwdPos ( TokResult . TrRdBack )
      ; LDeclNo
          := LookupDeclNoInScope ( DdiOrigScopeRef ^ , DdiAtom )
      ; <*ASSERT LDeclNo # FM3Globals . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdR2L
    
; PROCEDURE DeclIdR2L ( READONLY TokResult : TokResultTyp )
  : FM3Globals . DeclNoTyp
  (* ^This will be the only decl of DeclIdAtom in top decl scope. *) 

  = VAR DidAtom : FM3Base . AtomTyp
  ; VAR DidStdTok : FM3SrcToks . TokTyp 
  ; VAR DidDeclNo : FM3Globals . DeclNoTyp
  ; VAR DidPosition : tPosition
  ; VAR DidDeclKind : FM3Decls . DeclKindTyp

  ; PROCEDURE DidVisitRefNo ( RefNoI : INTEGER ) 
    (* A callback. *) 

    = VAR LScopeRef : FM3Scopes . ScopeRefTyp

    ; BEGIN (* DidVisitRefNo *) 
        LScopeRef := FM3Scopes . DeclScopeStackTopRef 
      ; FM3Graph . AddArc
          ( (*IN OUT*) LScopeRef ^ . ScpDeclGraph
          , DidDeclNo - LScopeRef ^ . ScpMinDeclNo
            (* v^Bias decl nos to zero in graph. *) 
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
        ; IF LDeclRef ^ . DclKind # Dkt . DkDuplDecl
          THEN <* ASSERT FALSE , "Preexisting non-duplicate decl." *>
          END (*IF*)  
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
          := FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ TRUE ]  
      ; LDeclRef ^ . DclDefType 
          := FM3Scopes . DeclScopeStackTopRef ^ . ScpCurDefExprs [ FALSE ]
      ; LDeclRef ^ . DclStdTok := DidStdTok 
          
      ; CASE DidDeclKind OF
        | Dkt . DkVar
        =>  IF NOT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                   IN FM3Scopes . ScopeKindSetOpen
            THEN <* ASSERT FALSE , "VAR decl in non-open decl scope" *> 
            END (*IF*)

        | Dkt . DkConst
        =>  IF NOT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                   IN FM3Scopes . ScopeKindSetOpen
            THEN <* ASSERT FALSE , "CONST decl in non-open decl scope" *> 
            END (*IF*)

        | Dkt . DkType                        
        , Dkt . DkProc
        , Dkt . DkReveal 
        =>  IF NOT FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
                   IN FM3Scopes . ScopeKindSetOpen
            THEN <* ASSERT FALSE , "Decl in non-open decl scope" *> 
            END (*IF*)

        | Dkt . DkVALUEFormal
        , Dkt . DkVARFormal
        , Dkt . DkROFormal
        , Dkt . DkRecField
        , Dkt . DkObjField
        , Dkt . DkMethod
        =>  IF FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
               IN FM3Scopes . ScopeKindSetOpen
            THEN <* ASSERT FALSE , "Qual decl in open decl scope" *> 
            END (*IF*)

        | Dkt . DkExc
        , Dkt . DkEnumLit
        =>  IF FM3Scopes . DeclScopeStackTopRef ^ . ScpKind
               IN FM3Scopes . ScopeKindSetOpen
            THEN <* ASSERT FALSE
                 , "Exception or enum lit declared in open decl scope"
                 *>
            END (*IF*)
(* TODO: Create an expression for the enumlit value with 
            ExpScalarConstVal 
              := LDeclRef ^ . DclSelfDeclNo
                 - LDeclRef ^ . DclOwningScopeRef ^ . ScpMinDeclNo
         and set LDeclRef ^ . DclDefValue to point to it.
*) 

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

    ; WITH WOutRdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
      DO 
        DidDeclNo
          := LookupDeclNoInScope
               ( FM3Scopes . DeclScopeStackTopRef ^ , DidAtom ) 
      ; <*ASSERT DidDeclNo # FM3Globals . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem 
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap 
          , DidDeclNo
          , DidVisitDecl
          )
      ; PutBwdP2 ( WOutRdBack , VAL ( DidPosition . Column , LONGINT ) ) 
      ; PutBwdP2 ( WOutRdBack , VAL ( DidPosition . Line , LONGINT ) ) 
      ; PutBwdP2 ( WOutRdBack , VAL ( DidDeclNo , LONGINT ) ) 
      ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
      ; RETURN DidDeclNo
      END (*WITH*) 
    END DeclIdR2L

; PROCEDURE SynthIsUsable1 ( Parent : FM3Exprs . ExprTyp )

  = BEGIN (*SynthIsUsable1*)
      Parent . ExpIsUsable
        := Parent . ExpIsUsable AND Parent . ExpOpnd1 . ExpIsUsable 
    END SynthIsUsable1        

; PROCEDURE SynthIsUsable2 ( Parent : FM3Exprs . ExprTyp )

  = BEGIN (*SynthIsUsable2*)
      Parent . ExpIsUsable
        := Parent . ExpIsUsable AND Parent . ExpOpnd2 . ExpIsUsable 
    END SynthIsUsable2        

; PROCEDURE SynthIsUsable12 ( Parent : FM3Exprs . ExprTyp )

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
    ( IdentRefAtom : FM3Base . AtomTyp
    ; READONLY Position : FM3Base . tPosition
    )

  = BEGIN
      IF AreInsideADecl ( )
      THEN PushExprIgnore ( Position )
      ELSE 
        WITH WOutRdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        DO 
          PutBwdP2 ( WOutRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WOutRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WOutRdBack , VAL ( IdentRefAtom , LONGINT ) ) 
        ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkIdRefAtomNotUsable , LONGINT ) )
        END (*WITH*)
      END (*IF*) 
    END PutNotUsable

; <*INLINE*> PROCEDURE AreInsideADecl ( ) : BOOLEAN

  = VAR LOpenScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      (* Design change to build expr objects, in or out of an expression.
         Eventually strip out all the calls and false cases.
      *) 
      RETURN TRUE
      
    ; LOpenScopeRef := FM3Scopes . OpenScopeStackTopRef
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
    ; LScopeMinDeclNo := LOpenScopeRef ^ . ScpMinDeclNo
    ; IF RefDeclNo < LScopeMinDeclNo THEN RETURN END (*IF*) 
    ; IF RefDeclNo >= LScopeMinDeclNo + LOpenScopeRef ^ . ScpDeclCt
      THEN RETURN
      END (*IF*)
    (* Ref is to the current open scope.  Would it be legal in a recursion? *) 
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

; PROCEDURE CheckParamKind
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
              , " of \""
              , FM3SrcToks . Image ( Opcode )
              , "\" must be "
              , FM3Exprs . ExprKindSetMessage ( AllowedKinds ) 
              , "." 
              }
          , OpndExpr . ExpPosition 
          )
      ; RETURN FALSE
      ELSE RETURN TRUE
      END (*IF*) 
    END CheckParamKind

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
      
      | FM3Exprs . ExprTyp ( TOpExpr )
      =>  LOK := TRUE
        ; IF NOT CheckParamKind
                   ( TOpExpr . ExpOpnd1
                   , TOpExpr . ExpOpcode 
                   , "Left parameter"
                   , TOpExpr . ExpBinOpLtOpndKindsAllowed
                   )
          THEN LOK := FALSE
          END (*IF*)

        ; IF TOpExpr . ExpBinOpActualsCt > 1
             AND  NOT CheckParamKind
                        ( TOpExpr . ExpOpnd2 
                        , TOpExpr . ExpOpcode
                        , "Second parameter"
                        , TOpExpr . ExpBinOpRtOpndKindsAllowed
                        )
          THEN LOK := FALSE
          END (*IF*)

        ; TYPECASE OpExpr OF
          | NULL =>
          
          | FM3Exprs . ExprTyp ( TQuadOpExpr )
          =>  IF NOT CheckParamKind
                      ( TQuadOpExpr . ExpQuadOpOpnd3 
                      , TQuadOpExpr . ExpOpcode
                      , "Third parameter"
                      , FM3Exprs . EkSetValue 
                      )
              THEN LOK := FALSE
              END (*IF*)
            ; IF TOpExpr . ExpBinOpActualsCt > 3
                 AND NOT CheckParamKind
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

  = VAR LCallExpr : FM3Exprs . ExprTyp 
  ; VAR LOpnd1 , LOpnd2 : FM3Exprs . ExprTyp 
  ; VAR LNewExpr : FM3Exprs . ExprTyp
  ; VAR LPluralSuffix : TEXT
  ; VAR LActualsCt : INTEGER
  ; VAR LUnitTok , LDeclTok : FM3SrcToks . TokTyp

  ; BEGIN
      IF TRUE (* Disable this for now. *) 
         OR NOT OrigExpr . ExpIsUsable THEN RETURN OrigExpr END (*IF*) 
    ; TYPECASE OrigExpr OF
      | NULL => RETURN NIL
      | FM3Exprs . ExprTyp ( TCallExpr )
      =>  LCallExpr := TCallExpr 
      ELSE RETURN OrigExpr
      END (*TYPECASE*) 
    ; IF LCallExpr . ExpArgsList = NIL THEN RETURN OrigExpr END (*IF*)
    ; LActualsCt := NUMBER ( LCallExpr . ExpArgsList ^ )
    ; TYPECASE LCallExpr . ExpArgPrefix OF
    
      | NULL => <* ASSERT FALSE , "NIL ExpArgPrefix of ExprArgsObj." *> 

      | FM3Exprs . ExprTyp ( TReservedIdRef )  
       => LUnitTok := FM3Base . TokNull 
        ; LDeclTok := TReservedIdRef . ExpOpcode  

      | FM3Exprs . ExprTyp ( TExprIdentRef )  
       => GetStdToks 
            ( FM3Units . UnitStackTopRef ^ . UntSelfUnitNo 
            , TExprIdentRef . ExpIdentDeclNo 
            , (*OUT*) LUnitTok 
            , (*OUT*) LDeclTok 
            ) 

      | FM3Exprs . ExprTyp ( TExprRemoteRef ) 
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
          , LCallExpr . ExpArgPrefix . ExpPosition 
          ) 
      ; OrigExpr . ExpIsUsable := FALSE 
      ; RETURN OrigExpr 
      END (*IF*)

    ; LNewExpr
        := FM3Builtins . BuiltinExpr
             ( LDeclTok , LCallExpr . ExpArgPrefix . ExpPosition ) 
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
    ; LOpnd1 := LCallExpr . ExpArgsList ^ [ 0 ]
    ; LNewExpr . ExpOpnd1 := LOpnd1
    ; LNewExpr . ExpIsUsable := LCallExpr . ExpIsUsable AND LOpnd1 . ExpIsUsable
    ; IF LActualsCt = 1
      THEN
        LNewExpr . ExpReachedDeclNoSet := LOpnd1 . ExpReachedDeclNoSet 
      ELSE
        LOpnd2 := LCallExpr . ExpArgsList ^ [ 1 ]
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
  ; VAR LExprObj : FM3Exprs . ExprTyp 
  ; VAR LExprIdentRef : FM3Exprs . ExprTyp 
  ; VAR LExprRemoteRef : FM3Exprs . ExprTyp 
  ; VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LUnitNo : FM3Globals . UnitNoTyp
  ; VAR LIdentRefAtom : FM3Base . AtomTyp
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
    ; WITH WOutRdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
      
        (* Look for a reference to a decl in an enclosing* open scope. *) 
        LRefDeclNo := LookupAtomInOpenScopes ( LIdentRefAtom )
      ; IF LRefDeclNo # FM3Globals . DeclNoNull 
        THEN
          LUnitRef := FM3Units . UnitStackTopRef 
        ; IF AreInsideADecl ( )
          THEN (* Create an ExprIdentRefTyp node. *) 
            CheckRecursiveRef ( LRefDeclNo )
          ; LExprIdentRef := NEW ( FM3Exprs . ExprTyp )
          ; LExprIdentRef . ExpIdentDeclNo := LRefDeclNo 
          ; LExprIdentRef . ExpPosition := LPosition
          ; LExprIdentRef . ExpKind := Ekt . EkIdentRef
          ; LExprIdentRef . ExpUpKind := Ekt . EkNull
(* TODO     ^ Compute this            *) 
          ; LExprObj := LExprIdentRef
          ; DefExprRt ( LExprObj )
          ; DefExprLt ( LExprObj ) 
          ELSE (* Change to a reference token with DeclNo instead of Atom. *)
(* Probably remove this case, since we are building Expr objects everywhere. *) 
            PutBwdP2 ( WOutRdBack , VAL ( LPosition . Column , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LPosition . Line , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LRefDeclNo , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkIdentRefWDeclNo , LONGINT ) )
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
                LExprRemoteRef := NEW ( FM3Exprs . ExprTyp )
              ; LExprRemoteRef . ExpRemoteUnitNo := LUnitNo 
              ; LExprRemoteRef . ExpRemoteDeclNo := LRefDeclNo 
              ; LExprRemoteRef . ExpPosition := LPosition
              ; LExprRemoteRef . ExpKind := Ekt . EkRemoteRef
(* TODO     ^ Compute this            *) 
              ; LExprRemoteRef . ExpUpKind := Ekt . EkNull
              ; LExprObj := LExprRemoteRef
              ; DefExprRt ( LExprObj )
              ; DefExprLt ( LExprObj ) 
              ELSE (* Emit an unusable token. *)
                PushExprIgnore ( LPosition )
              (* Read the following backwards: *) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LPosition . Column , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LPosition . Line , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LRefDeclNo , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LUnitNo , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkExpImpRef , LONGINT ) )
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
    END IdentRefR2L

; PROCEDURE QualIdentR2L ( Pass1RdBack : RdBackFile . T )
  (* (NON)PRE: No operands have been read.
     PRE Neither atom denotes a reserved ident.
  *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LExprRemoteRef : FM3Exprs . ExprTyp 
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
    ; WITH WOutRdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
      
      (* Look for a left reference to a decl in an enclosing open scope. *) 
        LRefDeclNoLt := LookupAtomInOpenScopes ( LAtomLt )
      ; IF LRefDeclNoLt # FM3Globals . DeclNoNull
        THEN (* Lt names a local declaration, not an interface. *)
          IF AreInsideADecl ( ) 
          THEN (* Create an ExprIdNo node. *) 
            CheckRecursiveRef ( LRefDeclNo )
          ; WITH WDotExpr = NEW ( FM3Exprs . ExprTyp )
                 , WLtExpr = NEW ( FM3Exprs . ExprTyp )
            DO 
              WDotExpr . ExpOpnd1 := WLtExpr 
            ; WDotExpr . ExpDotIdAtom := LAtomRt
            ; WDotExpr . ExpPosition := LPosRt
            ; WDotExpr . ExpKind := Ekt . EkDot
            ; WDotExpr . ExpUpKind := Ekt . EkRef
            ; DefExprRt ( WDotExpr )
            ; WLtExpr . ExpIdentDeclNo := LRefDeclNoLt 
            ; WLtExpr . ExpPosition := LPosLt
            ; WLtExpr . ExpKind := Ekt . EkQualIdentRef 
            ; WLtExpr . ExpUpKind := Ekt . EkNull
(* TODO     ^ Compute this            *) 
            ; DefExprRt ( WLtExpr )
            END (*WITH*)
          ; EVAL FM3Exprs . PopExprStack ( ) (* WLtExpr *) 
          ELSE (* Emit tokens for a dot Id applied to a DeclNo Id reference. *)

          (* Turn the qualident into separate Id ref and dot Id. *)
            PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )

          ; PutBwdP2 ( WOutRdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LRefDeclNoLt , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkIdentRefWDeclNo , LONGINT ) )

          ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )
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
                LExprRemoteRef := NEW ( FM3Exprs . ExprTyp )
              ; LExprRemoteRef . ExpRemoteUnitNo
                  := LIntfUnitRef ^ . UntSelfUnitNo 
              ; LExprRemoteRef . ExpRemoteDeclNo := LRemoteDeclNoInt 
              ; LExprRemoteRef . ExpPosition := LPosLt
              ; LExprRemoteRef . ExpKind := Ekt . EkRemoteRef
              ; LExprRemoteRef . ExpUpKind := Ekt . EkNull
(* TODO     ^ Compute this            *) 
              ; DefExprRt ( LExprRemoteRef )
              ELSE 
                PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LPosLt . Column , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LPosLt . Line , LONGINT ) ) 
              ; PutBwdP2 ( WOutRdBack , VAL ( LRemoteDeclNoInt , LONGINT ) ) 
              ; PutBwdP2
                  ( WOutRdBack , VAL ( LIntfUnitRef ^ . UntSelfUnitNo , LONGINT ) ) 
              ; PutBwdP2
                  ( WOutRdBack , VAL ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )
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
              WITH WDotExpr = NEW ( FM3Exprs . ExprTyp )
              , WLtExpr = NEW ( FM3Exprs . ExprTyp )
              DO 
                WDotExpr . ExpDotIdAtom := LAtomRt
              ; WDotExpr . ExpOpnd1 := WLtExpr 
              ; WDotExpr . ExpPosition := LPosRt
              ; WDotExpr . ExpKind := Ekt . EkDot
              ; WDotExpr . ExpUpKind := Ekt . EkRef
              ; DefExprRt ( WDotExpr ) (* Which pushes. *) 
              ; WLtExpr . ExpRemoteUnitNo := LUnitNoLt 
              ; WLtExpr . ExpRemoteDeclNo := LRefDeclNoLt 
              ; WLtExpr . ExpPosition := LPosLt
              ; WLtExpr . ExpKind := Ekt . EkRemoteRef
              ; WLtExpr . ExpUpKind := Ekt . EkNull
(* TODO     ^ Compute this            *) 
              ; DefExprRt ( WLtExpr ) (* Which pushes. *)
              END (*WITH*)
            ; EVAL FM3Exprs . PopExprStack ( ) (* The WLtExpr. *) 
            ELSE 
              PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LAtomRt , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )

            ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LPosLt . Column , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LPosLt . Line , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LRefDeclNoLt , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LUnitNoLt , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL
                ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )

            ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( LAtomRt , LONGINT ) ) 
            ; PutBwdP2 ( WOutRdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )
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
          := RdBackFile . Create ( LPass2FileFullName )
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

  = VAR LFailureMsg : TEXT
  ; VAR LFailureLoc : TEXT

  ; BEGIN (*TranslatePass2*)
  
    (* Write the Pass2 RdBack. *)
      TRY 
        Pass2Tokens ( UnitRef ^ . UntPass1OutEmptyCoord ) 

      (* Write final successful pass 2 output tokens. *)
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkEOF , LONGINT ) )

      ; RdBackFile . Flush ( UnitRef ^ . UntPass2OutRdBack ) 

      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE (* Pass 2 failed. *)
        LFailureMsg
          := FM3RTFailures . ExcNameFromAddr ( Compiler . ThisException ( ) )
      ; LFailureLoc
          := FM3RTFailures . ActivationLocationFromAddr
               ( Compiler . ThisException ( ) )

      (* Disassemble what there is of the failed file. *)
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack
          , VAL ( Itk . ItkLeftEndIncomplete , LONGINT ) 
          )
      ; RdBackFile . Flush ( UnitRef ^ . UntPass2OutRdBack )

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
  ; VAR LPatchCoordL : LONGINT 

  ; BEGIN (*FinishPass2*)
    (* Close pass 2. *) 
      UnitRef ^ . UntPass2Result := 0 
    ; EVAL FM3Scanner . PopState ( )
(* TODO^ Maybe do this elsewhere. *) 

    (* Finish with and close patch stack. *)
    ; UnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPatchStackRdBack )
    ; LPatchCoordL := FM3Compress . GetBwd ( UnitRef ^ . UntPatchStackRdBack )
    (* This is the initial pseudo coord, & patch stack sentinal. *)
    ; IF LPatchCoordL # FM3Globals . PatchStackEmptySentinel
      THEN <* ASSERT FALSE , "Mismatched coordinate sentinel." *>
      END (*IF*) 
    ; LLengthL := RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack )
    ; RdBackFile . Close (  UnitRef ^ . UntPatchStackRdBack , TruncTo := 0L )
      (* No point in keeping the patch stack.  It has pogo-sticked and 
         now should be devoid of significant content. *)
    ; LPatchFullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPatchStackSimpleName
             , NIL
             )
    ; FM3SharedUtils . DeleteFile ( LPatchFullFileName )

    ; FM3Messages . InfoArr
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
      ; FM3Messages . FatalArr
          ( ARRAY OF REFANY
              { "Pass 1 output file final size = "
              , LLengthImage
              , " bytes, should be "
              , FM3Base . Int64Image ( UnitRef ^ . UntPass1OutEmptyCoord )
              , "."
              }
          )
      END (*IF*)

    (* Report size and maybe disassemble pass 2 output file. *) 
    ; LPass2FullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPass2OutSimpleName
             , NIL
             )
    ; UnitRef ^ . UntMaxPass2OutLength
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPass2OutRdBack )
    ; FM3Messages . InfoArr
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

