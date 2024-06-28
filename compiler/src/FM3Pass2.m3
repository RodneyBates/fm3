
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
; IMPORT FM3Dict_Int_Int
; IMPORT FM3ExpImp
; IMPORT FM3ExpImpProxy 
; IMPORT FM3RTFailures 
; IMPORT FM3Globals
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

; CONST PosImage = FM3Utils . PositionImage

; CONST ALOSE = FM3Messages . AtomListToOSError 

; TYPE Ust = FM3Units . UnitStateTyp 

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
               { "Unable to write to readback file: "
(*TODO: Give RdBackFile a "Filename" function,, then insert it here. *) 
               , ALOSE ( EMsg ) , "."  
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
               { "Unable to write to patch file: "
(*TODO: Give RdBackFile a "Filename" function,, then insert it here. *) 
               , ALOSE ( EMsg ) , "."  
               }
           ) 
      END (*EXCEPT*) 
    END PutBwdPatch

; PROCEDURE SkipLt ( SkipNo : INTEGER )

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
     (* And throw this token away. *) 
      END (*WITH*)
    END SkipLt

; PROCEDURE CopyOperands
    ( OpndCt : [ 0 .. 6 ] 
    ; FromRdBack , ToRdBack : RdBackFile . T
    ; MaybeSkip : BOOLEAN (* ToRdBack is conditional on SkipNoStack. *) 
    )
  (* Copy operands, up to 6, without final reversing.  Pop/Push reverses
     them, but this procedure does its own reversal, resulting in
     net same order.
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*CopyOperands*)
<* ASSERT MaybeSkip = ( ToRdBack = FM3Globals . P2RdBack ) *> 
      IF MaybeSkip
         AND VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
             > 0 
      THEN (* Just read and discard OpndCt values. *)
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
                  END (*IF*) 
                END (*IF*) 
              END (*IF*) 
            END (*IF*) 
          END (*IF*) 
        END (*IF*) 
      ELSE (* Actually copy, without net reversal. *) 
        (* The obvious loop is unrolled. *) 
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
    END CopyOperands

(*EXPORTED*)
; PROCEDURE Pass2 ( LMPass1Depth : LONGINT )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPass1RdBack : RdBackFile . T 
  ; VAR LPatchRdBack : RdBackFile . T 
  ; VAR LPass2RdBack : RdBackFile . T 
  ; VAR LPass1Coord : LONGINT
  ; VAR LPatchTokenL , LPatchedTokenL , LTokenL : LONGINT 
  ; VAR LPatchToken , LPatchedToken , LToken : Itk . TokTyp
  ; VAR LScopeNo : FM3Base . ScopeNoTyp
  ; VAR LAtom : FM3Base . AtomTyp
  ; VAR LPosition : tPosition
  ; VAR LDeclKind : FM3Decls . DeclKindTyp
  ; VAR LSkipNo : INTEGER

  ; PROCEDURE P2ReverseVariableValues
      ( MaybeSkip : BOOLEAN (* ToRdBack is conditional on SkipNoStack. *) )
    (* Reverse copy a variable number of values, with a copy of their
       count before and after.
    *) 

    = VAR LCountLt : LONGINT
    ; VAR LCountRt : LONGINT
    ; VAR LCharsRef : REF ARRAY OF LONGINT 
    ; VAR LCount : INTEGER

    ; BEGIN
        IF MaybeSkip
           AND VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
               > 0 
        THEN
        ELSE 
          LCountRt := FM3Compress . GetBwd ( LPass1RdBack )
        ; PutBwdP2 ( LPass2RdBack , LCountRt )
        ; LCount := VAL ( LCountRt , INTEGER )
        ; LCharsRef := NEW ( REF ARRAY OF LONGINT , LCount )
        ; FOR RI := 0 TO LCount - 1
          DO LCharsRef ^ [ RI ] := FM3Compress . GetBwd ( LPass1RdBack )
          END (*FOR*) 
        ; FOR RI := LCount - 1 TO 0 BY - 1 
          DO PutBwdP2 ( LPass2RdBack , LCharsRef ^ [ RI ] )
          END (*FOR*)
        ; LCharsRef := NIL 
        ; LCountLt := FM3Compress . GetBwd ( LPass1RdBack )
        ; <*ASSERT LCountLt = LCountRt *>
          PutBwdP2 ( LPass2RdBack , LCountLt )
        END (*IF*) 
      END P2ReverseVariableValues 

  ; BEGIN (* Pass2 *) 
      LUnitRef := FM3Units . UnitStackTopRef
    (* For now, let's assume the skip mechanism is only used during pass 2.*)
    ; LPass1RdBack := LUnitRef . UntPass1OutRdBack 
    ; LPatchRdBack := LUnitRef . UntPatchStackRdBack 
    ; LPass2RdBack := LUnitRef . UntPass2OutRdBack
    ; LMPass1Depth := MAX ( LMPass1Depth , LUnitRef . UntPass1OutEmptyCoord )
    ; EVAL GetBwd ( LUnitRef ^ . UntPass1OutRdBack ) 
    ; EVAL GetBwd ( LUnitRef ^ . UntPass1OutRdBack ) 

    ; LOOP (* One of three possible actions. *) 
        LPass1Coord := RdBackFile . LengthL ( LPass1RdBack )
      ; IF LPass1Coord <= LMPass1Depth
           (* ^Nothing more to pop off Pass1 strack. *) 
           AND RdBackFile . LengthL ( LPatchRdBack )
               <= LUnitRef . UntPatchStackTopCoord
           (* ^ Nothing more to pop off Patch stack. *) 
        THEN EXIT
        END (*IF*)
        
      (* Check first for a patch. *) 
      ; IF LPass1Coord <= LUnitRef . UntPatchStackTopCoord
        THEN

        (* Modify and move token from the patch stack to the output. *) 
          <*ASSERT


TRUE OR 
LPass1Coord = LUnitRef . UntPatchStackTopCoord
                   (* Haven't missed a patch stack token. *)
          *> 
          LPatchTokenL := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchToken := VAL ( LPatchTokenL , Itk . TokTyp ) 
        ; <*ASSERT
              IntSets . IsElement
                ( LPatchToken , FM3SharedGlobals . GTokSetPatch )
          *>
          IF LPatchToken = Itk . ItkSkipLtPatch
          THEN (* Special handling, patching ItkSkipLtPatch. *)
            SkipLt ( GetBwdInt ( FM3Globals . PatchRdBack ) )  
          ELSE 
            LPatchedToken := LPatchToken - Itk . LtToPatch
(* FIXME: The patch operation can apply to any non-Rt token.  I think
          the necessary bias is always the same as LtToPatch, but check
          this and then use a better name for it.
*) 

         (* Copy the operands, reversing them to counteract the reversal
            accomplished by pop & push stack operations. *)
          ; CopyOperands
              ( FM3Utils . TokenOpndCt ( LPatchedToken ) 
              , LPatchRdBack
              , LPass2RdBack
              , MaybeSkip := TRUE 
              ) 

          (* Put the patched token. *)
          ; LPatchedTokenL := VAL ( LPatchedToken , LONGINT ) 
          ; PutBwdP2 ( LPass2RdBack , LPatchedTokenL )
          END (*IF*) 

        (* Conceptually finish popping the Patch stack by caching the
           next patch coordinate, which, unusually, is on top of its token.
        *)
        ; LUnitRef . UntPatchStackTopCoord 
            := FM3Compress . GetBwd ( LPatchRdBack )
          
        (* Now loop, possibly for more patches. *)
        
        ELSE (* Look at the top token on the pass 1 output file. *) 
          LTokenL := FM3Compress . GetBwd ( LPass1RdBack )
        ; LToken := VAL ( LTokenL , Itk . TokTyp ) 
        ; IF IntSets . IsElement ( LToken , FM3SharedGlobals . GTokSetPatch )
          THEN 

          (* Move this token from the pass 1 output file to the patch stack. *)
            PutBwdPatch
              ( LPatchRdBack
              , LUnitRef . UntPatchStackTopCoord
              ) (* Uncache the existing patch coordinate by pushing it on top
                   of its token. *) 

          ; LUnitRef . UntPatchStackTopCoord
              := FM3Compress . GetBwd ( LPass1RdBack )
                 (* New cached top patch coordinate. *)
          ; CopyOperands 
              ( FM3Utils . TokenOpndCt ( LToken ) 
              , LPass1RdBack
              , LPatchRdBack
              , MaybeSkip := FALSE  
              ) 
          ; PutBwdPatch ( LPatchRdBack , LTokenL )
            (* ^Push the token code deeper than its patch coordinate. *)
            
          ELSE

            (* Directly handle this token and/or move it to Pass2 output. *) 
            CASE LToken OF

            (* Specially handled tokens. *)
            | Itk . ItkSkipLt
            => SkipLt ( GetBwdInt ( FM3Globals . P1RdBack ) ) 
 
            | Itk . ItkSkipRt
            => LSkipNo := GetBwdInt ( LPass1RdBack )
              ; WITH WSkipNoStack = FM3Globals . SkipNoStack
                DO VarArray_Int_Int . Assign
                    ( WSkipNoStack
                    , VarArray_Int_Int . TouchedRange ( WSkipNoStack ) . Hi + 1  
                    , LSkipNo
                    )
                (* Discard this token. *)
                END (*WITH*)

            | Itk . ItkScopeEmpty 
            =>  LScopeNo := GetBwdScopeNo ( LPass1RdBack )
(* TODO: Anything here? *) 

            | Itk . ItkDeclScopeRt 
            =>  LScopeNo := GetBwdScopeNo ( LPass1RdBack )
              ; FM3Scopes . PushDeclScopeRef
                  ( FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) )

            | Itk . ItkDeclScopeLt 
            =>  LScopeNo := GetBwdScopeNo ( LPass1RdBack ) 
              ; <* ASSERT FM3Scopes . PopDeclScopeRef ( )
                          = FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) *>

            | Itk . ItkLookupScopeRt 
            =>  LScopeNo := GetBwdScopeNo ( LPass1RdBack ) 
              ; FM3Scopes . PushLookupScopeRef
                  ( FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) )

            | Itk . ItkLookupScopeLt 
            =>  LScopeNo := GetBwdScopeNo ( LPass1RdBack ) 
              ; <* ASSERT FM3Scopes . PopLookupScopeRef ( )
                          = FM3Scopes . ScopeRefOfScopeNo ( LScopeNo ) *> 

(* CONSISTIFY: For some of these, fetch the operands inside the called proc. *) 
            | Itk . ItkDuplDeclId
            => LAtom := GetBwdAtom ( LPass1RdBack )
              ; LPosition := GetBwdPos ( LPass1RdBack )
              ; EVAL DuplDeclIdR2L ( LAtom , LPosition )

            | Itk . ItkDeclId
            => LDeclKind := GetBwdDeclKind ( LPass1RdBack )
              ; LAtom := GetBwdAtom ( LPass1RdBack )
              ; LPosition := GetBwdPos ( LPass1RdBack )
              ; EVAL DeclIdR2L ( LDeclKind , LAtom , LPosition )

(* FIXME: We now use different tokens for different declkinds, eg.
          ItkVALUEFormalIdListElem.  But is that necessary? *) 
              
            | Itk . ItkIdRefAtom 
            => LAtom := GetBwdAtom ( LPass1RdBack )
              ; LPosition := GetBwdPos ( LPass1RdBack )
              ; IdentRefR2L ( LAtom , LPosition )

            | Itk . ItkQualIdAtoms 
            => QualIdentR2L ( LPass1RdBack )

            | Itk . ItkBlockRt
            => LScopeNo := GetBwdScopeNo ( LPass1RdBack ) 
           (* Doesn't exist, probably ItkBlock[LR]t will disappear.

           *) 
              ; CopyOperands
                 ( 2 (*Position*)
                 , LPass1RdBack
                 , LPass2RdBack
                 , MaybeSkip := TRUE 
                 )
              ; PutBwdP2 ( LPass2RdBack , VAL ( LScopeNo , LONGINT ) ) 
              ; PutBwdP2 ( LPass2RdBack , VAL ( Itk . ItkBlockRt , LONGINT ) )

            | Itk . ItkBlockLt 
            => CopyOperands
                 ( FM3Utils . TokenOpndCt ( LToken ) 
                 , LPass1RdBack
                 , LPass2RdBack
                 , MaybeSkip := TRUE 
                 )
              ; PutBwdP2 ( LPass2RdBack , LTokenL )
              ; EVAL FM3Scopes . PopDeclScopeRef ( )

            | Itk . ItkTextLitRt
            , Itk . ItkWideTextLitRt
            => CopyOperands
                 ( 3 (* Atom, position. *)
                 , LPass1RdBack
                 , LPass2RdBack
                 , MaybeSkip := TRUE 
                 )
              ; PutBwdP2
                  ( LPass2RdBack
                  , VAL ( (*FM3Utils . SwitchTokL2R*) ( LToken ) , LONGINT )
                  )
              ; P2ReverseVariableValues ( MaybeSkip := TRUE ) 

            | Itk . ItkTextLitLt
            , Itk . ItkWideTextLitLt
            => CopyOperands
                 ( 3 (* Atom, position. *)
                 , LPass1RdBack
                 , LPass2RdBack
                 , MaybeSkip := TRUE 
                 )
              ; PutBwdP2
                  ( LPass2RdBack
                  , VAL ( (*FM3Utils . SwitchTokL2R*) ( LToken ) , LONGINT )
                  )

            ELSE (* Move directly, input to output.*)
              CopyOperands
                ( FM3Utils . TokenOpndCt ( LToken )
                , LPass1RdBack
                , LPass2RdBack
                , MaybeSkip := TRUE 
                ) 
            ; PutBwdP2 ( LPass2RdBack , LTokenL )
            END (*CASE*) 
          END (*IF*) 
        (* And loop *)           
        END (*IF*)

      END (*LOOP*)
    END Pass2

(* Right-to-left scope handling.  Call sites read the Itk and its operands,
   and pass the operands in. *) 

(*EXPORTED*)
; PROCEDURE LookupDeclNo
    ( READONLY Scope : FM3Scopes . ScopeTyp 
    ; IdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ) 
  : FM3Base . DeclNoTyp 
  (* PRE: IdAtom is in Scope's dictionary. *) 

  = VAR LDeclNoInt : INTEGER
  ; VAR LMsg : TEXT
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupDeclNo*)
      TRY
        LFound 
          := FM3Dict_Int_Int . LookupFixed
               ( Scope . ScpDeclDict
               , IdAtom
               , FM3Base . HashNull
               , (*OUT*) LDeclNoInt
               )
      ; IF LFound
        THEN
          RETURN LDeclNoInt (* Implied NARROW. *) 
        ELSE LMsg := "not found"
        END (*IF*) 
      EXCEPT FM3Dict_Int_Int . Error ( EMsg )
        => LFound := FALSE
        ; LMsg := EMsg 
      END (*EXCEPT*)
    ; IF NOT LFound
      THEN 
        FM3Messages . FatalArr
          ( ARRAY OF REFANY
              { "While looking up declaration of \""
              , FM3Units . TextOfIdAtom ( IdAtom ) 
              , "\" in scope at "
              , PosImage ( Scope . ScpPosition )
              , ", "
              , LMsg 
              , "." 
              }
          , Position 
          )
      ; RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    END LookupDeclNo

; <*UNUSED*> PROCEDURE LookupImport
     ( IdAtom : FM3Base . AtomTyp ) : FM3Base . DeclNoTyp
  (* In the current unit. *) 

  = VAR LDeclNoInt : INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      IF NOT IntSets . IsElement
               ( IdAtom , FM3Units . UnitStackTopRef ^ . UntExpImpIdSet )
      THEN RETURN FM3Base . DeclNoNull 
      ELSE
        TRY 
          LFound
            := FM3Dict_Int_Int . LookupFixed
                 ( FM3Units . UnitStackTopRef ^ . UntScopeRef
                   ^ . ScpDeclDict
                 , IdAtom
                 , FM3Base . HashNull
                 , (*OUT*) LDeclNoInt 
                 )
        EXCEPT FM3Dict_Int_Int . Error ( EMsg )
        => LFound := FALSE
        END (*EXCEPT*)
      ; <* ASSERT LFound *> 
        RETURN LDeclNoInt (* Implied NARROW. *)
      END (*IF*) 
    END LookupImport

; PROCEDURE LookupExpImp
    ( IdAtom : FM3Base . AtomTyp
    ; VAR (*OUT*) UnitNo : FM3Base . UnitNoTyp
    ; VAR (*OUT*) DeclNo : FM3Base . DeclNoTyp
    )
  : BOOLEAN (* It's present. *) 
  (* In the current unit.  *) 

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
  (* Starting at lookup TOS. *) 

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
          EXCEPT FM3Dict_Int_Int . Error ( EMsg )
            => LFound := FALSE
          END (*EXCEPT*)
        ; <* ASSERT LFound *>
          RETURN LDeclNoInt (* Implied NARROW. *) 
        ELSE (* Try the next outer scope. *) 
          LScopeRef := LScopeRef ^ . ScpLookupStackLink
        END (*IF*) 
      END (*LOOP*) 
    END LookupBlockRef

; PROCEDURE DuplDeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp
  (* Append a temporary, pseudo-decl node to the linked list rooted at
     the decl number.  The position of the original declaration of
     the ident, which is needed for the error message, is not known yet.
  *) 

  = PROCEDURE Visit ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *)  
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
 
    = VAR LOldDeclRef : FM3Decls . DeclRefTyp  
    ; VAR LNewDeclRef : FM3Decls . DeclRefTyp  

    ; BEGIN
        LOldDeclRef := DeclRefany (* Implicit NARROW. *) 
      ; LNewDeclRef
          := FM3Decls . NewDeclRef
               ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
      ; LNewDeclRef . DclLink := LOldDeclRef 
      ; LNewDeclRef . DclSelfScopeRef
          := FM3Scopes . DeclScopeStackTopRef (* Why not? *)
      ; LNewDeclRef . DclIdAtom := DeclIdAtom 
      ; LNewDeclRef . DclDeclNo := (* Implicit NARROW. *) DeclNoI 
      ; LNewDeclRef . DclPos := Position 
      ; LNewDeclRef . DclKind := FM3Decls . DeclKindTyp . DkDuplDecl
      ; VarArray_Int_Refany . Assign
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap
          , (* Implicit NARROW. *) DeclNoI
          , LNewDeclRef
          )
      END Visit

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block. *)
        LDeclNo
          := LookupDeclNo
               ( FM3Scopes . DeclScopeStackTopRef ^ , DeclIdAtom , Position )
      ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdR2L
    
; PROCEDURE DeclIdR2L
    ( DeclKind : FM3Decls . DeclKindTyp
    ; DeclIdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    )
  : FM3Base . DeclNoTyp
  (* ^This will be the only decl of DeclIdAtom in the current scope. *) 

  = PROCEDURE VisitDecl ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *) 
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *)
    
    = VAR LDeclRef : FM3Decls . DeclRefTyp
    ; VAR LIdentText : TEXT

    ; BEGIN (* VisitDecl *)
        LDeclRef := DeclRefany (* Implied NARROW. *)
      ; IF LDeclRef # NIL (* Some duplicate decls of DeclNoI also exist? *) 
        THEN (* Dispense with them with error messages. *) 
          LIdentText := FM3Units . TextOfIdAtom ( DeclIdAtom ) 
        ; WHILE LDeclRef # NIL
          DO
            ErrorArr
              ( ARRAY OF REFANY
                  { "Duplicate declaration of \""
                  , LIdentText
                  , "\", ignored, original at "
                  , PosImage ( Position )
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
      ; LDeclRef . DclLink := NIL 
      ; LDeclRef . DclSelfScopeRef := NIL
(* TODO: ^ Get this from parser.  Also set the reverse link ScpOwningDeclNo. *) 
      ; LDeclRef . DclIdAtom := DeclIdAtom 
      ; LDeclRef . DclDeclNo := DeclNoI
      ; LDeclRef . DclPos := Position 
      ; LDeclRef . DclKind := DeclKind 
      ; DeclRefany := LDeclRef 
      END VisitDecl

  ; BEGIN (*DeclIdR2L*) 
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    ; VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block *)
        WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        DO 
          LDeclNo
            := LookupDeclNo
                 ( FM3Scopes . DeclScopeStackTopRef ^ , DeclIdAtom , Position ) 
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          VarArray_Int_Refany . CallbackWithElem 
            ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , VisitDecl )
        ; PutBwdP2 ( Wp2RdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
(* FIXME: Put a token for the Id. *) 
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
      WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
      DO 
      (* Read the following backwards: *) 
        PutBwdP2 ( Wp2RdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( IdentRefAtom , LONGINT ) ) 
      ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdRefAtomNotUsable , LONGINT ) )
      END (*WITH*) 
    END PutNotUsable 

; PROCEDURE IdentRefR2L
    ( IdentRefAtom : FM3Base . AtomTyp
    ; READONLY Position : FM3Base . tPosition
    )
  (* PRE: The ident is not followed by dot Ident. (The parser has gone
          to some trouble to ensure this.)
  *) 

  = VAR LExpImpUnitRef : FM3Units . UnitRefTyp 
  ; VAR LUnitNo : FM3Base . UnitNoTyp
  ; VAR LDeclNo : FM3Base . DeclNoTyp
  ; VAR LIntfNameChars : FM3Atom_OAChars . KeyTyp
  ; VAR LIsUsable : BOOLEAN 

  ; BEGIN (*IdentRefR2L*)
      IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN 
      END (*IF*)
    ; WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO

      (* Look for a reference to a local declaration. *) 
        LDeclNo := LookupBlockRef ( IdentRefAtom )
      ; IF LDeclNo # FM3Base . DeclNoNull
        THEN (* It names a local declaration. *)
        (* Change to a reference token with DeclNo instead of Atom. *)
        (* Read the following backwards: *) 
          PutBwdP2 ( Wp2RdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )
        ELSIF LookupExpImp ( IdentRefAtom , (*OUT*) LUnitNo , (*OUT*) LDeclNo )
        THEN (* Export or import is present. *) 
          IF LUnitNo = FM3Base . UnitNoNull
(* CHECK: ^v Which of these ways denoting unusablilty can happen? *) 
          THEN LIsUsable := FALSE
          ELSE
            LExpImpUnitRef (* Implicit NARROW. *) 
              := VarArray_Int_Refany . Fetch
                   ( FM3Units . UnitsMap
                   , LUnitNo 
                   )
          ; LIsUsable := LExpImpUnitRef ^ . UntState # Ust . UsNotUsable
          END (*IF*)
        ; IF LIsUsable
          THEN (* It names an interface.  There is no selection. *)
            IF LDeclNo = FM3Base . DeclNoNull
            THEN (* Interface name w/o a selection--illegal. *) 
              IF NOT FM3Atom_OAChars . Key
                       ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                       , IdentRefAtom
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
                , Position
                ) 
            ; PutNotUsable( IdentRefAtom , Position ) 
            ELSE (* A remote decl brought in by EXPORTS or FROM I IMPORT. *)
            (* Read the following backwards: *) 
              PutBwdP2 ( Wp2RdBack , VAL ( Position . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( Position . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LDeclNo , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LUnitNo , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExpImpRef , LONGINT ) )
            END (*IF*)
          ELSE (* It was already unusable. *) 
            PutNotUsable( IdentRefAtom , Position ) 
          END (*IF*) 
        ELSE (* Undeclared. *) 
          BadIdentMessage ( "Undeclared identifier" , IdentRefAtom , Position )
        ; PutNotUsable( IdentRefAtom , Position ) 
        END (*IF*)
      END (*WITH*) 
    END IdentRefR2L

; PROCEDURE QualIdentR2L ( Pass1RdBack : RdBackFile . T )
  (* (NON)PRE: No operands have been read. *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp 
  ; VAR LUnitNoLt : FM3Base . UnitNoTyp
  ; VAR LDeclNoLt : FM3Base . DeclNoTyp
  ; VAR LAtomLt , LAtomRt : FM3Base . AtomTyp
  ; VAR LIntfAtomRt : FM3Base . AtomTyp
  ; VAR LIntfDeclNoInt : INTEGER 
  ; VAR LPosLt , LPosRt : FM3Base . tPosition
  ; VAR LProxy : FM3ExpImpProxy . T

  ; BEGIN (*QualIdentR2L*)
      LAtomLt := GetBwdAtom ( Pass1RdBack ) 
    ; LAtomRt := GetBwdAtom ( Pass1RdBack ) 
    ; LPosLt := GetBwdPos ( Pass1RdBack ) 
    ; LPosRt := GetBwdPos ( Pass1RdBack )
    ; IF VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN 
      END (*IF*) 
    ; WITH Wp2RdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
        (* Look for a left reference to a local declaration. *) 
        LDeclNoLt := LookupBlockRef ( LAtomLt )
      ; IF LDeclNoLt # FM3Base . DeclNoNull
        THEN (* Lt names a local declaration, not an interface. *)

        (* Turn the qualident into separate Id ref and dot Id. *)
        (* Read the following backwards: *) 
          PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )

        ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LDeclNoLt , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )

        ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
        ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )

        ELSIF LookupExpImp ( LAtomLt , (*OUT*) LUnitNoLt , (*OUT*) LDeclNoLt )
        THEN (* Lt ident is [ex|im]ported. *)  
          IF LUnitNoLt = FM3Base . UnitNoNull
          THEN (* Unusable. *) 
            PutNotUsable( LAtomLt , LPosLt ) 
          ELSIF LDeclNoLt = FM3Base . DeclNoNull
          THEN (* Lt names an imported interface. *)
            LIntfUnitRef (*Implicit NARROW*) 
              := VarArray_Int_Refany . Fetch
                   ( FM3Units . UnitsMap , LUnitNoLt )
          ; <* ASSERT LIntfUnitRef # NIL *>
            LIntfAtomRt
              := FM3Compile . ConvertIdentAtom
                   ( LAtomRt
                   , FromUnitRef := FM3Units . UnitStackTopRef
                   , ToUnitRef := LIntfUnitRef
                   )
          ; IF IntSets . IsElement
                 ( LIntfAtomRt , LIntfUnitRef ^ . UntExpImpIdSet )
            THEN (* Rt imported into interface, not transitively importable. *)
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
            (* Read the following backwards: *) 
              PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosRt . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
            ; PutBwdP2 ( Wp2RdBack , VAL ( LIntfDeclNoInt , LONGINT ) ) 
            ; PutBwdP2
                ( Wp2RdBack , VAL ( LIntfUnitRef ^ . UntUnitNo , LONGINT ) ) 
            ; PutBwdP2
                ( Wp2RdBack , VAL ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )
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
          ELSE (* Left denotes a remote decl, brought in by EXPORTS
                  or FROM-IMPORT.
               *)
          (* Turn it into separate QualId ref and dot Id *) 
          (* Read the following backwards: *) 
            PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotLt , LONGINT ) )

          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LDeclNoLt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LUnitNoLt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL
              ( Itk . ItkQualIdUnitNoDeclNo , LONGINT ) )

          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Column , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LPosLt . Line , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( LAtomRt , LONGINT ) ) 
          ; PutBwdP2 ( Wp2RdBack , VAL ( Itk . ItkExprDotRt , LONGINT ) )
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
               , ALOSE ( EMsg)
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
        Pass2 ( UnitRef ^ . UntPass2OutEmptyCoord )

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

  ; BEGIN (*FinishPass2*)
    (* Close pass 2. *) 
      UnitRef . UntPass2Result := 0 
    ; EVAL FM3Scanner . PopState ( )
(* TODO^ Maybe do this elsewhere. *) 

(* Finish with and close patch stack. *)

    ; UnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPatchStackRdBack ) 
    ; LLengthL := RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack )
    ; RdBackFile . Close (  UnitRef ^ . UntPatchStackRdBack , 0L )
      (* No point in keeping the patch stack.  It has pogo-sticked and 
         now should be empty. *)
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
              , "bytes, should be "  
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

(* This is new deferred until compile cleanup: 
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

