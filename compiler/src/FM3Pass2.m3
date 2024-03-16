
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Pass2

; IMPORT Atom
; IMPORT Compiler 
; FROM File IMPORT Byte  
; IMPORT FileWr
; IMPORT Fmt 
; IMPORT FS 
; IMPORT IntSets 
; IMPORT OSError
; IMPORT Pathname
; IMPORT Stdio
; IMPORT Text 
; IMPORT Thread 
; IMPORT UniRd
; IMPORT Wr

; IMPORT Ranges_Int
; IMPORT RangeUtils 
; IMPORT IntIntVarArray 
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; FROM FM3Base IMPORT tPosition 
; IMPORT FM3CLArgs
; IMPORT FM3Compile
; IMPORT FM3Compress
; FROM FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int
; IMPORT FM3DisAsm
; IMPORT FM3RTFailures 
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3IntToks AS Itk
; FROM FM3IntToks
    IMPORT LtToRt , LtToPatch , LtToOne , LtToOnePatch , LtToTwoPatch
           , LtToListSepPatch 
; FROM FM3StreamUtils
    IMPORT GetBwdInt , GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Parser
; IMPORT FM3Pass1 
; IMPORT FM3Predefined
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT RdBackFile 

; CONST PosImage = FM3Utils . PositionImage

; CONST ALOSE = FM3Messages . AtomListToOSError 

; PROCEDURE PutBwdP2 ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN (*PutBwdP2*) 
      <* ASSERT RdBack = FM3Globals . P2RdBack *> 
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0 
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

; PROCEDURE SkipLt ( SkipNo : INTEGER )

  = BEGIN 
      WITH WSkipNoStack = FM3Globals . SkipNoStack
      , WSkipRange = IntIntVarArray . TouchedRange ( WSkipNoStack )
      DO
        <* ASSERT WSkipRange . Hi > 0 *>
        <* ASSERT 
             IntIntVarArray . Fetch ( WSkipNoStack , WSkipRange . Hi )
             = SkipNo
        *>
        IntIntVarArray . Project (* Pop SkipNoStack. *) 
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
         AND IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
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
          <*ASSERT LPass1Coord = LUnitRef . UntPatchStackTopCoord
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
            accomplished by stack operations. *)
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
            PutBwdP2
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
          ; PutBwdP2 ( LPatchRdBack , LTokenL )
            (* ^Push the token code deeper than its patch coordinate. *)
            
          ELSE

            (* Move this token to the Pass2 stack. *) 
            CASE LToken OF

            (* Specially handled tokens. *)
            | Itk . ItkSkipLt
            => SkipLt ( GetBwdInt ( FM3Globals . P1RdBack ) ) 
 
            | Itk . ItkSkipRt
            => LSkipNo := GetBwdInt ( LPass1RdBack )
              ; WITH WSkipNoStack = FM3Globals . SkipNoStack
                DO IntIntVarArray . Assign
                    ( WSkipNoStack
                    , IntIntVarArray . TouchedRange ( WSkipNoStack ) . Hi + 1  
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
              ; EVAL IdentRefR2L ( LAtom , LPosition )

            | Itk . ItkQualIdAtoms 
            => EVAL QualIdentR2L ( LPass1RdBack )

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

            ELSE (* Move directly, unnest to the output.*)
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

; PROCEDURE LookupId
    ( READONLY Scope : FM3Scopes . ScopeTyp 
    ; IdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ) 
  : FM3Base . DeclNoTyp 
  (* PRE: IdAtom is in Scope's dictionary. *) 

  = VAR LDeclNoInt : INTEGER
  ; VAR LMsg : TEXT
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupId*)
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
              { "While looking up decl of \""
              , FM3Units . TextOfIdAtom ( IdAtom ) 
              , "\" in scope at "
              , PosImage ( Scope . ScpPosition )
              , ", "
              , LMsg 
              , "." 
              }
          , Position 
          ) 
      END (*IF*) 
    END LookupId

; PROCEDURE LookupBlockRef
    ( IdAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp
  (* POST: Error message written if not declared. *)

  = VAR LBlockScopeRef : FM3Scopes . ScopeRefTyp 
  ; VAR LDeclNoInt : INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupBlockRef*)
      LBlockScopeRef := FM3Scopes . LookupScopeStackTopRef
    ; LOOP
        IF LBlockScopeRef = NIL (* Undeclared? *) 
        THEN 
          FM3Messages . ErrorArr
             ( ARRAY OF REFANY
                 { "Undeclared identifier \""
                 , FM3Units . TextOfIdAtom ( IdAtom )
                 , "\", (2.1)." 
                 }
             , Position 
             )
        ; RETURN FM3Base . DeclNoNull
        ELSIF NOT LBlockScopeRef ^ . ScpKind IN FM3Scopes . ScopeKindSetBlock
           (* ^Can this happen? *) 
        THEN LBlockScopeRef := LBlockScopeRef ^ . ScpLookupStackLink
        ELSIF NOT IntSets . IsElement
                    ( IdAtom , LBlockScopeRef ^ . ScpDeclIdSet )   
        THEN LBlockScopeRef := LBlockScopeRef ^ . ScpLookupStackLink
        ELSE TRY 
            LFound := FM3Dict_Int_Int . LookupFixed
                        ( LBlockScopeRef . ScpDeclDict
                        , IdAtom
                        , FM3Base . HashNull
                        , (*OUT*) LDeclNoInt 
                        )
          EXCEPT FM3Dict_Int_Int . Error ( EMsg )
            => LFound := FALSE
(* CHECK: Do we want this to be a fatal compiler error? *) 
          END (*EXCEPT*)
        ; IF LFound
          THEN RETURN LDeclNoInt (* Implied NARROW. *)
          ELSE LBlockScopeRef := LBlockScopeRef ^ . ScpLookupStackLink 
          END (*IF*) 
        END (*IF*) 
      END (*LOOP*) 
    END LookupBlockRef
    
(* Right-to-left scope handling.  These are called during unnesting. *)
(* Call sites read the Itk and its args, and pass in the args. *) 

; PROCEDURE DuplDeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp
  (* Append a temporary, pseudo-decl node to the linked list rooted at
     the decl number.  The position of the original declaration of
     the ident is not known right now.
  *) 

  = PROCEDURE Visit ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *)  
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
 
    = VAR LOldDeclRef : FM3Decls . DeclRefTyp  
    ; VAR LNewDeclRef : FM3Decls . DeclRefTyp  

    ; BEGIN
        LOldDeclRef := DeclRefany (* Implied NARROW. *) 
      ; LNewDeclRef
          := FM3Decls . NewDeclRef
               ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
     (* ^This will have, as a subtle side-effect, made DeclRefany
         = LNewDeclRef. Can we code this less opaquely, while respecting
         that the signature of Visit is constrained by CallbackWithElem? *)
      ; LNewDeclRef . DclLink := LOldDeclRef 
      ; LNewDeclRef . DclSelfScopeRef
          := FM3Scopes . DeclScopeStackTopRef (* Why not? *)
      ; LNewDeclRef . DclIdAtom := DeclIdAtom 
      ; LNewDeclRef . DclDeclNo := DeclNoI 
      ; LNewDeclRef . DclPos := Position 
      ; LNewDeclRef . DclKind := FM3Decls . DeclKindTyp . DkDuplDecl
      END Visit

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block. *)
        LDeclNo
          := LookupId
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
  (* This will be the only decl of DeclIdAtom in the current scope. *) 

  = PROCEDURE VisitDecl ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *) 
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
    = VAR LDeclRef : FM3Decls . DeclRefTyp
    ; VAR LParentScopeRef : FM3Scopes . ScopeRefTyp 
    ; VAR LIdentText : TEXT

    ; BEGIN (* VisitDecl *)
        LDeclRef := DeclRefany (* Implied NARROW. *)
      ; LParentScopeRef := FM3Scopes . DeclScopeStackTopRef 
      ; IF LDeclRef # NIL (* Some duplicate decls of DeclNoI also exist? *) 
        THEN (* Dispense with them with error messages. *) 
          LIdentText := FM3Units . TextOfIdAtom ( DeclIdAtom ) 
        ; WHILE LDeclRef # NIL
          DO
            ErrorArr
              ( ARRAY OF REFANY
                  { PosImage ( LDeclRef . DclPos )
                  , " Duplicate declaration of \""
                  , LIdentText
                  , "\", ignored, original at "
                  , PosImage ( Position )
                  , "." 
                  } 
              )
          ; LDeclRef := LDeclRef ^ . DclLink
          END (*WHILE*) 
        END (*IF*)

      (* Now handle the original/only declaration. *) 
      ; LDeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
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
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    ; VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block *)
        WITH WppRdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        DO 
          LDeclNo
            := LookupId
                 ( FM3Scopes . DeclScopeStackTopRef ^ , DeclIdAtom , Position ) 
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          VarArray_Int_Refany . CallbackWithElem 
            ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , VisitDecl )
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
(* FIXME: Put a token for the Id. *) 
        ; RETURN LDeclNo
        END (*WITH*) 
      END (* Block *) 
    END DeclIdR2L

; PROCEDURE IdentRefR2L
    ( IdentRefAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp 

  = VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*IdentRefR2L*)
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    ; WITH WScope = FM3Scopes . LookupScopeStackTopRef ^  
           , WppRdBack
             = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
        IF IntSets . IsElement ( IdentRefAtom , WScope . ScpDeclIdSet )
        THEN (* Decl'd in this scope.  Replace Id with DeclNo. *) 
          LDeclNo := LookupId ( WScope , IdentRefAtom , Position )
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          PutBwdP2 ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )  
        ; RETURN LDeclNo
        ELSE (* Leave as-is, for later resolution. *)
          PutBwdP2 ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( IdentRefAtom , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkIdRefAtom , LONGINT ) )  
        ; RETURN FM3Base . DeclNoNull 
        END (*IF*)
      END (*WITH*) 
    END IdentRefR2L

; PROCEDURE QualIdentR2L
    ( Pass1RdBack : RdBackFile . T ) : FM3Base . DeclNoTyp 

  = VAR LAtomLt , LAtomRt : FM3Base . AtomTyp
  ; VAR LPosLt , LPosRt : tPosition
  ; VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*QualIdentL2R*)
      WITH WppRdBack = FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack 
      DO
        LAtomLt := GetBwdAtom ( Pass1RdBack ) 
      ; LAtomRt := GetBwdAtom ( Pass1RdBack ) 
      ; LPosLt := GetBwdPos ( Pass1RdBack ) 
      ; LPosRt := GetBwdPos ( Pass1RdBack )
      
      ; LDeclNo := LookupBlockRef ( LAtomLt , LPosLt )
      ; IF LDeclNo = FM3Base . DeclNoNull  
        THEN (* Undeclared.  Convert to invalid ref. *)
          PutBwdP2 ( WppRdBack , VAL ( LPosLt . Column , LONGINT ) )
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Line , LONGINT ) )
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkInvalidRef , LONGINT ) )
        ELSE (* Left Id Atom resolved in this scope.  Replace it with DeclNo. *)
          PutBwdP2 ( WppRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LAtomRt , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkQualIdDeclNoAtom , LONGINT ) )  
        ; RETURN LDeclNo
        END (*IF*)
      ; RETURN FM3Base . DeclNoNull 
      END (*WITH*) 
    END QualIdentR2L

(*EXPORTED*) 
; PROCEDURE RunPass2 ( UnitRef : FM3Units . UnitRefTyp ) 

  = BEGIN (*RunPass2*)
      InitPass2 ( UnitRef ) 
    ; TranslatePass2 ( UnitRef ) 
    ; FinishPass2 ( UnitRef ) 
    END RunPass2

(*EXPORTED*) 
; PROCEDURE DisAsmPass2
    ( UnitRef : FM3Units . UnitRefTyp ; DoEarlierPasses : BOOLEAN )

  = BEGIN (*DisAsmPass2*)
      IF NOT FM3Base . PassNo2 IN UnitRef ^ . UntPassNosDisAsmed 
      THEN (* Disassembly file is not already written. *) 
        FM3Compile . DisAsmPassFile ( UnitRef , FM3Globals . Pass2OutSuffix )
      ; FM3Base . InclPassNo
          ( UnitRef ^ . UntPassNosDisAsmed , FM3Base . PassNo2 ) 
      END (*IF*) 
    ; IF DoEarlierPasses
      THEN
(* This is unnecessary, as pass 1 has no earlier pass.  It is here to remind
   to do this in passes later than pass 2.
 
        FM3Pass1 . DisAsmPass1 ( DoEarlierPasses := TRUE )
*) 
      END (*IF*) 
    END DisAsmPass2

; PROCEDURE InitPass2 ( UnitRef : FM3Units . UnitRefTyp )

  = VAR LPass2FileFullName : TEXT
  
  ; BEGIN (*InitPass2*)
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

      (* Finish successful pass 2 output. *) 
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
      ; PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack , VAL ( Itk . ItkEOF , LONGINT ) )
      ; FM3Compile . MakePassFileCopy
          ( UnitRef , FM3Globals . Pass2OutSuffix , UnitRef ^ . UntPass2OutRdBack )
        (*^ This copy may be used by disassembly called for by command-line
            option, a later pass failure, or not at all. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE (* Pass 2 failed. *) 
        PutBwdP2
          ( UnitRef ^ . UntPass2OutRdBack
          , VAL ( Itk . ItkLeftEndIncomplete , LONGINT ) 
          )

      ; FM3Compile . MakePassFileCopy
          ( UnitRef , FM3Globals . Pass2OutSuffix , UnitRef ^ . UntPass2OutRdBack )
        (* ^This copy will be used immediately to disassemble. *)
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
  ; VAR LPass2FullCopyName : TEXT

  ; BEGIN (*FinishPass2*)
    (* Close pass 2. *) 
      UnitRef . UntPass2Result := 0 
    ; EVAL FM3Scanner . PopState ( )
(* TODO^ Maybe do this elsewhere. *) 

    (* Finish with patch stack. *) 
    ; UnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPatchStackRdBack ) 
    ; FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Patch stack "
              , UnitRef ^ . UntPatchStackName
              , " peak size = "
              , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPatchStackDepth )
              , " bytes."
            } 
        ) 
    ; IF NOT RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack )
             <= UnitRef ^ . UntPatchStackEmptyCoord 
      THEN
        UnitRef . UntPass2Result := FM3CLArgs . CcPatchStackNotEmpty  
      ; FM3Messages . FM3LogArr
          ( ARRAY OF REFANY
              { "Patch stack " 
              , UnitRef ^ . UntPatchStackName
              , " final size = "
              , FM3Base . Int64Image
                  ( RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack ) )
              }
          ) 
      ; FM3Messages . FatalArr
          ( ARRAY OF REFANY
              { "Patch stack is not sufficiently empty, should be "  
              , FM3Base . Int64Image ( UnitRef ^ . UntPatchStackEmptyCoord )
              , "." 
              } 
          )
      END (*IF*)
    ; RdBackFile . Close (  UnitRef ^ . UntPatchStackRdBack , 0L )
      (* No point in keeping the patch stack.  It has pogo-sticked and 
         now is empty. *)
    ; LPatchFullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPatchStackName
             , NIL
             )
    ; TRY FS . DeleteFile ( LPatchFullFileName )
      EXCEPT OSError . E => (* It didn't exist. *) 
      END (*EXCEPT*)

    (* Finish with pass 2 output. *) 
    ; FM3LogArr
        ( ARRAY OF REFANY 
            { "Pass 2 output file "
            , UnitRef ^ . UntPass2OutSimpleName 
            , " has " 
            , FM3Base . Int64Image 
                ( RdBackFile . LengthL ( UnitRef ^ . UntPass2OutRdBack ) ) 
            , " bytes."
            } 
        )
    ; PutBwdP2
        ( FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        , VAL ( Itk . ItkLeftEnd , LONGINT ) 
        )
    ; PutBwdP2
        ( FM3Units . UnitStackTopRef ^ . UntPass2OutRdBack
        , VAL ( Itk . ItkEOF , LONGINT ) 
        )

    ; RdBackFile . Close 
        ( UnitRef ^ . UntPass2OutRdBack , - 1L (* Leave full length. *) )
    (* ^When the next pass is implemented, don't do this. *)
    
    ; IF FM3Base . PassNo2 IN FM3Globals . PassNosToDisAsm 
      THEN DisAsmPass2 ( UnitRef , DoEarlierPasses := FALSE )
      END (*IF*) 

    ; <* ASSERT
           IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack )
           = Ranges_Int . RangeTyp { 0 , 0 } 
      *> 
      FM3Globals . SkipNoStack := NIL
    END FinishPass2

; BEGIN (*FM3Pass2*)


END FM3Pass2
.

