
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Patch

; IMPORT OSError 

; IMPORT IntIntVarArray AS VarArray_Int_Int (* Use FM3's naming convention. *)
; IMPORT IntSets
; IMPORT RangeUtils 

; IMPORT FM3IntToks AS Itk
; IMPORT FM3Compress
; IMPORT FM3Globals
; IMPORT FM3Messages
; IMPORT FM3SharedGlobals
; IMPORT FM3StreamUtils
; IMPORT FM3Units
; IMPORT FM3Utils 
; IMPORT RdBackFile 

; PROCEDURE PutBwdPatch ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd for patch stack 
     Catch OSError.E.
  *)

  = BEGIN (*PutBwdPatch*) 
      <* ASSERT RdBack = FM3Globals . PatchRdBack *>
  IF ValueL = 17L
  THEN ValueL := 27L
  END
; 
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

(*EXPORTED*)
; PROCEDURE SkipRt ( SkipNo : INTEGER )
  (* Reading R2L, initial (right) end of a range of to be skipped tokens. *)

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

(*EXPORTED*)
; PROCEDURE SkipLt ( SkipNo : INTEGER )
  (* Reading R2L final (left) end of a range of to be skipped tokens. *)

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

(*EXPORTED*) 
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

(*EXPORTED*) 
; PROCEDURE CopyOperandsInOrder
    ( OpndCt : [ 0 .. 6 ] ; FromRdBack , ToRdBack : RdBackFile . T )
  (* This is tricky.  Pop then push reverses them, but this procedure does
     a compensating reverse, for net same order. 
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*CopyOperandsInOrder*)
      (* Actually copy, with net reversal. *) 
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
    END CopyOperandsInOrder

(*EXPORTED*) 
; PROCEDURE CopyOperandsReverse
    ( OpndCt : [ 0 .. 6 ] ; FromRdBack , ToRdBack : RdBackFile . T )
  (* Copy operands, up to 6. This is could be confusing.  One-at-a-time pop, then
     push temporally reverses them left-to-right.  But if the caller is copying
     to a RdBack that is to be read in the opposite direction, that will
     implicitly reverse them, for net same temporal order when the next pass
     reads them. 
  *)
  
  = BEGIN
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
    END CopyOperandsReverse

(*Temporary, for debugging.*)
; PROCEDURE xxxtest ( Coord : LONGINT )

  = BEGIN (*xxxtest*)
      IF Coord = 25L
      THEN
        <* ASSERT Coord = 25L *>
      END 
    END xxxtest
      

(*EXPORTED*) 
; PROCEDURE GetTokCode
    ( LMPass1Coord : LONGINT ; VAR (*OUT*) Result : TokResultTyp )
  (* Return a token code and the RdBack it came from.  Callers will
     get its varying complement of arguments, from the returned RdBack.
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

; LDebug : INTEGER 
; PROCEDURE Catch ()
  = BEGIN IF LPatchStackTopCoord  = 17L
          THEN LDebug := 23
          END 
          END Catch

  ; BEGIN (* GetTokCode *) 
      LUnitRef := FM3Units . UnitStackTopRef
    (* ItkSkip[Lt|Rt] pairs are inserted during pass 1 and acted-on in pass 2.*)
    ; LPass1RdBack := LUnitRef ^ . UntPass1OutRdBack 
    ; LPatchRdBack := LUnitRef ^ . UntPatchStackRdBack 
    ; LMPass1Coord := MAX ( LMPass1Coord , LUnitRef ^ . UntPass1OutEmptyCoord )

    ; LOOP (* Thru' a sequence of SkipRt & SkipLt tokens plus one other. *)  
        LPass1Coord := RdBackFile . LengthL ( LPass1RdBack )
      ; LPatchStackTopCoord := FM3Compress . GetBwd ( LPatchRdBack )
; Catch ( ) 
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
          (* Finish with the skip stack. *) 
        ; <* ASSERT
               VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi 
               = FM3Units . UnitStackTopRef ^ . UntSkipStackBase 
          *> 
               
          Result . TrRdBack := NIL
        ; Result . TrTok := Itk . ItkBOF
        ; RETURN 
        END (*IF*)
        
      (* More to be done.  One of three possible actions. *)
      (* Check first for an already patched token on top of the patch stack. *)

      ; IF LPass1Coord = LPatchStackTopCoord
        THEN (* Give caller the token off the patch stack. *)
          LPatchedTokenL := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchedToken := VAL ( LPatchedTokenL , INTEGER (*Itk . TokTyp*) )
        ; IF LPatchedToken = Itk . ItkSkipLt
          THEN (* ItkSkipLt will come only from the patch stack.  Handle it
                  here, so multiple token handlers don't have to.
               *)
            SkipLt ( FM3StreamUtils . GetBwdInt ( FM3Globals . PatchRdBack ) )
            (* And loop. *)
          ELSIF (* Inside a skip range? *) 
            VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
          THEN (* Skip it and loop. *) 
            DiscardOperands
              ( FM3Utils . TokenOpndCt ( LPatchedToken ) , LPatchRdBack ) 
          ELSE (* Deliver a patched token. *) 
            (* Operands are still on the patch stack. *) 
            Result . TrRdBack := LPatchRdBack
          ; Result . TrTok := LPatchedToken
          ; RETURN 
          END (*IF*) 

        ELSIF LPass1Coord < LPatchStackTopCoord
        THEN <* ASSERT FALSE , "Missed a patch stack token."*>
        ELSE (* Look at the top token on the input file. *) 
          PutBwdPatch ( LPatchRdBack , LPatchStackTopCoord )
            (* ^Push the current patch coordinate back on patch stack. *)
        ; LTokenL := FM3Compress . GetBwd ( LPass1RdBack )
        ; LToken := VAL ( LTokenL , Itk . TokTyp )
        ; IF IntSets . IsElement ( LToken , FM3SharedGlobals . GTokSetPatch )
          THEN

          (* Move this token from the input file to the patch stack. *)
          (* Also change the token code to its patched counterpart. *) 
          (* Reading RtoL toward BOF.  Writing RtoL towards EOF/TOS. *) 
            LPatchStackTopCoord := FM3Compress . GetBwd ( LPass1RdBack ) 
; Catch ( ) 
          ; CopyOperandsInOrder
              ( FM3Utils . TokenOpndCt ( LToken )
                (* ^This number does not include the patch-at coordinate. *) 
              , LPass1RdBack
              , LPatchRdBack
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
            SkipRt ( FM3StreamUtils . GetBwdInt ( LPass1RdBack) ) 
          (* and loop. *)
          ELSIF (* Else it's a direct token from LPass1RdBack. *)  
            VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
          THEN (* Skip it and loop for another. *)
            DiscardOperands
              ( FM3Utils . TokenOpndCt ( LToken ) , LPass1RdBack ) 
          ELSE (* Return it directly. *)
            Result . TrRdBack := LPass1RdBack
          ; Result . TrTok := LToken
          ; RETURN 
          END (*IF*) 
        END (*IF*)
      END (*LOOP*)
    END GetTokCode
    
; BEGIN (*FM3Patch*)

END FM3Patch
.

