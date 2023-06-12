
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Things needed by the Cocktail-lalr-generated parser. *)

MODULE FM3ParseSem

; IMPORT FM3IntToks 
; FROM FM3IntToks IMPORT TokTyp 
; FROM FM3IntToks IMPORT LtToTemp , LtToPatch , LtToRt
; FROM FM3IntToks IMPORT RtToTemp , RtToPatch , RtToLt
; IMPORT RdBackFile 
(* ; IMPORT FM3TokSupport 
   ; FROM FM3TokSupport IMPORT TokTup , TokNull , RtToLtPatch
*) 

; TYPE ByteNoTyp = LONGINT 

; CONST TokNull = 0

; PROCEDURE PushTok ( Tok : TokTyp ; Arg0 : LONGINT )

  = BEGIN
    END PushTok 

; PROCEDURE PushTokPatch ( Tok : TokTyp ; Arg0 , Arg1 : LONGINT )

  = BEGIN
    END PushTokPatch 

; PROCEDURE PushDelim ( Tok : TokTyp ; TokArg : ByteNoTyp )

  = BEGIN
    END PushDelim 

; PROCEDURE List
    ( SubByteNo : ByteNoTyp ; RtTok : TokTyp ; LtPatchTok : TokTyp := TokNull )

  = VAR LSubDelimNo : ByteNoTyp

  ; BEGIN
      IF LtPatchTok = TokNull
      THEN LtPatchTok := RtTok + RtToPatch 
      END (*IF*)
    ; PushDelim ( RtTok , SubByteNo + 1L )
    ; PushDelim ( LtPatchTok , SubByteNo + 1L )  
    END List 

; BEGIN
  END FM3ParseSem 
.


