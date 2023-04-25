
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Things needed by the Cocktail-lalr-generated parser. *)

MODULE ParsSem


; IMPORT RdBAckFile 
; IMPORT FMTTokSupport 
; FROM FMTTokSupport IMPORT TokTup , TokNull , RtToLtPatch

; TYPE ByteNoTyp = RdBackFile . ByteNoTyp 

; PROCEDURE List
    ( SubByteNo : ByteNoTyp ; RtTok : TokTyp ; LtPatchTok : TokTyp := TokNull )

  = VAR LSubDelimNo : RdBackFile . ByteNoTyp

  ; BEGIN
      IF LtPatchTok = TokNull
      THEN LtPatchTok := RtToLtPatch ( RtTok )
      END (*IF*)
    ; PushDelim ( RtTok , SubByteNo + 1 )
    ; PuchDelim ( LtPatchTok 
    END List 

; BEGIN
  END ParsSem 
.


