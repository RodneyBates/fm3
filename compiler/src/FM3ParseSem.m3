
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Things needed by the Cocktail-lalr-generated parser. *)

MODULE FM3ParseSem

; IMPORT FM3TokDef 
; IMPORT RdBackFile 
(* ; IMPORT FM3TokSupport 
   ; FROM FM3TokSupport IMPORT TokTup , TokNull , RtToLtPatch
*) 

; TYPE ByteNoTyp = LONGINT 

; CONST TokNull = 0

; PROCEDURE PushTok ( Tok : FM3TokDef . TokTyp ; Arg0 : LONGINT )

  = BEGIN
    END PushTok 

; PROCEDURE PushTokPatch ( Tok : FM3TokDef . TokTyp ; Arg0 , Arg1 : LONGINT )

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
      THEN LtPatchTok := RtToLtPatch ( RtTok )
      END (*IF*)
    ; PushDelim ( RtTok , SubByteNo + 1 )
    ; PushDelim ( LtPatchTok )  
    END List 

; BEGIN
  END FM3ParseSem 
.


