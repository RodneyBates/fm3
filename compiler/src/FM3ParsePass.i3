
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3ParsePass

; IMPORT FM3Units 
; IMPORT FM3Scanner
; IMPORT FM3IntToks 
; FROM FM3IntToks IMPORT TokTyp  

  (* Lalr mandates this type, by name, and its Scan field, Q.V. *) 
; TYPE tParsAttribute
    = RECORD
        Scan : FM3Scanner . tScanAttribute
      ; PaLong : LONGINT 
      ; PaConstructNo : INTEGER
      ; PaListItemNo : INTEGER
      ; PaInt : INTEGER
      ; PaBool : BOOLEAN 
      END (* tParsAttribute *)

; CONST ParsAttrNull
    = tParsAttribute
        { Scan := FM3Scanner . ScanAttrNull
        , PaUnnestCoord := FIRST ( LONGINT ) 
        , PaLong := := FIRST ( LONGINT )
        , PaConstructNo := FIRST ( INTEGER ) 
        , PaListItemNo := := FIRST ( INTEGER )
        , PaInt := := FIRST ( INTEGER )
        , PaBool := FALSE
        }

; TYPE FormalMode = { FmVAL , FmVAR , FmREADONLY } 

; PROCEDURE UnnestCoord ( ) : LONGINT
  (* Of the current unit. *)
  
; PROCEDURE PushUnnestStk ( READONLY ParsAttr : tParsAttribute )
  (* Source token.  Some of these (in fact, probably the only ones that
     will be passed in) have arguments, depending on the value of Token.
     Used for source code variable terminals.
  *) 
  
; PROCEDURE PushUnnest ( Value : INTEGER )
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  (* Zero args. *) 

; PROCEDURE MakeList
    ( VAR LHSAttr : tParsAttribute
    ; READONLY ElemsAttr : tParsAttribute 
    ; TokLt : Itk . TokTyp
    )

(*
; PROCEDURE PushUnnestTokPatch0 ( Token : LONGINT )
  (* To be patched.  No additional args. *) 

; PROCEDURE PushUnnestTok1 ( Token : LONGINT )
  (* One arg. *) 

; PROCEDURE PushUnnestTokPatch1 ( Token : LONGINT )
  (* To be patched.  One additional arg. *) 

; PROCEDURE PushTok ( Tok : TokTyp ; Arg0 : LONGINT )

; PROCEDURE PushTokPatch ( Tok : TokTyp ; Arg0 , Arg1 : LONGINT )

*)

; PROCEDURE MakeBracket
    ( VAR LHSAttr : tParsAttribute
    ; PatchCoord : LONGINT
    ; TokLt : FM3IntToks . TokTyp
    ; ElemNo : INTEGER 
    )

; PROCEDURE Run ( ) 

; END FM3ParsePass
.


