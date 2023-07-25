INTERFACE FM3ParsePass

; IMPORT FM3Units 
; IMPORT FM3Scanner
; IMPORT FM3IntToks 
; FROM FM3IntToks IMPORT TokTyp  

; TYPE tParsAttribute (* Lalr expects this and field Scan to be so named. *) 
    = RECORD
        Scan : FM3Scanner . tScanAttribute
      ; PaUnnestStackLen : LONGINT 
      ; PaLong : LONGINT 
      ; PaConstructPtr : INTEGER
      ; PaListItemNo : INTEGER
      ; PaInt : INTEGER
      ; PaBool : BOOLEAN 
      END (* tParsAttribute *)

; CONST ParsAttrNull
    = tParsAttribute
        { Scan := FM3Scanner . ScanAttrNull
        , PaUnnestStackLen := 0L 
        , PaLong := 0L 
        , PaConstructPtr := 0
        , PaListItemNo := 0
        , PaInt := 0
        , PaBool := FALSE
        }

; PROCEDURE UnnestStackLen ( ) : LONGINT
  (* Of the current unit. *)
  
; PROCEDURE PushUnnestStk ( Token : INTEGER )
  (* Source token.  Some of these (in fact, probably the only ones that
     will be passed in) have arguments, depending on the value of Token.
     Used for source code variable terminals.
  *) 
  
; PROCEDURE PushUnnest ( Value : INTEGER )
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  (* Zero args. *) 


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

; PROCEDURE Run ( ) 

; END FM3ParsePass
.


