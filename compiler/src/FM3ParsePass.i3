
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3ParsePass

; IMPORT FM3Base  
; IMPORT FM3Units 
; IMPORT FM3Scanner
; IMPORT FM3Scopes 
; IMPORT FM3IntToks AS Itk  

  (* Lalr mandates this type, by name, and its Scan field, Q.V. *) 
; TYPE tParsAttribute
    = RECORD
        Scan : FM3Scanner . tScanAttribute
      ; PaUnnestCoord : LONGINT  
      ; PaLong : LONGINT 
      ; PaConstructNo : INTEGER
      ; PaListItemNo : INTEGER
      ; PaInt : INTEGER
      ; PaByte : [ 0 .. 16_FF ] 
      ; PaBool : BOOLEAN 
      END (* tParsAttribute *)

; CONST ParsAttrNull
    = tParsAttribute
        { Scan := FM3Scanner . ScanAttrNull
        , PaUnnestCoord := FIRST ( LONGINT ) 
        , PaLong := FIRST ( LONGINT )
        , PaConstructNo := FIRST ( INTEGER ) 
        , PaListItemNo := FIRST ( INTEGER )
        , PaInt := FIRST ( INTEGER )
        , PaByte := 16_FF 
        , PaBool := FALSE
        }

; TYPE FormalModeTyp = { FmVALUE , FmVAR , FmREADONLY } 

(* ---------------------------- Unnest stack ------------------------ *)

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

; PROCEDURE Push_T ( T : Itk . TokTyp )

; PROCEDURE Push_TP ( T : Itk . TokTyp ; Position : FM3Scanner . tPosition )

; PROCEDURE Push_TCr ( T : Itk . TokTyp ; C : LONGINT )

; PROCEDURE Push_TCPrp
   ( T : Itk . TokTyp ; C : LONGINT ; Position : FM3Scanner . tPosition )

; PROCEDURE Push_TCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

; PROCEDURE Push_TCIri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

; PROCEDURE Push_TI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

; PROCEDURE Push_TI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

; PROCEDURE Push_TCoCr ( T : Itk . TokTyp ; Ct , Co : LONGINT )

; PROCEDURE Push_TCIoCri
    ( T : Itk . TokTyp ; Ct : LONGINT ; I : INTEGER ; Co : LONGINT )

; PROCEDURE PushEXPORTSMain  ( READONLY Position : FM3Scanner . tPosition )

; PROCEDURE Pop4 ( )

; PROCEDURE Pop8 ( )

; PROCEDURE MakeList
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY ElemsAttr : tParsAttribute 
    )

; PROCEDURE MakeList2
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; PatchCoord : LONGINT
    ; ElemCt : INTEGER 
    )

(* ----------------------------- Parsing actions -------------------------- *)

; PROCEDURE ImportsLt (  )

; PROCEDURE ImportsRt (  )

; PROCEDURE Import
    ( Atom : FM3Base . AtomTyp ; Pos : FM3Base . tPosition ) 

; PROCEDURE FromImport
    ( IntfAtom : FM3Base . AtomTyp
    ; InftPos : FM3Base . tPosition
    ; DeclAtom : FM3Base . AtomTyp
    ; DeclPos : FM3Base . tPosition
    )

; PROCEDURE BeginBlock ( )

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


