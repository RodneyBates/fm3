
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3CTIntArith

; IMPORT Long
; IMPORT Word 

; IMPORT VarArray_Int_Refany 

; IMPORT FM3CLOptions 
; IMPORT FM3CLToks 
; IMPORT FM3Exprs
; IMPORT FM3LoTypes 
; IMPORT FM3Predefs
; IMPORT FM3SrcToks 
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Target

(*EXPORTED*) 
; PROCEDURE FromInt ( IntVal : INTEGER ; Signed : BOOLEAN ) : T

  = BEGIN
      IF Signed THEN RETURN VAL ( IntVal , LONGINT ) END (*IF*)
    ; RETURN Long . And ( 16_00000000FFFFFFFFL , VAL ( IntVal , LONGINT ) ) 
    END FromInt 

; PROCEDURE IsWithinI32 ( Value : T ) : BOOLEAN 

  = VAR LSignBits : LONGINT

  ; BEGIN
      LSignBits := Long . And ( 16_FFFFFFFF80000000L , Value ) 
    ; IF LSignBits = 0L THEN RETURN TRUE
      ELSIF LSignBits = 16_FFFFFFFF80000000L THEN RETURN TRUE
      ELSE RETURN FALSE
      END (*IF*)
    END IsWithinI32

; PROCEDURE IsWithinU32 ( Value : T ) : BOOLEAN 

  = VAR LHiBits : LONGINT
  
  ; BEGIN
      LHiBits := Long . And ( 16_FFFFFFFF00000000L , Value ) 
    ; IF LHiBits = 0L THEN RETURN TRUE
      ELSE RETURN FALSE
      END (*IF*) 
    END IsWithinU32 

(*EXPORTED*) 
; PROCEDURE ToInt ( Value : T ; Signed : BOOLEAN ) : INTEGER
  RAISES { ArithError }

  = BEGIN
      IF Signed
      THEN 
        IF IsWithinI32 ( Value )
        THEN RETURN VAL ( Value , INTEGER )
        ELSE RAISE ArithError ( "ToIntSigned" )
        END (*IF*) 
      ELSE
        IF IsWithinU32 ( Value )
        THEN RETURN VAL ( Value , INTEGER )
        ELSE RAISE ArithError ( "ToIntUnsigned" )
        END (*IF*) 
      END (*IF*) 
    END ToInt 

; TYPE PairTyp = RECORD Lo , Hi : LONGINT END
  (* Lo and Hi are 32-bit values inside LONGINT fields. *)

; PROCEDURE Split ( Value : LONGINT ) : PairTyp
  (* POST: Lo and Hi of result have all zeros in their left 32 bits. *) 

  = VAR LResult : PairTyp

  ; BEGIN
      LResult . Lo := Long . And ( 16_FFFFFFFF00000000L , Value )
    ; LResult . Hi := Long . RightShift ( Value , 32 )
    ; RETURN LResult 
    END Split

; PROCEDURE Join ( READONLY Pair : PairTyp ) : LONGINT

  = BEGIN
      RETURN Long . Or ( Long . LeftShift ( Pair . Hi , 32 ) , Pair . Lo ) 
    END Join

; PROCEDURE CheckOvflo32 ( Value : T ) : T RAISES { ArithError } 

  = VAR LSignBits : LONGINT

  ; BEGIN
      LSignBits := Long . And ( 16_FFFFFFFF80000000L , Value )
    ; IF LSignBits = 16_FFFFFFFF80000000L
      THEN RETURN Value 
      ELSIF LSignBits = 16_0000000000000000L
      THEN RETURN Value
      ELSE RAISE ArithError ( "32-bit overflow" )
      END (*IF*) 
    END CheckOvflo32

; PROCEDURE Plus64WOvflo ( Lt , Rt : LONGINT ) : LONGINT

  = VAR LPairLt : PairTyp 
  ; VAR LPairRt : PairTyp
  ; VAR LLo , LHi , LCarryoutLo , LCarryoutHi : LONGINT
  
  ; BEGIN
      LPairLt := Split ( Lt )
    ; LPairRt := Split ( Rt )
    ; LLo := Long . Plus ( LPairLt . Lo , LPairRt . Lo )
    ; (* ASSERT Long . And ( 16_0000000200000000L , LLo ) = 0L ) *)
      LCarryoutLo (* lsb. *) 
        := Long . RightShift ( Long . And ( 16_0000000100000000L , LLo ), 32 )
    ; LHi := Long . Plus
               ( Long . Plus ( LPairLt . Hi , LPairRt . Hi ) , LCarryoutLo )
      (* ASSERT Long . And ( 16_0000000200000000L , LHi ) = 0L *) 
    ; LCarryoutHi 
        := Long . RightShift ( Long . And ( 16_0000000100000000L , LHi ) , 32 )
    ; IF LCarryoutHi # 0L
      THEN RAISE ArithError ( "Arithmetic overflow" )
      END (*IF*)
    ; RETURN Join ( PairTyp { LLo , LHi } )
    END Plus64WOvflo

; PROCEDURE Plus32WOvflo ( Lt , Rt : LONGINT ) : LONGINT

  = VAR LPairLt : PairTyp 
  ; VAR LPairRt : PairTyp
  ; VAR LLo , LHi , LCarryoutLo , LCarryoutHi : LONGINT
  
  ; BEGIN
(* IMPLEMENTME *) 
      LPairLt := Split ( Lt )
    ; LPairRt := Split ( Rt )
    ; LLo := Long . Plus ( LPairLt . Lo , LPairRt . Lo )
    ; (* ASSERT Long . And ( 16_0000000200000000L , LLo ) = 0L ) *)
      LCarryoutLo (* lsb. *) 
        := Long . RightShift ( Long . And ( 16_0000000100000000L , LLo ), 32 )
    ; LHi := Long . Plus
               ( Long . Plus ( LPairLt . Hi , LPairRt . Hi ) , LCarryoutLo )
      (* ASSERT Long . And ( 16_0000000200000000L , LHi ) = 0L *) 
    ; LCarryoutHi 
        := Long . RightShift ( Long . And ( 16_0000000100000000L , LHi ) , 32 )
    ; IF LCarryoutHi # 0L
      THEN RAISE ArithError ( "Arithmetic overflow" )
      END (*IF*)
    ; RETURN Join ( PairTyp { LLo , LHi } )
    END Plus32WOvflo

; PROCEDURE Add ( Lt , Rt : T ; IsInt : BOOLEAN ) : T

  = BEGIN
      IF IsInt
      THEN RETURN Long . And ( 16_00000000FFFFFFFFL , Lt + Rt )   
      ELSE RETURN Lt + Rt 
      END (*IF*) 
    END Add 

; PROCEDURE BinOp
    ( Lt , Rt : T ; Opcode : FM3Exprs . OpcodeTyp ; IsInt : BOOLEAN ) : T 

  = VAR LPairLt , LPairRt : PairTyp
  ; VAR LCarryuout : LONGINT 

  ; BEGIN
      EnsureInit ( ) 
    ; CASE Opcode OF
      | Stk . StkPdPlus
      => RETURN Add ( Lt , Rt , IsInt ) 

      | FM3SrcToks . StkPlus
      => IF DoCheckOvflo
         THEN
           IF IsInt 
           THEN RETURN Plus32WOvflo ( Lt , Rt ) 
           ELSE RETURN Plus64WOvflo ( Lt , Rt )
         END (*IF*)
         ELSE RETURN Add ( Lt , Rt , IsInt )
         END (*IF*)

      | Stk . StkPdMinus
      => RETURN Long . Minus ( Lt , Rt )

      | Stk . StkPdTimes
      => RETURN Long . Times ( Lt , Rt )

      | Stk . StkPdDivide
      => RETURN Long . Divide ( Lt , Rt )

      | Stk . StkPdMod
      => RETURN Long . Mod ( Lt , Rt )

      ELSE <* ASSERT FALSE
           , "Unimplemented CT Int Opcode: " & Stk . Image ( Opcode ) & "."
           *>
      END (*CASE*)      
    END BinOp

(****************************
        ;  LCarryoutHi
             := ( LAST ( Int32 ) - Lt . CtiHi - Rt . CtiHi - ORD ( LCarryoutLo )
                < 0
        ;  LResult . CtiLo := Lt . CtiLo + Rt . CtiHi
        ;  LResult . CtiHi := Lt . CtiHi + Rt . CtiHi + ORD ( LCarryoutLo ) 
        IF LCarryoutHi
          THEN RAISE ArithError ( "Arithmetic overflow" ) 
          END (*IF*) 

*************************)
(*************************** What is this? 
  = VAR LResult : LONGINT
  ; VAR LPair := PairTyp 

  ; BEGIN  
      LPair := Split ( Lt )
    ; LCarryoutHi
        := ( LAST ( Int32 ) - Lt . CtiHi - Rt . CtiHi - ORD ( LCarryoutLo )
                < 0
        ;  LResult . CtiLo := Lt . CtiLo + Rt . CtiHi
        ;  LResult . CtiHi := Lt . CtiHi + Rt . CtiHi + ORD ( LCarryoutLo ) 
        IF LCarryoutHi
        THEN RAISE ArithError ( "Arithmetic overflow" ) 
        END (*IF*)

        LHiBits := Long . And ( 16_00000000FFFFFFFFL , LongVal )
      ; IF Value ^ . CtiLong < 0L
        THEN
          IF LHiBits # 16_FFFFFFFF00000000
          THEN RAISE ArithError ( )
          ELSE 
            RETURN VAL ( Word . And
                         ( FFFFFFFF00000000L , Value ^ . CtiLong ) , I(NTEGER ) 
     ; ELSE
         IF AND HiBits # 16_00000000000000000L
         THEN RAISE ArithError ( "Constant value exceeds range of INTEGER" )
         ELSE RETURN VAL ( Value ^ . CtiLong , INTEGER )
         END(*IF*)
       END (*IF*) 
      END ToInt

; PROCEDURE LongPlus ( Lt , Rt : T ) : T

  = VAR LPairLt := PairTyp 
  ; VAR LPairRt := PairTyp
  
  = BEGIN
      LPairLt := Split ( Lt )
    ; LPairRt := Split ( Rt )
      => Lo := Long . Plus ( LPairLt . Lo , LPairRt . Lo )
      ;  <* ASSERT Long . And ( 16_0000000200000000L , LLo ) = 0L ) 
         LCarryout
           := Long . RightShift
                Long . And ( 16_0000000100000000L , LLo ), 32 ) 
      ;  LHi := Long . Plus
                  ( Long . Plus ( LPairLt . Hi , LPairRt . Hi ) , LCarryout )
      ;  <* ASSERT Long . And ( 16_0000000200000000L , LHi ) = 0L ) 
         IF FM3CLOptions . CloCTOverflow
         THEN
           LCarryout
             := Long . RightShift
                  Long . And ( 16_0000000100000000L , LHi ), 32 )
         ; IF LCarryout @ 0L
           THEN RAISE ArithError ( "CTPlusOverflow" )
           END (*IF*)
         END (*IF*)
      ; RETURN Join ( PairTyp { Lo , LHi )
    END LongPlus
****************************)


(*EXPORTED:*) 
; PROCEDURE FromCTInt ( IntVal : INTEGER ; Signed : BOOLEAN ) : T

  = BEGIN
    END FromCTInt

 (*EXPORTED:*) 
; PROCEDURE ToCTInt ( Value : T ; Signed : BOOLEAN ) : INTEGER
  RAISES { ArithError } (* If the value won't fit. *) 
  = BEGIN
    END ToCTInt

 (*EXPORTED:*) 
; PROCEDURE FromLong ( LongVal : LONGINT ) : T

  = BEGIN
      RETURN LongVal 
    END FromLong
    
(*EXPORTED:*) 
; PROCEDURE ToLong ( Value : T ) : LONGINT
(* Just in case T ever changes. *)

  = BEGIN
      RETURN Value 
    END ToLong 

 (*EXPORTED:*) 
; PROCEDURE Unary
    ( Opnd : T ; Opcode : FM3SrcToks . TokTyp ; Integer : BOOLEAN ) : T
  RAISES { ArithError } 

  = BEGIN
    END Unary

 (*EXPORTED:*) 
; PROCEDURE Binary
    ( Lt , Rt : T ; Opcode : FM3SrcToks . TokTyp ; Integer : BOOLEAN ): T
  RAISES { ArithError }

  = BEGIN
    END Binary 

; VAR IsInitialized := FALSE 
; VAR DoCheckOvflo : BOOLEAN 
; VAR IntIs32 : BOOLEAN

; PROCEDURE EnsureInit ( )

  = BEGIN
      IF NOT IsInitialized
      THEN
        <* ASSERT FM3CLOptions . IsInitialized *> 
        DoCheckOvflo
          := FM3CLToks . CltOvflo IN FM3CLOptions . OptionTokSet
      ; IntIs32
          := NARROW
               ( VarArray_Int_Refany . Fetch
                   ( FM3LoTypes . LoTypeMap , FM3Target . LoTypeNoInt )
               , FM3LoTypes . LoTypeInfoRefTyp
               )
             . TiNatSize
             = 32
      ; IsInitialized := TRUE
      END (*IF*) 
    END EnsureInit 

; BEGIN
  END FM3CTIntArith
.


