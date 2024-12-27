
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3CTIntArith

; IMPORT Fmt  
; IMPORT Long

; IMPORT VarArray_Int_Refany 

; IMPORT FM3CLOptions 
; IMPORT FM3CLToks 
; IMPORT FM3Exprs
; IMPORT FM3LoTypes 
; IMPORT FM3SrcToks 
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Target

; CONST Lo32Mask = 16_00000000FFFFFFFFL
; CONST Hi32Mask = 16_FFFFFFFF00000000L

; PROCEDURE RaiseOnOvfloU32 ( Value : T ) : T RAISES { ArithError } 
  (* Treat as unsigned. *) 

  = VAR LHiBits : LONGINT
  
  ; BEGIN
      LHiBits := Long . And ( Hi32Mask , Value ) 
    ; IF LHiBits = 0L THEN RETURN Value 
      ELSE RAISE ArithError ( "32-bit unsigned overflow" )
      END (*IF*) 
    END RaiseOnOvfloU32

; PROCEDURE RaiseOnOvfloI32 ( Value : T ) : T RAISES { ArithError }
  (* Treat as signed. *) 

  = VAR LSignBits : LONGINT

  ; BEGIN
      LSignBits := Long . And ( 16_FFFFFFFF80000000L , Value )
    ; IF LSignBits = 16_FFFFFFFF80000000L
      THEN RETURN Value 
      ELSIF LSignBits = 16_0000000000000000L
      THEN RETURN Value
      ELSE RAISE ArithError ( "32-bit signed overflow" )
      END (*IF*) 
    END RaiseOnOvfloI32

; TYPE PairTyp = RECORD Lo , Hi : LONGINT END
  (* Lo and Hi are 32-bit values inside LONGINT fields. *)

; PROCEDURE Split ( Value : LONGINT ) : PairTyp
  (* POST: Lo and Hi of result have all zeros in their left 32 bits. *) 

  = VAR LResult : PairTyp

  ; BEGIN
      LResult . Lo := Long . And ( Lo32Mask , Value )
    ; LResult . Hi := Long . RightShift ( Value , 32 )
    ; RETURN LResult 
    END Split

; PROCEDURE Join ( READONLY Pair : PairTyp ) : LONGINT

  = BEGIN
      RETURN Long . Or ( Long . LeftShift ( Pair . Hi , 32 ) , Pair . Lo ) 
    END Join

; PROCEDURE Plus64WOvflo
    ( Lt , Rt , CarryIn : LONGINT ) : LONGINT RAISES { ArithError }

  = VAR LPairLt : PairTyp 
  ; VAR LPairRt : PairTyp
  ; VAR LLo , LHi , LCarryoutLo  : LONGINT
  
  ; BEGIN
      LPairLt := Split ( Lt )
    ; LPairRt := Split ( Rt )
    ; LLo := Long . Plus
               ( Long .Plus ( LPairLt . Lo , LPairRt . Lo )
               , CarryIn
               ) 
    ; (* ASSERT NOT IsOvFlo32 ( LLo ) *) 
      LCarryoutLo (* lsb. *) 
        := Long . RightShift ( Long . And ( 16_0000000100000000L , LLo ), 32 )
    ; LHi := Long . Plus
               ( Long . Plus ( LPairLt . Hi , LPairRt . Hi )
               , LCarryoutLo
               )
    ; EVAL RaiseOnOvfloI32 ( LHi ) 
    ; RETURN Join ( PairTyp { LLo , LHi } )
    END Plus64WOvflo

; PROCEDURE Add ( Lt , Rt : T ; IsInt : BOOLEAN ) : T
  (* No overflow detection. *) 

  = BEGIN
      IF IsInt AND IntIs32 
      THEN RETURN Long . And ( Lo32Mask , Lt + Rt )   
      ELSE RETURN Lt + Rt 
      END (*IF*) 
    END Add 

; PROCEDURE Subtract ( Lt , Rt : T ; Is32 : BOOLEAN ) : T
  (* No overflow detection. *) 

  = BEGIN
      IF Is32 AND IntIs32 
      THEN RETURN Long . And ( Lo32Mask , Lt - Rt )   
      ELSE RETURN Lt - Rt 
      END (*IF*) 
    END Subtract

; PROCEDURE LeftShift ( Arg , Ct : LONGINT ) : LONGINT 

   = VAR LCtI : INTEGER 

   ; BEGIN
       LCtI := VAL ( Ct , INTEGER ) 
     ; IF LCtI >= IntBitsizeI THEN RETURN 0L END (*IF*)
     ; RETURN Long . And ( IntMask , Long . LeftShift ( Arg , LCtI ) )  
     END LeftShift

; PROCEDURE RightShift ( Arg , Ct : LONGINT ) : LONGINT 

   = VAR LCtI : INTEGER 

   ; BEGIN
       LCtI := VAL ( Ct , INTEGER ) 
     ; IF LCtI >= IntBitsizeI THEN RETURN 0L END (*IF*)
     ; RETURN Long . RightShift ( Long . And ( IntMask , Arg ) , LCtI )   
     END RightShift

; PROCEDURE LeftRotate32 ( Arg , Ct : LONGINT ) : LONGINT 

   = VAR LLo , LDouble , LShifted : LONGINT
   ; VAR LCtI : INTEGER 

   ; BEGIN
       LCtI := VAL ( Ct MOD 32L , INTEGER )
     ; LLo := Long . And ( Lo32Mask , Arg  )
     ; LDouble := Long . Or ( Long . LeftShift ( LLo , 32 ) , LLo )
     ; LShifted := Long . LeftShift ( LDouble , LCtI )
     ; RETURN Long . RightShift ( LShifted , 32 )
     END LeftRotate32 
  
; PROCEDURE RightRotate32 ( Arg , Ct : LONGINT ) : LONGINT 

   = VAR LLo , LDouble , LShifted : LONGINT
   ; VAR LCtI : INTEGER 

   ; BEGIN
       LCtI := VAL ( Ct MOD 32L , INTEGER )
     ; LLo := Long . And ( Lo32Mask , Arg  )
     ; LDouble := Long . Or ( Long . LeftShift ( LLo , 32 ) , LLo )
     ; LShifted := Long . RightShift ( LDouble , 32 )
     ; RETURN Long . And ( Lo32Mask , LShifted )
     END RightRotate32 
  
(*EXPORTED:*) 
; PROCEDURE FromCTInt ( IntVal : INTEGER ; Signed : BOOLEAN ) : T

  = BEGIN
      IF Signed
      THEN RETURN VAL ( IntVal , LONGINT ) (* Should sign extend. *) 
      ELSE RETURN Long . And ( IntMask , VAL ( IntVal , LONGINT ) ) 
      END (*IF*) 
    END FromCTInt

(*EXPORTED:*) 
; PROCEDURE ToCTInt ( Value : T ; Signed : BOOLEAN ) : INTEGER
  RAISES { ArithError } (* If the value won't fit. *) 
  = BEGIN
      IF Signed
      THEN EVAL RaiseOnOvfloI32 ( Value )
      ELSE EVAL RaiseOnOvfloU32 ( Value )
      END (*IF*) 
    ; RETURN VAL ( Long . And ( Lo32Mask , Value ) , INTEGER ) 
    END ToCTInt

(*EXPORTED:*) 
; PROCEDURE UnOp
    ( Arg : T ; Opcode : FM3Exprs . OpcodeTyp ; IsInt : BOOLEAN ) : T

  = VAR LResult : LONGINT

  ; BEGIN
      EnsureInit ( ) 
    ; CASE Opcode OF
      | Stk . RidABS
      => RETURN ABS ( Arg )  

      | Stk . StkPdNot
      => LResult := Long . Not ( Arg ) 
      ;  IF IsInt AND IntIs32
         THEN LResult := Long . And ( 0L , LResult ) 
         END (*IF*)
      ;  RETURN LResult 

      ELSE <* ASSERT FALSE
           , "Unimplemented CT Int Opcode: " & Stk . Image ( Opcode ) & "."
           *>
      END (*CASE*)      
    END UnOp

(*EXPORTED:*) 
; PROCEDURE BinOp
    ( Lt , Rt : T ; Opcode : FM3Exprs . OpcodeTyp ; IsInt : BOOLEAN ) : T
  RAISES { ArithError }

  = VAR LResult : LONGINT

  ; BEGIN
      EnsureInit ( ) 
    ; CASE Opcode OF

      (* Full-fledged arithmetic: *)
      
      | Stk . StkPdPlus
      => RETURN Add ( Lt , Rt , IsInt ) 

      | FM3SrcToks . StkPlus
      => IF DoCheckOvflo
         THEN
           IF IsInt AND IntIs32  
           THEN RETURN RaiseOnOvfloI32 ( Long . Plus ( Lt , Rt ) )  
           ELSE RETURN Plus64WOvflo ( Lt , Rt , CarryIn := 0L )
         END (*IF*)
         ELSE RETURN Add ( Lt , Rt , IsInt )
         END (*IF*)

      | Stk . StkPdMinus
      => RETURN Subtract ( Lt , Rt , IsInt )

      | FM3SrcToks . StkMinus
      => IF DoCheckOvflo
         THEN
           IF IsInt AND IntIs32  
           THEN RETURN RaiseOnOvfloI32 ( Long . Minus ( Lt , Rt ) )  
           ELSIF Rt = 16_FFFFFFFFFFFFFFFFL
           THEN RETURN Plus64WOvflo ( Lt , Rt , CarryIn := 1L )
           ELSE RETURN Plus64WOvflo ( Lt , - Rt , CarryIn := 0L )
           END (*IF*)
         ELSE RETURN Subtract ( Lt , Rt , IsInt )
         END (*IF*)

      | Stk . StkPdTimes
      => RETURN Long . Times ( Lt , Rt )

      | Stk . StkPdDivide
      => RETURN Long . Divide ( Lt , Rt )

      | Stk . StkPdMod
      => RETURN Long . Mod ( Lt , Rt )

      | Stk . RidMIN 
      => RETURN MIN ( Lt , Rt )  

      | Stk . RidMAX 
      => RETURN MAX ( Lt , Rt )  

      (* Relations: *) 

      | Stk . StkEqual
      => RETURN VAL ( ORD ( Lt = Rt ) , LONGINT )  

      | Stk . StkUnequal
      => RETURN VAL ( ORD (  Lt # Rt ) , LONGINT ) 

      | Stk .  StkLess
      => RETURN VAL ( ORD ( Lt < Rt ) , LONGINT ) 

      | Stk . StkGreater 
      => RETURN VAL ( ORD ( Lt > Rt ) , LONGINT ) 

      | Stk . StkLessEqual 
      => RETURN VAL ( ORD ( Lt <= Rt ) , LONGINT ) 

      | Stk . StkGreaterEqual 
      => RETURN VAL ( ORD ( Lt >= Rt ) , LONGINT ) 

      | Stk . StkPdLT 
      => RETURN VAL ( ORD ( Long . LT ( Lt , Rt ) ) , LONGINT ) 

      | Stk . StkPdGT 
      => RETURN VAL ( ORD ( Long . GE ( Lt , Rt ) ) , LONGINT ) 

      | Stk . StkPdLE
      => RETURN VAL ( ORD ( Long . GE ( Lt , Rt ) ) , LONGINT ) 

      | Stk . StkPdGE
      => RETURN VAL ( ORD ( Long . GE ( Lt , Rt ) ) , LONGINT ) 

      (* Bitwise booleans: *)

      | Stk . StkPdAnd
      => LResult := Long . And ( Lt , Rt ) 
      ;  IF IsInt AND IntIs32
         THEN LResult := Long . And ( 0L , LResult ) 
         END (*IF*)
      ;  RETURN LResult 

      | Stk . StkPdOr
      => LResult := Long . Or ( Lt , Rt ) 
      ;  IF IsInt AND IntIs32
         THEN LResult := Long . And ( 0L , LResult ) 
         END (*IF*)
      ;  RETURN LResult 

      | Stk . StkPdXor
      => LResult := Long . Xor ( Lt , Rt ) 
      ;  IF IsInt AND IntIs32
         THEN LResult := Long . And ( 0L , LResult ) 
         END (*IF*)
      ;  RETURN LResult

      (* Shifts: *)

      | Stk . StkPdLeftShift
      => RETURN LeftShift ( Lt , Rt ) 

      | Stk . StkPdRightShift
      => RETURN RightShift ( Lt , Rt ) 

      | Stk . StkPdShift
      =>  IF Rt > 0L
          THEN RETURN LeftShift ( Lt , Rt ) 
          ELSIF Rt < 0L
          THEN RETURN RightShift ( Lt , Rt )
          ELSE RETURN Lt
          END (*IF*) 

      | Stk . StkPdRotate
      =>  IF IntIs32
          THEN
            IF Rt > 0L
            THEN RETURN LeftRotate32 ( Lt , Rt ) 
            ELSIF Rt < 0L
            THEN RETURN RightRotate32 ( Lt , Rt )
            ELSE RETURN Lt
            END (*IF*)
          ELSE RETURN Long . Rotate ( Lt , VAL ( Rt , INTEGER ) )
          END (*IF*) 

      ELSE <* ASSERT FALSE
           , "Unimplemented CT Int Opcode: " & Stk . Image ( Opcode ) & "."
           *>
      END (*CASE*)      
    END BinOp

(*EXPORTED:*) 
; PROCEDURE Extract
    ( x , i , n : T ) : T
  RAISES { ArithError }

  = VAR LiI , LnI : INTEGER

  ; BEGIN
      LiI := VAL ( i , INTEGER )
    ; LnI := VAL ( n , INTEGER )  
    ; EnsureInit ( )
    ; IF LnI + LiI > IntBitsizeI
      THEN
        RAISE ArithError
          ( "Extract beyond " & Fmt . Int ( IntBitsizeI ) & " bits." )
      ELSE RETURN Long . Extract ( x , LiI , LnI )
      END (*IF*) 
    END Extract

(*EXPORTED:*) 
; PROCEDURE Insert
    ( x , y, i , n : T ) : T 
  RAISES { ArithError }

  = VAR LiI , LnI : INTEGER

  ; BEGIN
      LiI := VAL ( i , INTEGER )
    ; LnI := VAL ( n , INTEGER )  
    ; EnsureInit ( )
    ; IF LnI + LiI > IntBitsizeI
      THEN
        RAISE ArithError
          ( "Insert beyond " & Fmt . Int ( IntBitsizeI ) & " bits." )
      ELSE RETURN Long . Insert ( x , y , LiI , LnI )
      END (*IF*) 
    END Insert

; VAR IntMask : LONGINT 
; VAR IntBitsizeI : INTEGER 
; VAR IntIs32 : BOOLEAN
; VAR DoCheckOvflo : BOOLEAN 
; VAR IsInitialized := FALSE 

; PROCEDURE EnsureInit ( )

  = BEGIN
      IF NOT IsInitialized
      THEN
        <* ASSERT FM3CLOptions . IsInitialized *> 
        DoCheckOvflo
          := FM3CLToks . CltOvflo IN FM3CLOptions . OptionTokSet
      ; IntBitsizeI
          := NARROW
               ( VarArray_Int_Refany . Fetch
                   ( FM3LoTypes . LoTypeMap , FM3Target . LoTypeNoInt )
               , FM3LoTypes . LoTypeInfoRefTyp
               )
             . TiNatSize
      ; IF IntBitsizeI = 32
        THEN 
          IntIs32 := TRUE 
        ; IntMask := Lo32Mask
        ELSE
          IntIs32 := FALSE
        ; IntMask := 16_FFFFFFFFFFFFFFFFL
        END (*IF*) 
      ; IsInitialized := TRUE
      END (*IF*) 
    END EnsureInit 

; BEGIN
    IsInitialized := FALSE 
  END FM3CTIntArith
.


