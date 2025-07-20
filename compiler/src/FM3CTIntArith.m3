
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
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

(* There is a 2x2x2 cartesian product of versions of arithmetic operations.
   1) Signed or unsigned. Ascertained by the opcode,
   2) 64 or 32-bit arithmetic. Ascertained by combination of parameter IsM3Int, which  
      specifies M3 INTEGER or M3 LONGINT, and global setting on the size of M3 INTEGER.
   3) Overflow check or not. Ascertained by the opcode and global setting,
   
   Detected ovflows are signalled by raising ArithOvflo.  Caller can choose
   what to do about it.

   Results of 32-bit operations always have the hi 32 bits sign-extended,
   but may be treated as signed or unsigned by subsequent operations.

   This module assumes it is compiled by a workng Modula-3 compiler with
   LONGINT and standard interface 'Long' supported for bootstrapping.
*) 

; PROCEDURE IsOvfloI32 ( Value : T ) : BOOLEAN 
  (* Treat as signed. *) 

  = VAR LSignBits : LONGINT

  ; BEGIN
      LSignBits := Long . And ( 16_FFFFFFFF80000000L , Value )
    ; IF LSignBits = 16_FFFFFFFF80000000L
      THEN RETURN FALSE
      ELSIF LSignBits = 16_0000000000000000L
      THEN RETURN FALSE 
      ELSE RETURN TRUE 
      END (*IF*) 
    END IsOvfloI32

; PROCEDURE RaiseOnOvfloU32 ( Value : T ) : T RAISES { ArithError } 
  (* Treat as unsigned. *) 

  = VAR LHiBits : LONGINT
  
  ; BEGIN
      EnsureInit ( )
    ; IF DoCheckOvflo
      THEN 
        LHiBits := Long . And ( Hi32Mask , Value ) 
      ; IF LHiBits = 0L THEN RETURN Value 
        ELSE RAISE ArithError ( "32-bit unsigned overflow" )
        END (*IF*) 
       END (*IF*) 
    END RaiseOnOvfloU32

; PROCEDURE RaiseOnOvfloI32 ( Value : T ) : T RAISES { ArithError }
  (* Treat as signed. *) 

  = VAR LSignBits : LONGINT

  ; BEGIN
      EnsureInit ( )
    ; IF DoCheckOvflo
      THEN 
        LSignBits := Long . And ( 16_FFFFFFFF80000000L , Value )
      ; IF LSignBits = 16_FFFFFFFF80000000L
        THEN RETURN Value 
        ELSIF LSignBits = 16_0000000000000000L
        THEN RETURN Value
        ELSE RAISE ArithError ( "32-bit signed overflow" )
        END (*IF*) 
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
      RETURN Long . Or
               ( Long . LeftShift ( Pair . Hi , 32 )
               , Long . And ( Lo32Mask , Pair . Lo ) 
               ) 
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
    ; (* ASSERT NOT IsOvFloI32 ( LLo ) *) 
      LCarryoutLo (* lsb. *) 
        := Long . RightShift ( Long . And ( 16_0000000100000000L , LLo ), 32 )
    ; LHi := Long . Plus
               ( Long . Plus ( LPairLt . Hi , LPairRt . Hi )
               , LCarryoutLo
               )
    ; EVAL RaiseOnOvfloI32 ( LHi ) 
    ; RETURN Join ( PairTyp { LLo , LHi } )
    END Plus64WOvflo

; PROCEDURE Times64WOvflo
    ( Lt , Rt : LONGINT ) : LONGINT RAISES { ArithError }

  = VAR LPairLt : PairTyp 
  ; VAR LPairRt : PairTyp
  ; VAR LSum : LONGINT 
  ; VAR LHiTimesLo , LLoTimesHi , LLoTimesLo : LONGINT 
  ; VAR LLtNeg , LRtNeg : BOOLEAN 

  ; BEGIN
      LLtNeg := Lt < 0L 
    ; IF LLtNeg THEN Lt := - Lt END (*IF*) 
      (* Works for 16_8000000000000000L because we treat result as unsigned. *) 
    ; LRtNeg := Rt < 0L
    ; IF LRtNeg THEN Rt := - Rt END (*IF*) 
    ; LPairLt := Split ( Lt )
    ; LPairRt := Split ( Rt )
    ; IF LPairLt . Hi > 0L AND LPairRt . Hi > 0L
      THEN (* The Hi*Hi Term, > 0L, after left shift by 64, would be >= 2^64 *)
        RAISE ArithError ( "Overflow in 64-bit multiply" )
      END (*IF*)
    ; LHiTimesLo := Long . Times ( LPairLt . Hi , LPairRt . Lo ) 
    ; IF Long . And ( Hi32Mask , LHiTimesLo ) # 0L 
      THEN (* LHiTimesLo, after left shift by 32, would be >= 2^64 *)
        RAISE ArithError ( "Overflow in 64-bit multiply" )
      END (*IF*) 
    ; LLoTimesHi := Long . Times ( LPairLt . Lo , LPairRt . Hi )
    ; IF Long . And ( Hi32Mask , LLoTimesHi ) # 0L 
      THEN (* LLoTimesHi, after left shift by 32, would be >= 2^64 *)
        RAISE ArithError ( "Overflow in 64-bit multiply" )
      END (*IF*)
    ; LSum := Long . Plus ( LHiTimesLo , LLoTimesHi ) 
    ; LLoTimesLo := Long . Times ( LPairLt . Lo , LPairRt . Lo )
    ; LSum := Long . Plus ( LSum , LLoTimesLo )
    ; LSum := Long . LeftShift ( LSum , 32 ) 
    ; IF Long . GT ( LSum , 16_8000000000000000L )
      (* If LSum = 16_8000000000000000L, negating it below will give
         the maximally negative value.
      *) 
      THEN (* LSum > 2^63 *)
        RAISE ArithError ( "Overflow in 64-bit multiply" )
      END (*IF*)
    ; IF LLtNeg # LRtNeg THEN LSum := - LSum END (*IF*)
    ; RETURN LSum 
    END Times64WOvflo 

; PROCEDURE Times32WOvflo
    ( Lt , Rt : LONGINT ) : LONGINT RAISES { ArithError }

  = VAR LProduct : LONGINT 
  ; VAR LtNeg , RtNeg : BOOLEAN 

  ; BEGIN
      Lt := Long . And ( Lo32Mask , Lt ) 
    ; LtNeg := Lt < 0L 
    ; IF LtNeg THEN Lt := - Lt END (*IF*)  
    ; Rt := Long . And ( Lo32Mask , Rt ) 
    ; RtNeg := Rt < 0L 
    ; IF RtNeg THEN Rt := - Rt END (*IF*)
    ; LProduct := RaiseOnOvfloU32 ( Long . Times ( Lt , Rt ) )  
    ; IF LtNeg # RtNeg THEN LProduct := - LProduct END (*IF*)
    ; RETURN LProduct 
    END Times32WOvflo

; TYPE TxT_TProcTyp = PROCEDURE ( Lt , Rt : LONGINT ) : LONGINT  

; PROCEDURE BinTxT_T
    ( Lt , Rt : LONGINT ; OpProc : TxT_TProcTyp ; IsM3Int : BOOLEAN ) : T  

  = VAR LLt , LRt , LResult : LONGINT

  ; BEGIN
      EnsureInit ( )
    ; IF IsM3Int AND RTIntIs32
      THEN
        LLt := Long . And ( Lo32Mask , Lt ) 
      ; LRt := Long . And ( Lo32Mask , Rt )
      ; LResult := OpProc ( LLt , LRt )
      ; RETURN Long . And ( Lo32Mask , LResult ) 
      ELSE RETURN OpProc ( Lt , Rt ) 
      END (*IF*) 
    END BinTxT_T 

; TYPE TxT_BoolProcTyp = PROCEDURE ( Lt , Rt : LONGINT ) : BOOLEAN 

; PROCEDURE BinTxT_Bool
    ( Lt , Rt : LONGINT ; OpProc : TxT_BoolProcTyp ; IsM3Int : BOOLEAN ) : LONGINT 

  = VAR LLt , LRt : LONGINT
  ; VAR LResult : BOOLEAN 

  ; BEGIN
      EnsureInit ( )
    ; IF IsM3Int AND RTIntIs32
      THEN
        LLt := Long . And ( Lo32Mask , Lt ) 
      ; LRt := Long . And ( Lo32Mask , Rt )
      ; LResult := OpProc ( LLt , LRt )
      ELSE LResult := OpProc ( Lt , Rt ) 
      END (*IF*)
    ; RETURN VAL ( ORD ( LResult ) , LONGINT ) 
    END BinTxT_Bool 

; PROCEDURE LeftShift ( Arg , Ct : LONGINT ) : LONGINT 

   = VAR LCtI : INTEGER 

   ; BEGIN
       EnsureInit ( )
     ; LCtI := VAL ( Ct , INTEGER ) 
     ; IF LCtI >= RTIntBitsizeI THEN RETURN 0L END (*IF*)
     ; RETURN Long . And ( RTIntMask , Long . LeftShift ( Arg , LCtI ) )  
     END LeftShift

; PROCEDURE RightShift ( Arg , Ct : LONGINT ) : LONGINT 

   = VAR LCtI : INTEGER 

   ; BEGIN
       EnsureInit ( )
     ; LCtI := VAL ( Ct , INTEGER ) 
     ; IF LCtI >= RTIntBitsizeI THEN RETURN 0L END (*IF*)
     ; RETURN Long . RightShift ( Long . And ( RTIntMask , Arg ) , LCtI )   
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
     ; LShifted := Long . RightShift ( LDouble , LCtI )
     ; RETURN Long . And ( Lo32Mask , LShifted )
     END RightRotate32 
  
(*EXPORTED:*) 
; PROCEDURE FromCTInt ( IntVal : INTEGER ; Signed : BOOLEAN ) : T

  = BEGIN
      EnsureInit ( )
    ; IF Signed
      THEN RETURN VAL ( IntVal , LONGINT ) (* Should sign extend. *) 
      ELSE RETURN Long . And ( RTIntMask , VAL ( IntVal , LONGINT ) ) 
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
    ( Arg : T ; Opcode : FM3SrcToks . TokTyp ; IsM3Int : BOOLEAN ) : T
  RAISES { ArithError , Unimplemented } 

  = VAR LResult : LONGINT

  ; BEGIN
      EnsureInit ( ) 
    ; CASE Opcode OF
      | Stk . RidABS
      => EnsureInit ( )
      ;  IF IsM3Int AND RTIntIs32
         THEN
           IF Long . And ( Lo32Mask , Arg ) = 16_00000000FFFFFFFFL
           THEN RAISE ArithError ( "Overflow in ABS" ) 
           END (*IF*) 
         ELSE
           IF Arg  = 16_FFFFFFFFFFFFFFFFL
           THEN RAISE ArithError ( "Overflow in ABS" ) 
           END (*IF*) 
         END (*IF*) 
      ;  RETURN ABS ( Arg )

      | Stk . StkRwNOT (* Integer representation of BOOLEAN. *) 
      => WITH WArg = Long . And ( 1L , Arg ) = 1L
         DO RETURN VAL ( ORD ( NOT WArg  ) , LONGINT )
         END (*WITH*) 

      | Stk . StkPdNot (* Bitwise. *) 
      => EnsureInit ( )
      ;  LResult := Long . Not ( Arg ) 
      ;  IF IsM3Int AND RTIntIs32
         THEN LResult := Long . And ( Lo32Mask , LResult ) 
         END (*IF*)
      ;  RETURN LResult 

      ELSE RAISE Unimplemented ( Stk . Image ( Opcode ) ) 
      END (*CASE*)      
    END UnOp

(*EXPORTED:*) 
; PROCEDURE BinOp
    ( Lt , Rt : T ; Opcode : FM3SrcToks . TokTyp ; IsM3Int : BOOLEAN ) : T
  RAISES { ArithError , Unimplemented }

  = BEGIN
      EnsureInit ( ) 
    ; CASE Opcode OF

      (* Full-fledged arithmetic: *)
      
      | FM3SrcToks . StkPlus
      => EnsureInit ( )
      ;  IF DoCheckOvflo
         THEN
           IF IsM3Int AND RTIntIs32  
           THEN RETURN RaiseOnOvfloI32 ( Long . Plus ( Lt , Rt ) )  
           ELSE RETURN Plus64WOvflo ( Lt , Rt , CarryIn := 0L )
         END (*IF*)
         ELSE RETURN Lt + Rt 
         END (*IF*)

      | FM3SrcToks . StkMinus
      => EnsureInit ( )
      ;  IF DoCheckOvflo
         THEN
           IF IsM3Int AND RTIntIs32  
           THEN RETURN RaiseOnOvfloI32 ( Long . Minus ( Lt , Rt ) )  
           ELSIF Rt = 16_8000000000000000L
           THEN RETURN Plus64WOvflo ( Lt , 16_FFFFFFFFFFFFFFFFL , CarryIn := 1L )
           ELSE RETURN Plus64WOvflo ( Lt , - Rt , CarryIn := 0L )
           END (*IF*)
         ELSE RETURN Lt - Rt 
         END (*IF*)

      | FM3SrcToks . StkStar
      => EnsureInit ( )
      ;  IF DoCheckOvflo
         THEN
           IF IsM3Int AND RTIntIs32
           THEN RETURN Times32WOvflo ( Lt , Rt ) 
           ELSE RETURN Times64WOvflo ( Lt , Rt )
           END (*IF*) 
         ELSE RETURN Lt * Rt 
         END (*IF*)

      | FM3SrcToks . StkRwDIV 
      => IF Lt = 0L THEN RAISE ArithError ( "DIV by zero" ) END (*IF*)
         (* ^Certain RT Error, check this regardless of DoCheckOvflo. *) 
      ;  RETURN Lt DIV Rt 

      | FM3SrcToks . StkRwMOD  
      => IF Lt = 0L THEN RAISE ArithError ( "MODzero" ) END (*IF*) 
         (* ^Certain RT Error, check this regardless of DoCheckOvflo. *) 
      ;  RETURN Lt MOD Rt 

      | Stk . StkRwAND
      => WITH WLt = Long . And ( 1L , Lt ) = 1L
         ,    WRt = Long . And ( 1L , Rt ) = 1L
         DO RETURN VAL ( ORD ( WLt AND WRt ) , LONGINT )
         END (*WITH*) 

      | Stk . StkRwOR
      => WITH WLt = Long . And ( 1L , Lt ) = 1L
         ,    WRt = Long . And ( 1L , Rt ) = 1L
         DO RETURN VAL ( ORD ( WLt OR WRt ) , LONGINT )
         END (*WITH*) 

      | Stk . RidMIN 
      => RETURN MIN ( Lt , Rt )  

      | Stk . RidMAX 
      => RETURN MAX ( Lt , Rt )  

      | Stk . StkPdPlus
      => RETURN BinTxT_T ( Lt , Rt , Long . Plus, IsM3Int )  

      | Stk . StkPdMinus
      => RETURN BinTxT_T ( Lt , Rt , Long . Minus , IsM3Int ) 

      | Stk . StkPdTimes
      => RETURN BinTxT_T ( Lt , Rt , Long . Times , IsM3Int ) 

      | Stk . StkPdDivide
      => IF Lt = 0L THEN RAISE ArithError ( "Divide by zero" ) END (*IF*) 
         (* ^Certain RT Error, check this regardless of DoCheckOvflo. *) 
      ;  RETURN BinTxT_T ( Lt , Rt , Long . Divide , IsM3Int ) 

      | Stk . StkPdMod
      => IF Lt = 0L THEN RAISE ArithError ( "Mod zero" ) END (*IF*) 
         (* ^Certain RT Error, check this regardless of DoCheckOvflo. *) 
      ;  RETURN BinTxT_T ( Lt , Rt , Long . Mod , IsM3Int ) 

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
      => RETURN BinTxT_Bool ( Lt , Rt , Long . LT , IsM3Int ) 

      | Stk . StkPdGT 
      => RETURN BinTxT_Bool ( Lt , Rt , Long . GT , IsM3Int ) 

      | Stk . StkPdLE
      => RETURN BinTxT_Bool ( Lt , Rt , Long . LE , IsM3Int ) 

      | Stk . StkPdGE
      => RETURN BinTxT_Bool ( Lt , Rt , Long . GE , IsM3Int ) 

      (* Bitwise booleans: *)

      | Stk . StkPdAnd
      => RETURN BinTxT_T ( Lt , Rt , Long . And ,IsM3Int ) 

      | Stk . StkPdOr
      => RETURN BinTxT_T ( Lt , Rt , Long . Or , IsM3Int ) 

      | Stk . StkPdXor
      => RETURN BinTxT_T ( Lt , Rt , Long . Xor , IsM3Int )  

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
      =>  EnsureInit ( )
      ;   IF IsM3Int AND RTIntIs32
          THEN
            IF Rt > 0L
            THEN RETURN LeftRotate32 ( Lt , Rt ) 
            ELSIF Rt < 0L
            THEN RETURN RightRotate32 ( Lt , Rt )
            ELSE RETURN Lt
            END (*IF*)
          ELSE RETURN Long . Rotate ( Lt , VAL ( Rt , INTEGER ) )
          END (*IF*) 

      ELSE RAISE Unimplemented ( Stk . Image ( Opcode ) ) 
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
    ; IF LnI + LiI > RTIntBitsizeI
      THEN
        RAISE ArithError
          ( "Extract beyond " & Fmt . Int ( RTIntBitsizeI ) & " bits." )
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
    ; IF LnI + LiI > RTIntBitsizeI
      THEN
        RAISE ArithError
          ( "Insert beyond " & Fmt . Int ( RTIntBitsizeI ) & " bits." )
      ELSE RETURN Long . Insert ( x , y , LiI , LnI )
      END (*IF*) 
    END Insert

; VAR RTIntMask : LONGINT 
; VAR RTIntBitsizeI : INTEGER 
; VAR RTIntIs32 : BOOLEAN
; VAR DoCheckOvflo : BOOLEAN 
; VAR IsInitialized := FALSE 

; PROCEDURE EnsureInit ( )

  = BEGIN
      IF NOT IsInitialized
      THEN
        <* ASSERT FM3CLOptions . IsInitialized *> 
        DoCheckOvflo
          := FM3CLToks . CltOvflo IN FM3CLOptions . OptionTokSet
      ; RTIntBitsizeI
          := NARROW
               ( VarArray_Int_Refany . Fetch
                   ( FM3LoTypes . LoTypeMap , FM3Target . LoTypeNoInt )
               , FM3LoTypes . LoTypeInfoRefTyp
               )
             . TiNatSize
      ; IF RTIntBitsizeI = 32
        THEN 
          RTIntIs32 := TRUE 
        ; RTIntMask := Lo32Mask
        ELSE
          RTIntIs32 := FALSE
        ; RTIntMask := 16_FFFFFFFFFFFFFFFFL
        END (*IF*) 
      ; IsInitialized := TRUE
      END (*IF*) 
    END EnsureInit 

; BEGIN
    IsInitialized := FALSE 
  END FM3CTIntArith
.
