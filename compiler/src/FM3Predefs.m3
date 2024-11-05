
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Predefs 

; IMPORT FM3SrcToks AS Stk
; IMPORT FM3IntToks AS Itk

(*EXPORTED:*)
; PROCEDURE Stk2Itk ( StkTok : Stk . TokTyp ) : Itk . TokTyp 

  = VAR LResult : Itk . TokTyp 

  ; BEGIN
      CASE StkTok OF 
      | Stk . RidNull           => LResult := Itk . ItkNull 

      (* Unary reserved Ident functions. *) 
      | Stk . RidABS            => LResult := Itk . ItkABSLt
      | Stk . RidADR            => LResult := Itk . ItkADRLt
      | Stk . RidADRSIZE        => LResult := Itk . ItkADRSIZELt
      | Stk . RidBITSIZE        => LResult := Itk . ItkBITSIZELt
      | Stk . RidBYTESIZE       => LResult := Itk . ItkBYTESIZELt
      | Stk . RidCEILING        => LResult := Itk . ItkCEILINGLt
      | Stk . RidFIRST          => LResult := Itk . ItkFIRSTLt
      | Stk . RidFLOAT          => LResult := Itk . ItkFLOATLt
      | Stk . RidFLOOR          => LResult := Itk . ItkFLOORLt
      | Stk . RidISTYPE         => LResult := Itk . ItkISTYPELt
      | Stk . RidLAST           => LResult := Itk . ItkLASTLt
      | Stk . RidNUMBER         => LResult := Itk . ItkNUMBERLt
      | Stk . RidORD            => LResult := Itk . ItkORDLt
      | Stk . RidROUND          => LResult := Itk . ItkROUNDLt
      | Stk . RidTRUNC          => LResult := Itk . ItkTRUNCLt
      | Stk . RidTYPECODE       => LResult := Itk . ItkTYPECODELt
      | Stk . RidDEC            => LResult := Itk . ItkDECLt
      | Stk . RidDISPOSE        => LResult := Itk . ItkDISPOSELt
      | Stk . RidINC            => LResult := Itk . ItkINCLt

      (* Binary reserved Ident functions. *) 
      | Stk . RidLOOPHOLE       => LResult := Itk . ItkLOOPHOLELt
      | Stk . RidMAX            => LResult := Itk . ItkMAXLt
      | Stk . RidMIN            => LResult := Itk . ItkMINLt
      | Stk . RidNARROW         => LResult := Itk . ItkNARROWLt
      | Stk . RidVAL            => LResult := Itk . ItkVALLt

      (* Unary imported predefined functions. *) 
      | Stk . Word_T            => LResult := Itk . ItkWord_TLt
      | Stk . Word_Size         => LResult := Itk . ItkWord_SizeLt
      | Stk . Word_Not          => LResult := Itk . ItkWord_NotLt
      | Stk . Long_T            => LResult := Itk . ItkLong_TLt
      | Stk . Long_Size         => LResult := Itk . ItkLong_SizeLt
      | Stk . Long_Not          => LResult := Itk . ItkLong_NotLt

      (* Binary imported predefined functions. *) 
      | Stk . Word_Plus         => LResult := Itk . ItkWord_PlusLt
      | Stk . Word_Times        => LResult := Itk . ItkWord_TimesLt
      | Stk . Word_Minus        => LResult := Itk . ItkWord_MinusLt
      | Stk . Word_Divide       => LResult := Itk . ItkWord_DivideLt
      | Stk . Word_Mod          => LResult := Itk . ItkWord_ModLt
      | Stk . Word_LT           => LResult := Itk . ItkWord_LTLt
      | Stk . Word_LE           => LResult := Itk . ItkWord_LELt
      | Stk . Word_GT           => LResult := Itk . ItkWord_GTLt
      | Stk . Word_GE           => LResult := Itk . ItkWord_GELt
      | Stk . Word_And          => LResult := Itk . ItkWord_AndLt
      | Stk . Word_Or           => LResult := Itk . ItkWord_OrLt
      | Stk . Word_Xor          => LResult := Itk . ItkWord_XorLt
      | Stk . Word_Shift        => LResult := Itk . ItkWord_ShiftLt
      | Stk . Word_LeftShift    => LResult := Itk . ItkWord_LeftShiftLt
      | Stk . Word_RightShift   => LResult := Itk . ItkWord_RightShiftLt
      | Stk . Word_Rotate       => LResult := Itk . ItkWord_RotateLt
      | Stk . Word_LeftRotate   => LResult := Itk . ItkWord_LeftRotateLt
      | Stk . Word_RightRotate  => LResult := Itk . ItkWord_RightRotateLt

      | Stk . Long_Plus         => LResult := Itk . ItkLong_PlusLt
      | Stk . Long_Times        => LResult := Itk . ItkLong_TimesLt
      | Stk . Long_Minus        => LResult := Itk . ItkLong_MinusLt
      | Stk . Long_Divide       => LResult := Itk . ItkLong_DivideLt
      | Stk . Long_Mod          => LResult := Itk . ItkLong_ModLt
      | Stk . Long_LT           => LResult := Itk . ItkLong_LTLt
      | Stk . Long_LE           => LResult := Itk . ItkLong_LELt
      | Stk . Long_GT           => LResult := Itk . ItkLong_GTLt
      | Stk . Long_GE           => LResult := Itk . ItkLong_GELt
      | Stk . Long_And          => LResult := Itk . ItkLong_AndLt
      | Stk . Long_Or           => LResult := Itk . ItkLong_OrLt
      | Stk . Long_Xor          => LResult := Itk . ItkLong_XorLt
      | Stk . Long_Shift        => LResult := Itk . ItkLong_ShiftLt
      | Stk . Long_LeftShift    => LResult := Itk . ItkLong_LeftShiftLt
      | Stk . Long_RightShift   => LResult := Itk . ItkLong_RightShiftLt
      | Stk . Long_Rotate       => LResult := Itk . ItkLong_RotateLt
      | Stk . Long_LeftRotate   => LResult := Itk . ItkLong_LeftRotateLt
      | Stk . Long_RightRotate  => LResult := Itk . ItkLong_RightRotateLt

      (* Ternary imported predefined functions. *) 
      | Stk . Word_Extract      => LResult := Itk . ItkWord_ExtractLt
      | Stk . Word_Insert       => LResult := Itk . ItkWord_InsertLt
      | Stk . Long_Extract      => LResult := Itk . ItkLong_ExtractLt
      | Stk . Long_Insert       => LResult := Itk . ItkLong_InsertLt
      ELSE                         LResult := Itk . ItkNull 
      END (*CASE*) 
    ; RETURN LResult 
    END Stk2Itk 

; BEGIN
  END FM3Predefs
. 
