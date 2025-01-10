
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Std 

; IMPORT IntSets 

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

      ELSE                         LResult := Itk . ItkNull 
      END (*CASE*) 
    ; RETURN LResult 
    END Stk2Itk 


; BEGIN
    ReservedIdSet := IntSets . FromArray ( ReservedIdValues )
  ; StdIntfSet := IntSets . FromArray ( StdIntfValues ) 
  ; WordLongQualifierSet := IntSets . FromArray ( WordLongQualifierValues ) 
  ; OneParamSet := IntSets . FromArray ( OneParamValues ) 
  ; TwoParamSet := IntSets . FromArray ( TwoParamValues ) 
  ; ThreeParamSet := IntSets . FromArray ( ThreeParamValues )  
  ; OneOrMoreParamSet := IntSets . FromArray ( OneOrMoreParamValues )
  ; ConstantSet := IntSets . FromArray ( ConstantValues )  
  ; TypeSet := IntSets . FromArray ( TypeValues )
  ; ProcSet := IntSets . Union ( OneParamSet , TwoParamSet )  
  ; ProcSet := IntSets . Union ( ProcSet , ThreeParamSet )  
  ; ProcSet := IntSets . Union ( ProcSet , OneOrMoreParamSet )  
  END FM3Std
. 
