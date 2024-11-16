
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Predefs

; IMPORT IntSets

; IMPORT FM3SrcToks AS Stk
; IMPORT FM3IntToks AS Itk

; TYPE BuiltinOpTyp = [ Stk . TokMinReservedId .. Stk . TokMaxPredef ]

; PROCEDURE Stk2Itk ( StkTok : Stk . TokTyp ) : Itk . TokTyp 
  (* Translate source tokens for reserved idents and predefined imported idents
     into equivalent internal LEFToperator codes.
  *) 

; TYPE ConstructorTyp = ARRAY OF INTEGER 

(* SIGH. Cm3 crashes on some of these sets when ConstructorTyp
         = SET FO BuiltinOpTyp.
   So we use IntSets instead of Modula-3's SET OF.  Heavier weight
   than desirable, but I don't want to go off on the tangent right now
   of fixing CM3.  IntSet calls are in the module.  Editing shouldn't
   be too bad if this is ever fixed. 
*) 

(* Reserved identifiers. *) 
; VAR ReservedIdSet : IntSets . T
; CONST ReservedIdValues = ConstructorTyp
    { Stk . RidABS 
    , Stk . RidADDRESS 
    , Stk . RidADR 
    , Stk . RidADRSIZE 
    , Stk . RidBITSIZE 
    , Stk . RidBOOLEAN 
    , Stk . RidBYTESIZE 
    , Stk . RidCARDINAL 
    , Stk . RidCEILING 
    , Stk . RidCHAR 
    , Stk . RidDEC 
    , Stk . RidDISPOSE 
    , Stk . RidEXTENDED 
    , Stk . RidFALSE 
    , Stk . RidFIRST 
    , Stk . RidFLOAT 
    , Stk . RidFLOOR 
    , Stk . RidINC 
    , Stk . RidINTEGER 
    , Stk . RidISTYPE 
    , Stk . RidLAST 
    , Stk . RidLONGCARD 
    , Stk . RidLONGINT 
    , Stk . RidLONGREAL 
    , Stk . RidLOOPHOLE 
    , Stk . RidMAX 
    , Stk . RidMIN 
    , Stk . RidMUTEX 
    , Stk . RidNARROW 
    , Stk . RidNEW 
    , Stk . RidNIL 
    , Stk . RidNULL 
    , Stk . RidNUMBER 
    , Stk . RidORD 
    , Stk . RidREAL 
    , Stk . RidREFANY 
    , Stk . RidROUND 
    , Stk . RidSUBARRAY 
    , Stk . RidTEXT 
    , Stk . RidTRUE 
    , Stk . RidTRUNC 
    , Stk . RidTYPECODE 
    , Stk . RidVAL
    , Stk . RidWIDECHAR 
    } 

(* Qualifier idents in Word and Long: *)
; VAR WordLongQualifierSet : IntSets . T 
; CONST WordLongQualifierValues = ConstructorTyp
    { Stk . Word_T 
    , Stk . Word_Size 
    , Stk . Word_Plus 
    , Stk . Word_Times 
    , Stk . Word_Minus 
    , Stk . Word_Divide 
    , Stk . Word_Mod 
    , Stk . Word_LT 
    , Stk . Word_LE 
    , Stk . Word_GT 
    , Stk . Word_GE 
    , Stk . Word_And 
    , Stk . Word_Or 
    , Stk . Word_Xor 
    , Stk . Word_Not 
    , Stk . Word_Shift 
    , Stk . Word_LeftShift 
    , Stk . Word_RightShift 
    , Stk . Word_Rotate 
    , Stk . Word_LeftRotate 
    , Stk . Word_RightRotate 
    , Stk . Word_Extract 
    , Stk . Word_Insert
    } 

(* Predefined functions with one parameter: *)
; VAR OneParamSet : IntSets . T 
; CONST OneParamValues = ConstructorTyp
    { Stk . RidABS 
    , Stk . RidADR 
    , Stk . RidADRSIZE 
    , Stk . RidBITSIZE 
    , Stk . RidBYTESIZE 
    , Stk . RidCEILING 
    , Stk . RidFIRST 
    , Stk . RidFLOAT 
    , Stk . RidFLOOR 
    , Stk . RidISTYPE 
    , Stk . RidLAST 
    , Stk . RidNUMBER 
    , Stk . RidORD 
    , Stk . RidROUND 
    , Stk . RidTRUNC 
    , Stk . RidTYPECODE
    , Stk . RidDEC 
    , Stk . RidDISPOSE 
    , Stk . RidINC 
    , Stk . Word_Not 
    }

(* Predefined functions with two parameters: *)
; VAR TwoParamSet : IntSets . T 
; CONST TwoParamValues = ConstructorTyp
    { Stk . RidLOOPHOLE 
    , Stk . RidMAX 
    , Stk . RidMIN 
    , Stk . RidNARROW
    , Stk . RidVAL
    , Stk . Word_Plus 
    , Stk . Word_Times 
    , Stk . Word_Minus 
    , Stk . Word_Divide 
    , Stk . Word_Mod 
    , Stk . Word_LT 
    , Stk . Word_LE 
    , Stk . Word_GT 
    , Stk . Word_GE 
    , Stk . Word_And 
    , Stk . Word_Or 
    , Stk . Word_Xor 
    , Stk . Word_Shift 
    , Stk . Word_LeftShift 
    , Stk . Word_RightShift 
    , Stk . Word_Rotate 
    , Stk . Word_LeftRotate 
    , Stk . Word_RightRotate 
    }

(* Predefined functions with three parameters: *)
; VAR ThreeParamSet : IntSets . T 
; CONST ThreeParamValues = ConstructorTyp 
    { Stk . RidSUBARRAY
    , Stk . Word_Extract 
    , Stk . Word_Insert
    }

(* Predefined functions with a variable number of actuals: *)
; VAR OneOrMoreParamSet : IntSets . T 
; CONST OneOrMoreParamValues = ConstructorTyp 
    { Stk . RidNEW }

(* Predefined constant names: *) 
; VAR ConstantSet : IntSets . T
; CONST ConstantValues = ConstructorTyp 
    { Stk . RidFALSE 
    , Stk . RidNIL 
    , Stk . RidTRUE
    , Stk . Word_Size 
    } 

(* Predefined type names: *) 
; VAR TypeSet : IntSets . T
; CONST TypeValues = ConstructorTyp 
    { Stk . RidADDRESS 
    , Stk . RidBOOLEAN 
    , Stk . RidCARDINAL 
    , Stk . RidCHAR 
    , Stk . RidEXTENDED 
    , Stk . RidINTEGER 
    , Stk . RidLONGCARD 
    , Stk . RidLONGINT 
    , Stk . RidLONGREAL 
    , Stk . RidMUTEX 
    , Stk . RidNULL 
    , Stk . RidREAL 
    , Stk . RidREFANY 
    , Stk . RidTEXT
    , Stk . RidWIDECHAR 
    , Stk . RidROOT 
    , Stk . RidUNTRACEDROOT
    , Stk . Word_T 
    } 

(* Modula-3 language definition sections: *) 
(****     
    , Stk . RidABS 
    , Stk . RidADR 
    , Stk . RidADRSIZE 
    , Stk . RidBITSIZE 
    , Stk . RidBYTESIZE 
    , Stk . RidCEILING 
    , Stk . RidFIRST 
    , Stk . RidFLOAT 
    , Stk . RidFLOOR 
    , Stk . RidISTYPE 
    , Stk . RidLAST 
    , Stk . RidNUMBER 
    , Stk . RidORD 
    , Stk . RidROUND 
    , Stk . RidTRUNC 
    , Stk . RidTYPECODE
    , Stk . RidDEC 
    , Stk . RidDISPOSE 
    , Stk . RidINC 
    
    , Stk . RidLOOPHOLE 
    , Stk . RidMAX 
    , Stk . RidMIN 
    , Stk . RidNARROW
    , Stk . RidVAL

    , Stk . RidSUBARRAY 

    , Stk . RidNEW 

    (* Constants: *) 
    , Stk . RidFALSE 
    , Stk . RidNIL 
    , Stk . RidTRUE
****) 

; END FM3Predefs
.

