
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Predefs

; IMPORT FM3SrcToks AS Stk
; IMPORT FM3IntToks AS Itk

; TYPE BuiltinOpTyp = [ Stk . TokMinPredef .. Stk . TokMaxPredef ]
(* BEWARE.  cm3 crashed on sets with lower bound not in the 1st word. *) 

(* Functions with one parameter: *)
; CONST RidOneParamSet = SET OF BuiltinOpTyp { }
(*
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
    }
*)     
(* Functions with two parameters: *)
; CONST RidTwoParamSet = SET OF BuiltinOpTyp { }
(*
    { Stk . RidLOOPHOLE 
    , Stk . RidMAX 
    , Stk . RidMIN 
    , Stk . RidNARROW
    , Stk . RidVAL
    }
*)
(* Functions with three parameters: *)
; CONST RidThreeParamSet = SET OF BuiltinOpTyp { }
(*
    { Stk . RidSUBARRAY }
*)

(* Functions with a variable number of actuals: *)
; CONST RidOneOrMoreParamSet = SET OF BuiltinOpTyp { }
(*
    { Stk . RidNEW }
*) 

; CONST ReservedIdSet
    = RidOneParamSet + RidTwoParamSet + RidThreeParamSet + RidOneOrMoreParamSet

; PROCEDURE Stk2Itk ( StkTok : Stk . TokTyp ) : Itk . TokTyp 
  (* Translate source tokens for reserved idents and predefined imported idents
     into equivalent internal LEFToperator codes.
  *) 

(****

        (* Constants: *) 
        , Stk . RidFALSE 
        , Stk . RidNIL 
        , Stk . RidTRUE 

        (* Types: *)  
        , Stk . RidADDRESS 
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


(* Modula-3 language definition sections: *) 
     
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

    (* Types: *)  
    , Stk . RidADDRESS 
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
    }

****)

(*
; CONST Qualifiers
    = SET OF BuiltinOpTyp
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
        
        , Stk . Long_T 
        , Stk . Long_Size 
        , Stk . Long_Plus 
        , Stk . Long_Times 
        , Stk . Long_Minus 
        , Stk . Long_Divide 
        , Stk . Long_Mod 
        , Stk . Long_LT 
        , Stk . Long_LE 
        , Stk . Long_GT 
        , Stk . Long_GE 
        , Stk . Long_And 
        , Stk . Long_Or 
        , Stk . Long_Xor 
        , Stk . Long_Not 
        , Stk . Long_Shift 
        , Stk . Long_LeftShift 
        , Stk . Long_RightShift 
        , Stk . Long_Rotate 
        , Stk . Long_LeftRotate 
        , Stk . Long_RightRotate 
        , Stk . Long_Extract 
        , Stk . Long_Insert 
        } 
*)
; END FM3Predefs
.

