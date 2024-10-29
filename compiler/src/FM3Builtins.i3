
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Builtins

; TYPE BuiltinOpTyp = [ FM3SrcToks . TokMinPredef .. FM3SrcToks . TokMaxPredef ] 

(* Functions with one parameter: *)
; CONST OneActual = SET OF BuiltinOpTyp 
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
    
(* Functions with two parameters: *)
; CONST TwoActuals = SET OF BuiltinOpTyp
    { Stk . RidLOOPHOLE 
    , Stk . RidMAX 
    , Stk . RidMIN 
    , Stk . RidNARROW
    , Stk . RidVAL
    }

(* Functions with three parameters: *)
; CONST ThreeActuals = SET OF BuiltinOpTyp
    { Stk . RidSUBARRAY }

(* Functions with a variable number of actuals: *)
; CONST NActuals = SET OF BuiltinOpTyp
    { Stk . RidNEW 



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

; END FM3Builtins
.




