
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Properties of operations builtin to Modula-3. *)

MODULE FM3BuiltinOps

; IMPORT FM3Exprs
; IMPORT FM3SrcToks 

; CONST OpPropertiesNull
    = OpPropertiesTyp
         { OpResultType := NIL
         , OpExprKind := FM3Exprs . ExprKindTyp . EkNull 
         , OpOpndCt := 0 
         , OpLtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { } 
         , OpRtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { }
         } 

; CONST OpPropertiesType
    = OpPropertiesTyp
         { OpResultType := NIL
         , OpExprKind := FM3Exprs . ExprKindTyp . EkType 
         , OpOpndCt := 0 
         , OpLtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { } 
         , OpRtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { }
         } 

; CONST OpPropertiesConstValue
    = OpPropertiesTyp
         { OpResultType := NIL
         , OpExprKind := FM3Exprs . ExprKindTyp . EkValue 
         , OpOpndCt := 0 
         , OpLtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { } 
         , OpRtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { }
         } 

; CONST OpPropertiesUnOp
    = OpPropertiesTyp
         { OpResultType := NIL
         , OpExprKind := FM3Exprs . ExprKindTyp . EkValue 
         , OpOpndCt := 0 
         , OpLtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { } 
         , OpRtExprKindsAllowed := FM3Exprs . ExprKindSetTyp { }
         } 

(*EXPORTED.*)
; PROCEDURE Properties ( Tok : FM3SrcToks . TokTyp ) : OpPropertiesTyp

  = BEGIN
      CASE Tok OF
 (*
 (* Functions: *)   
   RidABS              "ABS" . 
   RidADR              "ADR" . 
   RidADRSIZE          "ADRSIZE" . 
   RidBITSIZE          "BITSIZE" . 
   RidBYTESIZE         "BYTESIZE" . 
   RidCEILING          "CEILING" .
   RidFIRST            "FIRST" . 
   RidFLOAT            "FLOAT" . 
   RidFLOOR            "FLOOR" . 
   RidISTYPE           "ISTYPE" . 
   RidLAST             "LAST" . 
   RidLOOPHOLE         "LOOPHOLE" . 
   RidMAX              "MAX" . 
   RidMIN              "MIN" . 
   RidNARROW           "NARROW" . 
   RidNEW              "NEW" . 
   RidNUMBER           "NUMBER" . 
   RidORD              "ORD" . 
   RidROUND            "ROUND" . 
   RidSUBARRAY         "SUBARRAY" . 
   RidTRUNC            "TRUNC" . 
   RidTYPECODE         "TYPECODE" . 
   RidVAL              "VAL" .

   (* Proper procedures: *) 
   RidDISPOSE          "DISPOSE" . 
   RidDEC              "DEC" . 
   RidINC              "INC" .
 *) 
      ELSE RETURN OpPropertiesNull
      END (*CASE*) 
    END Properties 

; BEGIN
  END FM3BuiltinOps
.
 