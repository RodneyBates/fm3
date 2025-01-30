
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

(*EXPORTED.*)
; PROCEDURE Properties ( Tok : FM3SrcToks . TokTyp ) : OpPropertiesTyp

  = BEGIN
      CASE Tok OF
      | FM3SrcToks . RidADDRESS  
      , FM3SrcToks . RidBOOLEAN 
      , FM3SrcToks . RidCARDINAL 
      , FM3SrcToks . RidCHAR 
      , FM3SrcToks . RidINTEGER 
      , FM3SrcToks . RidEXTENDED 
      , FM3SrcToks . RidLONGCARD 
      , FM3SrcToks . RidLONGINT 
      , FM3SrcToks . RidLONGREAL 
      , FM3SrcToks . RidMUTEX 
      , FM3SrcToks . RidNULL 
      , FM3SrcToks . RidREAL 
      , FM3SrcToks . RidREFANY 
      , FM3SrcToks . RidTEXT  
      , FM3SrcToks . RidWIDECHAR
      => RETURN OpPropertiesType

      | FM3SrcToks . RidFALSE
      , FM3SrcToks . RidNIL
      , FM3SrcToks . RidTRUE
      => RETURN OpPropertiesType

(*


      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
      , FM3SrcToks . Rid
*)
      ELSE RETURN OpPropertiesNull
      END (*CASE*) 
    END Properties 

; BEGIN
  END FM3BuiltinOps
.
 