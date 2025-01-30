
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Exprs for types builtin to Modula-3. *)

MODULE FM3BuiltinTypes

; IMPORT IntSets
; IMPORT IntRanges
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Exprs
; IMPORT FM3LoTypes
; IMPORT FM3LoTypes AS Lt
; IMPORT FM3SrcToks
; IMPORT FM3SrcToks AS Stk

; VAR GTokArray : VarArray_Int_Refany . T 

; PROCEDURE InitOneTypeExpr
    ( Opcode : FM3SrcToks . TokTyp
    ; LoTypeNo : FM3LoTypes . LoTypeNoTyp 
    )

  = VAR LTypeExpr : FM3Exprs . ExprTyp

  ; BEGIN
      LTypeExpr := NEW ( FM3Exprs . ExprTyp ) 
    ; LTypeExpr . ExpType := LTypeExpr (* Self referential. *) 
    ; LTypeExpr . ExpLoTypeInfoRef
        := FM3LoTypes . InfoRef ( LoTypeNo )  
    ; LTypeExpr . ExpReachedDeclNoSet := IntSets . Empty ( ) 
    ; LTypeExpr . ExpSelfExprNo := - Opcode (* < 0 for builtin types. *) 
    ; LTypeExpr . ExpOpcode := Opcode
    ; LTypeExpr . ExpUpKind
        := FM3Exprs . ExprKindTyp . EkType (* Spontaneous. *)  
    ; LTypeExpr . ExpState := FM3Exprs . ExprStateTyp . EsResolved
    ; LTypeExpr . ExpIsUsable := TRUE
    ; LTypeExpr . ExpIsLegalRecursive := TRUE
    ; VarArray_Int_Refany . Assign ( GTokArray , Opcode , LTypeExpr ) 
    END InitOneTypeExpr

; PROCEDURE InitTypeExprs ( ) 

  = BEGIN
      InitOneTypeExpr ( Stk . RidADDRESS   , Lt . LoTypeNoAddr ) 
    ; InitOneTypeExpr ( Stk . RidBOOLEAN   , Lt . LoTypeNoU8 )
    ; InitOneTypeExpr ( Stk . RidCARDINAL  , Lt . LoTypeNoInt )
    ; InitOneTypeExpr ( Stk . RidCHAR      , Lt . LoTypeNoU8 )
    ; InitOneTypeExpr ( Stk . RidEXTENDED  , Lt . LoTypeNoExtended )
    ; InitOneTypeExpr ( Stk . RidINTEGER   , Lt . LoTypeNoInt )
    ; InitOneTypeExpr ( Stk . RidLONGCARD  , Lt . LoTypeNoLong )
    ; InitOneTypeExpr ( Stk . RidLONGINT   , Lt . LoTypeNoLong )
    ; InitOneTypeExpr ( Stk . RidLONGREAL  , Lt . LoTypeNoLongReal )
    ; InitOneTypeExpr ( Stk . RidMUTEX     , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidNULL      , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidREAL      , Lt . LoTypeNoReal )
    ; InitOneTypeExpr ( Stk . RidREFANY    , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidTEXT      , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidWIDECHAR  , Lt . LoTypeNoU32 )
    END InitTypeExprs

; PROCEDURE InitOneConstExpr
    ( Opcode : FM3SrcToks . TokTyp
    ; ConstTypeExpr : FM3Exprs . ExprTyp 
    ; LoTypeNo : FM3LoTypes . LoTypeNoTyp 
    )

  = VAR LConstExpr : FM3Exprs . ExprTyp

  ; BEGIN
      LConstExpr := NEW ( FM3Exprs . ExprTyp ) 
    ; LConstExpr . ExpType := ConstTypeExpr (* Self referential. *) 
    ; LConstExpr . ExpLoTypeInfoRef
        := FM3LoTypes . InfoRef ( LoTypeNo )  
    ; LConstExpr . ExpReachedDeclNoSet := IntSets . Empty ( ) 
    ; LConstExpr . ExpSelfExprNo := - Opcode (* < 0 for builtin types. *) 
    ; LConstExpr . ExpOpcode := Opcode
    ; LConstExpr . ExpUpKind
        := FM3Exprs . ExprKindTyp . EkConst (* Spontaneous. *)  
    ; LConstExpr . ExpState := FM3Exprs . ExprStateTyp . EsResolved
    ; LConstExpr . ExpIsUsable := TRUE
    ; LConstExpr . ExpIsLegalRecursive := TRUE
    ; VarArray_Int_Refany . Assign ( GTokArray , Opcode , LConstExpr ) 
    END InitOneConstExpr

; PROCEDURE InitConstExprs ( ) 

  = VAR LTypeExpr : FM3Exprs . ExprTyp 

  ; BEGIN
      LTypeExpr := VarArray_Int_Refany . Fetch ( GTokArray , Stk . RidBOOLEAN ) 
    ; InitOneConstExpr ( Stk . RidFALSE , LTypeExpr , Lt . LoTypeNoU8   ) 
    ; InitOneConstExpr ( Stk . RidTRUE  , LTypeExpr , Lt . LoTypeNoU8 )
    ; LTypeExpr := VarArray_Int_Refany . Fetch ( GTokArray , Stk . RidREFANY ) 
    ; InitOneConstExpr ( Stk . RidNIL , LTypeExpr , Lt . LoTypeNoAddr)
    END InitConstExprs

; PROCEDURE Init (  )

  = BEGIN (*Init*)
      GTokArray
        := VarArray_Int_Refany . New
             ( NIL
             , IntRanges . RangeTyp { Stk . RidADDRESS , Stk . RidWIDECHAR }
             )
    ; InitTypeExprs ( )
    ; InitConstExprs ( ) 
    END Init
      

(*EXPORTED.*)
; PROCEDURE TypeExpr ( Tok : FM3SrcToks . TokTyp )
  : FM3Exprs . ExprTyp (* NIL if not a reserved id denoting a type > *) 

  = BEGIN
      RETURN
        NARROW
          ( VarArray_Int_Refany . Fetch ( GTokArray , Tok )
          , FM3Exprs . ExprTyp
          )   
    END TypeExpr 



(*
      CASE Tok OF
      | FM3SrcToks . RidADDRESS  
      , FM3SrcToks . RidBOOLEAN 
      , FM3SrcToks . RidCARDINAL 
      , FM3SrcToks . RidCHAR 
      , FM3SrcToks . RidEXTENDED 
      , FM3SrcToks . RidINTEGER 
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
      ELSE RETURN OpPropertiesNull
      END (*CASE*) 
    END Properties 
*)

; BEGIN Init ( ) 
  END FM3BuiltinTypes
.
 