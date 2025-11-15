
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Exprs for things builtin to Modula-3. *)

(* These build FM3Expr.ExprTyp objects for builtin things.
   This happens in Pass2, when references to declared entities
   can not yet be followed, so types are not known in general.
   Builtin types and builtin constants (whose types are always
   builtin) have types set, but other builtins not.

   An ExprTyp node for a builtin type or constant is always a leaf
   of an expression tree.  Since it has no descendents, a single ExprTyp
   object is created at initialization for each builtin and returned
   possibly multiple times.

   In contrast, each occurrence of a function or procedure needs a
   distinct ExprTyp object with its own descendents, types, etc. So
   each call returns a newly allocated object with only properties
   that are the same in every instance set.
*) 

MODULE FM3Builtins

; IMPORT IntSets

; IMPORT FM3Base 
; IMPORT FM3Exprs
; IMPORT FM3LoTypes
; IMPORT FM3LoTypes AS Lt
; IMPORT FM3SrcToks
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Utils 

; TYPE Ekt = FM3Exprs . ExprKindTyp

; TYPE StaticTokRange
       = [ FM3SrcToks . StkMinStatic .. FM3SrcToks . StkMaxStatic ] 
; VAR GStaticArray : ARRAY StaticTokRange OF FM3Exprs . ExprTyp
      (* ^Contains ExprTyp values (object pointers) that are complete.
         Repeated calls for the same token return the same ExprRef, taken
         from this array.  It's initialized once and used many times. 
      *) 

; PROCEDURE InitOneTypeExpr
    ( Opcode : FM3SrcToks . TokTyp ; LoTypeNo : FM3LoTypes . LoTypeNoTyp )

  = VAR LTypeExpr : FM3Exprs . ExprTyp

  ; BEGIN
      LTypeExpr := NEW ( FM3Exprs . ExprTyp ) 
    ; LTypeExpr . ExpType := LTypeExpr (* Self referential. *) 
    ; LTypeExpr . ExpLoTypeInfoRef := FM3LoTypes . InfoRef ( LoTypeNo )  
    ; LTypeExpr . ExpReachedDeclNoSet := IntSets . Empty ( ) 
    ; LTypeExpr . ExpSelfExprNo := - Opcode (* < 0 for builtin types. *) 
    ; LTypeExpr . ExpOpcode := Opcode
    ; LTypeExpr . ExpUpKind
        := FM3Exprs . ExprKindTyp . EkType (* Spontaneous. *)  
    ; LTypeExpr . ExpRepExprNo := FM3Exprs . RepExprNoSingleton
    ; LTypeExpr . ExpState := FM3Exprs . ExprStateTyp . EsResolved
    ; LTypeExpr . ExpIsUsable := TRUE
    ; LTypeExpr . ExpIsLegalRecursive := TRUE
    ; FM3Utils . ContribToHashI ( LTypeExpr . ExpHash , Opcode ) 
    ; GStaticArray [ Opcode ] := LTypeExpr  
    END InitOneTypeExpr

; PROCEDURE InitTypeExprs ( ) 

  = BEGIN 
      InitOneTypeExpr ( Stk . RidNull         , Lt . LoTypeNoNull ) 
    ; InitOneTypeExpr ( Stk . RidADDRESS      , Lt . LoTypeNoAddr ) 
    ; InitOneTypeExpr ( Stk . RidBOOLEAN      , Lt . LoTypeNoU8 )
    ; InitOneTypeExpr ( Stk . RidCARDINAL     , Lt . LoTypeNoInt )
    ; InitOneTypeExpr ( Stk . RidCHAR         , Lt . LoTypeNoU8 )
    ; InitOneTypeExpr ( Stk . RidEXTENDED     , Lt . LoTypeNoExtended )
    ; InitOneTypeExpr ( Stk . RidINTEGER      , Lt . LoTypeNoInt )
    ; InitOneTypeExpr ( Stk . RidLONGCARD     , Lt . LoTypeNoLong )
    ; InitOneTypeExpr ( Stk . RidLONGINT      , Lt . LoTypeNoLong )
    ; InitOneTypeExpr ( Stk . RidLONGREAL     , Lt . LoTypeNoLongReal )
    ; InitOneTypeExpr ( Stk . RidMUTEX        , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidNULL         , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidREAL         , Lt . LoTypeNoReal )
    ; InitOneTypeExpr ( Stk . RidREFANY       , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidROOT         , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidUNTRACEDROOT , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidTEXT         , Lt . LoTypeNoAddr )
    ; InitOneTypeExpr ( Stk . RidWIDECHAR     , Lt . LoTypeNoU32 )
    (* Let the normal declaration mechanism lead StlPdT to INTEGER. *) 
    END InitTypeExprs

; PROCEDURE InitOneConstExpr
    ( Opcode : FM3SrcToks . TokTyp
    ; ConstTypeExpr : FM3Exprs . ExprTyp 
    ; LoTypeNo : FM3LoTypes . LoTypeNoTyp
    ; ConstValue : LONGINT 
    )

  = VAR LConstExpr : FM3Exprs . ExprTyp

  ; BEGIN
      LConstExpr := NEW ( FM3Exprs . ExprTyp ) 
    ; LConstExpr . ExpType := ConstTypeExpr (* Self referential. *) 
    ; LConstExpr . ExpLoTypeInfoRef := FM3LoTypes . InfoRef ( LoTypeNo )  
    ; LConstExpr . ExpReachedDeclNoSet := IntSets . Empty ( ) 
    ; LConstExpr . ExpSelfExprNo := - Opcode (* < 0 for builtin types. *) 
    ; LConstExpr . ExpOpcode := Opcode
    ; LConstExpr . ExpUpKind
        := FM3Exprs . ExprKindTyp . EkValue (* Spontaneous. *)  
    ; LConstExpr . ExpState := FM3Exprs . ExprStateTyp . EsResolved
    ; LConstExpr . ExpIsUsable := TRUE
    ; LConstExpr . ExpScalarConstVal := ConstValue 
    ; LConstExpr . ExpConstValIsKnown := TRUE
    ; LConstExpr . ExpIsConst := TRUE
    ; LConstExpr . ExpRepExprNo := FM3Exprs . RepExprNoSingleton
    ; LConstExpr . ExpIsLegalRecursive := TRUE
    ; FM3Utils . ContribToHashI ( LConstExpr . ExpHash , Opcode ) 
    ; FM3Utils . ContribToHashL ( LConstExpr . ExpHash , ConstValue ) 
    ; LConstExpr . ExpRepExprNo := FM3Exprs . RepExprNoSingleton
    ; LConstExpr . ExpState := FM3Exprs . ExprStateTyp . EsResolved
    ; GStaticArray [ Opcode ] := LConstExpr  
    END InitOneConstExpr

; PROCEDURE InitConstExprs ( ) 

  = BEGIN
      WITH WTypeExpr = GStaticArray [ Stk . RidBOOLEAN ]
      DO 
        InitOneConstExpr ( Stk . RidFALSE , WTypeExpr , Lt . LoTypeNoU8 , 0L ) 
      ; InitOneConstExpr ( Stk . RidTRUE  , WTypeExpr , Lt . LoTypeNoU8 , 1L )
      END (*WITH*) 
    ; WITH WTypeExpr = GStaticArray [ Stk . RidREFANY ] 
      DO InitOneConstExpr ( Stk . RidNIL , WTypeExpr , Lt . LoTypeNoAddr , 0L )
      END (*WITH*) 
    END InitConstExprs

; TYPE OpTokRange
    = [ FM3SrcToks . StkMinOperation .. FM3SrcToks . StkMaxOperation ] 
      (* ^Arrays subscripted by this type contain values of certain fields
         that are the same for all ExprTyp instances of the OpCode.
         Every call for a builtin opcode will allocate a new ExprTyp
         and initialize some of this fields from these arrays.  The
         arrays are initialized once and used many times. 
      *) 

; TYPE OpPropertiesTyp
    = RECORD
        OpOpndCt : INTEGER 
      ; OpExprKind := FM3Exprs . ExprKindTyp . EkNull 
      ; OpLtOpndKindsAllowed : FM3Exprs . ExprKindSetTyp
      ; OpRtOpndKindsAllowed : FM3Exprs . ExprKindSetTyp
      END

; VAR GOpPropertiesArray : ARRAY OpTokRange OF OpPropertiesTyp 

; VAR GOpTypesArray : ARRAY OpTokRange OF FM3Exprs . ExprTyp 

; PROCEDURE InitOperatorTypes ( )

  = BEGIN (*InitOperatorTypes*)
      WITH WOp = GOpTypesArray , WSt = GStaticArray
      DO FOR ROp := FM3SrcToks . StkMinOperation TO FM3SrcToks . StkMaxWordLong
        DO CASE ROp OF 
          | Stk . RidNull          => WOp [ ROp ] := WSt [ Stk . RidNull ]

          | Stk . RidABS           => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidADR           => WOp [ ROp ] := WSt [ Stk . RidADDRESS ] 
          | Stk . RidADRSIZE       => WOp [ ROp ] := WSt [ Stk . RidCARDINAL ] 
          | Stk . RidBITSIZE       => WOp [ ROp ] := WSt [ Stk . RidCARDINAL ] 
          | Stk . RidBYTESIZE      => WOp [ ROp ] := WSt [ Stk . RidCARDINAL] 
          | Stk . RidCEILING       => WOp [ ROp ] := WSt [ Stk . RidINTEGER ]
          | Stk . RidFIRST         => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidFLOAT         => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidFLOOR         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . RidISTYPE        => WOp [ ROp ] := WSt [ Stk . RidBOOLEAN ] 
          | Stk . RidLAST          => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidLOOPHOLE      => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidMAX           => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidMIN           => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidNARROW        => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidNEW           => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidNUMBER        => WOp [ ROp ] := WSt [ Stk . RidCARDINAL ] 
          | Stk . RidORD           => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . RidROUND         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . RidSUBARRAY      => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidTRUNC         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . RidTYPECODE      => WOp [ ROp ] := WSt [ Stk . RidCARDINAL ] 
          | Stk . RidVAL           => WOp [ ROp ] := WSt [ Stk . RidNull ]

          | Stk . RidDISPOSE       => WOp [ ROp ] := WSt [ Stk . RidNull ] 
          | Stk . RidDEC           => WOp [ ROp ] := WSt [ Stk . RidNull ]
          | Stk . RidINC           => WOp [ ROp ] := WSt [ Stk . RidNull ] 

          | Stk . StkPdSize        => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdPlus        => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdTimes       => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdMinus       => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdDivide      => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdMod         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdLT          => WOp [ ROp ] := WSt [ Stk . RidBOOLEAN ] 
          | Stk . StkPdLE          => WOp [ ROp ] := WSt [ Stk . RidBOOLEAN ] 
          | Stk . StkPdGT          => WOp [ ROp ] := WSt [ Stk . RidBOOLEAN ] 
          | Stk . StkPdGE          => WOp [ ROp ] := WSt [ Stk . RidBOOLEAN ] 
          | Stk . StkPdAnd         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ]
          | Stk . StkPdOr          => WOp [ ROp ] := WSt [ Stk . RidINTEGER ]
          | Stk . StkPdXor         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdNot         => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdShift       => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdLeftShift   => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdRightShift  => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdRotate      => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdLeftRotate  => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdRightRotate => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdExtract     => WOp [ ROp ] := WSt [ Stk . RidINTEGER ] 
          | Stk . StkPdInsert      => WOp [ ROp ] := WSt [ Stk . RidINTEGER ]
          | Stk . StkRTUniqueBrand => WOp [ ROp ] := WSt [ Stk . RidTEXT    ]
          ELSE
            WOp [ ROp ] := WSt [ Stk . RidNull ]
          END (*CASE*) 
        END (*FOR*) 
      END (*WITH*)
      
    (* Copy/adjust types from interface Word to interface Long. *) 
    ; FOR ROp := FM3SrcToks . StkMinWordLong TO FM3SrcToks . StkMaxWordLong
      DO WITH WLong
                = GOpTypesArray [ ROp - Stk . StkMinWordLong + Stk . StkMinLong ]
              , WWord = GOpTypesArray [ ROp ]
         DO IF WWord = GStaticArray [ Stk . RidINTEGER ]
           THEN WLong := GStaticArray [ Stk . RidLONGINT ]
           ELSIF WWord = GStaticArray [ Stk . RidCARDINAL ]
           THEN WLong := GStaticArray [ Stk . RidLONGCARD ]
           ELSE WLong := WWord
           END (*IF*) 
         END (*WITH*)
      END (*FOR*) 
    END InitOperatorTypes

; PROCEDURE InitOperatorProperties ( )

  = BEGIN (*InitOperatorProperties*)
      FOR ROp := FM3SrcToks . StkMinOperation TO FM3SrcToks . StkMaxOperation
      DO
        CASE ROp OF
        | Stk . RidABS
        , Stk . RidADR            
        , Stk . RidCEILING       
        , Stk . RidFLOOR          
        , Stk . RidROUND          
        , Stk . RidTRUNC          
        => WITH WProp = GOpPropertiesArray [ ROp ]
           DO WProp . OpOpndCt := 1 
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetTyp { }  
           END (*WITH*)

        | Stk . RidADRSIZE        
        , Stk . RidBITSIZE        
        , Stk . RidBYTESIZE       
        , Stk . StkPdSize
        , Stk . StkPd_Long_Size          
        , Stk . RidFIRST          
        , Stk . RidLAST           
        , Stk . RidNUMBER         
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 1 
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetTypeOrValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetTyp { }  
           END (*WITH*)

        | Stk . RidFLOAT          
        , Stk . RidISTYPE         
        , Stk . RidLOOPHOLE       
        , Stk . RidNARROW         
        , Stk . RidVAL           
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 2
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetType
           END (*WITH*)

        | Stk . RidMAX            
        , Stk . RidMIN            
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 2 
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetValue  
           END (*WITH*)

        | Stk . RidNEW            
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := ActualsCtAtLeastOne 
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetType 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetValue
                     (* Could be bindings. *)  
           END (*WITH*)

        | Stk . RidORD            
        , Stk . RidTYPECODE       
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 1 
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetTyp { } 
                     (* Could be bindings. *)  
           END (*WITH*)

        | Stk . RidSUBARRAY       
        , Stk . StkPdExtract
        , Stk . StkPd_Long_Extract       
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 3 
           ; WProp . OpExprKind := Ekt . EkValue 
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetValue
           (* 3rd opnd is implicitly EkSetValue. *) 
           END (*WITH*) 

        | Stk . RidDISPOSE        
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 1
           ; WProp . OpExprKind := Ekt . EkNull
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetTyp { } 
           END (*WITH*)

        | Stk . RidDEC           
        , Stk . RidINC            
        => WITH WProp = GOpPropertiesArray [ ROp]
           DO WProp . OpOpndCt := 2
           ; WProp . OpExprKind := Ekt . EkNull
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetValue 
           END (*WITH*) 

        | Stk . StkPdPlus
        , Stk . StkPd_Long_Plus          
        , Stk . StkPdTimes        
        , Stk . StkPdMinus
        , Stk . StkPd_Long_Minus         
        , Stk . StkPdDivide
        , Stk . StkPd_Long_Divide        
        , Stk . StkPdMod
        , Stk . StkPd_Long_Mod           
        , Stk . StkPdLT
        , Stk . StkPd_Long_LT            
        , Stk . StkPdLE
        , Stk . StkPd_Long_LE            
        , Stk . StkPdGT
        , Stk . StkPd_Long_GT            
        , Stk . StkPdGE
        , Stk . StkPd_Long_GE            
        , Stk . StkPdAnd
        , Stk . StkPd_Long_And          
        , Stk . StkPdOr
        , Stk . StkPd_Long_Or           
        , Stk . StkPdXor
        , Stk . StkPd_Long_Xor           
        , Stk . StkPdNot
        , Stk . StkPd_Long_Not           
        , Stk . StkPdShift
        , Stk . StkPd_Long_Shift         
        , Stk . StkPdLeftShift
        , Stk . StkPd_Long_LeftShift     
        , Stk . StkPdRightShift
        , Stk . StkPd_Long_RightShift    
        , Stk . StkPdRotate
        , Stk . StkPd_Long_Rotate        
        , Stk . StkPdLeftRotate
        , Stk . StkPd_Long_LeftRotate    
        , Stk . StkPdRightRotate
        , Stk . StkPd_Long_RightRotate 
         => WITH WProp = GOpPropertiesArray [ ROp ]
           DO WProp . OpOpndCt := 2 
           ; WProp . OpExprKind := Ekt . EkValue
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetValue 
           END (*WITH*)

        | Stk . StkPdInsert
        , Stk . StkPd_Long_Insert       
        => WITH WProp = GOpPropertiesArray [ ROp ]
           DO WProp . OpOpndCt := 4 
           ; WProp . OpExprKind := Ekt . EkValue 
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetValue 
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetValue
           (* 3rd and 4th opnds are implicitly EkSetValue. *) 
           END (*WITH*)
        | Stk . StkRTUniqueBrand  
         => WITH WProp = GOpPropertiesArray [ ROp ]
           DO WProp . OpOpndCt := 0
           ; WProp . OpExprKind := Ekt . EkBrand
           ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetTyp { }  
           ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetTyp { } 
           END (*WITH*) 
        ELSE
(* TODO: CM3 warns not all values handled.  Find out what is missing. *) 
          WITH WProp = GOpPropertiesArray [ ROp ]
          DO WProp . OpOpndCt := 0 
          ; WProp . OpExprKind := Ekt . EkNull 
          ; WProp . OpLtOpndKindsAllowed := FM3Exprs . EkSetTyp { } 
          ; WProp . OpRtOpndKindsAllowed := FM3Exprs . EkSetTyp { } 
          END (*WITH*)
        END (*CASE*) 
      END (*FOR*) 
    END InitOperatorProperties

; PROCEDURE NewOpExpr
    ( Opcode : FM3SrcToks . TokTyp ; Position : FM3Base . tPosition )
  : FM3Exprs . ExprTyp

  = VAR LResult : FM3Exprs . ExprTyp

  ; BEGIN
      CASE Opcode OF
      | Stk . StkPdExtract
      , Stk . StkPd_Long_Extract
      , Stk . StkPdInsert
      , Stk . StkPd_Long_Insert
      => LResult := NEW ( FM3Exprs . ExprTyp )
      ; LResult . ExpOpnd3 := NIL 
      ; LResult . ExpOpnd4 := NIL
      | Stk . StkRTUniqueBrand
      => LResult := NEW ( FM3Exprs . ExprTyp )
      ELSE LResult := NEW ( FM3Exprs . ExprTyp )
      END (*CASE *) 
    ; LResult . ExpOpnd1 := NIL 
    ; LResult . ExpOpnd2 := NIL
(* FIXME: where do these Opnd* get set? *) 
    ; WITH WProps = GOpPropertiesArray [ Opcode ]
      DO 
        LResult . ExpType := GOpTypesArray [ Opcode ]   
      ; LResult . ExpReachedDeclNoSet := IntSets . Empty ( )  
      ; LResult . ExpPosition := Position 
      ; LResult . ExpOpcode := Opcode 
      ; LResult . ExpUpKind := WProps . OpExprKind  
      ; LResult . ExpBinOpActualsCt := WProps . OpOpndCt  
      ; LResult . ExpBinOpLtOpndKindsAllowed := WProps . OpLtOpndKindsAllowed 
      ; LResult . ExpBinOpRtOpndKindsAllowed := WProps . OpRtOpndKindsAllowed 
      ; FM3Utils . ContribToHashI ( LResult . ExpHash , Opcode ) 
      ; RETURN LResult
      END (*WITH*) 
    END NewOpExpr 

(*EXPORTED.*)
; PROCEDURE BuiltinExpr
    ( Opcode : FM3SrcToks . TokTyp ; Position := FM3Base . PositionNull )
  : FM3Exprs . ExprTyp (* NIL if not an Id denoting an ExprTyp *) 

  = VAR LResult : FM3Exprs . ExprTyp

  ; BEGIN
      CASE Opcode OF
      | FM3SrcToks . StkMinStatic .. FM3SrcToks . StkMaxStatic
      => LResult := GStaticArray [ Opcode ] 

      | FM3SrcToks . StkMinOperation .. FM3SrcToks . StkMaxOperation
      => LResult := NewOpExpr ( Opcode , Position )

      ELSE LResult := NIL
      END (*CASE*)
    ; RETURN LResult
    END BuiltinExpr

(*EXPORTED.*)
; PROCEDURE IsOperationTok ( Tok : FM3SrcToks . TokTyp ) : BOOLEAN 

  = BEGIN (*IsOperationTok*)
      IF Tok < FM3SrcToks . StkMinOperation THEN RETURN FALSE END (*IF*)  
    ; IF Tok > FM3SrcToks . StkMaxOperation THEN RETURN FALSE END (*IF*)
    ; RETURN TRUE 
    END IsOperationTok
    
(*EXPORTED.*)
; PROCEDURE OpExprKind ( BuiltinOpcode : FM3SrcToks . TokTyp )
  : FM3Exprs . ExprKindTyp
  (* Meaningful only for reserved ident (starting with StkRid) or something
     that can occcur in a standard interface (starting with StkPd).
  *)  

  = BEGIN (*OpExprKind*)
      IF IsOperationTok ( BuiltinOpcode )
      THEN RETURN FM3Exprs . ExprKindTyp . EkValue
      END (*IF*) 
    ; IF BuiltinOpcode >= FM3SrcToks . StkMinType
         AND BuiltinOpcode <= FM3SrcToks . StkMaxType
      THEN RETURN FM3Exprs . ExprKindTyp . EkType
      ELSE RETURN FM3Exprs . ExprKindTyp . EkNull
      END (*IF*)
    END OpExprKind

; PROCEDURE Init (  )

  = BEGIN (*Init*)
      InitTypeExprs ( )
    ; InitConstExprs ( )
    ; InitOperatorTypes ( ) 
    ; InitOperatorProperties ( ) 
    END Init
      
; BEGIN
    Init ( ) 
  END FM3Builtins
.
 