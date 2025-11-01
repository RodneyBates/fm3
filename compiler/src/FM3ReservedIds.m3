
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Expressions for things named by Modula-3 reserved identifiers. *)

(* COMPLETEME.  This needs major completion. *) 

MODULE FM3ReservedIds

; IMPORT FM3Exprs AS Ex 
; IMPORT FM3SrcToks
; IMPORT FM3SrcToks  AS Stk

; TYPE Ekt = Ex . ExprKindTyp 
; TYPE Est = Ex . ExprStateTyp

; PROCEDURE Init ( )

  = VAR IniExprRef : Ex . ExprTyp 

  ; PROCEDURE InitRidABS ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidABS


  ; PROCEDURE InitRidADDRESS ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidADDRESS


  ; PROCEDURE InitRidADR ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidADR


  ; PROCEDURE InitRidADRSIZE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidADRSIZE


  ; PROCEDURE InitRidBITSIZE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidBITSIZE


  ; PROCEDURE InitRidBOOLEAN ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidBOOLEAN


  ; PROCEDURE InitRidBYTESIZE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidBYTESIZE


  ; PROCEDURE InitRidCARDINAL ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidCARDINAL


  ; PROCEDURE InitRidCEILING ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidCEILING


  ; PROCEDURE InitRidCHAR ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidCHAR


  ; PROCEDURE InitRidDEC ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidDEC


  ; PROCEDURE InitRidDISPOSE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidDISPOSE


  ; PROCEDURE InitRidEXTENDED ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidEXTENDED


  ; PROCEDURE InitRidFALSE ( ) 
    = BEGIN 
     (* IniExprRef . ExpIsConst := TRUE   
      ; IniExprRef . ExpConstValueIsKnown := TRUE  
      ; IniExprRef . ExpScalarConstVal := 0L
      ; IniExprRef . Exp := 
   *) END InitRidFALSE


  ; PROCEDURE InitRidFIRST ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidFIRST


  ; PROCEDURE InitRidFLOAT ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidFLOAT


  ; PROCEDURE InitRidFLOOR ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidFLOOR


  ; PROCEDURE InitRidINC ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidINC


  ; PROCEDURE InitRidINTEGER ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidINTEGER


  ; PROCEDURE InitRidISTYPE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidISTYPE


  ; PROCEDURE InitRidLAST ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidLAST


  ; PROCEDURE InitRidLONGCARD ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidLONGCARD


  ; PROCEDURE InitRidLONGINT ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidLONGINT


  ; PROCEDURE InitRidLONGREAL ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidLONGREAL


  ; PROCEDURE InitRidLOOPHOLE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidLOOPHOLE


  ; PROCEDURE InitRidMAX ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidMAX


  ; PROCEDURE InitRidMIN ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidMIN


  ; PROCEDURE InitRidMUTEX ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidMUTEX


  ; PROCEDURE InitRidNARROW ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidNARROW


  ; PROCEDURE InitRidNEW ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidNEW


  ; PROCEDURE InitRidNIL ( ) 
    = BEGIN 
     (* IniExprRef . ExpIsConst := TRUE   
      ; IniExprRef . ExpConstValueIsKnown := TRUE  
      ; IniExprRef . ExpScalarConstVal := 0L
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidNIL


  ; PROCEDURE InitRidNULL ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidNULL


  ; PROCEDURE InitRidNUMBER ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidNUMBER


  ; PROCEDURE InitRidORD ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidORD


  ; PROCEDURE InitRidREAL ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidREAL


  ; PROCEDURE InitRidREFANY ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidREFANY


  ; PROCEDURE InitRidROUND ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidROUND


  ; PROCEDURE InitRidSUBARRAY ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidSUBARRAY


  ; PROCEDURE InitRidTEXT ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidTEXT


  ; PROCEDURE InitRidTRUE ( ) 
    = BEGIN 
     (* IniExprRef . ExpIsConst := TRUE   
      ; IniExprRef . ExpConstValueIsKnown := TRUE  
      ; IniExprRef . ExpScalarConstVal := 0L
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidTRUE


  ; PROCEDURE InitRidTRUNC ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidTRUNC


  ; PROCEDURE InitRidTYPECODE ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidTYPECODE


  ; PROCEDURE InitRidVAL ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidVAL


  ; PROCEDURE InitRidWIDECHAR ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidWIDECHAR


  ; PROCEDURE InitRidROOT ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidROOT


  ; PROCEDURE InitRidUNTRACEDROOT ( ) 
    = BEGIN 
     (* IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
      ; IniExprRef . Exp := 
   *) END InitRidUNTRACEDROOT


  ; PROCEDURE InitRes
      ( Opcode : FM3SrcToks . TokTyp
      ; ExprRef : Ex . ExprTyp  
      ; Kind : Ex . ExprKindTyp
      ) 

    = BEGIN
        WITH WExprRef = ExprRefs [ Opcode ]
        DO WExprRef := ExprRef
        ; IniExprRef := ExprRef (* For use by InitRid* procs. *) 
        ; WExprRef . ExpSelfExprNo := - Opcode 
        ; WExprRef . ExpOpcode := Opcode
        ; WExprRef . ExpKind := Kind
        ; WExprRef . ExpIsLegalRecursive := FALSE (*Irrelevant*)
        ; WExprRef . ExpState := Est . EsResolved 
        END (*WITH*) 
      END InitRes 

  ; BEGIN (*Init*)
      InitRes ( Stk . RidABS , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidABS ( )

    ; InitRes ( Stk . RidADDRESS , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidADDRESS ( )

    ; InitRes ( Stk . RidADR , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidADR ( )

    ; InitRes ( Stk . RidADRSIZE , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidADRSIZE ( )

    ; InitRes ( Stk . RidBITSIZE , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidBITSIZE ( )

    ; InitRes ( Stk . RidBOOLEAN , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidBOOLEAN ( )

    ; InitRes ( Stk . RidBYTESIZE , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidBYTESIZE ( )

    ; InitRes ( Stk . RidCARDINAL , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidCARDINAL ( )

    ; InitRes ( Stk . RidCEILING , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidCEILING ( )

    ; InitRes ( Stk . RidCHAR , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidCHAR ( )

    ; InitRes ( Stk . RidDEC , NEW ( Ex . ExprTyp ) , Ekt . EkProc )
    ; InitRidDEC ( )

    ; InitRes ( Stk . RidDISPOSE , NEW ( Ex . ExprTyp ) , Ekt . EkProc )
    ; InitRidDISPOSE ( )

    ; InitRes ( Stk . RidEXTENDED , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidEXTENDED ( )

    ; InitRes ( Stk . RidFALSE , NEW ( Ex . ExprTyp ) , Ekt . EkLiteral )
    ; InitRidFALSE ( )

    ; InitRes ( Stk . RidFIRST , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidFIRST ( )

    ; InitRes ( Stk . RidFLOAT , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidFLOAT ( )

    ; InitRes ( Stk . RidFLOOR , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidFLOOR ( )

    ; InitRes ( Stk . RidINC , NEW ( Ex . ExprTyp ) , Ekt . EkProc )
    ; InitRidINC ( )

    ; InitRes ( Stk . RidINTEGER , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidINTEGER ( )

    ; InitRes ( Stk . RidISTYPE , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidISTYPE ( )

    ; InitRes ( Stk . RidLAST , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidLAST ( )

    ; InitRes ( Stk . RidLONGCARD , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidLONGCARD ( )

    ; InitRes ( Stk . RidLONGINT , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidLONGINT ( )

    ; InitRes ( Stk . RidLONGREAL , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidLONGREAL ( )

    ; InitRes ( Stk . RidLOOPHOLE , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidLOOPHOLE ( )

    ; InitRes ( Stk . RidMAX , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidMAX ( )

    ; InitRes ( Stk . RidMIN , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidMIN ( )

    ; InitRes ( Stk . RidMUTEX , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidMUTEX ( )

    ; InitRes ( Stk . RidNARROW , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidNARROW ( )

    ; InitRes ( Stk . RidNEW , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidNEW ( )

    ; InitRes ( Stk . RidNIL , NEW ( Ex . ExprTyp ) , Ekt . EkLiteral )
    ; InitRidNIL ( )

    ; InitRes ( Stk . RidNULL , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidNULL ( )

    ; InitRes ( Stk . RidNUMBER , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidNUMBER ( )

    ; InitRes ( Stk . RidORD , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidORD ( )

    ; InitRes ( Stk . RidREAL , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidREAL ( )

    ; InitRes ( Stk . RidREFANY , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidREFANY ( )

    ; InitRes ( Stk . RidROUND , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidROUND ( )

    ; InitRes ( Stk . RidSUBARRAY , NEW ( Ex . ExprTyp) , Ekt . EkFunc )
    ; InitRidSUBARRAY ( )

    ; InitRes ( Stk . RidTEXT , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidTEXT ( )

    ; InitRes ( Stk . RidTRUE , NEW ( Ex . ExprTyp ) , Ekt . EkLiteral )
    ; InitRidTRUE ( )

    ; InitRes ( Stk . RidTRUNC , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidTRUNC ( )

    ; InitRes ( Stk . RidTYPECODE , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidTYPECODE ( )

    ; InitRes ( Stk . RidVAL , NEW ( Ex . ExprTyp ) , Ekt . EkFunc )
    ; InitRidVAL ( )

    ; InitRes ( Stk . RidWIDECHAR , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidWIDECHAR ( )

    ; InitRes ( Stk . RidROOT , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidROOT ( )

    ; InitRes ( Stk . RidUNTRACEDROOT , NEW ( Ex . ExprTyp ) , Ekt . EkType )
    ; InitRidUNTRACEDROOT ( )
    END Init 

; BEGIN
    Init ( ) 
  END FM3ReservedIds
. 