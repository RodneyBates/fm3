 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE DumpWork

; IMPORT Fmt
; IMPORT Long
; IMPORT Params 
; IMPORT Pathname
; IMPORT Wr 

; IMPORT IntSets 

; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 

; IMPORT FM3Compress
; IMPORT FM3SrcToks 
; IMPORT FM3IntToks 
; IMPORT RdBackFile

; VAR GResourceDirName : TEXT 
; VAR GIntFilePrefix : TEXT 
; VAR GSetsName : TEXT
; VAR GSetsFullName : TEXT

; VAR GTokSetTemp : IntSets . T 
; VAR GTokSetPatch : IntSets . T 
; VAR GTokSetGE1Arg : IntSets . T 
; VAR GTokSetGE2Args : IntSets . T 
; VAR GTokSetGE3Args : IntSets . T 
; VAR GTokSetGE4Args : IntSets . T 
; VAR GTokSetGE5Args : IntSets . T 
; VAR GTokSetGE6Args : IntSets . T 
; VAR GResourcesLoaded := FALSE  

; PROCEDURE DumpPrefix
    ( RBT : RdBackFile . T ; WrT : Wr . T )
  : LONGINT
  RAISES { RdBackFile .  BOF }
    
  = VAR LLengthL , LValueL : LONGINT

  ; BEGIN
      LLengthL := RdBackFile . LengthL ( RBT ) 
    ; LValueL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LLengthL ) , 6 ) )
    ; Wr . PutText ( WrT , " 16_" ) 
    ; Wr . PutText
        ( WrT
        , Fmt . Pad ( Fmt . LongInt ( LValueL , 16 ) , 16 , padChar := '0' )
        )
    ; Wr . PutText ( WrT , " " ) 
    ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LValueL ) , 21 ) )
    ; RETURN LValueL 
    END DumpPrefix

(*EXPORTED:*) 
; PROCEDURE DumpNumericBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = BEGIN
(* COMPLETEME: *) 
    END DumpNumericBwd 

; CONST OpndIndent = "   "

(*EXPORTED:*) 
; PROCEDURE DumpInterpretBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = VAR LTokenL : LONGINT
  ; VAR LToken : INTEGER 

  ; PROCEDURE DibPutOpnd ( Label : TEXT ) RAISES { RdBackFile . BOF } 

    = VAR LOpndL : LONGINT
    
    ; BEGIN
        LOpndL := DumpPrefix ( RBT , WrT )
      ; Wr . PutText ( WrT , OpndIndent ) 
      ; Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LOpndL ) )
      ; Wr . PutText ( WrT , Wr . EOL )
      END DibPutOpnd  

  ; BEGIN
      LoadResources ( ) 
    ; TRY 
        LOOP
          LTokenL := DumpPrefix ( RBT , WrT )
        ; IF Long . LE ( LTokenL , VAL ( LAST ( INTEGER ) , LONGINT ) )
          THEN (* Nonneg INTEGER *) 
            LToken := VAL ( LTokenL , INTEGER ) 
      
          ; IF FM3SrcToks . StkIdent  <= LToken  
               AND LToken <= FM3SrcToks . StkWideCharLit 
            THEN (* Variable terminal. *)
              Wr . PutText ( WrT , " " ) 
            ; Wr . PutText ( WrT , FM3SrcToks . Image ( LToken ) )
            ; Wr . PutText ( WrT , Wr . EOL )

            ; DibPutOpnd ( "Atom: " )  
            ; DibPutOpnd ( "Line: " )  
            ; DibPutOpnd ( "Column: " )  
(*
            ; CASE Token OF
     (* Keep these consistent with Fm3ParsePass.UnnestStk: *) 
           (* | FM3SrcToks . StkIdent => *) 
              | FM3SrcToks . StkIntLit 
              , FM3SrcToks . StkLongIntLit 
              , FM3SrcToks . StkBasedLit 
              , FM3SrcToks . StkLongBasedLit
              , FM3SrcToks . StkRealLit 
              , FM3SrcToks . StkLongRealLit 
              , FM3SrcToks . StkExtendedLit 
                => PushOACharsBwd ( WRdBack , FM3Scanner . Attribute . SaChars )
              | FM3SrcToks . StkTextLit 
                => PushOACharsBwd
                     ( WRdBack , FM3Scanner . Attribute . SaChars )
              | FM3SrcToks . StkWideTextLit 
                => PushOAWideCharsBwd
                     ( WRdBack , FM3Scanner . Attribute . SaWideChars )
              | FM3SrcToks . StkCharLit 
              , FM3SrcToks . StkWideCharLit 
                => FM3Compress . PutBwd
                     ( WRdBack
                     , VAL ( ORD ( FM3Scanner . Attribute . SaWCh ) , LONGINT ) 
                     )
           (* | FM3SrcToks . StkLexErrChars => Throw these away, for now. *) 
              ELSE
              END (*CASE*)
*)             
            ; Wr . PutText ( WrT , Wr . EOL )

            ELSIF FM3SrcToks . TkMinTok <= LToken  
                  AND LToken <= FM3SrcToks . TkMaxTok 
            THEN (* Constant terminal: (will this ever happen? *)
              Wr . PutText ( WrT , " " ) 
            ; Wr . PutText ( WrT , FM3SrcToks . Image ( LToken ) )
            ; Wr . PutText ( WrT , Wr . EOL )

            ELSIF FM3IntToks . TkMinTok <= LToken  
                  AND LToken <= FM3IntToks . TkMaxTok 
            THEN (* Internal token. *)
              Wr . PutText ( WrT , " " ) 
            ; Wr . PutText ( WrT , FM3IntToks . Image ( LToken ) ) 
            ; Wr . PutText ( WrT , Wr . EOL )
            ; IF IntSets . IsElement ( LToken , GTokSetPatch )
              THEN DibPutOpnd ( "Patch coord: " )
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE1Arg )
              THEN
                DibPutOpnd ( "Opnd 0: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE2Args )
              THEN DibPutOpnd ( "Opnd 1: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE3Args )
              THEN DibPutOpnd ( "Opnd 2: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE4Args )
              THEN DibPutOpnd ( "Opnd 3: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE5Args )
              THEN DibPutOpnd ( "Opnd 4: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE6Args )
              THEN DibPutOpnd ( "Opnd 5: " ) 
              END (*IF*) 
            ; Wr . PutText ( WrT , Wr . EOL )
            
            ELSE (* Don't know what this is. *) 
              Wr . PutText ( WrT , " unknown>" ) 
            ; Wr . PutText ( WrT , Wr . EOL )
            END (*IF*)

          ELSE (* out of nonneg INTEGER range *) 
            Wr . PutText ( WrT , " unknown, negative>" ) 
          ; Wr . PutText ( WrT , Wr . EOL )
          END (*IF*)
        END (*LOOP*)
      EXCEPT
      | RdBackFile . BOF
      => Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( 0L) , 6 ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (*EXCEPT*)
    END DumpInterpretBwd

                IF IntSets . IsElement ( LToken , GTokSetPatch )
                THEN DobPutOpnd ( "Patch coord: " )
                END (*IF*) 
              ; IF IntSets . IsElement ( LToken , GTokSetGE1Arg )
                THEN
                  DobPutOpnd ( "Opnd 0: " ) 
                END (*IF*) 
              ; IF IntSets . IsElement ( LToken , GTokSetGE2Args )
                THEN DobPutOpnd ( "Opnd 1: " ) 
                END (*IF*) 
              ; IF IntSets . IsElement ( LToken , GTokSetGE3Args )
                THEN DobPutOpnd ( "Opnd 2: " ) 
                END (*IF*) 
              ; IF IntSets . IsElement ( LToken , GTokSetGE4Args )
                THEN DobPutOpnd ( "Opnd 3: " ) 
                END (*IF*) 
              ; IF IntSets . IsElement ( LToken , GTokSetGE5Args )
                THEN DobPutOpnd ( "Opnd 4: " ) 
                END (*IF*) 
              ; IF IntSets . IsElement ( LToken , GTokSetGE6Args )
                THEN DobPutOpnd ( "Opnd 5: " ) 
                END (*IF*) 
              ; Wr . PutText ( WrT , Wr . EOL )
              END (*LOOP*) 

; PROCEDRE DobIntArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT

  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutText ( WrT , Fmt . Long ( LArgL ) ) 
    END DobIntArg 

; PROCEDRE DobLineNoArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT

  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutChar ( WrT , 'L' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LArgL ) ) 
    END DobLineNoArg 

; PROCEDRE DobColNoArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT

  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutChar ( WrT , 'H' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LArgL ) ) 
    END DobColNoArg 

; PROCEDRE DobPosArg ( ArgNo : INTEGER )
  = VAR LLineL , LColL: LONGINT

  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LLineL := FM3Compress . GetBwd ( RBT )
    ; LColL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutChar ( WrT , '[' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LLineL ) ) 
    ; Wr . PutChar ( WrT , ',' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LLineL ) ) 
    ; Wr . PutChar ( WrT , ']' ) 

    END DobPosArg 

; PROCEDRE DobBoolArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT
  ; VAR LVal : TEXT 

  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; IF LArgL = 0L THEN LVal := "FALSE" ELSE LVal := "TRUE" END (*IF*)  
    ; Wr . PutText ( WrT , LVal) 
    END DobBoolArg 

; PROCEDRE DobIdentAtomArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT
(*TODO: track down and display the atom's ident. *) 
  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutChar ( WrT , 'A' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LArgL ) ) 
    END DobAtomIdentArg 

; PROCEDRE DobDeclNoArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT
(*TODO: track down and display the decl's ident. *) 
  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutChar ( WrT , 'D' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LArgL ) ) 
    END DobDeclNoArg 

; PROCEDRE DobCoordArg ( ArgNo : INTEGER )
  = VAR LArgL : LONGINT
(* TODO: Show this in hex (and decimal both? *) 
  ; BEGIN
      IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' )
    ; LArgL := FM3Compress . GetBwd ( RBT )
    ; Wr . PutChar ( WrT , 'C' ) 
    ; Wr . PutText ( WrT , Fmt . Long ( LArgL ) ) 
    END DodCoordArg 

(*EXPORTED:*) 
; PROCEDURE DisassWOperandsBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = VAR LTokenL : LONGINT
  ; VAR LToken : INTEGER 

  ; BEGIN (* DisassWOperandsBwd *) 
      LoadResources ( ) 
    ; TRY 
        LOOP (* Thru' token-with-operands groups. *) 
          LTokenL := DumpPrefix ( RBT , WrT )
        ; IF Long . LE ( LTokenL , VAL ( LAST ( INTEGER ) , LONGINT ) )
          THEN (* Nonneg INTEGER *) 
            LToken := VAL ( LTokenL , INTEGER ) 

          ; CASE LToken OF
            | FM3SrcToks . StkIdent .. FM3SrcToks . StkWideCharLit
            => (* Variable terminal. *)
                Wr . PutText ( WrT , " " ) 
              ; Wr . PutText ( WrT , FM3SrcToks . Image ( LToken ) )
              ; Wr . PutText ( WrT , Wr . EOL )

              ; DobPutOpnd ( "Atom: " )  
              ; DobPutOpnd ( "Line: " )  
              ; DobPutOpnd ( "Column: " )  
              ; Wr . PutText ( WrT , Wr . EOL )
            (* Keep these consistent with Fm3ParsePass.UnnestStk: *) 
         (* | FM3SrcToks . StkIntLit 
            , FM3SrcToks . StkLongIntLit 
            , FM3SrcToks . StkBasedLit 
            , FM3SrcToks . StkLongBasedLit
            , FM3SrcToks . StkRealLit 
            , FM3SrcToks . StkLongRealLit 
            , FM3SrcToks . StkExtendedLit 
              => PushOACharsBwd ( WRdBack , FM3Scanner . Attribute . SaChars )
            | FM3SrcToks . StkTextLit 
              => PushOACharsBwd
                   ( WRdBack , FM3Scanner . Attribute . SaChars )
            | FM3SrcToks . StkWideTextLit 
              => PushOAWideCharsBwd
                   ( WRdBack , FM3Scanner . Attribute . SaWideChars )
            | FM3SrcToks . StkCharLit 
            , FM3SrcToks . StkWideCharLit 
              => FM3Compress . PutBwd
                   ( WRdBack
                   , VAL ( ORD ( FM3Scanner . Attribute . SaWCh ) , LONGINT ) 
                   )
            | FM3SrcToks . StkLexErrChars => Throw these away, for now. 
          *) 

            | FM3SrcToks . TkMinTok .. FM3SrcToks . TkMaxTok 
            =>  (* Constant terminal: (will this ever happen? *)
                Wr . PutText ( WrT , " " ) 
              ; Wr . PutText ( WrT , FM3SrcToks . Image ( LToken ) )
              ; Wr . PutText ( WrT , Wr . EOL )

            | FM3IntToks . TkMinTok .. FM3IntToks . TkMaxTok 
            => (* Internal token. *)
              Wr . PutText ( WrT , " " ) 
            ; Wr . PutText ( WrT , FM3IntToks . Image ( LToken ) ) 
            ; Wr . PutText ( WrT , Wr . EOL )

            (* Display operands. *) 
            ; LArgCt := ArgCt ( LToken )
            ; LArgNo := 0
            ; LArgRdT := TextRd . New ( ArgString ( LToken ) )
            ; LArgChar := Rd . GetChar ( LArgRdT ) 
            ; Wr . PutChar ( WrT , '(' ) 
            ; LOOP (* Thru' operands. *)
                IF Rd . EOF ( LArgString )
                THEN WHILE LArgNo < LArgCt
                  DO (* Counted argument. *)
                    DobIntArg ( LArgNo )
                  ; INC ( LArgNo ) 
                  END (*WHILE*)
                ; EXIT 
                ELSE (* A string-defined arg. *)
                  (* INVARIANT: NOT Rd . EOF ( LArgRdT ) *) 
                  LArgChar := Rd . GetChar ( LArgRdT )
                ; IF LArgChar = '_'
                  THEN
                    IF NOT Rd . EOF ( LArgRdT )
                    THEN 
                      LArgChar := Rd . GetChar ( LArgRdT )
                    ; CASE LArgChar OF
                      | 'I' => DobIntArg ( LArgNo )
                      | 'B' => DobBoolArg ( )
                      | 'D' => DobDeclNoArg ( )
                      | 'P' => DobPositiohArg ( )
                      | 'B' => DobColNoArg ( )
                      | 'L' => (* Is it "_L_B"? 
                          IF Rd . EOF ( LArgRdT )
                          THEN DobLineNoArg ( )
                          ELSE
                            LArgChar := Rd . GetChar ( LArgRdT )
                          ; IF LArgChar = '_' AND NOT Rd . EOF ( LArgRdT )
                            THEN
                              LArgChar := Rd . GetChar ( LArgRdT )
                            ; IF LArgChar = 'B'
                              THEN DobPositionArg ( )
                              END (*IF*) 
                            END (*IF*) 
                          END (*IF*)
                      ELSE
                      END (*CASE*) 
                    END (*IF*) 
                  END (*IF*) 
                END (*IF*)
              END (*LOOP*) 
            ; Wr . PutChar ( WrT , ')' ) 
            ELSE (* Don't know what this token is. *) 
              Wr . PutText ( WrT , " unknown>" ) 
            ; Wr . PutText ( WrT , Wr . EOL )
            END (*CASE*)

          ELSE (* out of nonneg INTEGER range *) 
            Wr . PutText ( WrT , " unknown, negative>" ) 
          ; Wr . PutText ( WrT , Wr . EOL )
          END (*IF*)
        END (*LOOP*)
      EXCEPT
      | RdBackFile . BOF
      => Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( 0L) , 6 ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (*EXCEPT*)
    END DisassWOperandsBwd

; PROCEDURE LoadResources ( )

  = BEGIN
      IF NOT GResourcesLoaded
      THEN
        GResourceDirName
          := FM3SharedUtils . SibDirectoryPath ( Params . Get ( 0 )  , "lib" ) 

      ; GIntFilePrefix := "FM3IntToks"
      ; GSetsName := GIntFilePrefix & "Sets"
      ; GSetsFullName
          := Pathname . Join ( GResourceDirName , GSetsName , "pkl" )
      ; FM3SharedUtils . ReadSets
          ( GSetsFullName
          , FM3SharedGlobals . FM3FileKindTokSetsPkl
          , GTokSetTemp
          , GTokSetPatch
          , GTokSetGE1Arg
          , GTokSetGE2Args
          , GTokSetGE3Args
          , GTokSetGE4Args
          , GTokSetGE5Args
          , GTokSetGE6Args
          ) 
      ; GResourcesLoaded := TRUE 
      END (*IF*) 
    END LoadResources   

; BEGIN
    GResourcesLoaded := FALSE  
  END DumpWork
.

