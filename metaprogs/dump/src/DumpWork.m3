 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE DumpWork

; IMPORT Fmt
; IMPORT Long
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
; VAR GTokSet1Arg : IntSets . T 
; VAR GTokSet2Args : IntSets . T 
; VAR GTokSet3Args : IntSets . T 
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
            ; IF IntSets . IsElement ( LToken , GTokSet1Arg )
              THEN
                DibPutOpnd ( "Opnd 0: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSet2Args )
              THEN DibPutOpnd ( "Opnd 1: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSet3Args )
              THEN DibPutOpnd ( "Opnd 2: " ) 
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

; PROCEDURE LoadResources ( )

  = BEGIN
      IF NOT GResourcesLoaded
      THEN
        GResourceDirName := "." 
      ; GIntFilePrefix := "FM3IntToks"
      ; GSetsName := GIntFilePrefix & "Sets"
      ; GSetsFullName
          := Pathname . Join ( GResourceDirName , GSetsName , "pkl" )
      ; FM3SharedUtils . ReadSets
          ( GSetsFullName
          , FM3SharedGlobals . FM3FileKindTokSetsPkl
          , GTokSetTemp
          , GTokSetPatch
          , GTokSet1Arg
          , GTokSet2Args
          , GTokSet3Args
          ) 
      ; GResourcesLoaded := TRUE 
      END (*IF*) 
    END LoadResources   

; BEGIN
    GResourcesLoaded := FALSE  
  END DumpWork
.

