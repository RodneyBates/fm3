 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3DisAsm

; IMPORT Fmt
; IMPORT Fmt AS FM3Fmt 
; IMPORT Long
; IMPORT Params 
; IMPORT Pathname
; IMPORT Rd 
; IMPORT Text 
; IMPORT TextRd  
; IMPORT Wr 

; IMPORT IntSets 

; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3CLToks AS Clt
; IMPORT FM3Compress
; IMPORT FM3Decls 
; IMPORT FM3SrcToks 
; IMPORT FM3IntToks
; IMPORT FM3OpenArray_Char
; IMPORT FM3UnsafeUtils 
; IMPORT RdBackFile
; IMPORT FM3Units 
; IMPORT FM3Utils 

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

; VAR GitRef : FM3Units . UnitRefTyp

; PROCEDURE FetchOptions ( )  

  = BEGIN
    Verbose := Clt . CltDisAsmVerbose IN FM3CLOptions . OptionTokSet 
    END FetchOptions 

; PROCEDURE ReadAndPutPrefix
    ( RBT : RdBackFile . T ; WrT : Wr . T )
  : LONGINT
  RAISES { RdBackFile . BOF }
    
  = VAR LLengthL , LValueL : LONGINT

  ; BEGIN
      LLengthL := RdBackFile . LengthL ( RBT ) 
    ; LValueL := FM3Compress . GetBwd ( RBT )
    ; IF Verbose
      THEN
        Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LLengthL ) , 6 ) )
      ; Wr . PutText ( WrT , " 16_" ) 
      ; Wr . PutText
          ( WrT
          , Fmt . Pad
              ( Fmt . LongUnsigned ( LValueL , 16 ) , 16 , padChar := '0' )
          )
      ; Wr . PutText ( WrT , " " ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LValueL ) , 21 ) )
      END (*IF*) 
    ; RETURN LValueL 
    END ReadAndPutPrefix

(*EXPORTED:*) 
; PROCEDURE DumpNumericBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = BEGIN
      FetchOptions ( ) 
(* COMPLETEME: *) 
    END DumpNumericBwd 

; CONST OpndIndent = "   "

(*EXPORTED:*)
; PROCEDURE DumpInterpretBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = VAR LTokenL : LONGINT
  ; VAR LToken : INTEGER 

  ; PROCEDURE DibPutOpnd ( Label : TEXT )
    : INTEGER
    RAISES { RdBackFile . BOF } 

    = VAR LOpndL : LONGINT
    ; VAR LOpndI : INTEGER 
    
    ; BEGIN
        LOpndL := ReadAndPutPrefix ( RBT , WrT )
      ; LOpndI := VAL ( LOpndL , INTEGER ) 
      ; Wr . PutText ( WrT , OpndIndent ) 
      ; Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LOpndL ) )
      ; Wr . PutText ( WrT , Wr . EOL )
      ; RETURN LOpndI 
      END DibPutOpnd  

  ; BEGIN (* DumpInterpretBwd *) 
      FM3SharedUtils . LoadSets ( ) (* Probably redundant. *)
    ; FetchOptions ( ) 

    ; TRY 
        LOOP
          LTokenL := ReadAndPutPrefix ( RBT , WrT )
        ; IF Long . LE ( LTokenL , VAL ( LAST ( INTEGER ) , LONGINT ) )
          THEN (* Nonneg INTEGER *) 
            LToken := VAL ( LTokenL , INTEGER ) 
      
          ; IF FM3SrcToks . StkIdent  <= LToken  
               AND LToken <= FM3SrcToks . StkWideCharLit 
            THEN (* Variable terminal. *)
              Wr . PutText ( WrT , " " ) 
            ; Wr . PutText ( WrT , FM3SrcToks . Image ( LToken ) )
            ; Wr . PutText ( WrT , Wr . EOL )

            ; EVAL DibPutOpnd ( "Atom: " )  
            ; EVAL DibPutOpnd ( "Line: " )  
            ; EVAL DibPutOpnd ( "Column: " )  
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
            ; Wr . PutText ( WrT , FM3IntToks . Name ( LToken ) ) 
            ; Wr . PutText ( WrT , Wr . EOL )
            ; IF IntSets . IsElement ( LToken , GTokSetPatch )
              THEN EVAL DibPutOpnd ( "Patch coord: " )
              END (*IF*)
            ; CASE LToken OF
              | FM3IntToks . ItkDeclScopeLt 
              , FM3IntToks . ItkDeclScopeRt 
              , FM3IntToks . ItkOpenScopeLt 
              , FM3IntToks . ItkDeclScopeRt
              =>
              ELSE
              END (*CASE*) 
              
            ; IF IntSets . IsElement ( LToken , GTokSetGE1Arg )
              THEN
                EVAL DibPutOpnd ( "Opnd 0: " )
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE2Args )
              THEN EVAL DibPutOpnd ( "Opnd 1: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE3Args )
              THEN EVAL DibPutOpnd ( "Opnd 2: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE4Args )
              THEN EVAL DibPutOpnd ( "Opnd 3: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE5Args )
              THEN EVAL DibPutOpnd ( "Opnd 4: " ) 
              END (*IF*) 
            ; IF IntSets . IsElement ( LToken , GTokSetGE6Args )
              THEN EVAL DibPutOpnd ( "Opnd 5: " ) 
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

(*  A naive way:
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
 *) 

; PROCEDURE NumArgCt ( IntToken : INTEGER ; MinCt : INTEGER )
    : INTEGER

  = BEGIN
      IF NOT IntSets . IsElement ( IntToken , GTokSetGE1Arg )
      THEN RETURN 0 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE2Args )
      THEN RETURN 1 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE3Args )
      THEN RETURN 2 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE4Args )
      THEN RETURN 3 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE5Args )
      THEN RETURN 4 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE6Args )
      THEN RETURN 5 
      ELSE RETURN 6
      END (*IF*) 
    END NumArgCt 

(*EXPORTED:*) 
; PROCEDURE DisAsmWOperands
    ( RBT : RdBackFile . T ; WrT : Wr . T ; L2R : BOOLEAN )
  RAISES { RdBackFile . BOF }

  (* PRE: RBT is open. *) 

  = VAR LTokenL : LONGINT
  ; VAR LArgString : TEXT
  ; VAR LToken : INTEGER
  ; VAR LArgNo : INTEGER
  ; VAR LArgCt : INTEGER
  ; VAR LArgRdT : Rd . T
  ; VAR LArgChar : CHAR 

  ; PROCEDURE DobPutOpnd ( Label : TEXT ) RAISES { RdBackFile . BOF } 

    = VAR LOpndL : LONGINT
    
    ; BEGIN
        LOpndL := ReadAndPutPrefix ( RBT , WrT )
      ; Wr . PutText ( WrT , OpndIndent ) 
      ; Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LOpndL ) )
      ; Wr . PutText ( WrT , Wr . EOL )
      END DobPutOpnd  

  ; PROCEDURE DobLongArg ( ArgNo : INTEGER ; Tag : CHAR )
    = VAR LArgL : LONGINT

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*) 
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , Tag ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      ; Wr . PutText ( WrT , "(16_" ) 
      ; Wr . PutText ( WrT , Fmt . LongUnsigned ( LArgL , 16 ) ) 
      ; Wr . PutChar ( WrT , ')') 
      END DobLongArg 

  ; PROCEDURE DobBoolArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT
    ; VAR LVal : TEXT 

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; IF LArgL = 0L THEN LVal := "FALSE" ELSE LVal := "TRUE" END (*IF*)  
      ; Wr . PutText ( WrT , LVal) 
      END DobBoolArg 

  ; PROCEDURE DobLineNoArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , 'N' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      END DobLineNoArg 

  ; PROCEDURE DobColNoArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , 'H' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      END DobColNoArg 

  ; PROCEDURE DobPosArg ( ArgNo : INTEGER )
    = VAR LLineL , LColL: LONGINT

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LLineL := FM3Compress . GetBwd ( RBT )
      ; LColL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , '[' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LLineL ) ) 
      ; Wr . PutChar ( WrT , ',' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LColL ) ) 
      ; Wr . PutChar ( WrT , ']' ) 
      END DobPosArg 

  ; PROCEDURE DobIdentAtomArg ( ArgNo : INTEGER )

    = VAR LArgL : LONGINT
    ; VAR LIdentOAChars : FM3OpenArray_Char . T 
    ; VAR LIdentName : TEXT 

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; WITH WUnitRef = FM3Units . UnitStackTopRef
        DO IF WUnitRef = NIL
          THEN LIdentName := "<NoUnit>"
          ELSIF FM3Atom_OAChars . Key 
                  ( WUnitRef ^ . UntIdentAtomDict
                  , VAL ( LArgL , FM3Base . AtomTyp )
                  , (*OUT*) LIdentOAChars
                  )
          THEN
            IF LIdentOAChars = NIL THEN LIdentName := "<NoAtomIdent>"
            ELSE LIdentName := Text . FromChars ( LIdentOAChars ^ )
            END (*IF*) 
          ELSE LIdentName := "<NoAtomIdent>" 
          END (*IF*)
        ; Wr . PutText ( WrT , "Id" ) 
        ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) )
        ; Wr . PutChar ( WrT , '\"' ) 
        ; Wr . PutText ( WrT , LIdentName )
        ; Wr . PutChar ( WrT , '\"' ) 
        END (*WITH*)
      END DobIdentAtomArg 

  ; PROCEDURE DobIdentReservedArg ( ArgNo : INTEGER ; )

    = VAR LArgL : LONGINT
    ; VAR LIdentImage : TEXT 

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; LIdentImage := FM3SrcToks . Image ( VAL ( LArgL , FM3SrcToks . TokTyp ) )  
      ; Wr . PutText ( WrT , "Rid\"" ) 
      ; Wr . PutText ( WrT , LIdentImage )
      ; Wr . PutChar ( WrT , '\"' ) 
      END DobIdentReservedArg 

  ; PROCEDURE DobDeclNoArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT
  (*TODO: track down and display the decl's ident. *) 
    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , 'D' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      END DobDeclNoArg 

  ; PROCEDURE DobDeclKindArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT
    ; VAR LResult : TEXT 

    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; CASE LArgL OF
        | VAL ( ORD ( FIRST ( FM3Decls . DeclKindTyp ) ) , LONGINT )  
          .. VAL ( ORD ( LAST ( FM3Decls . DeclKindTyp ) ) , LONGINT )
        => LResult
             := ", "
                & FM3Decls . DeclKindImage
                    ( VAL ( LArgL , FM3Decls . DeclKindTyp ) )
        ELSE
          LResult := ", OutOfRangeDeclKind(" & Fmt . LongInt ( LArgL ) & ")"
        END (*CASE*) 
      ; Wr . PutText ( WrT , LResult ) 
      END DobDeclKindArg 

  ; PROCEDURE DobCoordArg ( ArgNo : INTEGER )
  
    = VAR LArgL : LONGINT
    
(* TODO: Show this in hex (and decimal both? *) 
    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , 'C' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      END DobCoordArg 

  ; PROCEDURE DobBasedInt ( ValueL : LONGINT )
    (* PRE: It's the leftmost but not the rightmost arg. *) 

    = VAR LBaseL : LONGINT
    ; VAR LBase : FM3Fmt . Base

    ; BEGIN
(* For now, the base argument is omitted from these cases.
   This wants it after the value, but Pass2 wants it before.
   *)
   RETURN 
      ; LBaseL := FM3Compress . GetBwd ( RBT )
      ; CASE LBaseL OF
        (* Scanner always provides 16. *)  
(* FIXME^ Get the actual base somehow. *) 
        | 2L .. 16L
        => LBase := VAL ( LBaseL , FM3Fmt . Base ) 
        ; Wr . PutText ( WrT , FM3Fmt . Unsigned ( LBase , base := 10 ) )
        ; Wr . PutChar ( WrT , '_' ) 
        ; Wr . PutText
            ( WrT , FM3Fmt . LongUnsigned ( ValueL , base := LBase ) )
        ELSE (* Out of range base. *) 
          Wr . PutText ( WrT , FM3Fmt . LongUnsigned ( LBaseL , base := 10 ) )
        END (*CASE*) 
      END DobBasedInt

  ; PROCEDURE DobText ( DoString : BOOLEAN ; Wide : BOOLEAN )
    RAISES { RdBackFile . BOF } 

    = VAR LArgL : LONGINT
    ; VAR LCharL : LONGINT
    ; VAR LCt : INTEGER

    ; BEGIN 
        Wr . PutChar ( WrT , ' ' ) 
      ; Wr . PutText ( WrT , FM3IntToks . Name ( LToken ) )
      ; Wr . PutChar ( WrT , '(' )
      ; LArgL := FM3Compress . GetBwd ( RBT ) (* Atom. *) 
      ; Wr . PutText ( WrT , "16_" )
      ; Wr . PutText ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 16 ) )
      ; Wr . PutChar ( WrT , ')' )
      ; DobPosArg ( 1 ) 
      ; IF DoString
        THEN (* Display the string. *) 
          Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , "    ("  )
        ; LArgL := FM3Compress . GetBwd ( RBT ) (* Right char count. *)
        ; Wr . PutText ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 10 ) )
        ; Wr . PutChar ( WrT , ':' )
        ; IF Wide THEN Wr . PutChar ( WrT , 'W' ) END (*IF*) 
        ; Wr . PutChar ( WrT , '\"' )
        ; LCt := VAL ( LArgL , INTEGER )
        ; FOR RI := 0 TO LCt - 1 (* LM 1st, as reading backwards. *) 
          DO
            LCharL := FM3Compress . GetBwd ( RBT )
          ; FM3Utils . EscapeL ( WrT , LCharL, Wide )  
          END (*FOR*) 
        ; Wr . PutText ( WrT , "\":" )
        ; LArgL := FM3Compress . GetBwd ( RBT ) (* Left char count. *) 
        ; Wr . PutText ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 10 ) )
        END (*IF*) 
      ; Wr . PutChar ( WrT , ')' )
      ; Wr . PutText ( WrT , Wr . EOL )
      END DobText

  ; VAR LArgL : LONGINT 
  ; VAR LArgI : INTEGER 
  
  ; BEGIN (* DisAsmWOperands *) 
      FM3SharedUtils . LoadSets ( )
    ; FetchOptions ( )

    ; TRY 
        LOOP (* Thru' token-with-args groups. *) 
          LTokenL := ReadAndPutPrefix ( RBT , WrT )
        ; IF Long . LE ( LTokenL , VAL ( LAST ( INTEGER ) , LONGINT ) )
          THEN (* Nonneg INTEGER *) 
            LToken := VAL ( LTokenL , INTEGER ) 

          ; CASE LToken OF

            (* Variable terminals. *) 
            | FM3SrcToks . StkIdent 
              => Wr . PutText ( WrT , " " ) 
              ; Wr . PutText ( WrT , FM3SrcToks . Name ( LToken ) )
              ; Wr . PutChar ( WrT , '(')
              ; DobIdentAtomArg ( 0 )
              ; DobPosArg ( 1 )
              ; Wr . PutChar ( WrT , ')')
              ; Wr . PutText ( WrT , Wr . EOL )

            (* Keep these consistent with FM3Pass1.UnnestStk: *)
(* These should not happen: 
            | FM3SrcToks . StkIntLit 
            , FM3SrcToks . StkLongIntLit 
            , FM3SrcToks . StkBasedLit 
            , FM3SrcToks . StkLongBasedLit
            , FM3SrcToks . StkRealLit 
            , FM3SrcToks . StkLongRealLit 
            , FM3SrcToks . StkExtendedLit 
              => PushOACharsBwd
                   ( WRdBack , FM3Scanner . Attribute . SaChars )
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
                   , VAL ( ORD ( FM3Scanner . Attribute . SaWCh )
                         , LONGINT
                         ) 
                   )
            | FM3SrcToks . StkLexErrChars
              => (* Throw these away, for now. *) 
                Wr . PutText ( WrT , Wr . EOL )
*) 

            (* Fixed source terminals. *) 
            | FM3SrcToks . StkRwAND .. FM3SrcToks . StkClosePragma  
            =>  (* Constant terminal: (will this ever happen? *)
                Wr . PutText ( WrT , " " ) 
              ; Wr . PutText ( WrT , FM3SrcToks . Image ( LToken ) )
              ; Wr . PutText ( WrT , Wr . EOL )
            
            (* Special case internal tokens. *)
            
            | FM3IntToks . ItkIdRefAtom  
              => Wr . PutText ( WrT , " " ) 
              ; Wr . PutText ( WrT , FM3IntToks . Name ( LToken ) )
              ; Wr . PutChar ( WrT , '(')
              ; DobIdentAtomArg ( 0 )
              ; DobPosArg ( 1 )
              ; Wr . PutChar ( WrT , ')')
              ; Wr . PutText ( WrT , Wr . EOL )

            | FM3IntToks . ItkLiteral
            =>  Wr . PutChar ( WrT , ' ' ) 
              ; Wr . PutText ( WrT , FM3IntToks . Name ( LToken ) )
              ; Wr . PutChar ( WrT , '(' )
              ; LArgI := VAL ( FM3Compress . GetBwd ( RBT ) , INTEGER ) 
              ; Wr . PutText ( WrT , Fmt . Int ( LArgI ) )
              ; Wr . PutChar ( WrT , ' ' )
              ; Wr . PutText ( WrT , FM3SrcToks . Image ( LArgI ) )
              ; Wr . PutChar ( WrT , ',' )
              ; LArgL := FM3Compress . GetBwd ( RBT )
              ; Wr . PutText ( WrT , "(16_" )
              ; Wr . PutText
                  ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 16 ) )
              ; Wr . PutChar ( WrT , ')' )
              ; DobPosArg ( 2 ) 
              ; Wr . PutChar ( WrT , ')' )
              ; Wr . PutText ( WrT , Wr . EOL )
(*
            | FM3IntToks . ItkIntLit 
            , FM3IntToks . ItkLongIntLit 
            , FM3IntToks . ItkBasedLit 
            , FM3IntToks . ItkLongBasedLit
            , FM3IntToks . ItkRealLit 
            , FM3IntToks . ItkLongRealLit 
            , FM3IntToks . ItkExtendedLit
            , FM3IntToks . ItkCharLit
            , FM3IntToks . ItkWideCharLit
            =>  Wr . PutChar ( WrT , ' ' ) 
              ; Wr . PutText ( WrT , FM3IntToks . Name ( LToken ) )
              ; LArgL := FM3Compress . GetBwd ( RBT )
              ; Wr . PutText ( WrT , "(16_" )
              ; Wr . PutText
                  ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 16 ) )
              ; Wr . PutChar ( WrT , '(' )
              ; CASE LToken OF 
                |  FM3IntToks . ItkIntLit
                =>  Wr . PutText
                      ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 10 ) )
                
                | FM3IntToks . ItkLongIntLit 
                =>  Wr . PutText
                      ( WrT , FM3Fmt . LongUnsigned ( LArgL , base := 10 ) )
                  ; Wr . PutChar ( WrT , 'L' )
                  
                | FM3IntToks . ItkBasedLit 
                => DobBasedInt ( LArgL )
                
                | FM3IntToks . ItkLongBasedLit
                =>  DobBasedInt ( LArgL ) 
                  ; Wr . PutChar ( WrT , 'L' )
                  
                | FM3IntToks . ItkRealLit
                => Wr . PutText
                     ( WrT , FM3Fmt . Real
                         ( FM3UnsafeUtils . LongIntToReal ( LArgL )
                         , literal := TRUE )
                         )
                
                | FM3IntToks . ItkLongRealLit
                => Wr . PutText
                     ( WrT , FM3Fmt . LongReal
                         ( FM3UnsafeUtils . LongIntToLongReal ( LArgL ) 
                         , literal := TRUE )
                         )
                
                | FM3IntToks . ItkExtendedLit
                => Wr . PutText
                     ( WrT , FM3Fmt . Extended
                         ( FM3UnsafeUtils . LongIntToExtended ( LArgL ) 
                         , literal := TRUE )
                         )
                
                | FM3IntToks . ItkCharLit
                =>  Wr . PutChar ( WrT , '\'' )
                  ; FM3Utils . EscapeL ( WrT , LArgL , Wide := FALSE ) 
                  ; Wr . PutChar ( WrT , '\'' )
                  
                | FM3IntToks . ItkWideCharLit
                =>  Wr . PutText ( WrT , "W\'" )
                  ; FM3Utils . EscapeL ( WrT , LArgL , Wide := TRUE ) 
                  ; Wr . PutChar ( WrT , '\'' )

                END (*CASE*)
                
              ; Wr . PutChar ( WrT , ')' )
              ; DobPosArg ( 2 ) 
              ; Wr . PutChar ( WrT , ')' )
              ; Wr . PutText ( WrT , Wr . EOL )

            | FM3IntToks . ItkTextLitLt
            =>  DobText ( DoString := FALSE , Wide := FALSE ) 

            | FM3IntToks . ItkTextLitRt
            =>  DobText ( DoString := FALSE , Wide := FALSE ) 

            | FM3IntToks . ItkWideTextLitLt
            =>  DobText ( DoString := FALSE , Wide := TRUE ) 

            | FM3IntToks . ItkWideTextLitRt
            =>  DobText ( DoString := FALSE , Wide := TRUE ) 
*)

            ELSE (* Of outer CASE. *) 
              CASE LToken OF 
              | FM3IntToks . TkMinTok .. FM3IntToks . TkMaxTok 
              => (* Other internal token. Special case internal tokens
                    won't get here. *)
                Wr . PutText ( WrT , " " ) 
              ; Wr . PutText ( WrT , FM3IntToks . Name ( LToken ) ) 

              (* Display operands. *) 
              ; LArgNo := 0
              ; Wr . PutChar ( WrT , '(' )

              (* Tagged operands: *) 
              ; LArgString := FM3IntToks . Operands ( LToken ) 
              ; IF LArgString # NIL AND Text . Length ( LArgString ) > 0
                THEN
                  LArgRdT := TextRd . New ( LArgString )
                ; LArgChar := Rd . GetChar ( LArgRdT ) 
                ; LOOP (* Thru' arguments. *)
                    IF Rd . EOF ( LArgRdT )
                    THEN (* No more string-defined args. *)
                      EXIT 
                    END (*IF*) 
                  ; LArgChar := Rd . GetChar ( LArgRdT )
                  (* Arg does not necessarily have a leading '_' *)
                  ; IF LArgChar = '_'
                    THEN
                      IF Rd . EOF ( LArgRdT )
                      THEN EXIT
                      ELSE 
                        LArgChar := Rd . GetChar ( LArgRdT )
                      END (*IF*)
                    END (*IF*)
                  ; CASE LArgChar OF
                    | 'C' => DobCoordArg ( LArgNo )
                    | 'L' => DobLongArg ( LArgNo , 'L' )
                    | 'B' => DobBoolArg ( LArgNo )
                    | 'H' => DobColNoArg ( LArgNo )
                    | 'I' => DobIdentAtomArg ( LArgNo )
                    | 'D' => DobDeclNoArg ( LArgNo )
                    | 'k' => DobDeclKindArg ( LArgNo ) 
                    | 'P'
                    => DobPosArg ( LArgNo )
                      ; INC ( LArgNo ) 
                    | 'N'
                    => (* Is it "_N_H"? *)  
                      IF Rd . EOF ( LArgRdT )
                      THEN
                        DobLineNoArg ( LArgNo )
                      ; EXIT 
                      ELSE
                        LArgChar := Rd . GetChar ( LArgRdT )
                      ; IF LArgChar # '_'
                        THEN
                          Rd . UnGetChar ( LArgRdT )
                        ; DobLineNoArg ( LArgNo )
                        ELSIF Rd . EOF ( LArgRdT )
                        THEN
                          DobLineNoArg ( LArgNo )
                        ; EXIT 
                        ELSIF LArgChar = 'H'
                        THEN DobPosArg ( LArgNo )
                        ; INC ( LArgNo ) 
                        ELSE
                          Rd . UnGetChar ( LArgRdT )
                        ; DobLineNoArg ( LArgNo )
                        (* Go around for another arg letter, whose
                           leading '_' is in this case, already consumed.
                        *)  
                        END (*IF*) 
                      END (*IF*)
                    ELSE
                      DobLongArg ( LArgNo , '?' )
                    END (*CASE*)
                  ; INC ( LArgNo )
                  END (*LOOP*)
                END (*IF*) 

              (* Any remaining counted args: *) 
              ; LOOP 
                  CASE LArgNo OF 
                  | 0 => IF NOT IntSets . IsElement ( LToken , GTokSetGE1Arg )
                         THEN EXIT END (*IF*)
                  | 1 => IF NOT IntSets . IsElement ( LToken , GTokSetGE2Args )
                         THEN EXIT END (*IF*)
                  | 2 => IF NOT IntSets . IsElement ( LToken , GTokSetGE3Args )
                         THEN EXIT END (*IF*)
                  | 3 => IF NOT IntSets . IsElement ( LToken , GTokSetGE4Args )
                         THEN EXIT END (*IF*)
                  | 4 => IF NOT IntSets . IsElement ( LToken , GTokSetGE5Args )
                         THEN EXIT END (*IF*)
                  | 5 => IF NOT IntSets . IsElement ( LToken , GTokSetGE6Args )
                         THEN EXIT END (*IF*)
                  ELSE EXIT 
                  END (*CASE*) 
                ; DobLongArg ( LArgNo , 'L' )
                ; INC ( LArgNo ) 
                END (*LOOP*) 

              ; Wr . PutChar ( WrT , ')' ) 
              ; Wr . PutText ( WrT , Wr . EOL )
              ELSE (* Don't know what this token is. *) 
                Wr . PutText ( WrT , " <unknown token:> " )
              ; Wr . PutText ( WrT , Fmt . LongInt ( LTokenL ) )
              ; Wr . PutText ( WrT , Wr . EOL )
              END (*CASE*)
            END (*CASE*) 

          ELSE (* out of nonneg INTEGER range *) 
            Wr . PutText ( WrT , " <unknown token, out of range:> " ) 
          ; Wr . PutText ( WrT , Fmt . LongInt ( LTokenL ) )
          ; Wr . PutText ( WrT , Wr . EOL )
          END (*IF*)
        END (*LOOP*)
      EXCEPT
      | RdBackFile . BOF
      => Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( 0L) , 6 ) )
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END (*EXCEPT*)
    END DisAsmWOperands

; BEGIN
  END FM3DisAsm
.

