 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Disass

; IMPORT Fmt
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
; IMPORT FM3Compress
; IMPORT FM3SrcToks 
; IMPORT FM3IntToks
; IMPORT FM3OpenArray_Char
; IMPORT RdBackFile
; IMPORT FM3Units 

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

; VAR GitRef : FM3Units . UnitRefTyp 

; PROCEDURE PutPrefix
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
        , Fmt . Pad
            ( Fmt . LongUnsigned ( LValueL , 16 ) , 16 , padChar := '0' )
        )
    ; Wr . PutText ( WrT , " " ) 
    ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . LongInt ( LValueL ) , 21 ) )
    ; RETURN LValueL 
    END PutPrefix

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
        LOpndL := PutPrefix ( RBT , WrT )
      ; Wr . PutText ( WrT , OpndIndent ) 
      ; Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LOpndL ) )
      ; Wr . PutText ( WrT , Wr . EOL )
      END DibPutOpnd  

  ; BEGIN
      LoadResources ( ) 
    ; TRY 
        LOOP
          LTokenL := PutPrefix ( RBT , WrT )
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

; EXCEPTION Check1 
; EXCEPTION Check2 
; EXCEPTION Check3 
; EXCEPTION Check4 
; EXCEPTION Check5 
; EXCEPTION Check6

; PROCEDURE NumArgCt ( IntToken : INTEGER ; MinCt : INTEGER )
    : INTEGER

  = BEGIN
    (* This is really crazy, but if the compiler optimizes an
       in-procedure RAISE-EXCEPT pair into a static branch or
       fall-through, it should be as fast as if we had a statement
       like C's 'switch', where alternatives fall though, by default.
       This simulates that.
       
       This avoids duplicated code, runtime loop execution with
       subscript arithmetic and internal procedure calls.  This
       procedure is likely quite frequently executed, but the
       dynamic number of the GTokenSetGE* sets to be checked is
       typically quite small.
    *) 
      CASE MinCt OF 
      | 0 => RAISE Check1
      | 1 => RAISE Check2
      | 2 => RAISE Check3
      | 3 => RAISE Check4
      | 4 => RAISE Check5
      | 5 => RAISE Check6
      ELSE RETURN MinCt 
      END (*CASE*) 

    ; TRY RAISE Check1
      EXCEPT Check1
        => IF NOT IntSets . IsElement ( IntToken , GTokSetGE1Arg )
           THEN RETURN 0 END (*IF*)
      END (*EXCEPT*) 

    ; TRY RAISE Check2
      EXCEPT Check2
        => IF NOT IntSets . IsElement ( IntToken , GTokSetGE2Args )
           THEN RETURN 1 END (*IF*)
      END (*EXCEPT*) 

    ; TRY RAISE Check3
      EXCEPT Check3
        => IF NOT IntSets . IsElement ( IntToken , GTokSetGE3Args )
           THEN RETURN 2 END (*IF*)
      END (*EXCEPT*) 

    ; TRY RAISE Check4
      EXCEPT Check4
        => IF NOT IntSets . IsElement ( IntToken , GTokSetGE4Args )
           THEN RETURN 3 END (*IF*)
      END (*EXCEPT*) 

    ; TRY RAISE Check5
      EXCEPT Check5
        => IF NOT IntSets . IsElement ( IntToken , GTokSetGE5Args )
           THEN RETURN 4 END (*IF*)
      END (*EXCEPT*) 

    ; TRY RAISE Check6
      EXCEPT Check6
        => IF NOT IntSets . IsElement ( IntToken , GTokSetGE6Args )
           THEN RETURN 5 END (*IF*)
      END (*EXCEPT*)

    ; RETURN 6

(* A brute-force way:
      IF NOT IntSets . IsElement ( IntToken , GTokSetGE1Arg )
      THEN RETURN 0 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE2Args )
      THEN RETURN 1 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE3Args )
      THEN RETURN 2 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE4Args )
      THEN RETURN 3 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE5Args )
      THEN RETURN4 
      ELSIF NOT IntSets . IsElement ( IntToken , GTokSetGE6Args )
      THEN RETURN 5 
      ELSE RETURN 6
*)
    END NumArgCt 

(*EXPORTED:*) 
; PROCEDURE DisassWOperandsBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

  = VAR LTokenL : LONGINT
  ; VAR LToken : INTEGER
  ; VAR LArgNo : INTEGER
  ; VAR LArgCt : INTEGER
  ; VAR LArgRdT : Rd . T
  ; VAR LArgChar : CHAR 

  ; PROCEDURE DobPutOpnd ( Label : TEXT ) RAISES { RdBackFile . BOF } 

    = VAR LOpndL : LONGINT
    
    ; BEGIN
        LOpndL := PutPrefix ( RBT , WrT )
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
      ; Wr . PutText ( WrT , Fmt . LongInt ( LLineL ) ) 
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
            LIdentName := Text . FromChars ( LIdentOAChars ^ ) 
          ELSE LIdentName := "<NoName>" 
          END (*IF*)
        ; Wr . PutChar ( WrT , 'I' ) 
        ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) )
        ; Wr . PutChar ( WrT , '(' ) 
        ; Wr . PutText ( WrT , LIdentName )
        ; Wr . PutChar ( WrT , ')' ) 
        END (*WITH*)
      END DobIdentAtomArg 

  ; PROCEDURE DobDeclNoArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT
  (*TODO: track down and display the decl's ident. *) 
    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , 'D' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      END DobDeclNoArg 

  ; PROCEDURE DobCoordArg ( ArgNo : INTEGER )
    = VAR LArgL : LONGINT
  (* TODO: Show this in hex (and decimal both? *) 
    ; BEGIN
        IF ArgNo > 0 THEN Wr . PutChar ( WrT , ',' ) END (*IF*)
      ; LArgL := FM3Compress . GetBwd ( RBT )
      ; Wr . PutChar ( WrT , 'C' ) 
      ; Wr . PutText ( WrT , Fmt . LongInt ( LArgL ) ) 
      END DobCoordArg 

  ; BEGIN (* DisassWOperandsBwd *) 
      LoadResources ( ) 
    ; TRY 
        LOOP (* Thru' token-with-args groups. *) 
          LTokenL := PutPrefix ( RBT , WrT )
        ; IF Long . LE
               ( LTokenL , VAL ( LAST ( INTEGER ) , LONGINT ) )
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
          *) 
            | FM3SrcToks . StkLexErrChars
              => (* Throw these away, for now. *) 

            | FM3SrcToks . StkRwAND .. FM3SrcToks . StkClosePragma  
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
            ; LArgNo := 0
            ; LArgRdT
                := TextRd . New ( FM3IntToks . Operands ( LToken) )
            ; LArgChar := Rd . GetChar ( LArgRdT ) 
            ; Wr . PutChar ( WrT , '(' ) 
            ; LOOP (* Thru' arguments. *)
                IF Rd . EOF ( LArgRdT )
                THEN (* No more string-defined args. *)
                  EXIT 
                END (*IF*) 
              ; LArgChar := Rd . GetChar ( LArgRdT )
              (* Arg does not necessarily have a leadnhg '_' *)
              ; IF LArgChar = '_'
                THEN
                  IF Rd . EOF ( LArgRdT )
                  THEN EXIT
                  ELSE 
                    LArgChar := Rd . GetChar ( LArgRdT )
                  END (*IF*)
                ; CASE LArgChar OF
                  | 'L' => DobLongArg ( LArgNo , 'L' )
                  | 'B' => DobBoolArg ( LArgNo )
                  | 'N' => (* Is it "_N_H"? *)  
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
                        ELSE
                          Rd . UnGetChar ( LArgRdT )
                        ; DobLineNoArg ( LArgNo )
                        (* Go around for another arg letter, whose
                           leading '_' is in this case, already
                           consumed.
                        *)  
                        END (*IF*) 
                      END (*IF*)
                  | 'H' => DobColNoArg ( LArgNo )
                  | 'P' => DobPosArg ( LArgNo )
                  | 'I' => DobIdentAtomArg ( LArgNo )
                  | 'D' => DobDeclNoArg ( LArgNo )
                  | 'C' => DobCoordArg ( LArgNo )
                  ELSE
                    DobLongArg ( LArgNo , '?' )
                  END (*CASE*)
                END (*IF*)
              END (*LOOP*)
              
            ; LArgCt := NumArgCt ( LToken , LArgNo )
            ; WHILE LArgNo < LArgCt
              DO (* Counted argument. *)
                DobLongArg ( LArgNo , 'L' )
              ; INC ( LArgNo ) 
              END (*WHILE*)
            ; Wr . PutChar ( WrT , ')' ) 
            ELSE (* Don't know what this token is. *) 
              Wr . PutText ( WrT , " <unknown token:> " )
            ; Wr . PutText ( WrT , Fmt . LongInt ( LTokenL ) )
            ; Wr . PutText ( WrT , Wr . EOL )
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
  END FM3Disass
.

