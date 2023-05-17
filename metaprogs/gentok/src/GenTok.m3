        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* GenTok: A metaprogram to generate a package containing declarations
   for internal tokens, and some additional support. *) 

MODULE GenTok

EXPORTS Main

; IMPORT FileRd
; IMPORT FileWr
; IMPORT Fmt 
; IMPORT Long 
; IMPORT OSError
; IMPORT Params
; IMPORT Pathname
; IMPORT Pickle2 AS Pickle 
; IMPORT Rd
; IMPORT Stdio
; IMPORT Text 
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr

; IMPORT IntSets 
; IMPORT Layout
; IMPORT VarArray_Int_Refany AS IntRefArray 

; <* IMPLICIT*> EXCEPTION Terminate 

; CONST EofChar = '\XFF'
; CONST CR = '\n'
; CONST LF = '\r' 
; CONST Letters = SET OF CHAR { 'A' .. 'Z' , 'a' .. 'z' }
; CONST Digits = SET OF CHAR { '0' .. '9' }
; CONST LettersNDigits = Letters + Digits 

; VAR TokChars := SET OF CHAR { '!' .. '~' } 

; VAR GInputRdT : Rd . T
; VAR EOFToken := Text . FromChars ( ARRAY OF CHAR { 'E' , 'O' , 'F' } )
               (* ^Make sure the pointer is unique. *) 
; VAR GInputLineNo : INTEGER := 1 
; VAR GNextInChar : CHAR
; VAR GAtEof : BOOLEAN := FALSE  

; VAR GOutputWrT : Wr . T 
; VAR GOStream : Layout . T 

; VAR GInputFileName : TEXT 
; VAR GInterfaceFullName : TEXT
; VAR GModuleFullName : TEXT
; VAR GPickleFullName : TEXT
; VAR GOutputDirName : TEXT := ""

; VAR GSemiTab : INTEGER
; VAR GArgCtTab : INTEGER := 40 (* Absolute *) 
; VAR GEqualSignTab : INTEGER := 52 (* Absolute *) 
; VAR GCompressedTab : INTEGER := 8 (* Relative *) 
; VAR GTokNoPad : INTEGER := 5 
; VAR GConstTag : TEXT
; VAR GTokNamesArrayRef : IntRefArray . T 
; VAR GTokSetTemp : IntSets . T 
; VAR GTokSetPatch : IntSets . T 
; VAR GTokSet1Arg : IntSets . T 
; VAR GTokSet2Args : IntSets . T 
; VAR GTokSet3Args : IntSets . T 
; VAR GMinTokDone := FALSE 

; VAR GNextTokNo : INTEGER
; VAR GMinTokNo : INTEGER
; VAR GInputName := "FM3Toks" 
; VAR GInterfaceName := "FM3Toks"
; VAR GModuleName := "FM3Toks" 
; VAR GPickleName := "FM3Toks" 

; VAR GToken : TEXT 

; CONST VersionString = "0.1"
; VAR GDoHelp : BOOLEAN := FALSE 
; VAR GDoCountSrcToks : BOOLEAN := FALSE
; VAR GDoGenSrcToks : BOOLEAN := FALSE
; VAR GDoCountIntToks : BOOLEAN := FALSE
; VAR GDoGenIntToks : BOOLEAN := FALSE
; VAR GDoGenInterface : BOOLEAN := FALSE
; VAR GDoGenModule : BOOLEAN := FALSE
; VAR GDoImageFunc : BOOLEAN := FALSE
; VAR GDoGenLexTable : BOOLEAN := FALSE

; TYPE TokKindTyp = { TkNull , TkList , TkFixed , TkSrc }
; VAR GTokKind := TokKindTyp . TkNull  
; CONST TokKindSetInt
    = SET OF TokKindTyp { TokKindTyp . TkList , TokKindTyp . TkFixed } 

; PROCEDURE SetArgDefaults ( )

  = BEGIN
      GDoHelp := FALSE 
    ; GDoCountSrcToks := FALSE
    ; GDoGenSrcToks := FALSE
    ; GDoCountIntToks := FALSE
    ; GDoGenIntToks := FALSE
    ; GDoGenInterface := FALSE
    ; GDoGenModule := FALSE
    ; GDoImageFunc := FALSE
    ; GDoGenLexTable := FALSE

    ; GInputFileName := "Toks" 
    ; GOutputDirName := "."
    END SetArgDefaults 

; EXCEPTION HelpExc 

; PROCEDURE ParseArgs ( )

  = VAR PaArgCt : INTEGER
  ; VAR PaArgNo : INTEGER
  ; VAR PaArgLen : INTEGER
  ; VAR PaArgText : TEXT 

  ; BEGIN

      PaArgCt := Params . Count 
    ; PaArgNo := 1

    ; PROCEDURE FetchArg ( MinLen : INTEGER := 1 ) RAISES { HelpExc } 

      = BEGIN
          PaArgText := Params . Get ( PaArgNo )
        ; IF PaArgText = NIL THEN PaArgText := "" END (*IF*)
        ; PaArgLen := Text . Length ( PaArgText )
        ; IF PaArgLen < MinLen  
          THEN
            GDoHelp := TRUE
          ; RAISE HelpExc 
          END (*IF*)
        END FetchArg 

    ; PROCEDURE ArgWMore ( ) : TEXT RAISES { HelpExc }  

      = VAR LResult : TEXT

      ; BEGIN
          IF PaArgLen > 2
          THEN LResult := Text . Sub ( PaArgText , 2 , PaArgLen - 2 )
          ELSE
            INC ( PaArgNo )
          ; IF PaArgNo >= PaArgCt
            THEN (* No value. *)
              GDoHelp := TRUE 
            ; RAISE HelpExc
            END (*IF*) 
          ; FetchArg ( MinLen := 1 ) 
          ; LResult := PaArgText 
          END (*IF*)
        ; RETURN LResult 
        END ArgWMore
        
    ; VAR LArgPrefix : TEXT 
    ; BEGIN
        TRY 
          WHILE PaArgNo < PaArgCt DO
            FetchArg ( MinLen := 2 ) 
          ; LArgPrefix := Text . Sub ( PaArgText , 0 , 2 )
          ; IF Text . Equal ( LArgPrefix , "-h" )
            THEN
              GDoHelp:= TRUE 
            ; RAISE HelpExc 
            ELSIF Text . Equal ( LArgPrefix , "-v" )
            THEN 
              DisplayVersion ( )
            ; RAISE Terminate 
            ELSIF Text . Equal ( LArgPrefix , "-s" )
            THEN GDoCountSrcToks := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-S" )
            THEN
              GDoCountSrcToks := TRUE 
            ; GDoGenSrcToks := TRUE 
            ; GDoGenInterface := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-t" )
            THEN GDoCountIntToks := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-T" )
            THEN
              GDoCountIntToks := TRUE 
            ; GDoGenIntToks := TRUE 
            ; GDoGenInterface := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-n" )
            THEN
              GDoImageFunc := TRUE 
            ; GDoGenInterface := TRUE 
            ; GDoGenModule := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-l" )
            THEN GDoGenLexTable := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-F" )
            THEN GInputFileName := ArgWMore ( ) 
            ELSIF Text . Equal ( LArgPrefix , "-D" )
            THEN GOutputDirName := ArgWMore ( ) 
            ELSE
              GDoHelp := TRUE
            ; RAISE HelpExc 
            END (*IF*)
          ; INC ( PaArgNo )
          END (*WHILE*)
        EXCEPT HelpExc
        => DisplayVersion ( )
        ; IF GDoHelp THEN DisplayHelp ( ) END (*IF*)
        ; RAISE Terminate 
        END (*EXCEPT*)
      END (*BEGIN*)
    END ParseArgs

; PROCEDURE DisplayVersion ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText
        ( Stdio . stderr , ": Program to generate tokens, version. " ) 
    ; Wr . PutText ( Stdio . stderr , VersionString ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    END DisplayVersion 

; PROCEDURE DisplayHelp ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , "Usage: ") 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText ( Stdio . stderr , " Options infile (default stdin)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    
    ; Wr . PutText ( Stdio . stderr , "  -v Display version and exit." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText ( Stdio . stderr , "  -h Display this help and exit." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText ( Stdio . stderr , "  -s Count and number source tokens." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -S Declare source tokens (implies -s)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -t Count and number intermediate tokens." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -T Declare intermediate  tokens (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -n Generate Image function (tokNo to string map)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "       (implies generate a MODULE too." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText ( Stdio . stderr , "  -l Generate lexer table pickle file." )
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -F <prefix> of generated file names." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "       (default: \"Toks\"." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -D <directory> to put output files into." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "    (default: \".\"." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    END DisplayHelp 

; PROCEDURE MessageLine ( Msg : TEXT )

  = BEGIN
      Wr . PutText ( Stdio . stderr , "In Line " ) 
    ; Wr . PutText ( Stdio . stderr , Fmt . Int ( GInputLineNo ) ) 
    ; Wr . PutText ( Stdio . stderr , ", " ) 
    ; Wr . PutText ( Stdio . stderr , Msg  ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . Flush ( Stdio . stderr )
    END MessageLine

; PROCEDURE OpenInput ( FileName : TEXT ) : Rd . T 

  = VAR LResult : Rd . T
  
  ; BEGIN
      IF FileName = NIL
         OR Text . Equal ( FileName , "" )
         OR Text . Equal ( FileName , "-" )
      THEN LResult := Stdio . stdin 
      ELSE
        TRY 
          LResult := FileRd . Open ( FileName )
        EXCEPT OSError . E ( <*UNUSED*> Code )
        => MessageLine ( "Unable to open input file " & FileName )
        ; LResult := NIL 
        ; RAISE Terminate 
        END (*EXCEPT*)
      END (*IF*)
    ; GInputLineNo := 1 
    ; GNextInChar := '\X00'
    ; GInputRdT := LResult 
    ; ConsumeChar ( )
    ; RETURN LResult 
    END OpenInput

; PROCEDURE ConsumeChar ( )

  = VAR LCh : CHAR
  ; VAR EOLChars := SET OF CHAR { CR , LF } 

  ; BEGIN
      IF GNextInChar = EofChar THEN RETURN END (*IF*)
    ; IF Rd . EOF ( GInputRdT )
      THEN 
        GNextInChar := EofChar 
      ; INC ( GInputLineNo ) 
      ELSE
        IF GNextInChar = CR THEN INC ( GInputLineNo ) END (*IF*) 
      ; GNextInChar := Rd . GetChar ( GInputRdT ) 
      ; IF GNextInChar IN EOLChars (*SET OF CHAR { CR , LF }*)    
        THEN (* New line. *) 
          IF NOT Rd . EOF ( GInputRdT )
          THEN 
            LCh := Rd . GetChar ( GInputRdT ) 
          ; IF LCh IN SET OF CHAR { CR , LF } AND GNextInChar # LCh 
            THEN (* Treat CR,LF or LF,CR as just one new line. *)
            ELSE Rd . UnGetChar ( GInputRdT ) 
            END (*IF *)
          END (*IF *)
        ; GNextInChar := CR 
        END (*IF*)
      END (*IF*) 
    END ConsumeChar

; TYPE TwoCharTyp = ARRAY [ 0 .. 1 ] OF CHAR
; CONST OpenCmnt = TwoCharTyp { '(' , '*' } 
; CONST CloseCmnt = TwoCharTyp { '*' , ')' } 

; PROCEDURE GetTok ( Required : BOOLEAN := TRUE ) : TEXT 
  (* Here, "Tok" refers to a token of the input language to
     this metaprogram.  Such tokenization is very crude, 
     consisting of comments in Modula-3 syntax and contiguous
     sequences of non-whitespace characters. NIL result
     means EOF. *)

  = VAR LWrT : TextWr . T
  ; VAR LResult : TEXT 
  ; VAR LCommentDepth : INTEGER := 0 

  ; PROCEDURE StartsWithDelim ( Delim : TwoCharTyp )
    : BOOLEAN (* Delim found, consumed, and copied to LWrT.
                 Otherwise, nothing is different. *) 

    = VAR SoughtText : TEXT

    ; BEGIN
        IF GNextInChar = EofChar
        THEN
          IF Required
          THEN
            SoughtText
              := Text . FromChar ( Delim [ 0 ] )
                 & Text . FromChar ( Delim [ 1 ] )
          ; MessageLine
              ( "EOF encountered looking for " & SoughtText )
          ; RAISE Terminate 
          END (*IF*) 
        END (*IF*)
        
      ; IF GNextInChar # Delim [ 0 ] 
        THEN RETURN FALSE
        ELSE 
          ConsumeChar ( )
        ; IF GNextInChar = Delim [ 1 ] 
          THEN (* Recognized Delim. *) 
            Wr . PutChar ( LWrT , Delim [ 0 ] ) 
          ; Wr . PutChar ( LWrT , Delim [ 1 ] )
          ; ConsumeChar ( )
          ; RETURN TRUE 
          ELSE (* No, put things back the way they were. *) 
            Rd . UnGetChar ( GInputRdT )
          ; GNextInChar := Delim  [ 0 ]
          ; RETURN FALSE 
          END (*IF*) 
        END (*IF*) 
      END StartsWithDelim 

  ; BEGIN (* GetTok *)
      LWrT := TextWr . New ( )
    ; LOOP 
        IF GNextInChar = EofChar
        THEN
          GAtEof := TRUE
        ; LResult := EOFToken
        ; RETURN LResult 
        ELSIF NOT GNextInChar IN TokChars THEN ConsumeChar ( ) (* And loop. *)  
        ELSIF StartsWithDelim ( OpenCmnt )
        THEN (* It's a comment, and so is this. *)
          LCommentDepth := 1 
        ; LOOP (* Thru' the comment, including nested comments and new lines. *)
            IF StartsWithDelim ( CloseCmnt )
            THEN
              DEC ( LCommentDepth )
            ; IF LCommentDepth = 0 THEN EXIT END (*IF*)
            ELSIF StartsWithDelim ( OpenCmnt )
            THEN INC ( LCommentDepth )
            ELSE
              Wr . PutChar ( LWrT , GNextInChar )
            ; ConsumeChar ( ) 
            END (*IF*) 
          END (*LOOP*)
        ; LResult := TextWr . ToText ( LWrT )
        ; RETURN LResult 
        ELSE (* A token. *) 
          LOOP (* Thru' the token, i.e., while in TokChars. *) 
            Wr . PutChar ( LWrT , GNextInChar )
          ; ConsumeChar ( )
          ; IF NOT GNextInChar IN TokChars THEN EXIT END (*IF*) 
          END (*LOOP*)
        ; LResult := TextWr . ToText ( LWrT )
        ; RETURN LResult 
        END (*IF*)
      END (*LOOP*)
    END GetTok

; PROCEDURE SkipComments ( ) 

  = BEGIN 
      WHILE <*NOWARN*> GToken # EOFToken
            AND Text . Length ( GToken ) >= 2
            AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) , "(*")
      DO GToken := GetTok ( ) 
      END (*WHILE*)
    END SkipComments 

; PROCEDURE CopyComments ( ) 

  = BEGIN 
      WHILE <*NOWARN*> GToken # EOFToken
            AND Text . Length ( GToken ) >= 2
            AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) , "(*")
      DO (* It's a comment, and so is this. *) 
        Layout . PutText ( GOStream , GToken )
      ; Layout . PutEol ( GOStream ) 
      ; GToken := GetTok ( ) 
      END (*WHILE*)
    END CopyComments 

; PROCEDURE GetTokArgCt ( Kind : TEXT ) : INTEGER
  (* -1 means none found.  Otherwise, 0, 1, 2, or 3.
     Consumed if non-negative number found
  *) 

  = VAR LValue : INTEGER

  ; BEGIN
      IF NOT IsNum ( GToken , ((*VAR*) LValue ) ) 
      THEN RETURN - 1
      ELSIF LValue IN SET OF [ 0 .. 7 ] { 0 , 1 , 2 , 3 }
      THEN
        GToken := GetTok ( ) 
      ; RETURN LValue
      ELSE
        IF Kind # NIL
        THEN 
          MessageLine 
            ( "Bad " & Kind & " argument count: " & Fmt . Int ( LValue ) 
            & ", must be 0, 1, or 2.  Using 0. " 
            )
        ; GToken := GetTok ( ) 
        END (*IF*)
      ; RETURN 0 
      END (*IF*) 
    END GetTokArgCt 

; PROCEDURE TokEq ( Tok : TEXT ; Wanted : TEXT ) : BOOLEAN

  = VAR LTokLen , LWantedLen : INTEGER

  ; BEGIN
      IF <*NOWARN*> Tok = EOFToken OR Tok = NIL THEN RETURN FALSE END (*IF*)
    ; IF <*NOWARN*> Wanted = EOFToken OR Wanted = NIL
      THEN RETURN FALSE
      END (*IF*)
    ; LTokLen := Text . Length ( Tok ) 
    ; LWantedLen := Text . Length ( Wanted )
    ; IF LTokLen # LWantedLen
      THEN RETURN FALSE
      ELSIF LTokLen = 0
      THEN RETURN TRUE
      ELSE RETURN Text . Equal ( Tok , Wanted )
      END (*IF*)
    END TokEq

; PROCEDURE IsNum ( Token : TEXT ; VAR Value : INTEGER ) : BOOLEAN

  = VAR LLen , LCharNo , LValue : INTEGER
  ; VAR LChar : CHAR 

  ; BEGIN
      IF <*NOWARN*> Token = NIL OR Token = EOFToken THEN RETURN FALSE END (*IF*)
    ; LLen := Text . Length ( Token )
    ; IF LLen <= 0 THEN RETURN FALSE END (*IF*) 
    ; LValue := 0
    ; LCharNo := 0
    ; LOOP
        IF LCharNo >= LLen THEN EXIT END (*IF*)
      ; LChar := Text . GetChar ( Token , LCharNo ) 
      ; IF NOT LChar IN Digits THEN RETURN FALSE END (*IF*)
      ; LValue := LValue * 10 + ORD ( LChar ) - ORD ( '0' )  
      ; INC ( LCharNo ) 
      END (*LOOP*) 
    ; Value := LValue
    ; RETURN TRUE 
    END IsNum

; PROCEDURE IsIdent ( Token : TEXT ) : BOOLEAN

  = VAR LLen , LCharNo : INTEGER
  ; LChar : CHAR 

  ; BEGIN
      IF <*NOWARN*> Token = NIL OR Token = EOFToken THEN RETURN FALSE END (*IF*)
    ; LLen := Text . Length ( Token )
    ; IF LLen <= 0 THEN RETURN FALSE END (*IF*) 
    ; LChar := Text . GetChar ( Token , 0 ) 
    ; IF NOT LChar IN Letters THEN RETURN FALSE END (*IF*)
    ; LCharNo := 1
    ; LOOP
        IF LCharNo >= LLen THEN RETURN TRUE END (*IF*)
      ; LChar := Text . GetChar ( Token , LCharNo ) 
      ; IF NOT LChar IN LettersNDigits
        THEN RETURN FALSE
        END (*IF*)
      ; INC ( LCharNo ) 
      END (*LOOP*) 
    END IsIdent 

; PROCEDURE OpenOutput ( FileName : TEXT ) : Wr . T 

  = VAR LWrT : Wr . T 

  ; BEGIN
      IF FileName = NIL
         OR Text . Equal ( FileName , "" )
         OR Text . Equal ( FileName , "-" )
      THEN LWrT := Stdio . stdout 
      ELSE
        TRY 
          LWrT  := FileWr . Open ( FileName ) 
        EXCEPT OSError . E ( <*UNUSED*> Code )
        => MessageLine ( "Unable to open output file " & FileName ) 
        ; LWrT := NIL 
        ; RAISE Terminate 
        END (*EXCEPT*)
      END (*IF*)
    ; RETURN LWrT 
    END OpenOutput

; PROCEDURE OpenLayout ( Sink : Wr . T ) : Layout . T  

  = VAR LResult : Layout . T

  ; BEGIN
      LResult := NEW ( Layout . T ) 
    ; EVAL Layout . Init ( LResult , Sink )
    ; RETURN LResult 
    END OpenLayout 

; PROCEDURE CloseLayout ( VAR LayoutT : Layout . T )

  = VAR LWrT : Wr . T

  ; BEGIN 
      IF LayoutT # NIL 
      THEN 
        LWrT := Layout . UsedStream ( LayoutT )
      ; IF LWrT # NIL AND LWrT # Stdio . stdout AND LWrT # Stdio . stderr 
        THEN Wr . Close ( LWrT ) 
        END (*IF*)
      ; LayoutT := NIL 
      END (*IF*)
    END CloseLayout

; PROCEDURE PutSimpleTokDecl ( Name : TEXT ; TokNo : INTEGER )

  = BEGIN
      Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , GConstTag ) 
    ; GConstTag := "; CONST" (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream , Name ) 
    ; Layout . PadAbs ( GOStream , GEqualSignTab )
    ; Layout . PutText ( GOStream , " = " ) 
    ; Layout . PutText
        ( GOStream , Fmt . Pad ( Fmt . Int ( TokNo ) , GTokNoPad ) ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END PutSimpleTokDecl

; PROCEDURE MaybePutMinTokNo ( )

  = BEGIN
      IF NOT GMinTokDone
      THEN
        GMinTokNo := GNextTokNo 
      ; PutSimpleTokDecl ( "TkMinTok" , GNextTokNo ) 
      ; GMinTokDone := TRUE 
      END (*IF*)
    END MaybePutMinTokNo

; CONST TwoTo6thL = Long . Shift ( 1L , 6 ) 

; PROCEDURE CompressedHex ( IntL : LONGINT ) : TEXT
  (* Only the hex digits themselves.  Display them LSB on left. *) 

  = VAR LResidue : LONGINT 
  ; VAR LBitsL : LONGINT 
  ; VAR LBits : INTEGER
  ; VAR LBytesDoneCt : INTEGER
  ; VAR LResult : TEXT 
  ; VAR LWrT : TextWr . T 

  ; BEGIN
      LResidue := IntL
    ; LBytesDoneCt := 0
    ; LWrT := TextWr . New ( ) 
    ; LOOP
        IF LBytesDoneCt >= 8 (* 9th byte now. *)
        THEN
          LBitsL := Long . And ( LResidue , 16_FFL )
        ; LBits := VAL ( LBitsL , INTEGER )  
        ; Wr . PutText
            ( LWrT , Fmt . Pad ( Fmt . Unsigned ( LBits ) , 2 , '0' ) )
        ; Wr . PutChar ( LWrT , ' ' ) 
        ; EXIT
        END (*IF*) 
      ; LBitsL := Long . And ( LResidue , 16_7FL )
      ; LBits := VAL ( LBitsL , INTEGER )  
      ; LResidue := LResidue DIV TwoTo6thL 
      ; IF LResidue # 0L AND LResidue # 16_FFFFFFFFFFFFFFFFL  
        THEN (* More bytes will follow. *) 
          LBits := Word . Or ( LBits , 16_80 )
        ; Wr . PutText
            ( LWrT , Fmt . Pad ( Fmt . Unsigned ( LBits ) , 2 , '0' ) )
        ; Wr . PutChar ( LWrT , ' ' ) 
        ; LResidue := LResidue DIV 2L 
        ; INC ( LBytesDoneCt ) 
        ELSE (* Finishing with < 9 bytes. *) 
          Wr . PutText
            ( LWrT , Fmt . Pad ( Fmt . Unsigned ( LBits ) , 2 , '0' ) ) 
        ; Wr . PutChar ( LWrT , ' ' ) 
        ; EXIT 
        END (*IF*) 
      END (*LOOP*)
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END CompressedHex

; PROCEDURE EmitTok ( Name : TEXT ; ArgCt : INTEGER )
  (* PRE: Are generating this token. *) 

  = <* FATAL IntRefArray . AllocationFailure *>
    BEGIN
      IntRefArray . Assign ( GTokNamesArrayRef , GNextTokNo , Name ) 
    ; Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , GConstTag )
    ; GConstTag := "; CONST" (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream , Name )
    ; IF ArgCt >= 0
      THEN
        Layout . PadAbs ( GOStream , GArgCtTab )
      ; Layout . PutText ( GOStream , "(*ArgCt: " )
      ; Layout . PutText ( GOStream , Fmt . Int ( ArgCt) )
      ; Layout . PutText ( GOStream , "*)" )
      ; IF ArgCt >= 1
        THEN GTokSet1Arg := IntSets . Include ( GTokSet1Arg , GNextTokNo )
        END (*IF*) 
      ; IF ArgCt >= 2
        THEN GTokSet2Args := IntSets . Include ( GTokSet2Args , GNextTokNo )
        END (*IF*) 
      ; IF ArgCt >= 3
        THEN GTokSet3Args := IntSets . Include ( GTokSet3Args , GNextTokNo )
        END (*IF*) 
      END (*IF*)
    ; Layout . PadAbs ( GOStream , GEqualSignTab )
    ; Layout . PutText ( GOStream , " = "  )
    ; Layout . PutText
        ( GOStream , Fmt . Pad ( Fmt . Int ( GNextTokNo ) , GTokNoPad ) )

    ; Layout . PadAbs
        ( GOStream 
        , GEqualSignTab + GCompressedTab 
        )
    ; Layout . PutText ( GOStream , " (* 16_"  )
    ; Layout . PutText
        ( GOStream , CompressedHex ( VAL ( GNextTokNo , LONGINT ) ) )
    ; Layout . PutText ( GOStream , "*)" )
    ; Layout . PutEol ( GOStream )
    ; INC ( GNextTokNo ) 
    END EmitTok

; PROCEDURE EmitListToks ( ) 

  = VAR LArgCtOfList , LArgCtOfElem : [ - 1 .. 7 ] 
  ; VAR LRootName : TEXT 

  ; BEGIN
      LRootName := GToken 
    ; GToken := GetTok ( )
    ; LArgCtOfList := GetTokArgCt ( "list"  ) 
    ; LArgCtOfElem := GetTokArgCt ( "list element" ) 
    ; IF GDoGenIntToks
      THEN 
        MaybePutMinTokNo ( ) 
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "(* LIST " )
      ; Layout . PutText ( GOStream , LRootName )
      ; Layout . PutText ( GOStream , ": *)" )
      ; Layout . PutEol ( GOStream )

      ; EmitTok ( LRootName & "Lt" , LArgCtOfList )  
      ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
      ; EmitTok ( LRootName & "LtTemp" , LArgCtOfList )  
      ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
      ; EmitTok ( LRootName & "LtPatch" , LArgCtOfList + 1 )  
      ; EmitTok ( LRootName & "Rt" , LArgCtOfList )
      ; Layout . PutEol ( GOStream )

      ; EmitTok ( LRootName & "LtElem" , LArgCtOfElem )  
      ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
      ; EmitTok ( LRootName & "LtElemTemp" , LArgCtOfElem )  
      ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
      ; EmitTok ( LRootName & "LtElemPatch" , LArgCtOfElem + 1 )  
      ; EmitTok ( LRootName & "RtElem" , LArgCtOfElem )  
      ; Layout . PutEol ( GOStream )
      ELSIF GDoCountIntToks
      THEN INC ( GNextTokNo , 8 ) 
      END (*IF*) 
    END EmitListToks 

; PROCEDURE EmitFixedToks ( ) 

  = VAR LArgCtFixed , LArgCtSub : [ - 1 .. 7 ] 
  ; VAR LRootName , LSubName : TEXT 

  ; BEGIN
      LRootName := GToken 
    ; GToken := GetTok ( )  
    ; LArgCtFixed := GetTokArgCt ( "fixed"  ) 
    ; IF GDoGenIntToks
      THEN 
        MaybePutMinTokNo ( ) 
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "(* FIXED " )
      ; Layout . PutText ( GOStream , LRootName )
      ; Layout . PutText ( GOStream , ": *)" )
      ; Layout . PutEol ( GOStream )

      ; EmitTok ( LRootName & "Lt" , LArgCtFixed )  
      ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
      ; EmitTok ( LRootName & "LtTemp" , LArgCtFixed )  
      ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
      ; EmitTok ( LRootName & "LtPatch" , LArgCtFixed + 1 )
      ; EmitTok ( LRootName & "Rt" , LArgCtFixed )  
      ; Layout . PutEol ( GOStream )
      ELSIF GDoCountIntToks
      THEN INC ( GNextTokNo , 4 ) 
      END (*IF*) 

    ; WHILE IsIdent ( GToken ) 
      DO LSubName := GToken
      ; GToken := GetTok ( )  
      ; LArgCtSub := GetTokArgCt ( "interior token" ) 
      ; IF GDoGenIntToks
        THEN 
          EmitTok ( LRootName & LSubName & "Lt" , LArgCtSub )  
        ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
        ; EmitTok ( LRootName & LSubName & "LtTemp" , LArgCtSub )  
        ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
        ; EmitTok ( LRootName & LSubName & "LtPatch" , LArgCtSub + 1 )
        ; EmitTok ( LRootName & LSubName & "Rt" , LArgCtSub )
        ; Layout . PutEol ( GOStream )
        ELSIF GDoCountIntToks
        THEN INC ( GNextTokNo , 4 ) 
        END (*IF*) 
      END (*WHILE*) 
    END EmitFixedToks 

; PROCEDURE EmitSrcTok ( )

  = VAR LSrcName : TEXT 

  ; BEGIN
      LSrcName := GToken 
    ; GToken := GetTok ( )  
    ; IF GDoGenSrcToks
      THEN 
        MaybePutMinTokNo ( ) 
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "(* SRC " )
      ; Layout . PutText ( GOStream , LSrcName )
      ; Layout . PutText ( GOStream , ": *)" )
      ; Layout . PutEol ( GOStream )
      ; EmitTok ( GToken , - 1 )  
      ; Layout . PutEol ( GOStream )
      ELSIF GDoCountSrcToks
      THEN INC ( GNextTokNo ) 
      END (*IF*) 
    END EmitSrcTok

; PROCEDURE EmitInterfaceProlog ( )

  = BEGIN
      Layout . PutEol ( GOStream )
    ; Layout . PutText
        ( GOStream , "(* This file generated by GenTok program. *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream , "INTERFACE " ) 
    ; Layout . PutText ( GOStream , GInterfaceName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , "; IMPORT IntSets" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitInterfaceProlog

; PROCEDURE EmitInterfaceDecls ( )

  = BEGIN
      Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , "; TYPE TokTyp = INTEGER " ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    
    ; IF GDoImageFunc
      THEN
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText 
          ( GOStream , "; PROCEDURE Image ( TokNo : INTEGER ) : TEXT " ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
      END (*IF*) 
    
    ; IF GDoGenSrcToks
      THEN
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; VAR TokSetTemp : IntSets . T" )
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; VAR TokSetPatch : IntSets . T" )
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; VAR TokSetW1Arg : IntSets . T" )
      ; Layout . PutEol ( GOStream )
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "   (* ^At least one argument. *)" )
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; VAR TokSetArgs : IntSets . T" )
      ; Layout . PutEol ( GOStream )
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "   (* ^At least two arguments. *)" )
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; CONST LtToTemp = 1" )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Lt tokcode to get corresponding LtTemp tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; CONST LtToPatch = 2 " ) 
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Lt tokcode to get corresponding LtPatch tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; CONST LtToRt = 3    " )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Lt tokcode to get corresponding Rt tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; CONST RtToLt = - 3    " )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Rt tokcode to get corresponding Lt tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; CONST RtToTemp = - 2    " )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Rt tokcode to get corresponding LtTemp tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PutText ( GOStream , "; CONST RtToPatch = - 1    " )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Rt tokcode to get corresponding LtPatch tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PutEol ( GOStream )
      END (*IF*) 
    END EmitInterfaceDecls

; PROCEDURE EmitInterfaceEpilog ( )

  = BEGIN 
      Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , "; END " )
    ; Layout . PutText ( GOStream , GInterfaceName ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , "." ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitInterfaceEpilog 

; PROCEDURE GenTokConsts ( )

  = VAR LValue : INTEGER
  ; VAR LIsRel , LIsAbs : BOOLEAN  

  ; BEGIN
      LOOP
        IF GAtEof THEN EXIT END (*IF*) 
      ; CopyComments ( ) 

      (* Unit name, but it's too late now. *)
      ; IF TokEq ( GToken , "UNITNAME" )
        THEN
          MessageLine ( "Too late for a UNITNAME, ignored." )
        ; GToken := GetTok ( )
        ; IF GToken = EOFToken THEN RAISE Terminate END (*IF*) 
        ; IF IsIdent ( GToken ) THEN GToken := GetTok ( ) END (*IF*)
        END (*IF*) 
 
      (* Token Numbering commands. *)
      ; IF TokEq ( GToken , "REL" )
        THEN
          LIsRel := TRUE 
        ; LIsAbs := FALSE 
        ELSIF TokEq ( GToken , "ABS" )
        THEN
          LIsAbs := TRUE
        ; LIsRel := FALSE 
        END (*IF*)
      ; IF LIsRel OR LIsAbs
        THEN 
          GToken := GetTok ( ) 
        ; IF GToken = EOFToken
          THEN
            MessageLine ( "Premature EOF looking for a token number." )
          ; RAISE Terminate
          ELSIF NOT IsNum ( GToken , (*VAR*) LValue )   
          THEN 
            MessageLine ( "Invalid token number: " & GToken & ", ignored.")
          ELSE
            GToken := GetTok ( ) (* The number. *)  
          ; IF GDoCountIntToks AND GTokKind IN TokKindSetInt
               OR GDoCountSrcToks AND GTokKind = TokKindTyp . TkSrc
            THEN (* Execute the command. *)  
              IF LIsRel
              THEN INC ( GNextTokNo , LValue )
              ELSE (* => IsAbs *) 
                IF LValue < GNextTokNo 
                THEN 
                  MessageLine 
                    ( "Would-be decreasing token number: "
                      & GToken & ", retaining current value: " 
                      & Fmt . Int ( LValue )
                    )
                ELSE 
                  GNextTokNo := LValue
                END (*IF*) 
              END (*IF*)
            END (*IF*)
          END (*IF*)

        (* Token kind commands. *) 
        ELSIF TokEq ( GToken , "LIST" )  
        THEN
          GTokKind := TokKindTyp . TkList 
        ; GToken := GetTok ( ) 
        ELSIF TokEq ( GToken , "FIXED" )  
        THEN 
          GTokKind := TokKindTyp . TkFixed  
        ; GToken := GetTok ( ) 
        ELSIF TokEq ( GToken , "SRC" )  
        THEN 
          GTokKind := TokKindTyp . TkSrc 
        ; GToken := GetTok ( )
        ELSIF  IsIdent ( GToken )
        THEN
          CASE GTokKind OF
          | TokKindTyp . TkNull
          => MessageLine ( "No token kind specified, assuming SRC." )
          ; GTokKind := TokKindTyp . TkSrc 
          
          | TokKindTyp . TkList (* List construct tokens. *)
          => EmitListToks ( ) 
            
          | TokKindTyp . TkFixed (* Fixed construct tokens. *)
          => EmitFixedToks ( ) 

          | TokKindTyp . TkSrc (* Source token. *)
          => EmitSrcTok ( ) 
          END (*CASE*)
          
        ; IF TokEq ( GToken , "." ) 
          THEN GToken := GetTok ( )
          END (*IF*) 
        ELSE
          MessageLine ( "Unrecognized token: " & GToken ) 
        ; GToken := GetTok ( )  
        END (*IF*) 
      END (*LOOP*) 
    ; PutSimpleTokDecl ( "TkMaxTok" , GNextTokNo - 1 ) 
    END GenTokConsts 

; PROCEDURE Pass1 ( )

  = BEGIN
      GInputRdT := OpenInput ( GInputFileName )
    ; IF Rd . EOF ( GInputRdT )
      THEN 
        MessageLine ( "Empty input file: " & GInputFileName )
      ; RAISE Terminate
      END (*IF*)
    ; GToken := GetTok ( )
    ; SkipComments ( ) 
    ; IF TokEq ( GToken , "UNITNAME" )
      THEN 
        GToken := GetTok ( )
      ; IF GToken = EOFToken
        THEN
          MessageLine ( "Premature EOF looking for a unit name." )
        ; RAISE Terminate
        ELSIF IsIdent ( GToken )
        THEN
          GInterfaceName := GToken
        ; GModuleName := GToken 
        ; GPickleName := GToken
        ; GToken := GetTok ( )
        ELSE
          MessageLine
            ( "Invalid filename : \"" & GToken & ", using " & GInterfaceName )
        END (*IF*) 
      END (*IF*)
      
    ; GInterfaceFullName
        := Pathname . Join ( GOutputDirName , GInterfaceName , ".i3" )   
    ; GModuleFullName
        := Pathname . Join ( GOutputDirName , GModuleName , ".m3" )
    ; GPickleFullName
        := Pathname . Join ( GOutputDirName , GPickleName , ".pkl" ) 
    
    ; GOutputWrT := OpenOutput ( GInterfaceFullName )
    ; GOStream := OpenLayout ( GOutputWrT )
    ; EmitInterfaceProlog ( ) 
    ; EmitInterfaceDecls ( ) 
    ; GenTokConsts ( )
    ; EmitInterfaceEpilog ( ) 
    ; CloseLayout ( GOStream )  
    END Pass1
    
; PROCEDURE EmitModuleProlog ( )

  = BEGIN
      Layout . PutEol ( GOStream )
    ; Layout . PutText
        ( GOStream , "(* This file generated by GenTok program. *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream , "MODULE " ) 
    ; Layout . PutText ( GOStream , GModuleName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitModuleProlog 

; PROCEDURE GenImageProc ( Min , Max : INTEGER ) 

  = VAR LName : TEXT

  ; BEGIN
      IF GDoImageFunc
      THEN 
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText 
          ( GOStream , "; PROCEDURE Image ( TokNo : INTEGER ) : TEXT " ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab + 2 )
      ; Layout . PutText ( GOStream , "= BEGIN " ) 
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab + 6 )
      ; Layout . PutText ( GOStream , "CASE TokNo OF " ) 
      ; Layout . PutEol ( GOStream )

      ; FOR RI := Min TO Max
        DO
          LName := IntRefArray . Fetch ( GTokNamesArrayRef , RI )
             (* ^Implied NARROW *) 
        ; IF LName # NIL
          THEN 
            Layout . PadAbs ( GOStream , GSemiTab + 6 )
          ; Layout . PutText ( GOStream , "| " ) 
          ; Layout . PutText ( GOStream , Fmt . Int ( RI ) ) 
          ; Layout . PutText ( GOStream , " => RETURN \"" ) 
          ; Layout . PutText ( GOStream , LName ) 
          ; Layout . PutText ( GOStream , "\"") 
          ; Layout . PutEol ( GOStream )
          END (*IF*) 
        END (*FOR*)

      ; Layout . PadAbs ( GOStream , GSemiTab + 6 )
      ; Layout . PutText ( GOStream , "ELSE RETURN \"<TokUndef>\"" ) 
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab + 6 )
      ; Layout . PutText ( GOStream , "END (*CASE*) " ) 
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab + 4 )
      ; Layout . PutText ( GOStream , "END Image " ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
      END (*IF*) 
    END GenImageProc 

; PROCEDURE EmitModuleEpilog ( )

  = BEGIN 
    
      Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , "; BEGIN " )
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GSemiTab + 2 )
    ; Layout . PutText ( GOStream , "END " )
    ; Layout . PutText ( GOStream , GModuleName ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , "." ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitModuleEpilog 

; PROCEDURE Pass2 ( )

  = BEGIN
      GOutputWrT := OpenOutput ( GModuleFullName ) 
    ; GOStream := OpenLayout ( GOutputWrT )
    ; EmitModuleProlog ( ) 
    ; GenImageProc ( GMinTokNo , GNextTokNo - 1 ) 
    ; EmitModuleEpilog ( )
    ; CloseLayout ( GOStream ) 
    END Pass2

; CONST FM3FileTag = "FM3" 
; CONST FM3FileKindTokPkl = "A"
; CONST FM3Magic
    = ARRAY [ 0 .. 3 ] OF CHAR 
        { VAL ( 16_A2 , CHAR ) , VAL ( 16_0B , CHAR )
        , VAL ( 16_9F , CHAR ) , VAL ( 16_D9 , CHAR )
        }

(* TODO: Move this somewere universal & fix it up, maybe date & time, e.g.? *)
; PROCEDURE PicklePrefix ( ) : TEXT 

  = VAR LResult : TEXT
  
  ; BEGIN
      LResult
        := FM3FileTag & FM3FileKindTokPkl & Text . FromChars ( FM3Magic )
    ; RETURN LResult 
    END PicklePrefix 

; PROCEDURE WritePickle ( )

  = BEGIN
      GOutputWrT := OpenOutput ( GPickleFullName )
    ; TRY
        TRY 
          Wr . PutText ( GOutputWrT , (* FM3Utils . *) PicklePrefix ( ) )
        ; Pickle . Write
            ( GOutputWrT , GTokNamesArrayRef , write16BitWidechar := FALSE ) 
        ; Pickle . Write
            ( GOutputWrT , GTokSetTemp , write16BitWidechar := FALSE ) 
        ; Pickle . Write
            ( GOutputWrT , GTokSetPatch , write16BitWidechar := FALSE )
        ; Pickle . Write
            ( GOutputWrT , GTokSet1Arg , write16BitWidechar := FALSE ) 
        ; Pickle . Write
            ( GOutputWrT , GTokSet2Args , write16BitWidechar := FALSE )
        ; Pickle . Write
            ( GOutputWrT , GTokSet3Args , write16BitWidechar := FALSE )
        EXCEPT ELSE
          MessageLine ( "Unable to write pickle file" & GPickleFullName ) 
        END (*EXCEPT*)
      FINALLY
        Wr . Close ( GOutputWrT )  
      END (*FINALLY*)
    END WritePickle

; (* For calling within a debugger *) 
  PROCEDURE IntImage ( Val : IntSets . ValidElemT ) : TEXT
  
  = BEGIN
      RETURN Fmt . Int ( Val ) 
    END IntImage 

; <*UNUSED*> (* For calling within a debugger *) 
  PROCEDURE IntSetImage ( Set : IntSets . T ) : TEXT

  = BEGIN
      RETURN IntSets . Image ( Set , IntImage , "     " , 80 ) 
    END IntSetImage 

; PROCEDURE Init ( )
  = <* FATAL IntRefArray . AllocationFailure *>
    BEGIN 
      GMinTokDone := FALSE
    ; GNextTokNo := 0
    ; GMinTokNo := 0
    ; GConstTag := "; CONST"
    ; GTokNamesArrayRef := IntRefArray . New ( NIL ) 
    ; GTokSetTemp := IntSets . Empty ( )  
    ; GTokSetPatch := IntSets . Empty ( )  
    ; GTokSet1Arg := IntSets . Empty ( )  
    ; GTokSet2Args := IntSets . Empty ( )
    ; GTokSet3Args := IntSets . Empty ( )
    ; GSemiTab := 0
    ; GAtEof := FALSE
    END Init 

; <* FATAL Thread . Alerted *>
  <* FATAL Wr . Failure *>
  <* FATAL Rd . Failure *>
  <* FATAL Rd . EndOfFile *>
  BEGIN (*GenTok*)
  
    TRY 
      Init ( )
    ; SetArgDefaults ( )
    ; ParseArgs ( ) 
    ; Pass1 ( )
    ; Pass2 ( )
    ; WritePickle ( ) 
    EXCEPT Terminate => 
    END (*EXCEPT*)
  END GenTok
.

