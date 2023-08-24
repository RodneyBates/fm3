 
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

; IMPORT FM3BuildLexMachine 
; IMPORT FM3LexTable
; IMPORT FM3SharedGlobals 
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

; VAR GInterfaceFullName : TEXT
; VAR GModuleFullName : TEXT
; VAR GSetsFullName : TEXT
; VAR GIntFsmFullName : TEXT
; VAR GSrcFsmFullName : TEXT
; VAR GOutputFilePrefix : TEXT := "Tok"
; VAR GOutputDirName : TEXT := "."

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
; VAR GInputFileName := "FM3Toks.GenTok" 
; VAR GInterfaceName : TEXT  
; VAR GModuleName : TEXT 
; VAR GSetsName : TEXT 
; VAR GIntFsmName : TEXT 
; VAR GSrcFsmName : TEXT 

; VAR GToken : TEXT
; VAR GCopyingComments := FALSE 

; CONST VersionString = "0.1"
; VAR GDoHelp : BOOLEAN := FALSE 
; VAR GDoGenInterface : BOOLEAN := FALSE
; VAR GDoGenModule : BOOLEAN := FALSE
; VAR GDoImageFunc : BOOLEAN := FALSE
; VAR GDoCountIntToks : BOOLEAN := FALSE
; VAR GDoGenIntToks : BOOLEAN := FALSE
; VAR GDoGenSets : BOOLEAN := FALSE
; VAR GDoGenIntFsm : BOOLEAN := FALSE
; VAR GDoCountSrcToks : BOOLEAN := FALSE
; VAR GDoGenSrcToks : BOOLEAN := FALSE
; VAR GDoGenSrcFsm : BOOLEAN := FALSE
; VAR GDoOverrideUNITNAME : BOOLEAN := FALSE

; TYPE TokKindTyp = { TkNull , TkLone , TkList , TkFixed , TkSrc }
; VAR GTokKind := TokKindTyp . TkNull  
; CONST TokKindSetInt
    = SET OF TokKindTyp { TokKindTyp . TkList , TokKindTyp . TkFixed } 
; CONST TokKindSetSrc
    = SET OF TokKindTyp { TokKindTyp . TkSrc , TokKindTyp . TkNull } 

; PROCEDURE SetOutputFileNames ( )

  = BEGIN
      GInterfaceName := GOutputFilePrefix
    ; GModuleName := GOutputFilePrefix 
    ; GSetsName := GOutputFilePrefix & "Sets"
    ; GIntFsmName := GOutputFilePrefix & "IntFsm" 
    ; GSrcFsmName := GOutputFilePrefix & "SrcFsm"
    ; GInterfaceFullName
        := Pathname . Join ( GOutputDirName , GInterfaceName , "i3" )   
    ; GModuleFullName
        := Pathname . Join ( GOutputDirName , GModuleName , "m3" )
    ; GSetsFullName
        := Pathname . Join ( GOutputDirName , GSetsName , "pkl" ) 
    ; GIntFsmFullName
        := Pathname . Join ( GOutputDirName , GIntFsmName , "pkl" ) 
    ; GSrcFsmFullName
        := Pathname . Join ( GOutputDirName , GSrcFsmName , "pkl" ) 
    END SetOutputFileNames 

; PROCEDURE SetArgDefaults ( )

  = BEGIN
      GDoHelp := FALSE 
    ; GDoGenInterface := FALSE
    ; GDoGenModule := FALSE
    ; GDoImageFunc := FALSE
    ; GDoCountIntToks := FALSE
    ; GDoGenIntToks := FALSE
    ; GDoGenSets := FALSE
    ; GDoGenIntFsm := FALSE
    ; GDoCountSrcToks := FALSE
    ; GDoGenSrcToks := FALSE
    ; GDoGenSrcFsm := FALSE

    ; GInputFileName := "FM3Toks.gentok"
    ; GOutputFilePrefix := "Tok"
    ; GOutputDirName := "."
    ; SetOutputFileNames ( )
    ; GDoOverrideUNITNAME := FALSE 
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
            ELSIF Text . Equal ( LArgPrefix , "-c" )
            THEN GDoGenSets := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-l" )
            THEN GDoGenIntFsm := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-L" )
            THEN GDoGenSrcFsm := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-F" )
            THEN
              GOutputFilePrefix := ArgWMore ( ) 
            ; SetOutputFileNames ( )
            ; GDoOverrideUNITNAME := TRUE 
            ELSIF Text . Equal ( LArgPrefix , "-D" )
            THEN
              GOutputDirName := ArgWMore ( ) 
            ; SetOutputFileNames ( )
            ELSIF Text . GetChar ( PaArgText , 0 ) = '-'
            THEN 
              GDoHelp:= TRUE 
            ; RAISE HelpExc 
            ELSE GInputFileName := PaArgText  
            END (*IF*)
          ; INC ( PaArgNo )
          END (*WHILE*)
        EXCEPT HelpExc
        => DisplayVersion ( )
        ; IF GDoHelp THEN DisplayHelp ( ) END (*IF*)
        ; RAISE Terminate 
        END (*EXCEPT*)
      ; IF GDoGenIntFsm AND GDoGenSrcFsm
        THEN
          Wr . PutText
            ( Stdio . stderr , "-l and -L are mutually exclusive." ) 
        ; Wr . PutText ( Stdio . stderr , "  Doing -L only." ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
        ELSIF GDoGenIntFsm 
        THEN
          Wr . PutText
            ( Stdio . stderr
            , "Command-line option \"-l\" is not yet implemented, ignored."
            ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
        END (*IF*)
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
        ( Stdio . stderr , "      (implies generate an INTERFACE.)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -t Count and number intermediate tokens." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -T Declare intermediate tokens (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE.)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -c Emit intermediate token property sets." ) 
    ; Wr . PutText ( Stdio . stderr , " (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE.)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    
    ; Wr . PutText
        ( Stdio . stderr , "  -l Generate intermediate token lex table." ) 
    ; Wr . PutText ( Stdio . stderr , " (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -L Generate source token spelling lex table." ) 
    ; Wr . PutText ( Stdio . stderr , " (implies -s)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -l and -L are mutually exclusive." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -n Generate Image function (tokNo-to-string map)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE and MODULE.)" )
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -F <prefix> of generated file names." ) 
    ; Wr . PutText
        ( Stdio . stderr , " (default: \"Toks\".)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -D <directory> to put output files into." ) 
    ; Wr . PutText
        ( Stdio . stderr , " (default: \".\".)" ) 
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


; PROCEDURE SkipComments ( ) 

  = BEGIN
      WHILE <*NOWARN*> GToken # NIL AND GToken # EOFToken
            AND Text . Length ( GToken ) >= 2
            AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) , "(*")
      DO GToken := GetTok ( ) 
      END (*WHILE*)
    END SkipComments

; PROCEDURE CopyComments ( ) 

  = BEGIN 
      WHILE <*NOWARN*> GToken # NIL AND GToken # EOFToken
            AND Text . Length ( GToken ) >= 2
            AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) , "(*")
      DO (* It's a comment, and so is this. *) 
        Layout . PutText ( GOStream , GToken )
      ; Layout . PutEol ( GOStream ) 
      ; GToken := GetTok ( ) 
      END (*WHILE*)
    END CopyComments 

; PROCEDURE GetSyntTok ( Required : BOOLEAN := TRUE ) : TEXT
  (* Does not return a comment.  Either skips it or copies it,
     but always consumes it.
  *) 

  = VAR LToken : TEXT

  ; BEGIN
      LToken := GetTok ( Required ) (* Consume the current token. *)
    ; WHILE <*NOWARN*> LToken # NIL AND LToken # EOFToken
            AND Text . Length ( LToken ) >= 2
            AND Text . Equal ( Text . Sub ( LToken , 0 , 2 ) , "(*")
      DO (* It's a comment, and so is this. *) 
        IF GCopyingComments AND GOStream # NIL
        THEN 
          Layout . PutText ( GOStream , LToken )
        ; Layout . PutEol ( GOStream )
        END (**IF*) 
      ; LToken := GetTok ( )
      END (*WHILE*)
    ; RETURN LToken 
    END GetSyntTok 

; PROCEDURE GetTok ( Required : BOOLEAN := TRUE ) : TEXT 
  (* Here, "Tok" refers to a token of the input language to
     this metaprogram.  Such tokenization is very crude, 
     consisting of comments in Modula-3 syntax and contiguous
     sequences of non-whitespace characters. NIL result
     means EOF.
  *)

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
        GToken := GetSyntTok ( ) (* Consume the number. *) 
      ; RETURN LValue
      ELSE
        IF Kind # NIL
        THEN 
          MessageLine 
            ( "Bad " & Kind & " argument count: " & Fmt . Int ( LValue ) 
            & ", must be 0, 1, or 2.  Using 0. " 
            )
        ; GToken := GetSyntTok ( ) (* Consume the invalid number. *)
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

; PROCEDURE IsString ( Token : TEXT ; ) : BOOLEAN

  = VAR LLen : INTEGER

  ; BEGIN
      IF <*NOWARN*> Token = NIL OR Token = EOFToken THEN RETURN FALSE END (*IF*)
    ; LLen := Text . Length ( Token )
    ; IF LLen <= 1 THEN RETURN FALSE END (*IF*)
(*TODO: make this handle literal quotes inside the string. *)
    ; IF Text . GetChar ( Token , 0 ) # '\"' THEN RETURN FALSE END (*IF*)
    ; IF Text . GetChar ( Token , LLen - 1 ) # '\"' THEN RETURN FALSE END (*IF*)
    ; RETURN TRUE 
    END IsString

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

(* TODO: Move this to FM3Compress.
         Then use it various places, e.g. dump.
         Also make a LSB-on-right version. *) 

; PROCEDURE CompressedHex ( IntL : LONGINT ) : TEXT
  (* Only the hex digits themselves.  Display them LSB on left. *) 

  = VAR LResidueL : LONGINT 
  ; VAR LBitsL : LONGINT 
  ; VAR LBits : INTEGER
  ; VAR LBytesDoneCt : INTEGER
  ; VAR LResult : TEXT 
  ; VAR LWrT : TextWr . T 

  ; BEGIN
      LResidueL := IntL
    ; LBytesDoneCt := 0
    ; LWrT := TextWr . New ( ) 
    ; LOOP
        IF LBytesDoneCt >= 8 (* Now do 9th byte. *)
        THEN
          LBitsL := Long . And ( LResidueL , 16_FFL )
        ; LBits := VAL ( LBitsL , INTEGER )  
        ; Wr . PutText
            ( LWrT , Fmt . Pad ( Fmt . Unsigned ( LBits ) , 2 , '0' ) )
        ; Wr . PutChar ( LWrT , ' ' ) 
        ; EXIT
        END (*IF*) 
      ; LBitsL := Long . And ( LResidueL , 16_7FL )
      ; LBits := VAL ( LBitsL , INTEGER )  
      ; LResidueL := LResidueL DIV TwoTo6thL 
      ; IF LResidueL # 0L AND LResidueL # 16_FFFFFFFFFFFFFFFFL  
        THEN (* More bytes will follow. *) 
          LBits := Word . Or ( LBits , 16_80 )
        ; Wr . PutText
            ( LWrT , Fmt . Pad ( Fmt . Unsigned ( LBits ) , 2 , '0' ) )
        ; Wr . PutChar ( LWrT , ' ' ) 
        ; LResidueL := LResidueL DIV 2L 
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

; PROCEDURE ArgCtPlus1 ( ArgCt : INTEGER ) : INTEGER

  = BEGIN
      (* ArgCt < 0 means it was not specified. *) 
      RETURN MAX ( 0 , ArgCt ) + 1  
    END ArgCtPlus1 

; PROCEDURE EmitTok ( Name : TEXT ; ArgCt : INTEGER ; String : TEXT := NIL )
  (* PRE: Are generating this token. *) 
  (* No parsing or input consuming done.
     ArgCt < 0 => arg count not emitted.
  *) 

  = <* FATAL IntRefArray . AllocationFailure *>
    BEGIN
      IntRefArray . Assign ( GTokNamesArrayRef , GNextTokNo , Name ) 
    ; Layout . PadAbs ( GOStream , GSemiTab )
    ; Layout . PutText ( GOStream , GConstTag )
    ; GConstTag := "; CONST" (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream , Name )
    ; IF String # NIL
      THEN
        Layout . PutText ( GOStream , " (*" )
      ; Layout . PutText ( GOStream , String )
      ; Layout . PutText ( GOStream , "*)" )
      END (*IF*) 
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

    ; Layout . PadAbs ( GOStream , GEqualSignTab + GCompressedTab )
    ; Layout . PutText ( GOStream , " (*16_"  )
    ; Layout . PutText
        ( GOStream , CompressedHex ( VAL ( GNextTokNo , LONGINT ) ) )
    ; Layout . PutText ( GOStream , "*)" )
    ; Layout . PutEol ( GOStream )
    ; INC ( GNextTokNo ) 
    END EmitTok

; PROCEDURE EmitLoneTok ( ) 

  = VAR LArgCt : [ - 1 .. 7 ] 
  ; VAR LTokName : TEXT 

  ; BEGIN
      LTokName := GToken 
    ; GToken := GetSyntTok ( ) (* Consume the root name. *) 
    ; LArgCt := GetTokArgCt ( "lone"  ) 
    ; IF GDoGenIntToks
      THEN 
        MaybePutMinTokNo ( ) 
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "(* LONE " )
      ; Layout . PutText ( GOStream , LTokName )
      ; Layout . PutText ( GOStream , ": *)" )
      ; Layout . PutEol ( GOStream )
      ; EmitTok ( LTokName  , LArgCt )  
      ELSIF GDoCountIntToks
      THEN INC ( GNextTokNo ) 
      END (*IF*) 
    ; IF Text . Equal ( GToken , "." ) THEN GToken := GetSyntTok ( ) END (*IF*)
    END EmitLoneTok 

; PROCEDURE EmitListToks ( ) 

  = VAR LArgCtOfList , LArgCtOfElem : [ - 1 .. 7 ] 
  ; VAR LRootName : TEXT 

  ; BEGIN
      LRootName := GToken 
    ; GToken := GetSyntTok ( ) (* Consume the root name. *) 
    ; LArgCtOfList
        := MAX ( 0 , GetTokArgCt ( "list" ) )
           + 1 (* Implicit element count argument. *)
 (* ; LArgCtOfElem := GetTokArgCt ( "list element" ) + 1 (* Implicit element count argument. *) *)
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
      ; EmitTok ( LRootName & "LtPatch" , LArgCtOfList )  
      ; EmitTok ( LRootName & "Rt" , LArgCtOfList )
      ; Layout . PutEol ( GOStream )

(* Separate bracketing of each list element is removed, in favor of
   the specific tokens around the specific list element. **
      ; EmitTok ( LRootName & "ElemLt" , LArgCtOfElem )  
      ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
      ; EmitTok ( LRootName & "ElemLtTemp" , LArgCtOfElem )  
      ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
      ; EmitTok ( LRootName & "ElemLtPatch" , LArgCtOfElem )  
      ; EmitTok ( LRootName & "ElemRt" , LArgCtOfElem )  
      ; Layout . PutEol ( GOStream )
*) 
      ELSIF GDoCountIntToks
      THEN INC ( GNextTokNo , 8 ) 
      END (*IF*) 
    ; IF Text . Equal ( GToken , "." ) THEN GToken := GetSyntTok ( ) END (*IF*)
    END EmitListToks 

; PROCEDURE EmitFixedToks ( ) 

  = VAR LArgCtFixed , LArgCtSub : [ - 1 .. 7 ] 
  ; VAR LRootName , LSubName : TEXT 

  ; BEGIN
      LRootName := GToken 
    ; GToken := GetSyntTok ( ) (* Consume the root name. *)
    ; LArgCtFixed := GetTokArgCt ( "fixed" ) 
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
      ; EmitTok ( LRootName & "LtPatch" , LArgCtFixed )
      ; EmitTok ( LRootName & "Rt" , LArgCtFixed )  
      ; Layout . PutEol ( GOStream )
      ELSIF GDoCountIntToks
      THEN INC ( GNextTokNo , 4 ) 
      END (*IF*) 

    ; WHILE IsIdent ( GToken ) 
      DO LSubName := GToken
      ; GToken := GetSyntTok ( ) (* Consume the token suffix. *) 
      ; LArgCtSub := GetTokArgCt ( "interior token" ) 
      ; IF GDoGenIntToks
        THEN 
          EmitTok ( LRootName & LSubName , LArgCtSub )  
        ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
        ; EmitTok ( LRootName & LSubName & "Temp" , LArgCtSub )  
        ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
        ; EmitTok ( LRootName & LSubName & "Patch" , LArgCtSub )
        ; Layout . PutEol ( GOStream )
        ELSIF GDoCountIntToks
        THEN INC ( GNextTokNo , 3 ) 
        END (*IF*) 
      END (*WHILE*)
    ; IF Text . Equal ( GToken , "." ) THEN GToken := GetSyntTok ( ) END (*IF*)
    END EmitFixedToks

; VAR GDoEmitSrcComments := FALSE 

; PROCEDURE EmitSrcTok ( )

  = VAR LSrcName : TEXT
  ; VAR LString : TEXT 
  ; VAR LStringLen : INTEGER 
  ; VAR LNoQuoteString : TEXT 

  ; BEGIN
      LSrcName := GToken 
    ; GToken := GetSyntTok ( ) (* Consume the source token name. *) 
    ; IF IsString ( GToken )
      THEN
        LString := GToken 
      ; GToken := GetSyntTok ( ) (* Consume the string value. *)
      ; IF GDoGenSrcFsm  
        THEN
          LStringLen := Text . Length ( LString ) 
        ; LNoQuoteString := Text . Sub ( LString , 1 , LStringLen - 2 ) 
        ; FM3BuildLexMachine . AddPair
            ( LNoQuoteString , GNextTokNo , ReverseMap := FALSE )
        END (*IF*)
      ; IF NOT GDoEmitSrcComments
        THEN LString := NIL 
        END (*IF*)
      ELSE LString := NIL 
      END (*IF*)
    ; IF GDoGenSrcToks
      THEN 
        MaybePutMinTokNo ( )
      ; IF GDoEmitSrcComments
        THEN 
          Layout . PadAbs ( GOStream , GSemiTab )
        ; Layout . PutText ( GOStream , "(* SRC " )
        ; Layout . PutText ( GOStream , LSrcName )
        ; Layout . PutText ( GOStream , ": *)" )
        ; Layout . PutEol ( GOStream )
        END (*IF*) 
      ; EmitTok ( LSrcName , - 1 , LString )  
      ELSIF GDoCountSrcToks
      THEN INC ( GNextTokNo ) 
      END (*IF*) 
    ; IF Text . Equal ( GToken , "." ) THEN GToken := GetSyntTok ( ) END (*IF*)
    END EmitSrcTok

; PROCEDURE EmitCopyright ( )

  = BEGIN
      Layout . PutEol ( GOStream )
    ; Layout . PutText
        ( GOStream 
        , "(* -----------------------------------------------------------------------1- *)"
        )
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PutText
        ( GOStream 
        , "(* This file is part of the FM3 Modula-3 compiler.                           *)"
        )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText
        ( GOStream 
        , "(* Copyright 2023        Rodney M. Bates.                                    *)"
        )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText
        ( GOStream 
        , "(* rodney.m.bates@acm.org                                                    *)"
        )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText
        ( GOStream 
        , "(* Licensed under the MIT License.                                           *)"
        )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText
        ( GOStream 
        , "(* -----------------------------------------------------------------------2- *)"
        )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitCopyright

; PROCEDURE ArgListAsText ( ) : TEXT

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , Params . Get ( 0 ) ) 
    ; FOR RArgNo := 1 TO Params . Count - 1
      DO
        Wr . PutChar ( LWrT , ' ' )
      ; Wr . PutText ( LWrT , Params . Get ( RArgNo ) ) 
      END (*FOR*) 
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END ArgListAsText 

; PROCEDURE EmitHeader ( )

  = BEGIN
      Layout . PutText
        ( GOStream , "(* This file was generated by FM3's GenTok metaprogram," ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutText ( GOStream , "   from input file \"" ) 
    ; Layout . PutText ( GOStream , GInputFileName ) 
    ; Layout . PutText ( GOStream , "\", with command line " ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutText ( GOStream , "     \"" ) 
    ; Layout . PutText ( GOStream , ArgListAsText ( ) ) 
    ; Layout . PutText ( GOStream , "\". *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitHeader 

; PROCEDURE EmitInterfaceProlog ( )

  = BEGIN
      EmitCopyright ( )
    ; EmitHeader ( ) 

    ; Layout . PutText ( GOStream , "INTERFACE " ) 
    ; Layout . PutText ( GOStream , GInterfaceName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    
    ; IF GDoGenSets 
      THEN
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; IMPORT IntSets" ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
      END (*IF*) 
    END EmitInterfaceProlog

; PROCEDURE EmitInterfaceDecls ( )

  = BEGIN
      IF GDoImageFunc
      THEN
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; TYPE TokTyp = INTEGER " ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
    
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText 
          ( GOStream , "; PROCEDURE Image ( TokNo : TokTyp ) : TEXT " ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
      END (*IF*) 
    
    ; IF GDoGenIntToks
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
      ; Layout . PutText ( GOStream , "; CONST LtToOne = 4    " )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Lt tokcode to get LM Infix tokcode. *)"
          )
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; CONST LtToOnePatch = 5    " )  
      ; Layout . PutEol ( GOStream )

      ; Layout . PadAbs ( GOStream , 8 )
      ; Layout . PutText
          ( GOStream
          , "(* ^Add this to Lt tokcode to get LM Infix patch tokcode. *)"
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

; PROCEDURE GetNumber ( ) : INTEGER
  (* FIRST ( INTEGER ) => Caller should take no further action. *) 

  = VAR LValue : INTEGER
  ; VAR LLabel : TEXT 
  
  ; BEGIN
      LLabel := GToken 
    ; GToken := GetSyntTok ( ) (* Consume "REL" or "ABS" *)  
    ; IF GToken = EOFToken
      THEN
        MessageLine
          ( "Premature EOF looking for a token number after " & LLabel & "." )
      ; RAISE Terminate
      ELSIF NOT IsNum ( GToken , (*VAR*) LValue )
            OR LValue < 0 
      THEN 
        MessageLine
          ( "Invalid number: " & GToken & " following " & LLabel & ", ignored.")
      ; RETURN FIRST ( INTEGER ) 
      ELSE (* LValue is set and valid. *)
        GToken := GetSyntTok ( ) (* Consume The number. *)
      ; Layout . PutText ( GOStream , "(* " ) 
      ; Layout . PutText ( GOStream , LLabel ) 
      ; Layout . PutText ( GOStream , " " ) 
      ; Layout . PutText ( GOStream , Fmt . Int ( LValue ) ) 
      ; Layout . PutText ( GOStream , ": *)" ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
      ; RETURN LValue 
      END (*IF*) 
    END GetNumber 

; PROCEDURE GenTokConsts ( )

  = VAR LValue : INTEGER

  ; BEGIN
      LOOP
        IF GAtEof THEN EXIT END (*IF*)
(*
      ; CopyComments ( ) 
      ; IF GAtEof THEN EXIT END (*IF*) (* CopyComments could make this happen. *)
*)

      (* Unit name, but it's too late now. *)
      ; IF TokEq ( GToken , "UNITNAME" )
        THEN
          MessageLine ( "Too late for a UNITNAME, ignored." )
        ; GToken := GetSyntTok ( ) (* Consume UNITNAME. *)
        ; IF GToken = EOFToken THEN RAISE Terminate END (*IF*) 
        ; IF IsIdent ( GToken ) THEN GToken := GetSyntTok ( ) END (*IF*)
 
      (* Token Numbering commands. *)
        ELSIF TokEq ( GToken , "REL" )
        THEN
          LValue := GetNumber ( )
        ; IF LValue >= 0 THEN INC ( GNextTokNo , LValue ) END (*IF*)  

        ELSIF TokEq ( GToken , "ABS" )
        THEN
          LValue := GetNumber ( )
        ; IF LValue >= 0
          THEN
            IF LValue < GNextTokNo 
            THEN 
              MessageLine 
                ( "Would-be decreasing token number: "
                  & Fmt . Int ( LValue ) & " after "
                  & Fmt . Int ( GNextTokNo )
                  & ", retaining current value: " 
                )
            ELSE 
              GNextTokNo := LValue
            END (*IF*) 
          END (*IF*)  

        (* Token kind commands. *)
        ELSIF TokEq ( GToken , "LONE" )
        THEN 
          GTokKind := TokKindTyp . TkLone 
        ; GToken := GetSyntTok ( ) 
        ELSIF TokEq ( GToken , "LIST" )  
        THEN
          GTokKind := TokKindTyp . TkList 
        ; GToken := GetSyntTok ( ) 
        ELSIF TokEq ( GToken , "FIXED" )  
        THEN 
          GTokKind := TokKindTyp . TkFixed  
        ; GToken := GetSyntTok ( ) 
        ELSIF TokEq ( GToken , "SRC" )  
        THEN 
          GTokKind := TokKindTyp . TkSrc 
        ; GToken := GetSyntTok ( )
        ELSIF IsIdent ( GToken )
        THEN
          CASE GTokKind OF
          | TokKindTyp . TkNull
          => MessageLine ( "No token kind specified, assuming SRC." )
          ; GTokKind := TokKindTyp . TkSrc 
          ; EmitSrcTok ( ) 
          
          | TokKindTyp . TkLone (* Standalone internal token. *)
          => EmitLoneTok ( ) 
            
          | TokKindTyp . TkList (* List construct tokens. *)
          => EmitListToks ( ) 
            
          | TokKindTyp . TkFixed (* Fixed construct tokens. *)
          => EmitFixedToks ( ) 

          | TokKindTyp . TkSrc (* Source token. *)
          => EmitSrcTok ( ) 
          END (*CASE*)
          
        ; IF TokEq ( GToken , "." ) THEN GToken := GetSyntTok ( ) END (*IF*) 
        ELSE
          MessageLine ( "Unrecognized token: " & GToken ) 
        ; GToken := GetSyntTok ( )  
        END (*IF*) 
      END (*LOOP*) 
    ; PutSimpleTokDecl ( "TkMaxTok" , GNextTokNo - 1 ) 
    END GenTokConsts

; PROCEDURE Pass1 ( )

  = VAR LIntFsm , LSrcFsm : FM3LexTable . T

  ; BEGIN
      GInputRdT := OpenInput ( GInputFileName )
    ; IF Rd . EOF ( GInputRdT )
      THEN 
        MessageLine ( "Empty input file: " & GInputFileName )
      ; RAISE Terminate
      END (*IF*)
    ; IF GDoGenIntFsm OR GDoGenSrcFsm
      THEN FM3BuildLexMachine . MakeEmpty ( )
      END (*IF*)
    ; GToken := GetSyntTok ( )
    ; IF TokEq ( GToken , "UNITNAME" )
      THEN 
        GToken := GetSyntTok ( ) 
      ; IF GToken = EOFToken
        THEN
          MessageLine ( "Premature EOF looking for a unit name." )
        ; RAISE Terminate
        ELSIF IsIdent ( GToken )
        THEN
          IF GDoOverrideUNITNAME
             AND NOT Text . Equal ( GToken , GOutputFilePrefix ) 
          THEN
            MessageLine
              ( "UNITNAME \"" & GToken & "\" Overridden by -F option \""
                & GOutputFilePrefix & "\"."
              ) 
          ELSE
            GOutputFilePrefix := GToken 
          ; SetOutputFileNames ( )
          END (*IF*)
        ; GToken := GetSyntTok ( ) (* Consume the name of the unit. *) 
        ELSE
          MessageLine
            ( "Invalid filename : \"" & GToken & ", using " & GInterfaceName )
        END (*IF*)
      ; GCopyingComments := TRUE 
      END (*IF*)
      
    ; IF GDoGenInterface
      THEN
        GOutputWrT := OpenOutput ( GInterfaceFullName )
      ; GOStream := OpenLayout ( GOutputWrT )
      ; EmitInterfaceProlog ( ) 
      ; EmitInterfaceDecls ( )
      ; GenTokConsts ( )
      ; GCopyingComments := FALSE  
      
      ; EmitInterfaceEpilog ( ) 
      ; CloseLayout ( GOStream )
      END (*IF*) 
    ; IF GDoGenSets
      THEN EmitSetsPickle ( ) 
      END (*IF*) 
    ; IF GDoGenIntFsm
      THEN
        LIntFsm := FM3BuildLexMachine . Build ( )
      ; EmitFsmPickle ( GIntFsmFullName , FM3FileKindIntPkl , LIntFsm )  
      ELSIF GDoGenSrcFsm
      THEN
        LSrcFsm := FM3BuildLexMachine . Build ( )
      ; EmitFsmPickle ( GSrcFsmFullName , FM3FileKindSrcPkl , LSrcFsm )  
      END (*IF*)
    END Pass1
    
; PROCEDURE EmitModuleProlog ( )

  = BEGIN
      EmitCopyright ( ) 
    ; EmitHeader ( ) 

    ; Layout . PutText ( GOStream , "MODULE " ) 
    ; Layout . PutText ( GOStream , GModuleName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    
    ; IF GDoGenSets 
      THEN
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText ( GOStream , "; IMPORT IntSets" ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PutEol ( GOStream )
      END (*IF*) 
    END EmitModuleProlog 

; PROCEDURE GenImageFunc ( Min , Max : INTEGER ) 

  = VAR LName : TEXT

  ; BEGIN
      IF GDoImageFunc
      THEN 
        Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText 
          ( GOStream , "(*EXPORTED*)" ) 
      ; Layout . PutEol ( GOStream )
      ; Layout . PadAbs ( GOStream , GSemiTab )
      ; Layout . PutText 
          ( GOStream , "; PROCEDURE Image ( TokNo : TokTyp ) : TEXT " ) 
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
    END GenImageFunc 

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
      IF GDoGenModule
      THEN
        GOutputWrT := OpenOutput ( GModuleFullName ) 
      ; GOStream := OpenLayout ( GOutputWrT )
      ; EmitModuleProlog ( ) 
      ; GenImageFunc ( GMinTokNo , GNextTokNo - 1 ) 
      ; EmitModuleEpilog ( )
      ; CloseLayout ( GOStream )
      END (*IF*) 
    END Pass2

(* TODO: Move These somewhere shared: (FM3Utils?) *)
; CONST FM3FileTag = "FM3" 
; VAR FM3FileKindIntPkl := FM3SharedGlobals . FM3FileKindTokSetsPkl
; CONST FM3FileKindSrcPkl = 'B'

; CONST FM3Magic
    = ARRAY [ 0 .. 3 ] OF CHAR 
        { VAL ( 16_A2 , CHAR ) , VAL ( 16_0B , CHAR )
        , VAL ( 16_9F , CHAR ) , VAL ( 16_D9 , CHAR )
        }

(* TODO: Move this somewhere universal & fix it up, maybe date & time, e.g.? *)
; PROCEDURE FM3FilePrefix ( Kind : CHAR ) : TEXT 

  = VAR LResult : TEXT
  
  ; BEGIN
      LResult
        := FM3FileTag & Text . FromChar ( Kind ) & Text . FromChars ( FM3Magic )
    ; RETURN LResult 
    END FM3FilePrefix 

; PROCEDURE EmitSetsPickle ( )

  = VAR LWrT : Wr . T

  ; BEGIN
      LWrT := OpenOutput ( GSetsFullName )
    ; TRY (*FINALLY*) 
        TRY (*EXCEPT*)
          Wr . PutText
            ( LWrT , FM3FilePrefix ( FM3SharedGlobals . FM3FileKindTokSetsPkl ) )
     (* ; Pickle . Write
            ( LWrT , GTokNamesArrayRef , write16BitWidechar := FALSE )
     *) 
        ; Pickle . Write
            ( LWrT , GTokSetTemp , write16BitWidechar := FALSE ) 
        ; Pickle . Write
            ( LWrT , GTokSetPatch , write16BitWidechar := FALSE )
        ; Pickle . Write
            ( LWrT , GTokSet1Arg , write16BitWidechar := FALSE ) 
        ; Pickle . Write
            ( LWrT , GTokSet2Args , write16BitWidechar := FALSE )
        ; Pickle . Write
            ( LWrT , GTokSet3Args , write16BitWidechar := FALSE )
        EXCEPT ELSE
          MessageLine ( "Unable to write pickle file" & GSetsFullName ) 
        END (*EXCEPT*)
      FINALLY
        Wr . Close ( LWrT )  
      END (*FINALLY*)
    END EmitSetsPickle

; PROCEDURE EmitFsmPickle
    ( FullName : TEXT
    ; FileKind : CHAR 
    ; LexTable : FM3LexTable . T
    )

  = VAR LWrT : Wr . T

  ; BEGIN
      LWrT := OpenOutput ( FullName )
    ; TRY (*FINALLY*)
        TRY (*EXCEPT*)
          Wr . PutText
            ( LWrT , (* FM3Utils . *) FM3FilePrefix ( FileKind ) )
        ; Pickle . Write
            ( LWrT , LexTable , write16BitWidechar := FALSE )
        EXCEPT ELSE
          MessageLine ( "Unable to write pickle file \"" & FullName & "\"." ) 
        END (*EXCEPT*)
      FINALLY
        Wr . Close ( LWrT )  
      END (*FINALLY*)
    END EmitFsmPickle

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
    ; GCopyingComments := FALSE 
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
    EXCEPT Terminate => 
    END (*EXCEPT*)
  END GenTok
.

