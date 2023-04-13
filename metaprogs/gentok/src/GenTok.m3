        
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
; IMPORT Pickle2 AS Pickle 
; IMPORT Rd
; IMPORT Stdio
; IMPORT Text 
; IMPORT TextRd 
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr

; IMPORT IntSets 
; IMPORT Layout 

; <* IMPLICIT*> EXCEPTION Terminate 

; CONST EofChar = '\XFF'
; CONST CR = '\n'
; CONST LF = '\r' 
; VAR TokChars := SET OF CHAR { '!' .. '~' } 

; VAR GInputRdT : Rd . T
; VAR GInputLineNo : INTEGER := 1 
; VAR GNextInChar : CHAR
; VAR GAtEof : BOOLEAN := FALSE  

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
        EXCEPT OSError . E ( Code )
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
        GNextInChar := Rd . GetChar ( GInputRdT ) 
      ; IF GNextInChar IN EOLChars (*SET OF CHAR { CR , LF }*)    
        THEN (* New line. *) 
          LCh := GNextInChar 
        ; GNextInChar := Rd . GetChar ( GInputRdT ) 
        ; IF GNextInChar IN SET OF CHAR { CR , LF } AND GNextInChar # LCh 
          THEN (* Tread CR,LF or LF,CR as just one new line. *) 
            GNextInChar := Rd . GetChar ( GInputRdT ) 
          END (*IF *) 
        ; INC ( GInputLineNo ) 
        END (*IF*)
      END (*IF*) 
    END ConsumeChar

; TYPE TwoChar = ARRAY [ 0 .. 1 ] OF CHAR
; CONST OpenCmnt = TwoChar { '(' , '*' } 
; CONST CloseCmnt = TwoChar { '*' , ')' } 

; PROCEDURE GetTok ( Required : BOOLEAN := TRUE ) : TEXT 
  (* Here, "Tok" refers to a token of the input language to
     this metaprogram.  Such tokenization is very crude, 
     consisting of comments in Modula-3 syntax and contiguous
     sequences of non-whitespace characters. NIL result
     means EOF. *)

  = VAR LWrT : TextWr . T
  ; VAR LResult : TEXT 
  ; VAR LCommentDepth : INTEGER := 0 

  ; PROCEDURE StartsWithDelim ( Delim : TwoChar )
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
        ; RETURN NIL 
        END (*IF*)
      ; IF NOT GNextInChar IN TokChars THEN ConsumeChar ( ) 
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
          ; IF NOT GNextInChar IN TokChars
            THEN EXIT
            END (*IF*) 
          END (*LOOP*)
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
        END (*IF*)
      END (*LOOP*)
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END GetTok

; PROCEDURE IsNum ( Token : TEXT ; VAR Value : INTEGER ) : BOOLEAN

  = VAR LLen , LCharNo , LValue : INTEGER
  ; VAR LChar : CHAR 

  ; BEGIN
      IF Token = NIL THEN RETURN FALSE END (*IF*)
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

; CONST Letters = SET OF CHAR { 'A' .. 'Z' , 'a' .. 'z' }
; CONST Digits = SET OF CHAR { '0' .. '9' }
; CONST LettersNDigits = Letters + Digits 

; PROCEDURE IsIdent ( Token : TEXT ) : BOOLEAN

  = VAR LLen , LCharNo : INTEGER

  ; BEGIN
      IF Token = NIL THEN RETURN FALSE END (*IF*)
    ; LLen := Text . Length ( Token )
    ; IF LLen <= 0 THEN RETURN FALSE END (*IF*) 
    ; IF NOT Text . GetChar ( Token , 0 ) IN Letters
      THEN RETURN FALSE
      END (*IF*)
    ; LCharNo := 1
    ; LOOP
        IF LCharNo >= LLen THEN RETURN TRUE END (*IF*)
      ; IF NOT Text . GetChar ( Token , LCharNo ) IN LettersNDigits
        THEN RETURN FALSE
        END (*IF*)
      ; INC ( LCharNo ) 
      END (*LOOP*) 
    END IsIdent 

; VAR GOutputWrT : Wr . T 
; GOStream : Layout . T 

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
        EXCEPT OSError . E ( Code )
        => MessageLine ( "Unable to open output file " & FileName ) 
        ; LWrT := NIL 
        ; RAISE Terminate 
        END (*EXCEPT*)
      END (*IF*)
    ; RETURN LWrT 
    END OpenOutput

; PROCEDURE OpenLayout ( LayoutStream : Layout . T ; Sink : Wr . T ) 

  = BEGIN
      EVAL Layout . Init ( LayoutStream , Sink )
    END OpenLayout 

; PROCEDURE CloseLayout ( VAR LayoutStream : Layout . T )

  = VAR LWrT : Wr . T

  ; BEGIN 
      IF LayoutStream # NIL 
      THEN 
        LWrT := Layout . UsedStream ( LayoutStream )
      ; IF LWrT # NIL AND LWrT # Stdio . stdout 
        THEN Wr . Close ( LWrT ) 
        END (*IF*)
      ; LayoutStream := NIL 
      END (*IF*)
    END CloseLayout

; VAR GInputFileName : TEXT 
; VAR GInterfaceFileName : TEXT
; VAR GModuleFileName : TEXT
; VAR GPickleFileName : TEXT

; PROCEDURE TokEq ( Tok : TEXT ; Wanted : TEXT ) : BOOLEAN

  = VAR LTokLen , LWantedLen : INTEGER

  ; BEGIN
      IF Tok = NIL
      THEN IF Wanted = NIL
        THEN RETURN TRUE
        ELSE RETURN FALSE
        END (*IF*)
      ELSIF Wanted = NIL THEN RETURN FALSE
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

; VAR GSemiIndentPos : INTEGER
; VAR GEqualSignIndentPos : INTEGER := 40 (* Relative *) 
; VAR GCompressedIndentPos : INTEGER := 8 (* Relative *) 
; VAR GTokNoPad : INTEGER := 5 
; VAR GConstTag : TEXT
; VAR GTokNamesArrayRef : REF ARRAY OF TEXT 
; VAR GTokSetTemp : IntSets . T 
; VAR GTokSetPatch : IntSets . T 
; VAR GTokSet1Arg : IntSets . T 
; VAR GTokSet2Args : IntSets . T 
; VAR GMaxToks := 500 (* Non-expanding, hopefully enough. *)
; VAR GMinTokDone := FALSE 

; PROCEDURE PutSimpleTokDecl ( Name : TEXT ; TokNo : INTEGER )

  = BEGIN
      Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , GConstTag ) 
    ; GConstTag := "; CONST" (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream , Name ) 
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
      ; Layout . PutEol ( GOStream )
      ; GMinTokDone := TRUE 
      END (*IF*)
    END MaybePutMinTokNo

; CONST TwoTo6thL = Long . Shift ( 1L , 6 ) 

; PROCEDURE CompressedLongintHex ( IntL : LONGINT ) : TEXT
  (* Only the hex digits themselves. *) 

  = VAR LResidue : LONGINT 
  ; VAR LBitsLong : LONGINT 
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
          LBitsLong := Long . And ( LResidue , 16_FFL )
        ; LBits := VAL ( LBitsLong , INTEGER )  
        ; Wr . PutText ( LWrT , Fmt . Unsigned ( LBits ) ) 
        ; EXIT
        END (*IF*) 
      ; LBitsLong := Long . And ( LResidue , 16_7FL )
      ; LBits := VAL ( LBitsLong , INTEGER )  
      ; LResidue := LResidue DIV TwoTo6thL 
      ; IF LResidue # 0L AND LResidue # 16_FFFFFFFFFFFFFFFFL  
        THEN (* More bytes will follow. *) 
          LBits := Word . And ( LBits , 16_80 )
        ; Wr . PutText ( LWrT , Fmt . Unsigned ( LBits ) )
        ; LResidue := LResidue DIV 2L 
        ; INC ( LBytesDoneCt ) 
        ELSE (* Finishing with < 9 bytes. *) 
          Wr . PutText ( LWrT , Fmt . Unsigned ( LBits ) ) 
        ; EXIT 
        END (*IF*) 
      END (*LOOP*)
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END CompressedLongintHex

; PROCEDURE EmitOneTok ( Name : TEXT ; ArgCt : INTEGER )

  = BEGIN
      MaybePutMinTokNo ( ) 
    ; GTokNamesArrayRef ^ [ GNextTokNo ] := Name 
    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , GConstTag )
    ; GConstTag := "; CONST " (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream , Name )
    ; IF ArgCt >= 0
      THEN
        Layout . PutText ( GOStream , " (*" )
      ; Layout . PutText ( GOStream , Fmt . Int ( ArgCt) )
      ; Layout . PutText ( GOStream , "*)" )
      ; IF ArgCt >= 1
        THEN GTokSet1Arg := IntSets . Include ( GTokSet1Arg , GNextTokNo )
        END (*IF*) 
      ; IF ArgCt >= 2
        THEN GTokSet2Args := IntSets . Include ( GTokSet2Args , GNextTokNo )
        END (*IF*) 
      END (*IF*)
    ; Layout . PadAbs ( GOStream , GSemiIndentPos + GEqualSignIndentPos )
    ; Layout . PutText ( GOStream , " = "  )
    ; Layout . PutText
        ( GOStream , Fmt . Pad ( Fmt . Int ( GNextTokNo ) , GTokNoPad ) )
 
    ; Layout . PadAbs
        ( GOStream 
        , GSemiIndentPos + GEqualSignIndentPos + GCompressedIndentPos 
        )
    ; Layout . PutText ( GOStream , "(* 16_"  )
    ; Layout . PutText
        ( GOStream , CompressedLongintHex ( VAL ( GNextTokNo , LONGINT ) ) )
    ; Layout . PutText ( GOStream , "*)" )
    ; Layout . PutEol ( GOStream )
 
    ; INC ( GNextTokNo ) 
    END EmitOneTok

; PROCEDURE EmitListToks
    ( RootName : TEXT ; ArgCtOfList , ArgCtOfElmt : INTEGER )

  = BEGIN
      Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "(* LIST " )
    ; Layout . PutText ( GOStream , RootName )
    ; Layout . PutText ( GOStream , ": *)" )
    ; Layout . PutEol ( GOStream )

    ; EmitOneTok ( RootName & "Lt" , ArgCtOfList )  
    ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
    ; EmitOneTok ( RootName & "LtTemp" , ArgCtOfList )  
    ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
    ; EmitOneTok ( RootName & "LtPatch" , ArgCtOfList )  
    ; EmitOneTok ( RootName & "Rt" , ArgCtOfList )
    
    ; EmitOneTok ( RootName & "LtElmt" , ArgCtOfElmt )  
    ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
    ; EmitOneTok ( RootName & "LtElmtTemp" , ArgCtOfElmt )  
    ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
    ; EmitOneTok ( RootName & "LtElmtPatch" , ArgCtOfElmt )  
    ; EmitOneTok ( RootName & "RtElmt" , ArgCtOfElmt )  
    ; Layout . PutEol ( GOStream )
    END EmitListToks 

; VAR GNextTokNo : INTEGER
; VAR GMinTokNo : INTEGER
; VAR GInputName := "FM3Toks" 
; VAR GInterfaceName := "FM3Toks"
; VAR GModuleName := "FM3Toks" 
; VAR GPickleName := "FM3Toks" 

; PROCEDURE Args ( ) 

   = BEGIN
     END Args 

; PROCEDURE EmitInterfaceProlog ( )

  = BEGIN
      Layout . PutText
        ( GOStream , "(* This file generated by GenTok program. *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream , "INTERFACE " ) 
    ; Layout . PutText ( GOStream , GInterfaceName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitInterfaceProlog

; PROCEDURE EmitInterfaceDecls ( )

  = BEGIN
      Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText 
        ( GOStream , "; PROCEDURE Image ( TokNo : INTEGER ) : TEXT " ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; VAR TokSetTemp : IntSets . T" )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; VAR TokSetPatch : IntSets . T" )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; VAR TokSetW1Arg : IntSets . T" )
    ; Layout . PutEol ( GOStream )
    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "   (* ^At least one argument. *)" )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; VAR TokSetArgs : IntSets . T" )
    ; Layout . PutEol ( GOStream )
    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "   (* ^At least two arguments. *)" )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; CONST LtToTemp = 1 " )  
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; CONST LtToPatch = 2 " ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; CONST LtToRt = 3 " )  
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitInterfaceDecls

(* Just coding fodder: 

    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 

    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 

    ; Layout . PutEol ( GOStream )
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutText ( GOStream ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
*)

; PROCEDURE EmitInterfaceEpilog ( )

  = BEGIN 
      Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; END " )
    ; Layout . PutText ( GOStream , GInterfaceName ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "." ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitInterfaceEpilog 

; PROCEDURE SkipComments ( ) 

  = BEGIN 
      WHILE <*NOWARN*> GToken # NIL
            AND Text . Length ( GToken ) >= 2
            AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) , "(*")
      DO GToken := GetTok ( ) 
      END (*WHILE*)
    END SkipComments 

; PROCEDURE CopyComments ( ) 

  = BEGIN 
      WHILE <*NOWARN*> GToken # NIL
            AND Text . Length ( GToken ) >= 2
            AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) , "(*")
      DO (* It's a comment, and so is this. *) 
        Layout . PutText ( GOStream , GToken )
      ; Layout . PutEol ( GOStream ) 
      ; GToken := GetTok ( ) 
      END (*WHILE*)
    END CopyComments 

; VAR GToken : TEXT 

; PROCEDURE GetTokArgCt ( Kind : TEXT ) : INTEGER
  (* -1 means none found.  Otherwise, 0, 1 or 2.
     Consumed if non-negative number found
  *) 

  = VAR LValue : INTEGER

  ; BEGIN
      IF NOT IsNum ( GToken , ((*VAR*) LValue ) ) 
      THEN RETURN - 1
      ELSIF LValue IN SET OF [ 0 .. 7 ] { 0 , 1 , 2 }
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
        END (*IF*)
      ; RETURN 0 
      END (*IF*) 
    END GetTokArgCt 

; PROCEDURE GenTokConsts ( )

  = VAR LValue : INTEGER
  ; VAR LArgCtList , LArgCtElmt : INTEGER
  ; VAR LRootName : TEXT 
  ; VAR LSubName : TEXT
  ; VAR LArgCtFixed : [ - 1 .. 7 ]
  ; VAR LArgCtSub : [  - 1  .. 7 ]
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
        ; IF GToken = NIL THEN RAISE Terminate END (*IF*) 
        ; IF IsIdent ( GToken ) THEN GToken := GetTok ( ) END (*IF*)
        END (*IF*) 
 
      (* Token Numbering. *)
      ; LIsAbs := FALSE 
      ; LIsRel := FALSE 
      ; IF TokEq ( GToken , "REL" )
        THEN LIsRel := TRUE 
        ELSIF TokEq ( GToken , "ABS" )
        THEN LIsAbs := TRUE
        END (*IF*)
        
      ; IF LIsRel OR LIsAbs
        THEN 
          GToken := GetTok ( ) 
        ; IF GToken = NIL
          THEN
            MessageLine ( "Premature EOF loking for a token number." )
          ; RAISE Terminate
          ELSIF NOT IsNum ( GToken , ((*VAR*) LValue ) )  
          THEN 
            MessageLine ( "Invalid token number: " & GToken & ", ignored.")
          ; GToken := GetTok ( ) 
          ELSE
            IF LIsRel
            THEN INC ( GNextTokNo , LValue )
            ELSE
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

        (* List construct tokens. *) 
        ELSIF TokEq ( GToken , "LIST" )  
        THEN 
          GToken := GetTok () 
        ; IF GToken = NIL 
          THEN
            MessageLine ( "Premature EOF looking for a list root name." )
          ; RAISE Terminate
          ELSIF NOT IsIdent ( GToken )
          THEN
            MessageLine
              ( "Invalid list root name: " & GToken & ", ignoring." )
          ; LArgCtList := GetTokArgCt ( NIL ) 
          ; LArgCtElmt := GetTokArgCt ( NIL ) 
          ELSE
            LRootName := GToken 
          ; LArgCtList := GetTokArgCt ( "list"  ) 
          ; LArgCtElmt := GetTokArgCt ( "list element" ) 
          ; EmitListToks ( LRootName , LArgCtList , LArgCtElmt ) 
          END (*IF*) 
        ; GToken := GetTok ( ) 
; Wr . Flush (GOutputWrT ) 
        (* Fixed construct tokens. *) 
        ELSIF TokEq ( GToken , "FIXED" )  
        THEN 
          GToken := GetTok () 
        ; IF GToken = NIL 
          THEN
            MessageLine ( "Premature EOF looking for a fixed root name." )
          ; RAISE Terminate
          ELSIF NOT IsIdent ( GToken )
          THEN
            MessageLine
              ( "Invalid fixed root name: " & GToken & ", ignoring." )
          ; WHILE IsIdent ( GToken ) 
            DO LSubName := GToken
            ; LArgCtSub := GetTokArgCt ( "interior token"  ) 
            ; GToken := GetTok ()  
            END (*WHILE*) 
          ELSE
            LRootName := GToken 
          ; GToken := GetTok ()  
          ; LArgCtFixed := GetTokArgCt ( "fixed"  ) 
          ; EmitOneTok ( LRootName & "Lt" , LArgCtFixed )  
          ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
          ; EmitOneTok ( LRootName & "LtTemp" , LArgCtFixed )  
          ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
          ; EmitOneTok ( LRootName & "LtPatch" , LArgCtFixed )
          ; EmitOneTok ( LRootName & "Rt" , LArgCtFixed )  

          ; WHILE IsIdent ( GToken ) 
            DO LSubName := GToken
            ; GToken := GetTok ()  
            ; LArgCtSub := GetTokArgCt ( "interior token" ) 
            ; EmitOneTok ( LRootName & LSubName & "Lt" , LArgCtSub )  
            ; GTokSetTemp := IntSets . Include ( GTokSetTemp , GNextTokNo ) 
            ; EmitOneTok ( LRootName & LSubName & "LtTemp" , LArgCtSub )  
            ; GTokSetPatch := IntSets . Include ( GTokSetPatch , GNextTokNo )  
            ; EmitOneTok ( LRootName & LSubName & "LtPatch" , LArgCtSub )
            ; EmitOneTok ( LRootName & LSubName & "Rt" , LArgCtSub )
            ; GToken := GetTok ( )  
            END (*WHILE*) 
; Wr . Flush (GOutputWrT ) 
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
      GInputFileName := GInputName & ".gentok" 
    ; GInputRdT := OpenInput ( GInputFileName )
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
      ; IF GToken = NIL
        THEN
          MessageLine ( "Premature EOF looking for a unit name." )
        ; RAISE Terminate
        ELSIF IsIdent ( GToken )
        THEN
          GInterfaceName := GToken
        ; GModuleName := GToken 
        ; GPickleName := GToken 
        ELSE
          MessageLine
            ( "Invalid filename : \"" & GToken & ", using " & GInterfaceName )
        END (*IF*) 
      ; GToken := GetTok ( ) 
      END (*IF*)
      
    ; GInterfaceFileName := GInterfaceName & ".i3"  
    ; GModuleFileName := GModuleName & ".m3"  
    ; GPickleFileName := GPickleName & ".pkl"
    
    ; GOutputWrT := OpenOutput ( GInterfaceFileName )
    ; GOStream := NEW ( Layout . T ) 
    ; OpenLayout ( GOStream , GOutputWrT )
    ; EmitInterfaceProlog ( ) 
    ; GenTokConsts ( )
    ; EmitInterfaceDecls ( ) 
    ; EmitInterfaceEpilog ( ) 
    ; CloseLayout ( GOStream )  
    END Pass1
    
; PROCEDURE EmitModuleProlog ( )

  = BEGIN
      Layout . PutText
        ( GOStream , "(* This file generated by GenTok program. *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream , "MODULE " ) 
    ; Layout . PutText ( GOStream , GModuleName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitModuleProlog 

; PROCEDURE GenImageProc ( Min , Max : INTEGER ) 

  = BEGIN 
      Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText 
        ( GOStream , "; PROCEDURE Image ( TokNo : INTEGER ) : TEXT " ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos + 2 )
    ; Layout . PutText ( GOStream , "= BEGIN " ) 
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GSemiIndentPos + 6 )
    ; Layout . PutText ( GOStream , "CASE TokNo OF " ) 
    ; Layout . PutEol ( GOStream )

    ; FOR RI := Min TO Max
      DO
        IF GTokNamesArrayRef ^ [ RI ] # NIL
        THEN 
          Layout . PadAbs ( GOStream , GSemiIndentPos + 6 )
        ; Layout . PutText ( GOStream , "| " ) 
        ; Layout . PutText ( GOStream , Fmt . Int ( RI ) ) 
        ; Layout . PutText ( GOStream , " => RETURN \"" ) 
        ; Layout . PutText ( GOStream , GTokNamesArrayRef ^ [ RI ] ) 
        ; Layout . PutText ( GOStream , "\"") 
        ; Layout . PutEol ( GOStream )
        END (*IF*) 
      END (*FOR*)
      
    ; Layout . PadAbs ( GOStream , GSemiIndentPos + 6 )
    ; Layout . PutText ( GOStream , "ELSE " ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos + 6 )
    ; Layout . PutText ( GOStream , "END (*CASE*) " ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos + 4 )
    ; Layout . PutText ( GOStream , "END Image " ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END GenImageProc 

; PROCEDURE EmitModuleEpilog ( )

  = BEGIN 
    
      Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "; BEGIN " )
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GSemiIndentPos + 2 )
    ; Layout . PutText ( GOStream , "END " )
    ; Layout . PutText ( GOStream , GModuleName ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GSemiIndentPos )
    ; Layout . PutText ( GOStream , "." ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END EmitModuleEpilog 

; PROCEDURE Pass2 ( )

  = BEGIN
      GOutputWrT := OpenOutput ( GModuleFileName ) 
    ; GOStream := NEW ( Layout . T ) 
    ; OpenLayout ( GOStream , GOutputWrT )
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
      GOutputWrT := OpenOutput ( GPickleFileName )
    ; TRY
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
      FINALLY
        Wr . Close ( GOutputWrT )  
      END (*FINALLY*)
    END WritePickle 

; PROCEDURE Init ( )
  = BEGIN 
      GMinTokDone := FALSE
    ; GNextTokNo := 0
    ; GMinTokNo := 0
    ; GConstTag := "  CONST"
    ; GTokNamesArrayRef := NEW ( REF ARRAY OF TEXT , GMaxToks )
    ; FOR RI := FIRST ( GTokNamesArrayRef ^ ) TO LAST ( GTokNamesArrayRef ^ )
      DO GTokNamesArrayRef ^ [ RI ] := NIL
      END (*FOR*)
    ; GTokSetTemp := IntSets . Empty ( )  
    ; GTokSetPatch := IntSets . Empty ( )  
    ; GTokSet1Arg := IntSets . Empty ( )  
    ; GTokSet2Args := IntSets . Empty ( ) 
    ; GAtEof := FALSE
    END Init 

; <* FATAL Thread . Alerted *>
  <* FATAL Wr . Failure *>
  <* FATAL Rd . Failure *>
  <* FATAL Rd . EndOfFile *>
  BEGIN (*GenTok*)
  
    TRY 
      Init ( )
    ; Pass1 ( )
    ; Pass2 ( )
    ; WritePickle ( ) 
    EXCEPT Terminate => 
    END (*EXCEPT*)
  ; CloseLayout ( GOStream ) 
  END GenTok
.

