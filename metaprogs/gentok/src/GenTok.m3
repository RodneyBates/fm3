

(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A metaprogram to generate token declarations. *) 

MODULE GenTok

EXPORTS Main

; IMPORT FileRd
; IMPORT FileWr
; IMPORT OSError
; IMPORT Rd
; IMPORT Stdio
; IMPORT Text 
; IMPORT TextRd 
; IMPORT TextWr 
; IMPORT Wr

; IMPORT Layout 

; <* IMPLICIT*> EXCEPTION Terminate 

; CONST EofChar = '\XFF' 
; CONST SkipChars = SET OF CHAR { } 

; VAR GInputRdT : Rd . T
; VAR GNextInChar : CHAR
; VAR GEof : BOOLEAN := TRUE 

; PROCEDURE MessageLine ( Mst : TEXT )

  = BEGIN
      Wr . PutText ( Stdio . stderr , "In Line " ) 
    ; Wr . PutText ( Stdio . stderr , Fmt . Int ( GInputLineNo ) ) 
      Wr . PutText ( Stdio . stderr , ", " ) 
    ; Wr . PutText ( Stdio . stderr , Msg  ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOF )
    ; Wr . Flush ( Stdio . stderr )
    END MessageLine

; PROCEDURE OpenInput ( FileName : TEXT )

  = BEGIN
      IF FileName = NIL
         OR Text . Equal ( FileName , "" )
         OR Text . Equal ( FileName , "-" )
      THEN GInputRdT := Stdio . stdin 
      ELSE
        TRY 
          GInputRdT := FileRd . Open ( FileName )
        EXCEPT OSError . E ( Code )
        => MessageLine ( "Unable to open input file " & FileName )
        ; GInputRdT := NIL 
        ; RAISE Terminate 
        END (*EXCEPT*)
      END (*IF*)
    ; GNextInChar := '\X00'
    ; ConsumeChar ( ) 
    END OpenInput

; PROCEDURE ConsumeChar ( )
  = BEGIN
      IF GNextInChar = EofChar THEN RETURN END (*IF*)
    ; IF Rd . EOF ( GInputRdT )
      THEN GNextInChar := EofChar 
      ELSE GNextInChar := Rd . GetChar ( GInputRdT )
      END (*IF*) 
    END ConsumeChar

; TYPE TwoChar = ARRAY [ 0 .. 1 ] OF CHAR
; CONST OpenCmnt = TwoChar { '(' , '*' } 
; CONST CloseCmnt = TwoChar { '*' , ')' } 

; PROCEDURE GetTok ( Required : BOOLEAN := TRUE ) : TEXT 
  (* Here, "Tok" refers to a token of the input language to
     this metaprogram.  Such tokenization is very crude, 
     consisting of comments in Modula-3 syntax and contiguous
     sequences of non-whitespace characters. *)

  = VAR LWrT : TextWr . T
  ; VAR LCommentDepth : INTEGER := 0 

  ; PROCEDURE GetDelim ( Delim : TwoChar )
    : BOOLEAN (* Delim found, consumed, and copied to LWrT.
                 Otherwise, nothing is different. *) 

    = BEGIN
        IF GNextInChar = GEof
        THEN
          IF Rquired
          THEN
          END (*IF*) 
        END (*IF*) 
        IF GNextInChar = Delim [ 0 ] 
        THEN
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
      END GetDelim 

  ; BEGIN (* GetTok *) 
      WHILE GNextInChar IN SkipChars DO ConsumeChar ( ) END (*WHILE*)
    ; IF GNextInChar = EofChar THEN RETURN NIL END (*IF*) 
    ; IF GetDelim ( OpenCmnt )
      THEN (* It's a comment, and so is this. *)
        LCommentDepth := 1 
      ; LOOP
          IF GetDelim ( CloseCmnt )
          THEN
            DEC ( LCommentDepth )
          ; IF LCommentDepth = 0 THEN EXIT END (*IF*)
          ELSIF GetDelim ( OpenCmnt )
          THEN INC ( LCommentDepth )
          ELSE
            Wr . PutChar ( LWrT , GNextInChar )
          ; ConsumeChar ( ) 
          END (*IF*) 
        END (*LOOP*)
      ELSE (* A token. *) 
        LWrT := TextWr . New ( )
      ; LOOP
          Wr . PutChar ( LWrT , GNextInChar )
        ; ConsumeChar ( )
        ; IF NOT GNextInChar IN SkipChars
          THEN EXIT
          END (*IF*) 
        END (*LOOP*)
      END (*IF*)
    ; RETURN TextWr . ToText ( LWrT )
    END GetTok 

; VAR GOutputWrT : Wr . T 
; GOStr : Layout . T 

; PROCEDURE OpenOutput ( FileName : TEXT )

  = BEGIN
      IF FileName = NIL
         OR Text . Equal ( FileName , "" )
         OR Text . Equal ( FileName , "-" )
      THEN GOutputWrT := Stdio . stdout 
      ELSE
        TRY 
          GOutputWrT := FileWr . Open ( FileName ) 
        EXCEPT OSError . E ( Code )
        => Wr . PutText ( Stdio . stderr , "Unable to open output file " ) 
        ; Wr . PutText ( Stdio . stderr , FileName )
        ; GOutputWrT := NIL 
        ; RAISE Terminate 
        END (*EXCEPT*)
      ; GOStream := Layout . Init ( GOStream , GOutputWr ) 
      END (*IF*) 
    END OpenOutput

; VAR GInputFileName : TEXT 
; VAR GOutputFileName : TEXT

; PROCEDURE Options ( )

  = BEGIN
      GInputFileName := "-" 
    ; GOutputFileName := "-" 
    END Options

; PROCEDURE TokEq ( Tok : TEXT ; Wanted : TEXT ) : BOOLEAN

  = VAR LTokLen , LWantedLen : INTEGER

  ; BEGIN
      IF Tok = NIL
      THEN IF Wanted = NIL
        THEN RETURN TRUE
        ELSE RETURN FALSE
      ELSIF Wanted = NIL THEN RETURN FALSE
      END (*IF*)
    ; LTokLen := Text . Len ( Tok ) 
    ; LWantedLen := Text . Len ( Tok )
    ; IF LTokLen # LWantedLen
      THEN RETURN FALSE
      ELSIF LTokLen = 0
      THEN RETURN TRUE
      ELSE RETURN Text . Eq ( Tok , Wanted )
      END (*IF*)
    END TokEq

; VAR GIndentToks : INTEGER
; VAR GConstTag : TEXT 

; PROCEDURE PutTok ( RootName : TEXT ; Suffix : TEXT )

  = BEGIN
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , GConstTag  )
    ; Layout . PutText ( GOStream , RootName )
    ; Layout . PutText ( GOStream , Suffix )
    ; Layout . PutText ( GOStream , " = "  )
    ; Layout . PutText ( GOStream , Fmt . Int ( GNextTokNo ) )
    ; Layout . PutEol ( GOStream )
    ; GConstTag := "; CONST "
    ; INC ( GNextTokNo ) 
    END PutTok 

; PROCEDURE EmitListToks ( RootName : TEXT )

  = BEGIN
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , "(* LIST " )
    ; Layout . PutText ( GOStream , RootName )
    ; Layout . PutText ( GOStream , ": *)" )
    ; Layout . PutEol ( GOStream )

    ; PutTok ( RootName , "Lt" )  
    ; PutTok ( RootName , "LtTemp" )  
    ; PutTok ( RootName , "LtPatch" )  
    ; PutTok ( RootName , "LtSub" )  
    ; PutTok ( RootName , "LtSubTemp" )  
    ; PutTok ( RootName , "LtSubPatch" )  
    ; PutTok ( RootName , "RtSub" )  
    ; PutTok ( RootName , "Rt" )  
    ; Layout . PutEol ( GOStream )
    END EmitListToks 

; VAR GGileNameOK : BOOLEAN
; VAR GNextTokNo : INTEGER 

; PROCEDURE Generate ( )

  = VAR LToken : TEXT
  ; VAR LValue : INTEGER 

  ; BEGIN
      OpenInput ( GInputFileName ) 
    ; OpenOutput ( GOutputFileName )

    ; GFileNameOK := TRUE
    ; GNextTokNo := 0
    ; GConstTag := "  CONST "
    
    ; LToken := GetTok ( )
    ; IF LToken = NIL
      THEN
        MessageLine ( "Empty input file." )
      ; RAISE Terminate
      END (*IF*)
    ; LOOP

        (*( Comment. *)
        IF LTokLen # NIL
           AND Text . Len ( LToken ) >= 2
           AND Text . Equal ( Text . Sub ( LToken , 0 , 2 ) = "(*")
        THEN (* It's a comment *) 
          Wr . PutText ( GOutputWrT , LToken )
        ; Layout . PutEol ( GOStream ) 
        ; LToken := GetTok ( )

        (* File name. *)
        ELSIF TokEq ( LToken , "FILENAME" )
        THEN
          IF NOT GFileNameOK 
          THEN MessageLine ( "Too late for a FILENAME, ignored." )
          END (*IF*)
          LToken := GetTok ( )
        ; IF LToken = NIL
          THEN
            MessageLine ( "Premature EOF looking for a file name." )
          ; RAISE Terminate
          ELSIF IsIdent ( LToken )
          THEN
            IF GFileNameOK THEN GIntfName := LToken END (* IF *) 
          ELSE
            MessageLine
              ( "Invalid output file name: " & LToken & ", using stdout. )
          END (*IF*) 
        ; GFileNameOK := FALSE
        ; LToken := GetToken

        (* Relative Number. *) 
        IF TokEq ( "+" ) 
        THEN 
          LToken := GetTok ( ) 
        ; IF LToken = NIL
          THEN
            MessageLine ( "Premature EOF loking for a relative number." )
          ; RAISE Terminate
          END (*IF*) 
        ; IF IsNum ( LToken , ((*VAR*) LValue ) 
          THEN INC ( GNextTokNo , LValue ) 
          ELSE 
            MessageLine ( "Invalid relative number: " & LToken & ", ignored.")
          END (*IF*)
        ELSE LRelative := FALSE 
        END (*IF*) 

        (* Absolute Number. *) 
        IF IsNum ( LToken , ((*VAR*) LValue ) 
        THEN 
          IF LValue < GNextTokNo 
          THEN 
            MessageLine 
              ( "Decreasing token number: "
                & LToken & ", retaining current value: : " 
                & Fmt . Int ( LValue )
              )
          ELSE 
            GNextTokNo := LValue
          END (*IF*) 

        (* List tokens. *) 
        IF TokEq ( LToken , "LIST" 
        THEN 
          LToken := GetTok () 
        ; IF LToken = NIL
          THEN
            MessageLine ( "Premature EOF looking for a list root name." )
          ; RAISE Terminate
          ELSIF IsIdent ( LToken )
          THEN
            EmitListToks ( LToken ) 
          ELSE
            MessageLine
              ( "Invalid list root name: " & LToken & ", using stdout. )
          END (*IF*) 
        ; LToken := GetToken 

        ELSE LRelative := FALSE 
        END (*IF*) 
      END (*LOOP*)

    ; Rd . Close ( GInputRdT ) 
    ; Wr . Close ( GOutputWrT ) 
    END Generate



; BEGIN
  END GenTok
.

