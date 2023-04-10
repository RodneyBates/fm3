

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
    ; GNextInChar := '\X00'
    ; ConsumeChar ( )
    ; RETURN LResult 
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

; PROCEDURE OpenOutput ( FileName : TEXT ) : Wr . T 

  = VAR LResult : Wr . T 

  ; BEGIN
      IF FileName = NIL
         OR Text . Equal ( FileName , "" )
         OR Text . Equal ( FileName , "-" )
      THEN LResult := Stdio . stdout 
      ELSE
        TRY 
          LResult := FileWr . Open ( FileName ) 
        EXCEPT OSError . E ( Code )
        => Wr . PutText ( Stdio . stderr , "Unable to open output file " ) 
        ; Wr . PutText ( Stdio . stderr , FileName )
        ; LResult := NIL 
        ; RAISE Terminate 
        END (*EXCEPT*)
      ; GOStream := Layout . Init ( GOStream , GOutputWr )
      END (*IF*)
    ; RETURN LResult 
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
; VAR GSavedToksRef : REF ARRAY OF TEXT
; VAR GMaxToks := 500 (* Non-expanding, hopefully enough. *)
; VAR GMinTokDone := FALSE 

; PROCEDURE PutTokNo ( Name : TEXT )

  = BEGIN
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , GConstTag ) 
    ; GConstTag := "; CONST" (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream Name ) 
    ; Layout . PutText ( GOStream " = " ) 
    ; Layout . PutText ( GOStream , Fmt . Int ( GNextTokNo ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END PutTokNo

; PROCEDURE MaybePutMinTokNo ( )

  = BEGIN
      IF NOT GMinTokDone
      THEN
        GMinTokNo := GNextTokNo 
      ; PutTokNo ( "TkMinTok" ) 
      ; Layout . PutEol ( GOStream )
      ; GMinTokDone := TRUE 
      END (*IF*)
    END MaybePutMinTokNo 

; PROCEDURE PutOneTok ( Name : TEXT )

  = BEGIN
      MaybePutMinTokNo ( ) 
    ; GSavedToksRef ^ [ GNextTokNo ] := Name 
    ; Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , GConstTag  )
    ; GConstTag := "; CONST " (* For the future. *) 
    ; Layout . PutText ( GOStream , " "  )
    ; Layout . PutText ( GOStream , Name )
    ; Layout . PutText ( GOStream , " = "  )
    ; Layout . PutText ( GOStream , Fmt . Int ( GNextTokNo ) )
    ; Layout . PutEol ( GOStream )
 
    ; INC ( GNextTokNo ) 
    END PutOneTok 

; PROCEDURE EmitListToks ( RootName : TEXT )

  = BEGIN
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , "(* LIST " )
    ; Layout . PutText ( GOStream , RootName )
    ; Layout . PutText ( GOStream , ": *)" )
    ; Layout . PutEol ( GOStream )

    ; PutOneTok ( RootName & "Lt" )  
    ; PutOneTok ( RootName & "LtTemp" )  
    ; PutOneTok ( RootName & "LtPatch" )  
    ; PutOneTok ( RootName & "LtSub" )  
    ; PutOneTok ( RootName & "LtSubTemp" )  
    ; PutOneTok ( RootName & "LtSubPatch" )  
    ; PutOneTok ( RootName & "RtSub" )  
    ; PutOneTok ( RootName & "Rt" )  
    ; Layout . PutEol ( GOStream )
    END EmitListToks 

; VAR GGileNameOK : BOOLEAN
; VAR GNextTokNo : INTEGER
; VAR GMinTokNo : INTEGER
; VAR GInterfaceName := "FM3Toks"
; VAR GModuleName := "FM3Toks: 

; PROCEDURE Args ( ) 

   = BEGIN
       GInterfaceFileName := GInterfaceName & ".i3"  
     ; GModuleFileName := GModuleName & ".m3"  
     END Args 

; PROCEDURE InterfaceProlog ( )

  = BEGIN
      Layout . PutText ( "(* This file generated by GenTok program. *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream "INTERFACE " ) 
    ; Layout . PutText ( GOStream , GInterfaceName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END InterfaceProlog

; PROCEDURE InterfaceEpilog ( )

  = BEGIN 
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText 
        ( GOStream "; PROCEDURE Image ( TokNo : INTEGER ) : TEXT ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , "; END " )
    ; Layout . PutText ( GOStream , InterfaceName ) 
    ; Layout . PutEol ( GOStream )

      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream "." ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END InterfaceEpilog 

; PROCEDURE SkipComments ( ) 

  = BEGIN 
      WHILE  GToken # NIL
             AND Text . Length ( GToken ) >= 2
             AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) = "(*")
      DO GToken := GetTok ( ) 
      END (*WHILE*)
    END SkipComments 

; PROCEDURE CopyComments ( ) 

  = BEGIN 
      WHILE  GToken # NIL
             AND Text . Length ( GToken ) >= 2
             AND Text . Equal ( Text . Sub ( GToken , 0 , 2 ) = "(*")
      DO (* It's a comment, and so is this. *) 
        Wr . PutText ( GOutputWrT , GToken )
      ; Layout . PutEol ( GOStream ) 
      ; GToken := GetTok ( ) 
      END (*WHILE*)
    END CopyComments 

; VAR GToken : TEXT 

; PROCEDURE GenInterfaceToks ( )

  = VAR LValue : INTEGER 

  ; BEGIN
    ; LOOP
        CopyComments ( ) 

        (* Unit name, but it's too late now. *)
      ; IF TokEq ( GToken , "UNITNAME" )
        THEN
          MessageLine ( "Too late for a UNITNAME, ignored." )
        ; GToken := GetTok ( )
        ; IF GToken = NIL THEN RAISE Terminate END (*IF*) 
        ; IF IsIdent ( GToken ) THEN GToken := GetTok ( ) END (*IF*)

        (* Relative Number. *) 
        IF TokEq ( "+" ) 
        THEN 
          GToken := GetTok ( ) 
        ; IF GToken = NIL
          THEN
            MessageLine ( "Premature EOF loking for a relative number." )
          ; RAISE Terminate
          ELSIF IsNum ( GToken , ((*VAR*) LValue ) 
          THEN INC ( GNextTokNo , LValue ) 
          ELSE 
            MessageLine ( "Invalid relative number: " & GToken & ", ignored.")
          ELSE 
            INC ( GNextTokNo , LValue ) 
          ; GToken := GetTok ( ) 
          END (*IF*)
        END (*IF*) 

        (* Absolute Number. *) 
        IF IsNum ( GToken , ((*VAR*) LValue ) 
        THEN 
          IF LValue < GNextTokNo 
          THEN 
            MessageLine 
              ( "Decreasing token number: "
                & GToken & ", retaining current value: " 
                & Fmt . Int ( LValue )
              )
          ELSE 
            GNextTokNo := LValue
          END (*IF*) 
        ; GToken := GetTok ( ) 

        (* List tokens. *) 
        IF TokEq ( GToken , "LIST" )  
        THEN 
          GToken := GetTok () 
        ; IF GToken = NIL 
          THEN
            MessageLine ( "Premature EOF looking for a list root name." )
          ; RAISE Terminate
          ELSIF IsIdent ( GToken )
          THEN
            EmitListToks ( GToken ) (* Will consume. *) 
          ELSE
            MessageLine
              ( "Invalid list root name: " & GToken & ", using stdout. )
          END (*IF*) 
        ; GToken := GetToken 

        ELSE LRelative := FALSE 
        END (*IF*) 
      END (*LOOP*)

    END GenInterfaceToks 

; PROCEDURE Pass1 ( )

  = BEGIN
      GOutputWrT := OpenOutput ( GInterfaceFileName )
      
    ; GInputRdT := OpenInput ( GInputFileName ) 
    ; GToken := GetTok ( )
    ; IF GToken = NIL
      THEN
        MessageLine ( "Empty input file: " & GInterfaceFileName )
      ; RAISE Terminate
      END (*IF*)
      
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
        ; GInterfaceFileName := GInterfaceName & ".i3"  
        ; GModuleFileName := GModuleName & ".m3"  
        ELSE
          MessageLine
            ( "Invalid filename : \"" & GToken & ", using " & GInterfaceName )
        END (*IF*) 
      ; GToken := GetToken
      END (*IF*)
      
    ; GOutputWrT := OpenOutput ( GInterfaceFileName )
    ; GenInterfaceProlog ( ) 
    ; GenInterfaceToks ( )
    ; GenInterfaceEpilog
    ; IF GInputRdT # Stdio . stdin THEN Rd . Close ( GInputRdT ) END (*IF*) 
    ; IF GOutputWr # Stdio . stdout THEN Wr . Close ( GOutputWrT ) END (*IF*) 
    END Pass1
    
; PROCEDURE GenModuleProlog ( )

  = BEGIN
      Layout . PutText ( "(* This file generated by GenTok program. *)" ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PutText ( GOStream "MODULE " ) 
    ; Layout . PutText ( GOStream , ModuleName ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END GenModuleProlog 

; PROCEDURE GenImageProc ( Min , Max : INTEGER ) 

  = BEGIN 
      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText 
        ( GOStream "; PROCEDURE Image ( TokNo : INTEGER ) : TEXT ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GIndentToks + 2 )
    ; Layout . PutText ( GOStream , "= BEGIN " ) 
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GIndentToks + 6 )
    ; Layout . PutText ( GOStream , "CASE TokNo OF " ) 
    ; Layout . PutEol ( GOStream )

    ; FOR RI := Min TO Max
      DO
        IF GSavedToksRef ^ [ RI ] # NIL
        THEN 
          Layout . PadAbs ( GOStream , GIndentToks + 6 )
        ; Layout . PutText ( GOStream , "| " ) 
        ; Layout . PutText ( GOStream , Fmt . Int ( RI ) ) 
        ; Layout . PutText ( GOStream , " => \"" ) 
        ; Layout . PutText ( GOStream , GSavedToksRef ^ [ RI ] ) 
        ; Layout . PutText ( GOStream , "\"") 
        ; Layout . PutEol ( GOStream )
        END (*IF*) 
      END (*FOR*)
      
    ; Layout . PadAbs ( GOStream , GIndentToks + 6 )
    ; Layout . PutText ( GOStream , "ELSE " ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GIndentToks + 6 )
    ; Layout . PutText ( GOStream , "END (*CASE*) " ) 
    ; Layout . PutEol ( GOStream )

    ; Layout . PadAbs ( GOStream , GIndentToks +  )
    ; Layout . PutText ( GOStream , "END Image " ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END GenImageProc 

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



      Layout . PadAbs ( GOStream , GIndentToks )
      Layout . PadAbs ( GOStream , GIndentToks )
      Layout . PadAbs ( GOStream , GIndentToks )
      Layout . PadAbs ( GOStream , GIndentToks + 4 )
      
; PROCEDURE GenModuleEpilog ( )

  = BEGIN 
    
    ; Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream , "; BEGIN " )
    ; Layout . PutEol ( GOStream )
    
    ; Layout . PadAbs ( GOStream , GIndentToks + 2 )
    ; Layout . PutText ( GOStream , "END " )
    ; Layout . PutText ( GOStream , ModuleName ) 
    ; Layout . PutEol ( GOStream )

      Layout . PadAbs ( GOStream , GIndentToks )
    ; Layout . PutText ( GOStream "." ) 
    ; Layout . PutEol ( GOStream )
    ; Layout . PutEol ( GOStream )
    END GenModuleEpilog 

; PROCEDURE Pass2 ( )

  = BEGIN
      OpenOutput ( GModuleFileName )
    ; GenModuleProlog ( ) 
    ; GenImageProc ( GMinhTokNo , GNextTokNo - 1 ) 
    ; GenModuleEpilog
    ; IF GOutputWr # Stdio . stdout THEN Wr . Close ( GOutputWrT ) END (*IF*) 
    END Pass2
    

; PROCEDURE Init ( )
  = BEGIN 
      GMinTokDone := FALSE
    ; GFileNameOK := TRUE
    ; GNextTokNo := 0
    ; GMinTokNo := 0
    ; GConstTag := "  CONST"
    ; GSavedToksRef := NEW ( REF ARRAY OF TEXT , GMsxToks )
    ; GSavedToksRef ^ := ARRAY OF TEXT { NIL , .. } 
    END Init 

; BEGIN 
    TRY 
      Init ( )
    ; Pass1 ( ) 
    EXCEPT Terminate => 
    END (*EXCEPT*)
  ; IF GInputRdT # Stdio . stdin THEN Rd . Close ( GInputRdT ) END (*IF*) 
  ; IF GOutputWr # Stdio . stdout THEN Wr . Close ( GOutputWrT ) END (*IF*) 
  END GenTok
.

