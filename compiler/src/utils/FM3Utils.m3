 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Utils

; IMPORT AtomList
; IMPORT Fmt AS FM3Fmt (* Why? *)  
; IMPORT Long AS BitArith
; IMPORT Text
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Word
; IMPORT Wr 

; IMPORT IntRanges 
; IMPORT IntSets 
; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar
; IMPORT Layout

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3CLToks AS Clt
; IMPORT FM3IntToks AS Itk
; IMPORT FM3OpenArray_Char 
; IMPORT FM3SharedGlobals
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks
; IMPORT FM3Units 

; TYPE IntRangeTyp = IntRanges . RangeTyp

; VAR ShiftFactor := 7 
  (* Don't be changing this while hash computations and their uses
     are going on! *)

(* EXPORTED: *) 
; PROCEDURE PutHex ( WrT : Wr . T ; Value : INTEGER )

  = BEGIN 
      Wr.PutText
        ( WrT
        , FM3Fmt . Pad
            ( FM3Fmt . Unsigned ( Value , base := 16 )
            , length := 2 * BYTESIZE ( INTEGER )
            , padChar := '0'
            , align := FM3Fmt . Align . Right 
            )
        )
    END PutHex 

(*EXPORTED:*)
; PROCEDURE ContribToHashL
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : HashTyp ) 
  (* A value of HashNull, altered by a series of ContribToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *)

  = VAR LResult : HashTyp

  ; BEGIN
      LResult
        := BitArith . Xor
             ( BitArith . Shift ( Hash , ShiftFactor ) , Contribution )
    ; Hash := LResult 
    END ContribToHashL

(*EXPORTED:*)
; <* INLINE *> PROCEDURE ContribToHashI
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : INTEGER ) 

  = VAR LResult : HashTyp

  ; BEGIN
      ContribToHashL ( Hash , VAL ( Contribution , LONGINT ) ) 
    END ContribToHashI

(*EXPORTED:*)
; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := FM3Base . HashNull
    ; LLength := Text . Length ( Key )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHashL
          ( (*IN OUT*) LResult
          , VAL ( ORD ( Text . GetWideChar ( Key , RI ) ) , HashTyp )
          )  
      END (*FOR*) 
    ; RETURN LResult 
    END HashOfText

(*EXPORTED:*)
; PROCEDURE HashOfOAChars ( Key : REF ARRAY OF CHAR ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := FM3Base . HashNull
    ; LLength := NUMBER ( Key ^ )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHashL
          ( (*IN OUT*) LResult , VAL ( ORD ( Key ^ [ RI ] ) , HashTyp ) )  
      END (*FOR*) 
    ; RETURN LResult 
    END HashOfOAChars 

(*EXPORTED:*)
; PROCEDURE HashOfOAWChars ( Key : REF ARRAY OF WIDECHAR ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := FM3Base . HashNull
    ; LLength := NUMBER ( Key ^ )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHashL
          ( (*IN OUT*) LResult , VAL ( ORD ( Key ^ [ RI ] ) , HashTyp ) )  
      END (*FOR*) 
    ; RETURN LResult 
    END HashOfOAWChars 

(*EXPORTED:*)
; PROCEDURE CharVarArrayToOAChar
    ( VarArr : VarArr_Char . T ) : REF ARRAY OF CHAR

  = VAR LResult : REF ARRAY OF CHAR 
  ; VAR LTouchedRange : IntRangeTyp 
  ; VAR LNumber : Word . T  

  ; BEGIN
      LTouchedRange := VarArr_Char . TouchedRange ( VarArr ) 
    ; IF IntRanges . RangeIsEmpty ( LTouchedRange ) 
      THEN LResult := NEW ( REF ARRAY OF CHAR , 0 )  
      ELSE
        LNumber := IntRanges . NumberOfRange ( LTouchedRange ) 
      ; LResult := NEW ( REF ARRAY OF CHAR , LNumber )
      ; VarArr_Char . FetchSubarray ( VarArr , 0 , LResult ^ ) 
      END (*IF*) 
    ; RETURN LResult  
    END CharVarArrayToOAChar

(*EXPORTED:*)
; PROCEDURE WCharVarArrayToOAWChar
    ( VarArr : VarArr_WChar . T ) : REF ARRAY OF WIDECHAR 

  = VAR LResult : REF ARRAY OF WIDECHAR 
  ; VAR LTouchedRange : IntRangeTyp 
  ; VAR LNumber : Word . T  

  ; BEGIN 
      LTouchedRange := VarArr_WChar . TouchedRange ( VarArr ) 
    ; IF IntRanges . RangeIsEmpty ( LTouchedRange ) 
      THEN LResult := NEW ( REF ARRAY OF WIDECHAR , 0 )  
      ELSE
        LNumber := IntRanges . NumberOfRange ( LTouchedRange ) 
      ; LResult := NEW ( REF ARRAY OF WIDECHAR , LNumber )
      ; VarArr_WChar . FetchSubarray ( VarArr , 0 , LResult ^ ) 
      END (*IF*) 
    ; RETURN LResult  
    END WCharVarArrayToOAWChar

(*EXPORTED*) 
; PROCEDURE EscapeChar ( WrT : Wr . T ; WCh : WIDECHAR ; Wide : BOOLEAN ) 

  = VAR LChOrd : INTEGER
  ; VAR LPadLen : INTEGER
  ; LEscChar : CHAR 

  ; <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* EscapeChar *)
      CASE  WCh 
      OF '\"' , '\'' , '\\' 
      => Wr . PutChar ( WrT , '\\' ) 
      ; Wr . PutChar ( WrT ,  WCh ) 
      | '\n' 
      => Wr . PutChar ( WrT , '\\' ) 
      ; Wr . PutChar ( WrT , 'n' ) 
      | '\t' 
      => Wr . PutChar ( WrT , '\\' ) 
      ; Wr . PutChar ( WrT , 't' ) 
      | '\r' 
      => Wr . PutChar ( WrT , '\\' ) 
      ; Wr . PutChar ( WrT , 'r' ) 
      | '\f' 
      => Wr . PutChar ( WrT , '\\' ) 
      ; Wr . PutChar ( WrT , 'f' ) 
      | ' ' .. '!' , '#' .. '&' , '(' .. '[' , ']' .. '~'
      => Wr . PutChar ( WrT ,  WCh ) 
      ELSE 
        Wr . PutChar ( WrT , '\\' ) 
      ; LChOrd := ORD ( WCh )
      ; IF LChOrd > 16_FFFF
        THEN LEscChar := 'U' ; LPadLen := 6 
        ELSIF Wide OR LChOrd > 16_FF 
        THEN LEscChar := 'X' ; LPadLen := 4
        ELSE LEscChar := 'X' ; LPadLen := 2
        END (*IF*) 
      ; Wr . PutChar ( WrT , LEscChar ) 
      ; Wr . PutText 
          ( WrT 
          , FM3Fmt . Pad 
              ( FM3Fmt . Int ( ORD ( WCh ) , base := 16 ) 
              , length := LPadLen
              , padChar := '0' 
              , align := FM3Fmt . Align . Right 
              ) 
          ) 
      END (* CASE *) 
    END EscapeChar

  ; PROCEDURE EscapeL ( WrT : Wr . T ; ArgL : LONGINT ; Wide : BOOLEAN )

    = VAR LWCh : WIDECHAR

    ; BEGIN
        IF BitArith . LE ( ArgL , 16_FFL ) 
           OR Wide AND BitArith . LE ( ArgL , 16_10FFFFL ) 
        THEN
          LWCh := VAL ( ArgL , WIDECHAR ) 
        ; EscapeChar ( WrT , LWCh , Wide )
        ELSE
          Wr . PutText ( WrT , "\\<16_" )
        ; Wr . PutText ( WrT , FM3Fmt . LongUnsigned ( ArgL , base := 16 ) )
        ; Wr . PutText ( WrT , "" )  
        ; Wr . PutText ( WrT , "\\>" )
        END (*IF*) 
      END EscapeL 

(*EXPORTED*) 
; PROCEDURE TextLiteral ( READONLY CharsRef : REF ARRAY OF CHAR ) : TEXT
  (* Insert quotes and escapes. *) 

  = VAR LWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutChar ( LWrT , '\"' )
    ; FOR RI := FIRST ( CharsRef ^ ) TO LAST ( CharsRef ^ )
      DO EscapeChar ( LWrT , CharsRef ^ [ RI ] , Wide := FALSE ) 
      END (*FOR*) 
    ; Wr . PutChar ( LWrT , '\"' )
    ; LResult := TextWr . ToText ( LWrT ) 
    ; RETURN LResult 
    END TextLiteral 

(*EXPORTED*) 
; PROCEDURE WideTextLiteral ( READONLY CharsRef : REF ARRAY OF WIDECHAR ) : TEXT
  (* Insert quotes and escapes. *) 

  = VAR LWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "W\"" )
    ; FOR RI := FIRST ( CharsRef ^ ) TO LAST ( CharsRef ^ )
      DO EscapeChar ( LWrT , CharsRef ^ [ RI ] , Wide := TRUE ) 
      END (*FOR*) 
    ; Wr . PutChar ( LWrT , '\"' )
    ; LResult := TextWr . ToText ( LWrT ) 
    ; RETURN LResult 
    END WideTextLiteral 

(* EXPORTED: *) 
; PROCEDURE QuoteText ( String : TEXT ) : TEXT 
  (* Add escape sequences and string quotes. *) 

  = BEGIN 
      RETURN "\"" (*& EscapeText ( String )*) & "\""  
    END QuoteText 

(* ------------------------------------------------------------- *) 
(*
; PROCEDURE WideTextLiteral ( READONLY WChars : REF ARRAY OF WIDECHAR ) : TEXT
  (* Insert quotes and escapes. *) 
  (* Add Modula-3 TEXT literal escapes. Do not add enclosing double quotes. *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T
  ; VAR C : CHAR 

  ; BEGIN (* WideTextLiteral *)
      IF WChars = NIL THEN RETURN "" END (*IF*)
    ; LWrT := TextWr . New ( ) 
    ; FOR RI := 0 TO LAST ( WChars ^ ) 
      DO 
        C := WChars ^ [ RI ] 
      ; CASE C 
        OF '\"' , '\'' , '\\' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , C ) 
        | '\n' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 'n' ) 
        | '\t' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 't' ) 
        | '\r' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 'r' ) 
        | '\f' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 'f' ) 
        | ' ' .. '!' , '#' .. '&' , '(' .. '[' , ']' .. '~'
        => Wr . PutChar ( WrT , C ) 
        ELSE
          Wr . PutChar ( WrT , '\\' ) 
        ; LChVal := ORD ( C )
        ; IF LChVal > 16_FFFF
          THEN LEscChar := 'U' ; LPadLen := 6 
          THEN LEscChar := 'X' ; LPadLen := 4
          END (*IF*) 
        ; Wr . PutChar ( WrT , LEscChar ) 
        ; Wr . PutText 
            ( WrT 
            , FM3Fmt . Pad 
                ( FM3Fmt . Int ( ORD ( C ) , base := 16 ) 
                , length := LPadLen 
                , padChar := '0' 
                , align := FM3Fmt . Align . Right 
                ) 
            ) 
        END (*CASE*) 
      END (*FOR*) 
    ; RETURN TextWr . ToText ( WrT ) 
    END EscapeText 
*)

(* EXPORTED: *) 
; PROCEDURE SwitchTokL2R ( Tok : Itk . TokTyp ) : Itk . TokTyp
  (* Switch left and right tokens. *) 

  = BEGIN
      CASE Tok OF
(* FIXME: *) 
      ELSE RETURN Tok
      END (*CASE*) 
    END SwitchTokL2R 

(* EXPORTED: *) 
; PROCEDURE TextToRefArrayChars ( TextVal : TEXT) : REF ARRAY OF CHAR
  (* WARNING: Don't try this unless you know there are no characters
              outside the range of CHAR in TextVal. *) 

  = VAR LLength : INTEGER
  ; VAR LRef : REF ARRAY OF CHAR

  ; BEGIN
      IF TextVal = NIL THEN TextVal := "" END (*IF*)
    ; LLength := Text . Length ( TextVal )
    ; LRef := NEW ( REF ARRAY OF CHAR , LLength + 1 )
    ; Text . SetChars ( LRef ^ , TextVal )
    ; LRef ^ [ LLength ] := '\000'
    ; RETURN LRef 
    END TextToRefArrayChars 

(* EXPORTED: *) 
; PROCEDURE TokenOpndCt ( Token : FM3Base . TokTyp ) : INTEGER 

  = BEGIN (*TokOpndCt*)
      CASE Token OF
      | FM3SrcToks . StkEOF .. FM3SrcToks . StkBOF 
      , FM3SrcToks . StkRwAND .. FM3SrcToks . StkClosePragma
      => RETURN 0 
      | FM3SrcToks . StkIdent .. FM3SrcToks . StkWideCharLit
      => RETURN 3 (* Atom, Line, Column. *) 
(* NOTE: May need to adjust this if literals have additional operands. *) 
      | Itk . TkMinTok .. Itk . TkMaxTok
      => IF IntSets . IsElement ( Token , FM3SharedGlobals . GTokSetGE6Args )
         THEN RETURN 6
         ELSIF IntSets . IsElement ( Token , FM3SharedGlobals . GTokSetGE5Args )
         THEN RETURN 5
         ELSIF IntSets . IsElement ( Token , FM3SharedGlobals . GTokSetGE4Args )
         THEN RETURN 4
         ELSIF IntSets . IsElement ( Token , FM3SharedGlobals . GTokSetGE3Args )
         THEN RETURN 3
         ELSIF IntSets . IsElement ( Token , FM3SharedGlobals . GTokSetGE2Args )
         THEN RETURN 2
         ELSIF IntSets . IsElement ( Token , FM3SharedGlobals . GTokSetGE1Arg )
         THEN RETURN 1
         ELSE RETURN 0
         END (*IF*) 
      ELSE RETURN 0 (* Not an internal token. *) 
      END (*CASE*)
    END TokenOpndCt

(* EXPORTED: *) 
; PROCEDURE PositionImage ( Pos : FM3Base . tPosition ) : TEXT 

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*PositionImage*)
      LWrT := TextWr . New ( )
    ; Wr . PutChar ( LWrT , '[') 
    ; Wr . PutText ( LWrT , FM3Fmt . Int ( Pos . Line ) ) 
    ; Wr . PutChar ( LWrT , ',' ) 
    ; Wr . PutText ( LWrT , FM3Fmt . Int ( Pos . Column ) ) 
    ; Wr . PutChar ( LWrT , ']' ) 
    ; LResult := TextWr . ToText ( LWrT ) 
    ; RETURN LResult 
    END PositionImage

(*EXPORTED.*)
; PROCEDURE SrcTokImage ( SrcTok : FM3SrcToks . TokTyp ) : TEXT 

  = VAR LWrT : Wr . T
  ; LResult : TEXT

  ; BEGIN (*SrcTokImage*)
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "SrcTok " ) 
    ; Wr . PutText ( LWrT , FM3Fmt . Int ( SrcTok ) ) 
    ; Wr . PutChar ( LWrT , ' ') 
    ; Wr . PutText ( LWrT , FM3SrcToks . Name ( SrcTok ) ) 
    ; Wr . PutChar ( LWrT , ' ') 
    ; Wr . PutText ( LWrT , FM3SrcToks . Image ( SrcTok ) ) 
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END SrcTokImage
      
(*EXPORTED.*)
; PROCEDURE CharsOfAtom
    ( AtomMap : FM3Atom_OAChars . T ; Atom : FM3Base . AtomTyp )
  : FM3OpenArray_Char . T

(*TODO: Surely this could be inlined. *) 

  = VAR LCharsRef : FM3OpenArray_Char . T
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*CharsOfAtom*)
      LFound := FM3Atom_OAChars . Key ( AtomMap , Atom , (*OUT*) LCharsRef ) 
    ; IF LFound
      THEN RETURN LCharsRef 
      ELSE RETURN NIL 
      END (*IF*) 
    END CharsOfAtom

(*EXPORTED.*)
; PROCEDURE PutOACharsWr ( WrT : Wr . T ; CharsRef : FM3OpenArray_Char . T ) 

  = BEGIN
      IF CharsRef = NIL THEN RETURN END (*IF*)
    ; FOR RI := FIRST ( CharsRef ^ ) TO LAST ( CharsRef ^ )
      DO Wr . PutChar ( WrT , CharsRef ^ [ RI ] )
      END (*FOR*) 
    END PutOACharsWr 

(*EXPORTED.*) 
; PROCEDURE RefanyImage ( Value : REFANY ) : TEXT
  (* All asterisks, if --no-dump-addrs. *) 

  = BEGIN
      IF Value = NIL
      THEN RETURN "NIL" 
      ELSIF Clt . CltDumpAddrs IN FM3CLOptions . OptionTokSet
      THEN RETURN FM3SharedUtils . RefanyImage ( Value )
      ELSIF BITSIZE ( REFANY ) = 32
      THEN RETURN "16_********" 
      ELSE RETURN "16_****************"
      END (*IF*) 
    END RefanyImage

(*EXPORTED.*)
; PROCEDURE LongHexImage ( Value : LONGINT ) : TEXT 

  = BEGIN (*LongHexImage*)
      RETURN 
        "16_"
        & FM3Fmt . Pad
            ( FM3Fmt . LongUnsigned ( Value, 16 ) , 16 , padChar := '0' ) 
    END LongHexImage

(*EXPORTED.*)
; PROCEDURE IdImageOfAtom ( Atom : FM3Base . AtomTyp ) : TEXT 
  (* Atom no, ident spelling. *) 

  = VAR LSpelling : TEXT 
  ; VAR LTextWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*IdImageOfAtom*)
      LSpelling := FM3Units . IdAtomText ( Atom ) 
    ; LTextWrT := TextWr . New ( )
    ; Wr . PutText ( LTextWrT , "Id" ) 
    ; Wr . PutText ( LTextWrT , FM3Fmt . Int ( Atom ) ) 
    ; Wr . PutText ( LTextWrT , "(\"" ) 
    ; Wr . PutText ( LTextWrT , LSpelling ) 
    ; Wr . PutText ( LTextWrT , "\")" ) 
    ; LResult := TextWr . ToText ( LTextWrT )
    ; RETURN LResult 
    END IdImageOfAtom

(*EXPORTED.*) 
; PROCEDURE IntSetElemsImages
    ( IntSet : IntSets . T ; ElemImageProc : IntImageProcTyp )
  : REF ARRAY OF TEXT 
  (* By applying ElemImageProc to each element of IntSet. *) 

  = VAR IseArrayRef : REF ARRAY OF TEXT
  ; VAR IseElemNo : INTEGER 

  ; PROCEDURE OneElem ( Elem : IntSets . ElemT )
    (* A callback. *)
    = BEGIN 
        IseArrayRef ^ [ IseElemNo ] := ElemImageProc ( Elem ) 
      ; INC ( IseElemNo ) 
      END OneElem
      
  ; VAR LCt : INTEGER
      
  ; BEGIN (*IntSetElemsImages*)
      LCt := IntSets . Card ( IntSet ) 
    ; IseArrayRef := NEW ( REF ARRAY OF TEXT , LCt )
    ; IF LCt = 0 THEN RETURN IseArrayRef END (*IF*)
    ; IseElemNo := 0 
    ; IntSets . ForAllDo ( IntSet , OneElem )
    ; IF IseElemNo # LCt
      THEN <* ASSERT FALSE , "IntSetElemsImages, wrong element count." *> 
      END (*IF*)
    ; RETURN IseArrayRef   
    END IntSetElemsImages

(*EXPORTED.*)
; PROCEDURE ListImage
    ( READONLY Elems : ARRAY OF TEXT
    ; Delims := ARRAY [ 0..2 ] OF CHAR { '{' , ',' , '}' } 
    ; Prefix := ""
    ; OnePerLine := FALSE
    ; LineTo := 80 (* Zero origin. *) 
    )
  : TEXT
  (* Brace-enclosed, comma-separated, multiple-on-a-line (unless OnePerLine),
     list elements from Elems. 
     Lines after the first will start with Prefix.
  *) 

  = VAR LLeft , LSep , LRight : TEXT
  ; VAR LElem : TEXT 
  ; VAR LWrT : TextWr . T
  ; VAR LLayoutT : Layout . T
  ; VAR LResult : TEXT
  ; VAR LLineTo : INTEGER
  ; VAR LCt : INTEGER
  ; VAR LInFirstLine : BOOLEAN 

  ; BEGIN (*ListImage*)
      LCt := NUMBER ( Elems  )
    ; LLeft := Text . FromChar ( Delims [ 0 ] ) 
    ; LSep := Text . FromChar ( Delims [ 1 ] ) 
    ; LRight := Text . FromChar ( Delims [ 2 ] ) 
    ; IF LCt = 0 THEN RETURN LLeft & " " & LRight END (*IF*)
    ; LWrT := TextWr . New ( )
    ; LLayoutT := NEW ( Layout . T ) 
    ; EVAL Layout . Init ( LLayoutT , LWrT )
    ; LLineTo := LineTo - Text . Length ( Prefix ) 

    (* Unroll 1st iteration: *) 
    ; Layout . PutText ( LLayoutT , Wr . EOL )
    ; Layout . PutText ( LLayoutT , Prefix )
    ; Layout . PutText ( LLayoutT , LLeft ) 
    ; Layout . PutChar ( LLayoutT , ' ' )
    ; Layout .PutText ( LLayoutT , Elems [ 0 ] )  
    
    ; FOR RI := 1 TO LCt - 1
      DO
        LElem := Elems [ RI ]
      ; LInFirstLine := Layout . LineNo ( LLayoutT ) = 0  
      ; IF OnePerLine
           OR Layout . CharNo ( LLayoutT )
              + Text . Length ( LElem )
              + ( 1 + Text . Length ( LRight ) ) * ORD ( LInFirstLine )
                (* Don't put another on 1st line unless it would leave
                   room for the right stuff.
                *) 
              > LLineTo
        THEN (* Start a new line. *) 
          Layout . PutEol ( LLayoutT )
        ; Layout . PutText ( LLayoutT , Prefix )
        END (*IF*)
      ; Layout . PutText ( LLayoutT , LSep ) 
      ; Layout . PutChar ( LLayoutT , ' ' )
      ; Layout . PutText ( LLayoutT , LElem ) 
      END (*FOR*) 

    ; IF Layout . LineNo ( LLayoutT ) = 0 (* 1st line *) 
         AND Layout . CharNo ( LLayoutT ) + 1 + Text . Length ( LRight )
            <= LLineTo
      THEN (* Put right stuff on current line. *) 
        Layout . PutChar ( LLayoutT , ' ' )
      ELSE 
        Layout . PutEol ( LLayoutT )
      ; Layout . PutText ( LLayoutT , Prefix ) 
      END (*IF*)
    ; Layout . PutText ( LLayoutT , LRight )
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult        
    END ListImage
 
; BEGIN
  END FM3Utils 
.
