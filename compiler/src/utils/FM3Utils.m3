
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
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

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3CLToks AS Clt
; IMPORT FM3IntToks AS Itk
; IMPORT FM3OpenArray_Char 
; IMPORT FM3SharedGlobals
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks

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
  (* All asterisks, if --no-expr-addrs. *) 

  = BEGIN
      IF Value = NIL
      THEN RETURN "NIL" 
      ELSIF Clt . CltExprAddrs IN FM3CLOptions . OptionTokSet
      THEN RETURN FM3SharedUtils . RefanyImage ( Value )
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

 
; BEGIN
  END FM3Utils 
.
