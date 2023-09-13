
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Utils

; IMPORT AtomList
; IMPORT Fmt 
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
; IMPORT FM3OpenArray_Char 
; IMPORT FM3SharedGlobals 
; IMPORT FM3SrcToks 
; IMPORT FM3IntToks 

; TYPE IntRangeTyp = IntRanges . RangeTyp

; VAR GroundHashVar := 17345L
; VAR ShiftFactor := 7 
  (* Don't be changing this while hash computations and their uses
     are going on! *)

(*EXPORTED:*)
; PROCEDURE GroundHash ( ) : HashTyp

  = BEGIN
      RETURN GroundHashVar 
    END GroundHash 

(*EXPORTED:*)
; PROCEDURE ContribToHash
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : HashTyp ) 
  (* A value of GroundHash(), altered by a series of ContribToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *)

  = VAR LResult : HashTyp

  ; BEGIN
      LResult
        := BitArith . Xor
             ( BitArith . Shift ( Hash , ShiftFactor ) , Contribution )
    ; Hash := LResult 
    END ContribToHash

(*EXPORTED:*)
; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := GroundHash ( )
    ; LLength := Text . Length ( Key )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHash
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
      LResult := GroundHash ( )
    ; LLength := NUMBER ( Key ^ )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHash
          ( (*IN OUT*) LResult , VAL ( ORD ( Key [ RI ] ) , HashTyp ) )  
      END (*FOR*) 
    ; RETURN LResult 
    END HashOfOAChars 

(*EXPORTED:*)
; PROCEDURE HashOfOAWChars ( Key : REF ARRAY OF WIDECHAR ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := GroundHash ( )
    ; LLength := NUMBER ( Key ^ )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHash
          ( (*IN OUT*) LResult , VAL ( ORD ( Key [ RI ] ) , HashTyp ) )  
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

; PROCEDURE EscapeChar ( WrT : Wr . T ; WCh : WIDECHAR ) 

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
        ELSE LEscChar := 'X' ; LPadLen := 4
        END (*IF*) 
      ; Wr . PutChar ( WrT , LEscChar ) 
      ; Wr . PutText 
          ( WrT 
          , Fmt . Pad 
              ( Fmt . Int ( ORD ( WCh ) , base := 16 ) 
              , length := LPadLen
              , padChar := '0' 
              , align := Fmt . Align . Right 
              ) 
          ) 
      END (* CASE *) 
    END EscapeChar  

(*EXPORTED*) 
; PROCEDURE TextLiteral ( READONLY CharsRef : REF ARRAY OF CHAR ) : TEXT
  (* Insert quotes and escapes. *) 

  = VAR LWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutChar ( LWrT , '\"' )
    ; FOR RI := FIRST ( CharsRef ^ ) TO LAST ( CharsRef ^ )
      DO EscapeChar ( LWrT , CharsRef ^ [ RI ] ) 
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
      DO EscapeChar ( LWrT , CharsRef ^ [ RI ] ) 
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
            , Fmt . Pad 
                ( Fmt . Int ( ORD ( C ) , base := 16 ) 
                , length := LPadLen 
                , padChar := '0' 
                , align := Fmt . Align . Right 
                ) 
            ) 
        END (*CASE*) 
      END (*FOR*) 
    ; RETURN TextWr . ToText ( WrT ) 
    END EscapeText 
*)
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

  = BEGIN (*SrcTokOpndCt*)
      CASE Token OF
      | FM3SrcToks . StkEOF .. FM3SrcToks . StkBOF 
      , FM3SrcToks . StkRwAND .. FM3SrcToks . StkClosePragma
      => RETURN 0 
      | FM3SrcToks . StkIdent .. FM3SrcToks . StkWideCharLit
      => RETURN 3 (* Atom, Line, Column. *) 
(* NOTE: May need to adjust this if literals have additional operands. *) 
      | FM3IntToks . ItkNull .. FM3IntToks . ItkRightEnd
      , FM3IntToks . ItkImport .. FM3IntToks . TkMaxTok
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
      ELSE RETURN -1 (* Not a source token. *) 
      END (*CASE*)
    END TokenOpndCt

(* EXPORTED: *) 
; PROCEDURE PositionImage ( Pos : FM3Base . tPosition ) : TEXT 

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 

  ; BEGIN (*PositionImage*)
      LWrT := TextWr . New ( )
    ; Wr . PutChar ( LWrT , '(') 
    ; Wr . PutText ( LWrT , Fmt . Int ( Pos . Line ) ) 
    ; Wr . PutChar ( LWrT , ',' ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Pos . Column ) ) 
    ; Wr . PutChar ( LWrT , ')' ) 
    ; LResult := TextWr . ToText ( LWrT ) 
    ; RETURN LResult 
    END PositionImage

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
      

; BEGIN
  END FM3Utils 
.
