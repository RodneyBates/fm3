
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Utils

; IMPORT AtomList 
; IMPORT Long AS BitArith
; IMPORT Text 

; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar

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
; PROCEDURE HashOfOAChars ( READONLY Key : ARRAY OF CHAR ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := GroundHash ( )
    ; LLength := NUMBER ( Key )
    ; FOR RI := 0 TO LLength - 1
      DO
        ContribToHash
          ( (*IN OUT*) LResult , VAL ( ORD ( Key [ RI ] ) , HashTyp ) )  
      END (*FOR*) 
    ; RETURN LResult 
    END HashOfOAChars 

(*EXPORTED:*)
; PROCEDURE HashOfOAWChars ( READONLY Key : ARRAY OF WIDECHAR ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := GroundHash ( )
    ; LLength := NUMBER ( Key )
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

  = BEGIN
(* COMPLETEME *) 
      RETURN NIL 
    END CharVarArrayToOAChar

(*EXPORTED:*)
; PROCEDURE WCharVarArrayToOAWChar
    ( VarArr : VarArr_WChar . T ) : REF ARRAY OF WIDECHAR 

  = BEGIN
(* COMPLETEME *) 
      RETURN NIL 
    END WCharVarArrayToOAWChar

; PROCEDURE TextLiteral ( READONLY Chars : REF ARRAY OF CHAR ) : TEXT
  (* Insert quotes and escapes. *) 

  = BEGIN
(* COMPLETEME *)
      RETURN NIL 
    END TextLiteral 

(* From Schutz, Misc.m3: ---------------------------------------
(* EXPORTED: *) 
; PROCEDURE EscapeText ( String : TEXT ) : TEXT 
(* Add Modula-3 TEXT literal escapes. Do not add enclosing double quotes. *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    <* FATAL Rd . Failure *> 
    <* FATAL Rd . EndOfFile *> 
    VAR C : CHAR 

  ; BEGIN (* EscapeText *)
      IF String = NIL THEN RETURN "" END (* IF *) 
    ; EVAL WrT . init ( ) 
    ; EVAL RdT . init ( String ) 
    ; WHILE NOT Rd . EOF ( RdT ) 
      DO 
        C := Rd . GetChar ( RdT ) 
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
        , LbeStd . LeftPlaceholderDelimChar   
        , LbeStd . RightPlaceholderDelimChar   
(* FIXME: Make this adapt to placeholder delimiter strings of length > 1. *) 
        => Wr . PutChar ( WrT , C ) 
        ELSE 
          Wr . PutChar ( WrT , '\\' ) 
(* TODO: Use hex instead of octal here. *) 
        ; Wr . PutText 
            ( WrT 
            , Fmt . Pad 
                ( Fmt . Int ( ORD ( C ) , base := 8 ) 
                , length := 3 
                , padChar := '0' 
                , align := Fmt . Align . Right 
                ) 
            ) 
        END (* CASE *) 
      END (* WHILE *) 
    ; RETURN TextWr . ToText ( WrT ) 
    END EscapeText 

(* EXPORTED: *) 
; PROCEDURE QuoteText ( String : TEXT ) : TEXT 
  (* Add escape sequences and string quotes. *) 

  = BEGIN 
      RETURN "\"" & EscapeText ( String ) & "\""  
    END QuoteText 

   ------------------------------------------------------------- *) 




; PROCEDURE WideTextLiteral ( READONLY WChars : REF ARRAY OF WIDECHAR ) : TEXT
  (* Insert quotes and escapes. *) 

  = BEGIN
(* COMPLETEME *) 
      RETURN NIL 
    END WideTextLiteral 

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

; PROCEDURE AtomListToText ( AL : AtomList . T ): TEXT

  = BEGIN
      RETURN NIL 
    END AtomListToText 

; BEGIN
  END FM3Utils 
.
