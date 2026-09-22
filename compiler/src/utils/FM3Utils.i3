 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Utils

(* CM3 library: *)
; IMPORT Wr

(* Other libs:*)
; IMPORT IntSets 

(* FM3: *)
; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3IntToks
; IMPORT FM3OpenArray_Char
; IMPORT FM3SrcToks

; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar
; IMPORT VarArray_Int_Text AS VarArr_Text
(* TODO: regularize spellings of these vararrays. *) 

; TYPE HashTyp = FM3Base . HashTyp
; CONST HashNull = FM3Base . HashNull 

; PROCEDURE PutHex ( WrT : Wr . T ; Value : INTEGER )

; CONST GroundHash : HashTyp = FM3Base . HashNull   

; PROCEDURE ContribToHashL
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : HashTyp ) 
  (* A value of HashNull, altered by a series of ContribToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *)

; <* INLINE *> PROCEDURE ContribToHashI
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : INTEGER ) 

; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp 

; PROCEDURE HashOfOAChars ( Key : REF ARRAY OF CHAR ) : HashTyp

; PROCEDURE HashOfOAWChars ( Key : REF ARRAY OF WIDECHAR ) : HashTyp

; CONST HashByteSize = BYTESIZE ( HashTyp )  
; TYPE HashCharArrayTyp = ARRAY [ 0 .. HashByteSize - 1 ] OF CHAR 

; PROCEDURE HashToChars ( Hash : HashTyp ) : HashCharArrayTyp 

; PROCEDURE LongToChars ( Long : LONGINT ) : LongCharArrayTyp 
; TYPE LongCharArrayTyp = ARRAY [ 0 .. 7 ] OF CHAR 

; PROCEDURE CharVarArrayToOAChar
    ( READONLY VarArr : VarArr_Char . T ) : REF ARRAY OF CHAR 

; PROCEDURE WCharVarArrayToOAWChar
    ( READONLY VarArr : VarArr_WChar . T ) : REF ARRAY OF WIDECHAR

; PROCEDURE TextVarToOAText ( READONLY VarArr : VarArr_Text . T )
  : REF ARRAY OF TEXT

; PROCEDURE EscapeChar ( WrT : Wr . T ; WCh : WIDECHAR ; Wide : BOOLEAN )

; PROCEDURE EscapeL ( WrT : Wr . T ; ArgL : LONGINT ; Wide : BOOLEAN )

; PROCEDURE TextLiteral ( READONLY Chars : REF ARRAY OF CHAR ) : TEXT
  (* Insert quotes and escapes. *) 

; PROCEDURE WideTextLiteral ( READONLY WChars : REF ARRAY OF WIDECHAR ) : TEXT
  (* Insert quotes and escapes. *) 

; PROCEDURE SwitchTokL2R ( Tok : FM3IntToks . TokTyp ) : FM3IntToks . TokTyp
  (* Switch left and right tokens. *) 

; PROCEDURE TextToRefArrayChars ( TextVal : TEXT) : REF ARRAY OF CHAR
  (* WARNING: Don't try this unless you know there are no characters
              outside the range of CHAR in TextVal. *)
              
; PROCEDURE TokenOpndCt ( Token : FM3Base . TokTyp ) : INTEGER

; PROCEDURE PositionImage ( Pos : FM3Base . tPosition ) : TEXT 

; PROCEDURE SrcTokImage ( SrcTok : FM3SrcToks . TokTyp ) : TEXT 

; PROCEDURE CharsOfAtom
    ( AtomMap : FM3Atom_OAChars . T ; Atom : FM3Base . AtomTyp )
  : FM3OpenArray_Char . T

; PROCEDURE PutOACharsWr ( WrT : Wr . T ; CharsRef : FM3OpenArray_Char . T ) 

; PROCEDURE RefanyImage ( Value : REFANY ) : TEXT
  (* All asterisks, if --no-dump-addrs. *) 

; PROCEDURE LongHexImage ( Value : LONGINT ) : TEXT 

; PROCEDURE IdImageOfAtom ( Atom : FM3Base . AtomTyp ) : TEXT 
  (* Atom no, ident spelling. *) 

; TYPE IntImageProcTyp = PROCEDURE ( Int : INTEGER ) : TEXT 

; PROCEDURE IntSetElemsImages
    ( IntSet : IntSets . T ; ElemImageProc : IntImageProcTyp )
  : REF ARRAY OF TEXT
  (* By applying ElemImageProc to each element of IntSet. *) 

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

; END FM3Utils
.
