 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Utils

(* CM3 library: *)
; IMPORT Wr 

(* FM3: *)
; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3IntToks
; IMPORT FM3OpenArray_Char
; IMPORT FM3SrcToks

; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar

; TYPE HashTyp = FM3Base . HashTyp
; CONST HashNull = FM3Base . HashNull 

; PROCEDURE PutHex ( WrT : Wr . T ; Value : INTEGER )

; PROCEDURE GroundHash ( ) : HashTyp  

; PROCEDURE ContribToHash 
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : HashTyp ) 
  (* A value of GroundHash(), altered by a series of ContribToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *)

; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp 

; PROCEDURE HashOfOAChars ( Key : REF ARRAY OF CHAR ) : HashTyp

; PROCEDURE HashOfOAWChars ( Key : REF ARRAY OF WIDECHAR ) : HashTyp

; PROCEDURE CharVarArrayToOAChar
    ( VarArr : VarArr_Char . T ) : REF ARRAY OF CHAR 

; PROCEDURE WCharVarArrayToOAWChar
    ( VarArr : VarArr_WChar . T ) : REF ARRAY OF WIDECHAR

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
  (* All asterisks, if --no-expr-addrs. *) 

; END FM3Utils
.
