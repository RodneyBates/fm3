 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Utils

; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar

; TYPE HashTyp = LONGINT
; CONST HashNull = 0L

; PROCEDURE GroundHash ( ) : HashTyp  

; PROCEDURE ContribToHash 
    ( VAR (*IN OUT*) Hash : HashTyp ; Contribution : HashTyp ) 
  (* A value of GroundHash(), altered by a series of ContribToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *)

; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp 

; PROCEDURE HashOfOAChars ( READONLY Key : ARRAY OF CHAR ) : HashTyp

; PROCEDURE HashOfOAWChars ( READONLY Key : ARRAY OF WIDECHAR ) : HashTyp

; PROCEDURE CharVarArrayToOAChar
    ( VarArr : VarArr_Char . T ) : REF ARRAY OF CHAR 

; PROCEDURE WCharVarArrayToOAWChar
    ( VarArr : VarArr_WChar . T ) : REF ARRAY OF WIDECHAR

; PROCEDURE TextLiteral ( READONLY Chars : REF ARRAY OF CHAR ) : TEXT
  (* Insert quotes and escapes. *) 

; PROCEDURE WideTextLiteral ( READONLY WChars : REF ARRAY OF WIDECHAR ) : TEXT
  (* Insert quotes and escapes. *) 

; PROCEDURE TextToRefArrayChars ( TextVal : TEXT) : REF ARRAY OF CHAR
  (* WARNING: Don't try this unless you know there are no characters
              outside the range of CHAR in TextVal. *) 

; END FM3Utils
.
