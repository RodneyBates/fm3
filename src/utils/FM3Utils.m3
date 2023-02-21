
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Utils

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

; PROCEDURE WideTextLiteral ( READONLY WChars : REF ARRAY OF WIDECHAR ) : TEXT
  (* Insert quotes and escapes. *) 

  = BEGIN
(* COMPLETEME *) 
      RETURN NIL 
    END WideTextLiteral 

; BEGIN
  END FM3Utils 
.
