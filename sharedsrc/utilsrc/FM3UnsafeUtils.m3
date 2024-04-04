
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE FM3UnsafeUtils

; IMPORT Compiler

; CONST IsBE = Compiler . ThisEndian = Compiler . ENDIAN . BIG 

; CONST RealLen = BYTESIZE ( REAL ) 
; CONST LongLen = BYTESIZE ( LONGREAL ) 
; CONST ExtLen = BYTESIZE ( EXTENDED ) 

; CONST RealSkip = 4 * ORD ( RealLen < 8 AND IsBE ) 
; CONST LongSkip = 4 * ORD ( LongLen < 8 AND IsBE ) 
; CONST ExtSkip = 4 * ORD ( ExtLen < 8 AND IsBE ) 

; VAR Buffer : ARRAY [ 0 .. 7 ] OF CHAR  

; PROCEDURE RealToLongInt  ( Arg : REAL ) : LONGINT

  = BEGIN
      LOOPHOLE ( Buffer , LONGINT ) := 0L
    ; LOOPHOLE ( ADR ( Buffer [ RealSkip ] ) , UNTRACED REF REAL ) ^ := Arg 
    ; RETURN LOOPHOLE ( Buffer , LONGINT ) 
    END RealToLongInt 

; PROCEDURE LongRealToLongInt  ( Arg : LONGREAL ) : LONGINT

  = BEGIN
      LOOPHOLE ( Buffer , LONGINT ) := 0L  
    ; LOOPHOLE ( ADR ( Buffer [ LongSkip ] ) , UNTRACED REF LONGREAL ) ^ := Arg 
    ; RETURN LOOPHOLE ( Buffer , LONGINT ) 
    END LongRealToLongInt 

; PROCEDURE ExtendedToLongInt  ( Arg : EXTENDED ) : LONGINT

  = BEGIN
      LOOPHOLE ( Buffer , LONGINT ) := 0L  
    ; LOOPHOLE ( ADR ( Buffer [ ExtSkip ] ) , UNTRACED REF EXTENDED ) ^ := Arg 
    ; RETURN LOOPHOLE ( Buffer , LONGINT ) 
    END ExtendedToLongInt 

; BEGIN
    <* ASSERT RealLen = 4 OR RealLen = 8 *> 
    <* ASSERT LongLen = 4 OR LongLen = 8 *> 
    <* ASSERT ExtLen = 4 OR ExtLen = 8 *> 
  END FM3UnsafeUtils 
.

