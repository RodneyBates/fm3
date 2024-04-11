
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE FM3UnsafeUtils

; IMPORT Compiler

(* This should work for any of the three floating-point types being
   either 32-bit or 64-bit.
*) 

; CONST IsBE = Compiler . ThisEndian = Compiler . ENDIAN . BIG 

; CONST RealLen = BYTESIZE ( REAL ) 
; CONST LongLen = BYTESIZE ( LONGREAL ) 
; CONST ExtLen = BYTESIZE ( EXTENDED ) 

; CONST RealSkip = 4 * ORD ( RealLen < 8 AND IsBE ) 
; CONST LongSkip = 4 * ORD ( LongLen < 8 AND IsBE ) 
; CONST ExtSkip = 4 * ORD ( ExtLen < 8 AND IsBE ) 

(*EXPORTED:*)
; PROCEDURE RealToLongInt  ( Arg : REAL ) : LONGINT

  = VAR LBuffer : ARRAY [ 0 .. 7 ] OF CHAR

  ; BEGIN
      LOOPHOLE ( LBuffer , LONGINT ) := 0L
    ; LOOPHOLE ( ADR ( LBuffer [ RealSkip ] ) , UNTRACED REF REAL ) ^ := Arg 
    ; RETURN LOOPHOLE ( LBuffer , LONGINT ) 
    END RealToLongInt 

(*EXPORTED:*)
; PROCEDURE LongRealToLongInt  ( Arg : LONGREAL ) : LONGINT

  = VAR LBuffer : ARRAY [ 0 .. 7 ] OF CHAR

  ; BEGIN
      LOOPHOLE ( LBuffer , LONGINT ) := 0L  
    ; LOOPHOLE ( ADR ( LBuffer [ LongSkip ] ) , UNTRACED REF LONGREAL ) ^ := Arg 
    ; RETURN LOOPHOLE ( LBuffer , LONGINT ) 
    END LongRealToLongInt 

(*EXPORTED:*)
; PROCEDURE ExtendedToLongInt  ( Arg : EXTENDED ) : LONGINT

  = VAR LBuffer : ARRAY [ 0 .. 7 ] OF CHAR

  ; BEGIN
      LOOPHOLE ( LBuffer , LONGINT ) := 0L  
    ; LOOPHOLE ( ADR ( LBuffer [ ExtSkip ] ) , UNTRACED REF EXTENDED ) ^ := Arg 
    ; RETURN LOOPHOLE ( LBuffer , LONGINT ) 
    END ExtendedToLongInt 

(*EXPORTED:*)
; PROCEDURE LongIntToReal  ( Arg : LONGINT ) : REAL

  = VAR LBuffer : ARRAY [ 0 .. 7 ] OF CHAR

  ; BEGIN
      LOOPHOLE ( LBuffer , LONGINT ) := Arg 
    ; RETURN LOOPHOLE ( ADR ( LBuffer [ RealSkip ] ) , UNTRACED REF REAL ) ^   
    END LongIntToReal 

(*EXPORTED:*)
; PROCEDURE LongIntToLongReal  ( Arg : LONGINT ) : LONGREAL 

  = VAR LBuffer : ARRAY [ 0 .. 7 ] OF CHAR

  ; BEGIN
      LOOPHOLE ( LBuffer , LONGINT ) := Arg 
    ; RETURN
        LOOPHOLE ( ADR ( LBuffer [ LongSkip ] ) , UNTRACED REF LONGREAL ) ^ 
    END LongIntToLongReal
    
(*EXPORTED:*)
; PROCEDURE LongIntToExtended  ( Arg : LONGINT ) : EXTENDED 

  = VAR LBuffer : ARRAY [ 0 .. 7 ] OF CHAR

  ; BEGIN
      LOOPHOLE ( LBuffer , LONGINT ) := Arg 
    ; RETURN
        LOOPHOLE ( ADR ( LBuffer [ ExtSkip ] ) , UNTRACED REF EXTENDED ) ^ 
    END LongIntToExtended 

; BEGIN
    <* ASSERT RealLen = 4 OR RealLen = 8 *> 
    <* ASSERT LongLen = 4 OR LongLen = 8 *> 
    <* ASSERT ExtLen = 4 OR ExtLen = 8 *> 
  END FM3UnsafeUtils 
.

