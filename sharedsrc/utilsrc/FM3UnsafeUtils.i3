
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3UnsafeUtils

(* These are just loopholes, but with left zeros added or
   removed, when sizes don't match.
*) 

; PROCEDURE AddrToLongInt  ( Arg : ADDRESS ) : LONGINT

; PROCEDURE RefanyToLongInt  ( Arg : REFANY ) : LONGINT

; PROCEDURE RealToLongInt  ( Arg : REAL ) : LONGINT

; PROCEDURE LongRealToLongInt  ( Arg : LONGREAL ) : LONGINT

; PROCEDURE ExtendedToLongInt  ( Arg : EXTENDED ) : LONGINT

; PROCEDURE LongIntToAddr  ( Arg : LONGINT ) : ADDRESS

; PROCEDURE LongIntToRefany  ( Arg : LONGINT ) : REFANY

; PROCEDURE LongIntToReal  ( Arg : LONGINT ) : REAL

; PROCEDURE LongIntToLongReal  ( Arg : LONGINT ) : LONGREAL 

; PROCEDURE LongIntToExtended  ( Arg : LONGINT ) : EXTENDED 

; END FM3UnsafeUtils 
.
