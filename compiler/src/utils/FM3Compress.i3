        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Compress 

(* Reading and writing, to/from a RdBackFile.T,  a stream of 64-bit words,
   using a simple compression technique favoring values that, if viewed as
   64-bit integers have smaller absolute values. *) 

(* Compression scheme: 
   A variable length sequence of one to nine bytes, least significant to most
   significant.  The least significant 7 bits of each byte are data bits.
   The most significant bit is 1 to denote that another, more significant
   byte follows.  The 9th byte, if it occurs, has data in all 8 bits and
   never has a successor.  Nine bytes can hold any 64-bit value.  This
   compression scheme views the values as signed, but clients can view
   them either way, since it always reproduces the bit pattern.  

   For many values in the Unicode code point range of 0..16_10FFFF, this scheme
   is more compact than UTF-8, but is not self-synchronizing.

   Read procedures herein tolerate unnecessary bytes containing extra more 
   significant sign bits, but Write procedures do not produce these. 
*)   

; IMPORT OSError 
; IMPORT RdBackFile 

; PROCEDURE PutBwd ( File : RdBackFile . T ; ValueL : LONGINT )
  RAISES { OSError . E } 
  (* Write compressed bytes to File, in most- to least-significant order. *)

; PROCEDURE PutFwd  ( File : RdBackFile . T ; ValueL : LONGINT )
  RAISES { OSError . E } 
  (* Write compressed bytes to File, in least- to most-significant order. *)

; PROCEDURE GetBwd  ( File : RdBackFile . T ) : LONGINT
  RAISES { OSError . E } 
  (* Read and decode compressed bytes from File.  Treat them as
     being in least- to most-significant order. *)

; END FM3Compress 
. 

