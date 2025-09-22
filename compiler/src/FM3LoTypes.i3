
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3LoTypes

; IMPORT FM3Base
; IMPORT FM3Target 
; IMPORT VarArray_Int_Refany

; TYPE LoTypeNoTyp = FM3Target . LoTypeNoTyp

(* These are the same for all targets: *) 
; CONST LoTypeNoNull      = 0
; CONST LoTypeNoI64        = 1
; CONST LoTypeNoI32        = 2
; CONST LoTypeNoI16        = 3
; CONST LoTypeNoI8         = 4
; CONST LoTypeNoU64        = 5
; CONST LoTypeNoU32        = 6
; CONST LoTypeNoU16        = 7
; CONST LoTypeNoU8         = 8
; CONST LoTypeNoAddr64     = 9
; CONST LoTypeNoAddr32     = 10
; CONST LoTypeNoReal       = 11
; CONST LoTypeNoLongReal   = 12
; CONST LoTypeNoExtended   = 13
; CONST LoTypeNoMultiU32   = 14 (* Multi-word. *) 
; CONST LoTypeNoMultiU64   = 15 (* Multi-word. *) 

(* LoTypeMap, when subcripted by one of the below, will be a duplicate pointer
   to a LoTypeInfoTyp also pointed to at one of the above LoTypeNos. *) 
   
; CONST LoTypeNoLong       = 16
; CONST LoTypeNoInt        = 17
; CONST LoTypeNoWideChar   = 18
; CONST LoTypeNoAddr       = 19

; TYPE SizeTyp = [ 0 .. 65 ] (* 65 for multi-unit things. *)
; TYPE AlignTyp = [ 0 .. 64 ] 

; TYPE LoTypeInfoTyp
    = RECORD
        TiLoTypeNo : LoTypeNoTyp := LoTypeNoNull 
      ; TiNatSize : SizeTyp := 0
      ; TiMinSize : SizeTyp := 0
      ; TiAlign : AlignTyp := 0
      ; TiSigned : BOOLEAN := FALSE 
      END (*RECORD*)
; TYPE LoTypeInfoRefTyp = REF LoTypeInfoTyp 

; PROCEDURE InfoRef ( TypeNo : LoTypeNoTyp ) : LoTypeInfoRefTyp 

; VAR LoTypeMap : VarArray_Int_Refany . T
  (* ^Elements are of LoTypeInfoRefTyp. *) 

; END FM3LoTypes
.
