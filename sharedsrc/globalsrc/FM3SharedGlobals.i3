        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedGlobals

; FROM File IMPORT Byte

; IMPORT IntSets

; VAR GResourceDirName := "../lib"
(* FIXME: ^Make this a command line option, and maybe a better default. *)

; VAR GIntFilePrefix := "FM3IntToks"

; VAR GSetsLoaded := FALSE
; VAR GTokSetTemp : IntSets . T 
; VAR GTokSetPatch : IntSets . T 
; VAR GTokSetGE1Arg : IntSets . T 
; VAR GTokSetGE2Args : IntSets . T 
; VAR GTokSetGE3Args : IntSets . T 
; VAR GTokSetGE4Args : IntSets . T 
; VAR GTokSetGE5Args : IntSets . T 
; VAR GTokSetGE6Args : IntSets . T 

(* FileTag characters for files: *)

(* For all FM3-specific file formats: Don't compress these. *) klkkkkkkkkkk

; CONST FM3FileTagLtT = "FM3"

; CONST FM3FileTagLtOA
    := ARRAY [ 0 .. 2 ] OF Byte
      { VAL ( ORD ( 'F' ) , Byte )
      , VAL ( ORD ( 'M' ) , Byte )
      , VAL ( ORD ( '3' ) , Byte )
      }
  (* ^For normal forward reading. *) 

; VAR FM3FileTagRtBwdxxx (* Let's not put one on the right end. *) 
    := ARRAY [ 0 .. 2 ] OF Byte
      { VAL ( ORD ( '3' ) , Byte )
      , VAL ( ORD ( 'M' ) , Byte )
      , VAL ( ORD ( 'F' ) , Byte )
      }
  (* For backward reading. *)

; CONST FM3MagicT = "\xA2 , \x0B , \x9F , \xD9"

; CONST FM3MagicOA
    = ARRAY [ 0 .. 3 ] OF CHAR 
        { VAL ( 16_A2 , CHAR ) , VAL ( 16_0B , CHAR )
        , VAL ( 16_9F , CHAR ) , VAL ( 16_D9 , CHAR )
        }

(* Specific Filekinds: *)

(* These ought to be CONST, but that would preclude applying BYTESIZE. *)
(* ; VAR FM3FileKindIntPkl := 'A' *) 
; VAR FM3FileKindTokSetsPkl := 'A'
; VAR FM3FileKindM3RwPkl := 'B' (* Lex machine table. *) 
; VAR FM3FileKindPgRwPkl := 'B' (* Lex machine table. *)
; VAR FM3FileKindCltPkl  := 'B' (* Lex machine table. *)
  (* Command line option strings. *) 
; VAR FM3FileKindRdBackLt := 'D' 
  (* Left end of FM3RdBackFile. *) 
; VAR FM3FileKindRdBackRt  := 'E' 
  (* Right end of FM3RdBackFile. *) 

; END FM3SharedGlobals
.


