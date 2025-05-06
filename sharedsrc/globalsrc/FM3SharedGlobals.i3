        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedGlobals

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

(* For all FM3-specific file formats: Don't compress these. *) 

; CONST FM3FileTagT = "FM3"

; CONST FM3FileTagA
    = ARRAY [ 0 .. 2 ] OF CHAR
        { VAL ( ORD ( 'F' ) , CHAR )
        , VAL ( ORD ( 'M' ) , CHAR )
        , VAL ( ORD ( '3' ) , CHAR )
        }
  (* ^Normal forward reading and writing at BOF. *) 

; CONST FM3MagicT = "\xA2\x0B\x9F"

; CONST FM3MagicA
    = ARRAY [ 0 ..2 ] OF CHAR 
        { VAL ( 16_A2 , CHAR ) , VAL ( 16_0B , CHAR ) , VAL ( 16_9F , CHAR ) }

; TYPE FileKindTyp = CHAR

(* Specific Filekinds: *)

(* ; CONST FM3FileKindIntPkl = 'A' *) 
; CONST FM3FileKindTokSetsPkl = 'A'
; CONST FM3FileVersionTokSetsPkl = '0'
; CONST FM3FileKindM3RwPkl = 'B' (* Lex machine table. *) 
; CONST FM3FileKindPgRwPkl = 'B' (* Lex machine table. *)
; CONST FM3FileKindCltPkl  = 'B' (* Lex machine table. *)
; CONST FM3FileKindIntPkl  = 'C'
; CONST FM3FileKindSrcPkl  = 'D'
; CONST FM3FileKindRdBack  = 'E' (* Readback file. *)
; CONST FM3FileVersion0    = '0' 

; TYPE FileVersionTyp = CHAR(* Separate numbering for each Kind. *) 

; TYPE PrefixTyp = ARRAY [ 0 .. 7 ] OF CHAR
    (* 3 For the tag, one for kind char, one for version char, 3 for magic *) 

; END FM3SharedGlobals
.


