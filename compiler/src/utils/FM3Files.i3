
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Files

; IMPORT OSError 
; IMPORT Thread 

; IMPORT FM3LexTable
; IMPORT FM3SharedGlobals
; IMPORT Time 
; IMPORT UniRd 

; TYPE SuffixTyp = { SfxNull , Sfxi3 , Sfxig , Sfxm3 , Sfxmg }
; CONST M3SuffixSet
    = SET OF SuffixTyp
        { SuffixTyp . Sfxi3
        , SuffixTyp . Sfxig
        , SuffixTyp . Sfxm3
        , SuffixTyp . Sfxmg
        } 

; PROCEDURE FileSuffix ( FileName : TEXT ) : SuffixTyp 

; PROCEDURE RemoveSuffix ( FileName : TEXT ) : TEXT  

(*; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT *)

; PROCEDURE OpenUniRd
    ( DirName : TEXT
    ; FileName : TEXT
    ; VAR (*OUT*) UniRdT : UniRd . T
    ; VAR (*OUT*) Time : Time . T
    ) 
  RAISES { OSError . E (* Which means not found. *) } 

; PROCEDURE ReadFsm
    ( NamePrefix : TEXT ; Kind : FM3SharedGlobals . FileKindTyp )
  : FM3LexTable . T
  RAISES { Thread . Alerted } 

; END FM3Files
.

