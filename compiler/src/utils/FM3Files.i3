
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Files

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

; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT 

; PROCEDURE OpenUniRd
    ( FileName , PathHame , Note1 , Note2 : TEXT := "" ) : UniRd . T

; END FM3Files
.

