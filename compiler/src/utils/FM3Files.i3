
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Files

; IMPORT File 
; IMPORT OSError 
; IMPORT Thread 

; IMPORT FM3LexTable
; IMPORT FM3SharedGlobals
; IMPORT UniRd

; CONST SrcDirNamexxx = "src"

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

; PROCEDURE OpenUniRd ( SrcFileT : File . T ) : UniRd . T
  (* PRE: We already have an open File . T for the source file we want. *)
  (* Create an Rd.T on it and then a UniRd.T for that. *) 

; PROCEDURE CloseUniRd ( UniRdT : UniRd . T ) 

; PROCEDURE FindAndOpenRdFileT
    ( READONLY DirNameList : ARRAY OF TEXT 
    ; PkgChildDir : TEXT 
    ; FileSimpleName : TEXT 
    ; VAR (*OUT*) FoundInParentDirName : TEXT 
    ; VAR (*OUT*) ResultFileT : File . T
    )
    : BOOLEAN (* Found one. *) 
  (* Look in subdirectory ChildDir of each directory in DirNameList, in
     order, for the first file named FileSimpleName. Open and return
     a File . T, readonly, for it.
  *)

; PROCEDURE ReadFsm
    ( NamePrefix : TEXT ; Kind : FM3SharedGlobals . FileKindTyp )
  : FM3LexTable . T
  RAISES { Thread . Alerted } 

; END FM3Files
.

