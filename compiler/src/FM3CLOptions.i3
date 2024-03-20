
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3CLOptions

; IMPORT AtomList

; IMPORT FM3Base 
; IMPORT FM3CLToks 
; IMPORT FM3LexTable 

; VAR SourceDirNames : AtomList . T 
; VAR FileNames : AtomList . T
; VAR OptionsLexTable : FM3LexTable . T

; VAR BuildDirRelPath := "../build"
      (* ^Relative to where the current unit's source file lives. *)

; VAR ResourcePathName : TEXT := "."

; VAR OptionTokSet : OptionTokSetTyp := OptionTokSetEmpty  

; TYPE OptionTokTyp = [ FM3CLToks . TkMinTok .. FM3CLToks . TkMinTok ] 
; TYPE OptionTokSetTyp = SET OF OptionTokTyp
; CONST OptionTokSetEmpty = OptionTokSetTyp { }   

; VAR SrcFileName : TEXT := NIL

; VAR PassNosToKeep : FM3Base . PassNoSetTyp := FM3Base . PassNoSetEmpty    
; VAR PassNosToDisAsm : FM3Base . PassNoSetTyp := FM3Base . PassNoSetEmpty   
 
; END FM3CLOptions 
.

