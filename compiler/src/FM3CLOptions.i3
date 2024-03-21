
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3CLOptions

; IMPORT AtomList

; IMPORT FM3CLToks 
; IMPORT FM3LexTable 

; VAR SourceDirNames : AtomList . T
; VAR SourceFileNames : AtomList . T
; VAR IncludeDirNames : AtomList . T

; VAR OptionsLexTable : FM3LexTable . T

; VAR BuildDir := "" 
; VAR BuildDirRelPath := "../build"
      (* ^Relative to where the current unit's source file lives. *)

; VAR ResourcePathName : TEXT := "."

(* ------------------- Boolean options are kept in a set -------------------- *)

; TYPE OptionTokTyp = [ FM3CLToks . TkMinTok .. FM3CLToks . TkMaxTok ] 
; TYPE OptionTokSetTyp = SET OF OptionTokTyp
; CONST OptionTokSetEmpty = OptionTokSetTyp { }
; CONST OptionTokSetUniv
    = OptionTokSetTyp { FIRST ( OptionTokTyp ) .. LAST ( OptionTokTyp ) }

; VAR OptionTokSet : OptionTokSetTyp := OptionTokSetEmpty  

; PROCEDURE OptionTokSetUnion
    ( VAR Left : OptionTokSetTyp ; Right : OptionTokSetTyp )

; PROCEDURE OptionTokSetDiff
   ( VAR Left : OptionTokSetTyp ; Right : OptionTokSetTyp )

; PROCEDURE InclOptionTok ( VAR Set : OptionTokSetTyp ; No : OptionTokTyp )

; PROCEDURE ExclOptionTok ( VAR Set : OptionTokSetTyp ; No : OptionTokTyp )

; VAR SrcFileName : TEXT := NIL

(* --------------------- Pass numbers are kept in sets ---------------------- *)

; CONST PassNoNull = 0  
; CONST PassNo1 = 1 
; CONST PassNo2 = 2
; CONST PassNoMax = PassNo2 + 1
; TYPE PassNoTyp = [ PassNoNull .. PassNoMax ]

; TYPE PassNoSetTyp = SET OF PassNoTyp
; CONST PassNoSetEmpty = PassNoSetTyp { }
; CONST PassNoSetAll = PassNoSetTyp { PassNo1 .. PassNoMax }
; CONST PassNoSetUniv = PassNoSetTyp { PassNoNull .. PassNoMax }

; PROCEDURE PassNoSetUnion ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

; PROCEDURE PassNoSetDiff ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

; PROCEDURE InclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoTyp )

; PROCEDURE ExclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoTyp )

; VAR PassNosToKeep : PassNoSetTyp := PassNoSetEmpty    
; VAR PassNosToDisAsm : PassNoSetTyp := PassNoSetEmpty
 
; END FM3CLOptions 
.

