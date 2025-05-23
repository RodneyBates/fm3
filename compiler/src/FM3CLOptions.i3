
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Internal representation of command-line options.
   FM3CLArgs parses the command line and sets global variables herein
   with the results.  Code anywhere that depends on options tests these
   global variables to decide what to do.
*) 

INTERFACE FM3CLOptions

; IMPORT AtomList

; IMPORT FM3Atom_Text 
; IMPORT FM3CLToks 
; IMPORT FM3LexTable 

; VAR SrcDirList : REF ARRAY OF TEXT 
; VAR SrcDirMsg : TEXT 
; VAR SourceFileNames : AtomList . T := NIL 
; VAR ImportDirNames : AtomList . T := NIL 
; VAR SourceFileCt : INTEGER := 0 
; VAR ImportDirCt : INTEGER := 0


; VAR IsInitialized := FALSE 

; VAR SourceDirDict : FM3Atom_Text . T
      (* Full path names of directories.  Has reverse map. *) 

; VAR OptionsLexTable : FM3LexTable . T

; VAR BuildDir := "" 
; VAR BuildDirRelPath := "../build"
      (* ^Relative to where the current unit's source file lives. *)

; VAR ResourceDirName : TEXT := "."
; VAR ResourceDirNameList : REF ARRAY OF TEXT := NIL 

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
; CONST PassNo3 = 3
; CONST PassNoMax = PassNo2 + 1
; TYPE PassNoTyp = [ PassNoNull .. PassNoMax ]

; TYPE PassNoSetTyp = SET OF PassNoTyp
; CONST PassNoSetEmpty = PassNoSetTyp { }
; CONST PassNoSetValid = PassNoSetTyp { PassNo1 .. PassNoMax }
; CONST PassNoSetUniv = PassNoSetTyp { PassNoNull .. PassNoMax }

; PROCEDURE PassNoSetUnion ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

; PROCEDURE PassNoSetDiff ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

; PROCEDURE InclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoTyp )

; PROCEDURE ExclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoTyp )

; VAR PassNosToKeep : PassNoSetTyp := PassNoSetEmpty    
; VAR PassNosToDisAsm : PassNoSetTyp := PassNoSetEmpty
; VAR PassNosToDumpExprs : PassNoSetTyp := PassNoSetEmpty
 
; END FM3CLOptions 
.

