        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Compile

; IMPORT FM3Units
; IMPORT RdBackFile  

; PROCEDURE MakePassFileCopy
    ( UnitRef : FM3Units . UnitRefTyp
    ; PassFileSuffix : TEXT
    ; RdBackFile : RdBackFile . T  
    )
  (* This has to be done early, while the pass's RdBack file is
     at its right end.  This is before we know whether the copy
     will be needed.
  *)

; PROCEDURE DisAsmPassFile
    ( UnitRef : FM3Units . UnitRefTyp ; PassFileSuffix : TEXT )
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 


; PROCEDURE CleanPassFileCopies ( UnitRef : FM3Units . UnitRefTyp )
  (* Only after all passes have been run do we know what pass file
     copies are still hanging around.  Delete them. 
  *) 

; PROCEDURE CompileSrcFile ( SrcFileName : TEXT )

; END FM3Compile
.

