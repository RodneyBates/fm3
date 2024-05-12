        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Compile

(* Overall build and compilation process. *) 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Scopes 
; IMPORT FM3Units
; IMPORT RdBackFile

; PROCEDURE UnitOfFileName ( SrcFileName : TEXT ) : FM3Units . UnitRefTyp
  (* POST: Result, # NIL, references a unit whose source file is named in
           FM3Units . UnitsAtomDict, and has field UntSrcFileSimpleName set,
           both cases using the simple name taken from SrcFileName.
  *) 

; PROCEDURE FindAndOpenUnitSrcFile
    ( UnitRef : FM3Units . UnitRefTyp ; Adjective : TEXT )
  : BOOLEAN (* Success *)
  (* POST: IF result, then the source file for UnitRef^ was found and opened,
           and fields UntSrcFilePath, UntSrcUniRd, and UntState are set.
  *) 

; PROCEDURE CloseUnitSrcFile ( UnitRef : FM3Units . UnitRefTyp ) 

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
    ( UnitRef : FM3Units . UnitRefTyp ; PassFileSuffix : TEXT ; L2R : BOOLEAN )
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 


; PROCEDURE CleanPassFilesAndCopies ( UnitRef : FM3Units . UnitRefTyp )
  (* Only after all passes have been run do we know what pass file
     copies are still hanging around.  Delete them. 
  *) 

; PROCEDURE CompileUnitFromSrc ( UnitRef : FM3Units . UnitRefTyp )

; PROCEDURE CompileCLUnits ( )
  (* Compile the units specified on the command line. *) 

; END FM3Compile
.

