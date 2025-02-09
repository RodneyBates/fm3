        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Compile

(* Overall build and compilation process. *) 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base 
; IMPORT FM3Scopes 
; IMPORT FM3Units
; IMPORT RdBackFile

; PROCEDURE GetUnitRefOfFileName ( SrcFileName : TEXT ) : FM3Units . UnitRefTyp
  (* POST: Result, # NIL, references a UnitTyp, whose source file is named in
           FM3Units . UnitsAtomDict, and has field UntSrcFileSimpleName set,
           using the simple name taken from SrcFileName, which may include a
           path.  Allocate the UnitTyp if necessary. 
  *) 

; PROCEDURE FindAndOpenUnitSrcFile
    ( UnitRef : FM3Units . UnitRefTyp
    ; Adjective : TEXT
    ; ExpImpPosition : FM3Base . tPosition
    )
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

; PROCEDURE DumpPassExprs
    ( UnitRef : FM3Units . UnitRefTyp ; PassFileSuffix : TEXT ) 

; PROCEDURE CompileUnitFromSrc ( UnitRef : FM3Units . UnitRefTyp )

; PROCEDURE CompileOrLoadCLUnit ( SrcFileName : TEXT )
  (* Compile or load the top unit, as named on the command line. *) 

; PROCEDURE CompileCLUnits ( )
  (* Compile the units specified on the command line. *) 

; PROCEDURE ConvertIdentAtom
    ( FromAtom : FM3Base . AtomTyp
    ; FromUnitRef : FM3Units . UnitRefTyp 
    ; ToUnitRef : FM3Units . UnitRefTyp
    )
  : FM3Base . AtomTyp (* Could be FM3Base . AtomNull *)
  (* Return the ident atom in ToUnitRef that has the same spelling 
     that FromAtom has in FromUnitRef.  Null if anything fails.
  *) 

; PROCEDURE ConvertAndCreateIdentAtom
    ( FromAtom : FM3Base . AtomTyp
    ; FromUnitRef : FM3Units . UnitRefTyp 
    ; ToUnitRef : FM3Units . UnitRefTyp
    )
  : FM3Base . AtomTyp (* Could be FM3Base . AtomNull *)
  (* Return the ident atom in ToUnitRef that has the same spelling 
     that FromAtom has in FromUnitRef, creating the atom in ToUnitRef
     if it does not already exist.  
  *) 

; END FM3Compile
.

