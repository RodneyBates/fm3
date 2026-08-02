        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2026  Rodney M. Bates.                                    *)
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

; PROCEDURE GetUnitTRefOfFileName
    ( SrcFileName : TEXT ) : FM3Units . UnitTRefTyp
  (* POST: Result, # NIL, references a UnitTyp, whose source file is named in
           FM3Units . UnitsAtomDict, and has field UntSrcFileSimpleName set,
           using the simple name taken from SrcFileName, which may include a
           path.  Allocate the UnitTyp if necessary. 
  *) 

; PROCEDURE FindAndOpenUnitSrcFile
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; Adjective : TEXT
    ; ExpImpPosition : FM3Base . tPosition
    )
  : BOOLEAN (* Success *)
  (* POST: IF result, then the source file for UnitRef^ was found and opened,
           and fields UntSrcFilePath, UttSrcUniRd, and UntState are set.
  *) 

; PROCEDURE CloseUnitSrcFile ( UnitTRef : FM3Units . UnitTRefTyp ) 

; PROCEDURE MakePassFileCopy
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; PassFileSuffix : TEXT
    ; RdBackFile : RdBackFile . T  
    )
  (* This has to be done early, while the pass's RdBack file is
     at its right end.  This is before we know whether the copy
     will be needed.
  *)

; PROCEDURE DisAsmPassFile
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; PassFileSuffix : TEXT
    ; L2R : BOOLEAN
    )
  RAISES { RdBackFile . BOF }
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 


; PROCEDURE CleanPassFilesAndCopies ( UnitTRef : FM3Units . UnitTRefTyp )
  (* Only after all passes have been run do we know what pass file
     copies are still hanging around.  Delete them. 
  *)

; PROCEDURE DumpPassExprs
    ( UnitTRef : FM3Units . UnitTRefTyp ; PassFileSuffix : TEXT ) 

; PROCEDURE DumpScopes ( UnitRef : FM3Units . UnitRefTyp ) 

; PROCEDURE DumpDecls ( UnitRef : FM3Units . UnitRefTyp ) 

; PROCEDURE CompileUnitFromSrc ( UnitTRef : FM3Units . UnitTRefTyp )

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

