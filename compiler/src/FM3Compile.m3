        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE  FM3Compile

(* Overall build and compilation process. *) 

; IMPORT FileWr
; IMPORT OSError 
; IMPORT Pathname
; IMPORT Text 
; IMPORT UniRd
; IMPORT Wr

; IMPORT IntSets 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_Text 
; IMPORT FM3Base
; IMPORT FM3CLOptions
; IMPORT FM3DisAsm 
; IMPORT FM3Files 
; IMPORT FM3Globals
; IMPORT FM3Messages 
; IMPORT FM3Pass1 
; IMPORT FM3Pass2
; IMPORT FM3SharedUtils 
; IMPORT FM3Units
; IMPORT FM3Utils
; IMPORT RdBackFile
; IMPORT VarArray_Int_Refany

; TYPE Us = FM3Units . UnitStateTyp 

(*EXPORTED.*)
; PROCEDURE GetUnitRefOfFileName ( SrcFileName : TEXT ) : FM3Units . UnitRefTyp
  (* POST: Result, # NIL, references a UnitTyp whose source file is named in
           FM3Units . UnitsAtomDict, and has field UntSrcFileSimpleName set,
           using the simple name taken from SrcFileName.
           Allocate the UnitTyp if necessary. 
  *) 

  = VAR LSimpleName : TEXT
  ; VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LUnitNameAtom : FM3Base . AtomTyp 

  ; BEGIN
      LSimpleName := Pathname . Last ( SrcFileName ) 
    ; LUnitNameAtom  
        := FM3Atom_Text . MakeAtom  
             ( FM3Units . UnitsAtomDict
             , LSimpleName
             , Hash := FM3Utils . HashOfText ( LSimpleName ) 
             )
    ; LUnitRef 
        := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , LUnitNameAtom )
      (* ^Implied NARROW *)
    ; IF LUnitRef = NIL
      THEN
        LUnitRef := FM3Units . NewUnitRef ( )
      ; LUnitRef ^ . UntSrcFileSimpleName := LSimpleName 
      ; VarArray_Int_Refany . Assign
          ( FM3Units . UnitsMap , LUnitNameAtom , LUnitRef )
      END (*IF*)
    ; RETURN LUnitRef
    END GetUnitRefOfFileName

; VAR SearchPathShown := FALSE 

; PROCEDURE SrcSearchPathOnce ( ) : TEXT  

  = BEGIN
      IF SearchPathShown THEN RETURN "" END (*IF*) 
    ; SearchPathShown := TRUE
    ; RETURN
        FM3Messages . NLIndent
        & "Search directories for source files are:"
        & FM3CLOptions . SrcDirMsg 
    END SrcSearchPathOnce 

(*EXPORTED*) 
; PROCEDURE FindAndOpenUnitSrcFile
    ( UnitRef : FM3Units . UnitRefTyp
    ; Adjective : TEXT
    ; ExpImpPosition : FM3Base . tPosition
    )
  : BOOLEAN (* Success *)
  (* POST: IF result, then the source file for UnitRef^ was found and opened,
           and fields UntSrcFilePath, UntSrcUniRd, and UntState are set.
  *) 

  = VAR LUniRdT : UniRd . T
  ; VAR LSrcDirList : REF ARRAY OF TEXT 
  ; VAR LSearchDir : TEXT 
  ; VAR LDirNumber : INTEGER
  ; VAR LDirSs : INTEGER

  ; BEGIN
      IF UnitRef = NIL THEN RETURN FALSE END (*IF*) 
    ; IF UnitRef . UntSrcFileSimpleName = NIL THEN RETURN FALSE END (*IF*) 
    ; IF UnitRef . UntState # Us . UsNull THEN RETURN FALSE END (*IF*)
    ; LSrcDirList := FM3CLOptions . SrcDirList
    ; IF LSrcDirList = NIL THEN RETURN FALSE END (*IF*)
    ; LDirNumber := NUMBER ( LSrcDirList ^ ) 
    ; LDirSs := 0
    ; LOOP
        IF LDirSs >= LDirNumber
        THEN (* No more directories to search. *) 
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Unable to locate "
                , Adjective
                , "source file "
                , UnitRef ^ . UntSrcFileSimpleName
                , SrcSearchPathOnce ( ) 
                }
            , ExpImpPosition 
            )
        ; UnitRef . UntState := Us . UsNotUsable
        ; RETURN FALSE
        END (*IF*) 
      ; LSearchDir
          := FM3SharedUtils . AbsFileName
               ( FM3CLOptions . SrcDirList ^ [ LDirSs ] )
      ; LUniRdT := NIL 
      ; TRY 
          LUniRdT
            := FM3Files . OpenUniRd
                 ( LSearchDir
                 , UnitRef ^ . UntSrcFileSimpleName
                 , "source file "
                 , NIL
                 )
        EXCEPT
        | OSError . E ( EMsg ) (* This can't happen. *) 
          => LUniRdT := NIL 
        END (*EXCEPT*)
      ; IF LUniRdT # NIL
        THEN (* Found a source file. *)
          UnitRef ^ . UntSrcFilePath := LSearchDir
        ; UnitRef ^ . UntSrcUniRd := LUniRdT
        ; UnitRef ^ . UntState := Us . UsExporting
        ; RETURN TRUE 
        ELSE
          INC ( LDirSs )
          (* And loop. *) 
        END (*IF*)
      END (*LOOP*) 
    END FindAndOpenUnitSrcFile 

(*EXPORTED*) 
; PROCEDURE CloseUnitSrcFile ( UnitRef : FM3Units . UnitRefTyp ) 

  = BEGIN
      IF UnitRef = NIL THEN RETURN END (*IF*) 
    ; IF UnitRef ^ . UntSrcUniRd = NIL THEN RETURN END (*IF*)
    ; UniRd . Close ( UnitRef ^ . UntSrcUniRd )
    ; UnitRef ^ . UntSrcUniRd := NIL 
    END CloseUnitSrcFile 

(*EXPORTED*) 
; PROCEDURE MakePassFileCopy
    ( UnitRef : FM3Units . UnitRefTyp
    ; PassFileSuffix : TEXT
    ; RdBackFileT : RdBackFile . T  
    )
  (* This has to be done early, while the pass's RdBack file is
     at its right end.  This is before we know whether the copy
     will be needed.
  *) 

  = VAR LPassFileSimpleName : TEXT 
  ; VAR LPassFileFullName : TEXT 
  ; VAR LCopyFullName : TEXT
  
  ; BEGIN
      LPassFileSimpleName
        := Pathname . Join
             ( NIL , UnitRef ^ . UntSrcFileSimpleName , PassFileSuffix )
    ; LPassFileFullName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath , LPassFileSimpleName , NIL )
    ; LCopyFullName 
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
    ; RdBackFile . Copy 
        ( RdBackFileT , LCopyFullName , - 1L )
    END MakePassFileCopy 

(*EXPORTED*) 
; PROCEDURE DisAsmPassFile
    ( UnitRef : FM3Units . UnitRefTyp ; PassFileSuffix : TEXT ; L2R : BOOLEAN ) 
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 

  = VAR LPassFileFullName : TEXT 
  ; VAR LCopyFileFullName : TEXT
  ; VAR LDisAsmFileFullName : TEXT
  ; VAR LDisAsmWrT : Wr . T
  ; VAR LRdBack : RdBackFile . T
  
  ; BEGIN
      LPassFileFullName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath
             , UnitRef ^ . UntSrcFileSimpleName 
             , PassFileSuffix
             )
    ; LCopyFileFullName 
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
    ; LDisAsmFileFullName
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . DisAsmFileSuffix ) 
    ; LDisAsmWrT := FileWr . Open ( LDisAsmFileFullName )
    ; LRdBack := RdBackFile . Open ( LCopyFileFullName )

    ; FM3DisAsm . DisAsmWOperands ( LRdBack , LDisAsmWrT , L2R )
    
    ; RdBackFile . Close ( LRdBack , - 1L )      
    ; Wr . Close ( LDisAsmWrT ) 

    ; FM3SharedUtils . DeleteFile ( LCopyFileFullName )

    END DisAsmPassFile

; CONST PassNoSuffixes
    = ARRAY FM3CLOptions . PassNoTyp OF TEXT
        { "<PassNoNull>"
        , FM3Globals . Pass1OutSuffix 
        , FM3Globals . Pass2OutSuffix
        , "<PassNoNull>"
        , ..
        } 

(*EXPORTED*) 
; PROCEDURE CleanPassFilesAndCopies ( UnitRef : FM3Units . UnitRefTyp )
  (* Only after all passes have been run do we know what pass file
     copies are still hanging around.  Delete them.  Also, the existence
     of a copy file implies that no disassembly file was written during
     this pass, which in turn implies an existing disassembly file is
     a leftover from a previous compile.  Delete it too.
  *) 

  = VAR LPassFileFullName : TEXT
  ; VAR LCopyFileFullName : TEXT

  ; BEGIN (*CleanPassFilesAndCopies*)
      FOR RPassNo := FIRST ( FM3CLOptions . PassNoTyp ) 
                  TO LAST ( FM3CLOptions . PassNoTyp )
      DO 
        IF FM3CLOptions . PassNo2 IN UnitRef ^ . UntPassNosDisAsmed
        THEN
          LPassFileFullName
            := Pathname . Join
                 ( UnitRef ^ . UntBuildDirPath
                 , UnitRef ^ . UntSrcFileSimpleName 
                 , PassNoSuffixes [ RPassNo ] 
                 )
        ; LCopyFileFullName 
            := Pathname . Join
                 ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
        ; IF NOT RPassNo IN FM3CLOptions . PassNosToKeep  
          THEN
            FM3SharedUtils . DeleteFile ( LPassFileFullName )
          END (*IF*) 
        ; FM3SharedUtils . DeleteFile ( LCopyFileFullName )
        END (*IF*) 
      END (*FOR*) 
    END CleanPassFilesAndCopies

(*EXPORTED*)
; PROCEDURE CompileUnitFromSrc ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN (*CompileUnitFromSrc*)
      UnitRef ^ . UntSkipStackBase
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi 
    ; FM3Pass1 . RunPass1 ( )
    ; FM3Pass2 . RunPass2 ( )

    ; RdBackFile . Close 
        ( UnitRef ^ . UntPass2OutRdBack , - 1L (* Leave full length. *) )
      (* ^When the next pass is implemented, don't do this. *)

    ; CleanPassFilesAndCopies ( UnitRef ) 
    ; <*ASSERT
          UnitRef ^ . UntSkipStackBase 
            = VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
      *> 
      FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Finished compiling " , UnitRef ^ . UntSrcFileSimpleName , "." }
        )
    ; Wr . Close ( UnitRef ^ . UntLogWrT ) 
    ; FM3Messages . EndUnit ( UnitRef . UntSrcFileSimpleName ) 
    END CompileUnitFromSrc

; PROCEDURE CompileOrLoadCLUnit ( SrcFileName : TEXT )
  (* Compile or load the top unit, as named on the command line. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; BEGIN 
      LUnitRef := GetUnitRefOfFileName ( SrcFileName )
    ; IF LUnitRef . UntState = Us . UsNull 
      THEN (* Haven't seen this unit yet. *)
      (* Compile it. *)
      (* Compare this to similar code in FM3ImpExp.Interface *) 
        IF FindAndOpenUnitSrcFile
             ( LUnitRef
             , Adjective := ""
             , ExpImpPosition := FM3Base . PositionNull
             )
        THEN 
          LUnitRef ^ . UntState := Us . UsExporting 
        ; FM3Units . PushUnit ( LUnitRef )
        ; FM3Units . CacheTopUnitValues ( )
        (* SetUnitLog will have to wait until Pass1.InitPass1 has
             created the WrT. *) 
        ; CompileUnitFromSrc ( LUnitRef ) 
        ; <* ASSERT FM3Units . PopUnit ( ) = LUnitRef *>
          FM3Messages . SetUnitLog ( LUnitRef ^ . UntLogWrT ) 
        ; FM3Units . CacheTopUnitValues ( ) 
        END (*IF*)
      END (*IF*)
    END CompileOrLoadCLUnit

(*EXPORTED*)
; PROCEDURE CompileCLUnits ( )
  (* Compile the units specified on the command line. *) 

  = BEGIN
      CompileOrLoadCLUnit ( FM3CLOptions . SrcFileName )
(* COMPLETEME: Do the rest of the CL units. *) 
    END CompileCLUnits

(*EXPORTED*)
; PROCEDURE ConvertIdentAtom
    ( FromAtom : FM3Base . AtomTyp
    ; FromUnitRef : FM3Units . UnitRefTyp 
    ; ToUnitRef : FM3Units . UnitRefTyp
    )
  : FM3Base . AtomTyp (* Could be FM3Base . AtomNull *)
  (* Return the ident atom in ToUnitRef that has the same spelling 
     that FromAtom has in FromUnitRef.  Null if anything fails.
  *) 

  = VAR LIdentChars : FM3Atom_OAChars . KeyTyp
        (* The chars are the same in both units. *) 
  ; VAR LToAtom : FM3Base . AtomTyp 

  ; BEGIN
      IF NOT FM3Atom_OAChars . Key
               ( FromUnitRef ^ . UntIdentAtomDict
               , FromAtom
               , (*OUT*) LIdentChars
               )
      THEN RETURN FM3Base . AtomNull
      END (*IF*) 
    ; LToAtom (* Lookup the ident among the remote unit's atoms. *) 
        := FM3Atom_OAChars . LookupKey  
             ( ToUnitRef ^ . UntIdentAtomDict
             , LIdentChars 
             , FM3Utils . HashNull 
             )
    ; RETURN LToAtom  
    END ConvertIdentAtom 
    
(*EXPORTED*)
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

  = VAR LIdentChars : FM3Atom_OAChars . KeyTyp
        (* The chars are the same in both units. *) 
  ; VAR LToAtom : FM3Base . AtomTyp 

  ; BEGIN
      IF NOT FM3Atom_OAChars . Key
               ( FromUnitRef ^ . UntIdentAtomDict
               , FromAtom
               , (*OUT*) LIdentChars
               )
      THEN RETURN FM3Base . AtomNull
      END (*IF*) 
    ; LToAtom (* Lookup/create the ident among the to-unit's atoms. *) 
        := FM3Atom_OAChars . MakeAtom   
             ( ToUnitRef ^ . UntIdentAtomDict
             , LIdentChars 
             , FM3Utils . HashNull 
             )
    ; RETURN LToAtom  
    END ConvertAndCreateIdentAtom 
    
; BEGIN
    SearchPathShown := FALSE 
  END FM3Compile
.

