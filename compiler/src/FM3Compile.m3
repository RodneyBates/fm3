        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE  FM3Compile

(* Overall build and compilation process. *) 

; IMPORT FileWr
; IMPORT Fmt 
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
; IMPORT FM3Exprs 
; IMPORT FM3Files 
; IMPORT FM3Globals
; IMPORT FM3Messages 
; IMPORT FM3Pass1 
; IMPORT FM3Pass2
; IMPORT FM3SharedUtils
; IMPORT FM3SrcToks
; IMPORT FM3Dict_Text_Int 
; IMPORT FM3Units
; IMPORT FM3Utils
; IMPORT RdBackFile
; IMPORT VarArray_Int_Refany

; TYPE Us = FM3Units . UnitStateTyp 

(*EXPORTED.*)
; PROCEDURE GetUnitRefOfFileName ( SrcFileName : TEXT ) : FM3Units . UnitRefTyp
  (* POST: Result, # NIL, references a UnitTyp, whose source file is named in
           FM3Units . UnitsAtomDict, and has field UntSrcFileSimpleName set,
           using the file name taken from SrcFileName, which may include a
           path.  Allocate the UnitTyp if necessary. 
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

; VAR GSearchPathShown := FALSE 

; PROCEDURE SrcSearchPathOnce ( ) : TEXT  

  = BEGIN
      IF GSearchPathShown THEN RETURN "" END (*IF*) 
    ; GSearchPathShown := TRUE
    ; RETURN
        FM3Messages . NLIndent
        & "Search directories for source files are:"
        & FM3CLOptions . SrcDirMsg 
    END SrcSearchPathOnce

; VAR GStdUnitFileNames : FM3Dict_Text_Int . FixedTyp 

; PROCEDURE InitStdFileNames ( )

  = VAR LUnitName : TEXT

  ; BEGIN (*InitStdFileNames*)
      GStdUnitFileNames
        := FM3Dict_Text_Int . NewFixed ( 28 , FM3Utils . HashOfText ) 
    ; FOR RTok := FM3SrcToks . StkMinStdIntf TO FM3SrcToks . StkMaxStdIntf
      DO LUnitName := FM3SrcToks . Image ( RTok )
      ; FM3Dict_Text_Int . InsertFixed
           ( GStdUnitFileNames , LUnitName & ".i3" , FM3Base . HashNull , RTok )
      ; IF RTok # FM3SrcToks . StkPdMain (* Main.m3 is not a standard unit. *) 
        THEN FM3Dict_Text_Int . InsertFixed
               ( GStdUnitFileNames , LUnitName & ".m3" , FM3Base . HashNull , RTok )
        END (*IF*) 
      END (*FOR*)
    ; FM3Dict_Text_Int . FinalizeFixed ( GStdUnitFileNames ) 
    END InitStdFileNames

; PROCEDURE IsStdUnitName  ( UnitName : TEXT ) : BOOLEAN 

  = VAR LTok : FM3SrcToks . TokTyp

  ; BEGIN
      RETURN FM3Dict_Text_Int . LookupFixed
          ( GStdUnitFileNames , UnitName , FM3Base . HashNull , (*OUT*) LTok )
   END IsStdUnitName  

; PROCEDURE StdUnitTok ( UnitName : TEXT ) : FM3SrcToks . TokTyp  

  = VAR LTok : FM3SrcToks . TokTyp

  ; BEGIN
      IF NOT FM3Dict_Text_Int . LookupFixed
           ( GStdUnitFileNames , UnitName , FM3Base . HashNull , (*OUT*) LTok )
      THEN LTok := FM3Base . TokNull
      END  (*IF*)
    ; RETURN LTok
    END StdUnitTok 

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
  ; VAR LStdUnit : FM3SrcToks . TokTyp 

  ; BEGIN
      IF UnitRef = NIL THEN RETURN FALSE END (*IF*) 
    ; IF UnitRef ^ . UntSrcFileSimpleName = NIL THEN RETURN FALSE END (*IF*) 
    ; IF UnitRef ^ . UntState # Us . UsNull THEN RETURN FALSE END (*IF*)
    ; UnitRef ^ . UntStdTok
        := StdUnitTok ( UnitRef ^ . UntSrcFileSimpleName )
    ; IF UnitRef ^ . UntStdTok # FM3Base . TokNull 
      THEN LSrcDirList := FM3CLOptions . ResourceDirNameList
      ELSE LSrcDirList := FM3CLOptions . SrcDirList 
      END (*IF*) 
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
      ; LSearchDir := FM3SharedUtils . AbsFileName ( LSrcDirList ^ [ LDirSs ] )
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
  RAISES { RdBackFile . BOF }
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 

  = VAR LPassFileFullName : TEXT 
  ; VAR LDisAsmFileFullName : TEXT
  ; VAR LDisAsmWrT : Wr . T
  ; VAR LSaveCoord : LONGINT 
  ; VAR LRdBack : RdBackFile . T
  
  ; BEGIN
      LPassFileFullName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath
             , UnitRef ^ . UntSrcFileSimpleName 
             , PassFileSuffix
             )
    ; LDisAsmFileFullName
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . DisAsmFileSuffix ) 
    ; LDisAsmWrT := FileWr . Open ( LDisAsmFileFullName )
    ; LRdBack := RdBackFile . Open ( LPassFileFullName )
    (* ^Skip using the copy. *)
    ; LSaveCoord := RdBackFile . LengthL ( LRdBack )
    ; EVAL RdBackFile . Seek ( LRdBack , RdBackFile . MaxLengthL ( LRdBack ) )  

    ; TRY
        FM3DisAsm . DisAsmWOperands ( LRdBack , LDisAsmWrT , L2R )
      ; EVAL RdBackFile . Seek ( LRdBack , LSaveCoord )  
    
      ; RdBackFile . Close ( LRdBack , - 1L )      
      ; Wr . Close ( LDisAsmWrT ) 
      EXCEPT
      | RdBackFile . BOF
        => 
          FM3Messages . InfoArr
            ( ARRAY OF REFANY
                { "Unable to complete disassembly file  "
                , LDisAsmFileFullName
                }
            )
        ; RdBackFile . Close ( LRdBack , - 1L )      
        ; Wr . Close ( LDisAsmWrT ) 
      END (*EXCEPT*)

    END DisAsmPassFile

(*EXPORTED*) 
; PROCEDURE DumpPassExprs
    ( UnitRef : FM3Units . UnitRefTyp ; PassFileSuffix : TEXT )
  (* As they are when this is called.  After a particular pass. *) 

  = VAR LPassFileName : TEXT 
  ; VAR LExprsFileFullName : TEXT
  ; VAR LDisAsmFileFullName : TEXT
  ; VAR ExprNosDumped : IntSets . T 
  ; VAR LExprMap : FM3Base . MapTyp
  ; VAR LWrT : Wr . T
  
  ; BEGIN
      LPassFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath
             , UnitRef ^ . UntSrcFileSimpleName 
             , PassFileSuffix
             )
    ; LExprsFileFullName 
        := Pathname . Join
             ( NIL , LPassFileName , FM3Globals . ExprsFileSuffix ) 
    ; LWrT := FileWr . Open ( LExprsFileFullName )
    ; LExprMap := UnitRef ^ . UntExprMap
    ; IF LExprMap = NIL
      THEN
        Wr . PutText ( LWrT , "<No expression map>" )
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ELSE
        ExprNosDumped := IntSets . Empty ( ) 
      ; FOR RExprNo
            := VarArray_Int_Refany . TouchedRange ( LExprMap ) . Lo
            TO  VarArray_Int_Refany . TouchedRange ( LExprMap ) . Hi
        DO IF NOT IntSets . IsElement ( RExprNo , ExprNosDumped )
          THEN Wr . PutText ( LWrT , "From unit expression map, Expr No " )
          ; Wr . PutText ( LWrT , Fmt . Int ( RExprNo ) )
          ; Wr . PutChar ( LWrT , ' ' )
          ; TYPECASE VarArray_Int_Refany . Fetch ( LExprMap , RExprNo ) OF
            | NULL
            =>  Wr . PutText ( LWrT , "NIL" )
              ; Wr . PutText ( LWrT , Wr . EOL )

            | FM3Exprs . ExprRefTyp ( TExpr )
            =>  Wr . PutText ( LWrT , FM3Utils . RefanyImage ( TExpr ) ) 
              ; Wr . PutText ( LWrT , Wr . EOL ) 
              ; FM3Exprs . DumpExpr ( TExpr , LWrT , (*IN OUT*) ExprNosDumped )

            ELSE
              Wr . PutText ( LWrT , "<notExprRefTyp>" )
            ; Wr . PutText ( LWrT , Wr . EOL )
            END (*TYPECASE*)
          ; Wr . PutText ( LWrT , Wr . EOL )
          END (*IF*) 
        END (*FOR*)
      END (*IF*) 
    ; Wr . Close ( LWrT ) 
    END DumpPassExprs 

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
        END (*IF*) 
      END (*FOR*) 
    END CleanPassFilesAndCopies

(*EXPORTED*)
; PROCEDURE CompileUnitFromSrc ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN (*CompileUnitFromSrc*)
      FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Getting dependencies of "
            , FM3Messages . NLIndent
            , "  " 
            , Pathname . Join
                ( UnitRef ^ . UntSrcFilePath
                , UnitRef ^ . UntSrcFileSimpleName
                ) 
            , " ..."
            }
        )
    ; UnitRef ^ . UntSkipStackBase
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
    END CompileUnitFromSrc

(*EXPORTED*)
; PROCEDURE CompileOrLoadCLUnit ( SrcFileName : TEXT )
  (* Compile or load the top unit, as named on the command line. *) 

  = VAR LUnitRef , LUnitRef2 : FM3Units . UnitRefTyp
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
        ; <* ASSERT FM3Units . UnitStackTopRef = LUnitRef *>
          EVAL FM3Units . PopUnit ( ) 
        ; FM3Messages . SetUnitLog ( LUnitRef ^ . UntLogWrT ) 
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
      IF FromUnitRef ^ . UntStdTok # FM3Base . TokNull AND FromAtom < 0 
      THEN RETURN FromAtom 
      ELSIF NOT FM3Atom_OAChars . Key
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
    GSearchPathShown := FALSE 
  ; InitStdFileNames ( )
  END FM3Compile
.

