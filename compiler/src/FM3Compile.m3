       
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE  FM3Compile

(* Overall build and compilation process. *) 

; IMPORT Atom 
; IMPORT File 
; IMPORT FileRd 
; IMPORT FileWr 
; IMPORT Fmt
; IMPORT FS 
; IMPORT OSError 
; IMPORT Pathname
; IMPORT Pickle2 AS Pickle
; IMPORT PickleStubs (* Oly ver2 has this. *)  
; IMPORT Rd
; IMPORT Stdio
; IMPORT Text 
; IMPORT Thread 
; IMPORT Time 
; IMPORT UniRd
; IMPORT Wr

; IMPORT IntSets
; IMPORT IntRanges AS Ranges_Int 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_Text 
; IMPORT FM3Base
; IMPORT FM3CLOptions
; IMPORT FM3CLToks  
; IMPORT FM3Decls 
; IMPORT FM3DisAsm 
; IMPORT FM3Exprs 
; IMPORT FM3Files 
; IMPORT FM3Globals
; IMPORT FM3Messages 
; IMPORT FM3Pass1
; IMPORT FM3Pass2
; IMPORT FM3Scopes 
; IMPORT FM3SharedGlobals
; IMPORT FM3SharedUtils
; IMPORT FM3SrcToks
; IMPORT FM3Dict_Text_Int 
; IMPORT FM3Units
; IMPORT FM3Utils
; IMPORT RdBackFile
; IMPORT VarArray_Int_Refany

; TYPE Us = FM3Units . UnitStateTyp
; TYPE Uttr = FM3Units . UnitReqKindTyp
; TYPE Utts = FM3Units . UnitTStateTyp

(*EXPORTED.*)
; PROCEDURE GetUnitTRefOfFileName ( SrcFilePath : TEXT )
  : FM3Units . UnitTRefTyp
  (* POST: Result, # NIL, references a UnitTTyp, whose source file is named in
           FM3Units . UnitsAtomDict, has field UttSrcFilePath set.
           If it doesn't already exist, allocate the UnitTTyp and
           set UttState := UttsNew. 
  *) 

  = VAR LSimpleName : TEXT
  ; VAR LUnitTRef : FM3Units . UnitTRefTyp
  ; VAR LUnitNameAtom : FM3Base . AtomTyp 

  ; BEGIN
      IF SrcFilePath = NIL THEN RETURN NIL END (*IF*) 
    ; LSimpleName := Pathname . Last ( SrcFilePath )
    ; IF Text . Length ( LSimpleName ) <= 0 THEN RETURN NIL END (*IF*)  
    ; LUnitNameAtom  
        := FM3Atom_Text . MakeAtom  
             ( FM3Units . UnitsAtomDict
             , LSimpleName
             , Hash := FM3Utils . HashOfText ( LSimpleName ) 
             )
    ; LUnitTRef 
        := VarArray_Int_Refany . Fetch ( FM3Units . UnitTMap , LUnitNameAtom )
      (* ^Implied NARROW *)
    ; IF LUnitTRef = NIL
      THEN
        LUnitTRef := FM3Units . NewUnitTRef ( )
      ; LUnitTRef ^ . UttSrcFilePath := SrcFilePath 
      ; LUnitTRef ^ . UttState := Utts . UttsNew 
      ; VarArray_Int_Refany . Assign
          ( FM3Units . UnitTMap , LUnitNameAtom , LUnitTRef )
      END (*IF*)
    ; RETURN LUnitTRef 
    END GetUnitTRefOfFileName

; VAR GSearchPathShown := FALSE 

; PROCEDURE SrcSearchPathOnce ( ) : TEXT  

  = BEGIN
      IF GSearchPathShown THEN RETURN "" END (*IF*) 
    ; GSearchPathShown := TRUE
    ; RETURN
        FM3Messages . NLIndent
        & "Look for source files in "
        & FM3CLOptions . PkgDirAbsName 
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
; PROCEDURE MakePassFileCopy
    ( UnitTRef : FM3Units . UnitTRefTyp
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
             ( NIL , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
             , PassFileSuffix
             )
    ; LPassFileFullName
        := Pathname . Join
             ( UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath
             , LPassFileSimpleName , NIL
             )
    ; LCopyFullName 
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
    ; RdBackFile . Copy 
        ( RdBackFileT , LCopyFullName , - 1L )
    END MakePassFileCopy 

(*EXPORTED*) 
; PROCEDURE DisAsmPassFile
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; PassFileSuffix : TEXT
    ; L2R : BOOLEAN
    ) 
  RAISES { RdBackFile . BOF }
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 

  = VAR LPassFileFullName : TEXT 
  ; VAR LDisAsmFileFullName : TEXT
  ; VAR LDisAsmWrT : Wr . T
  ; VAR LSaveCoord : LONGINT 
  ; VAR LRdBack : RdBackFile . T
  
  ; <* FATAL Wr . Failure , Thread . Alerted *>
    BEGIN
      LPassFileFullName
        := Pathname . Join
             ( UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath
             , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName 
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

(* --------- Sequentially dumping anything with an FM3Base . MapTyp ---------*) 

; TYPE DumpInfoObj
    = OBJECT
        DpiUnitRef : FM3Units . UnitRefTyp 
      ; DpiMap : FM3Base . MapTyp
      ; DpiTypeLabel : TEXT
      METHODS
        DpiDump ( Ref : REFANY ; WrT : Wr . T ) := DpiMethNIL  
      END 

; PROCEDURE DpiMethNIL 
    ( <*UNUSED*> Info : DumpInfoObj ; <*UNUSED*> Ref : REFANY ; WrT : Wr . T )   
    (* Dispatched-to. *) 

  = <* FATAL Wr . Failure , Thread . Alerted *>
    BEGIN (*DpiMethNIL*) 
      Wr . PutText ( WrT , " is NIL " )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END DpiMethNIL

; PROCEDURE DpiMethScope
    ( <*UNUSED*> Info : DumpInfoObj ; Ref : REFANY ; WrT : Wr . T )
  (* Dispatched-to. *)
  = VAR LScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN (*DpiMethScope*) 
      LScopeRef := Ref (* Implied NARROW. *)
    ; FM3Scopes . DumpScope
        ( LScopeRef
        , WrT
        , DoFields := TRUE (* The dump will end with a NL. *) 
        , DefaultFields := FALSE
        , Prefix := "  "
        ) 
    END DpiMethScope

(*EXPORTED.*)
; PROCEDURE DumpScopes ( UnitRef : FM3Units . UnitRefTyp ) 

  = VAR LInfo : DumpInfoObj 

  ; BEGIN (*DumpScopes*)
      LInfo 
        := NEW ( DumpInfoObj 
               , DpiUnitRef := UnitRef 
               , DpiMap := UnitRef ^ . UntScopeMap 
               , DpiTypeLabel := "Scope"
               , DpiDump := DpiMethScope 
               )
    ; DumpMappedRecs ( LInfo )
    END DumpScopes 
         
; PROCEDURE DpiMethDecl
    ( <*UNUSED*> Info : DumpInfoObj ; Ref : REFANY ; WrT : Wr . T )
  (* Dispatched-to. *)
  = VAR LDeclRef : FM3Decls . DeclRefTyp

  ; BEGIN (*DpiMethDecl*)
      LDeclRef := Ref (* Implied NARROW. *)
    ; FM3Decls . DumpDecl
        ( LDeclRef
        , WrT
        , DoFields := TRUE (* The dump will end with a NL. *) 
        , DefaultFields := FALSE
        ) 
    END DpiMethDecl

(*EXPORTED.*)
; PROCEDURE DumpDecls ( UnitRef : FM3Units . UnitRefTyp ) 

  = VAR LInfo : DumpInfoObj 

  ; BEGIN (*DumpDecls*)
      LInfo 
        := NEW ( DumpInfoObj 
               , DpiUnitRef := UnitRef 
               , DpiMap := UnitRef ^ . UntDeclMap 
               , DpiTypeLabel := "Decl"
               , DpiDump := DpiMethDecl
               )
    ; DumpMappedRecs ( LInfo )
    END DumpDecls 
         
; PROCEDURE DumpMappedRecs ( Info : DumpInfoObj ) 

  = VAR LFileSuffix : TEXT
  ; VAR LFileFullName : TEXT
  ; VAR LWrT : Wr . T
  ; VAR LRange : Ranges_Int . RangeTyp 
  
  ; <* FATAL Wr . Failure , Thread . Alerted *>
    BEGIN (*DumpMappedRecs*)
      LFileSuffix := Info . DpiTypeLabel & "s" 
    ; LFileFullName 
        := Pathname . Join
             ( Info . DpiUnitRef ^ . UntBuildDirPath
             , Info . DpiUnitRef ^ . UntSrcFileSimpleName 
             , LFileSuffix   
             )
    ; LWrT := FileWr . Open ( LFileFullName )
    ; Wr . PutText ( LWrT , "Dump of unit " )
    ; Wr . PutText ( LWrT , LFileSuffix )
    ; Wr . PutText ( LWrT , " of " )
    ; Wr . PutText ( LWrT , Info . DpiUnitRef ^ . UntSrcFileSimpleName ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText
        ( LWrT , "Most fields equal to their default values are omitted." )
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    
    ; IF Info . DpiMap = NIL
      THEN
        Wr . PutText ( LWrT , "<NIL " )
      ; Wr . PutText ( LWrT , Info . DpiTypeLabel )
      ; Wr . PutText ( LWrT , " map>" )
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ELSE
        LRange := VarArray_Int_Refany . TouchedRange ( Info . DpiMap ) 
      ; FOR RRecNo := LRange . Lo TO LRange . Hi
        DO
        (* Duplicated inside FM3Decls . DumpDecl :
          Wr . PutText ( LWrT , Info . DpiTypeLabel )
        ; Wr . PutText ( LWrT , " No " )
        ; Wr . PutText ( LWrT , Fmt . Int ( RRecNo ) )
        *)
          WITH WRefAny = VarArray_Int_Refany . Fetch ( Info . DpiMap , RRecNo )
          DO
            IF WRefAny = NIL
            THEN
              Wr . PutText ( LWrT , Info . DpiTypeLabel )             
            ; Wr . PutText ( LWrT , " no " )             
            ; Wr . PutText ( LWrT , Fmt . Int ( RRecNo ) ) 
            ; Wr . PutText ( LWrT , " has a NIL ref." )             
            ; Wr . PutText ( LWrT , Wr . EOL ) 
            ELSE
              Wr . PutText ( LWrT , Info . DpiTypeLabel ) 
            ; Wr . PutText ( LWrT , " map element no ") 
            ; Wr . PutText ( LWrT , Fmt . Int ( RRecNo ) ) 
            ; Wr . PutChar ( LWrT , ':' )             
            ; Wr . PutText ( LWrT , Wr . EOL ) 
            ; Info . DpiDump ( WRefAny , LWrT )
            END (*IF*) 
          ; Wr . PutText ( LWrT , Wr . EOL ) (* Blank line after each record. *)
          END (*WiTH *)
        END (*FOR*)
      END (*IF*) 
    ; Wr . PutText ( LWrT , "End dump of unit " )
    ; Wr . PutText ( LWrT , LFileSuffix )
    ; Wr . PutText ( LWrT , " of " )
    ; Wr . PutText ( LWrT , Info . DpiUnitRef ^ . UntSrcFileSimpleName ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . Close ( LWrT ) 
    END DumpMappedRecs 

(* ------------------------------ Dumping Exprs. ---------------------------- *) 
      
(*EXPORTED*)
; PROCEDURE DumpPassExprs
    ( UnitTRef : FM3Units . UnitTRefTyp ; PassFileSuffix : TEXT )
  (* As they are when this is called.  After a particular pass. *) 

  = VAR LPassFileName : TEXT 
  ; VAR LExprsFileFullName : TEXT
  ; VAR LExprNosDumped : IntSets . T 
  ; VAR LExprMap : FM3Base . MapTyp
  ; VAR LWrT : Wr . T
  
  ; <* FATAL Wr . Failure , Thread . Alerted *>
    BEGIN
      LPassFileName
        := Pathname . Join
             ( UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath
             , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName 
             , PassFileSuffix
             )
    ; LExprsFileFullName 
        := Pathname . Join
             ( NIL , LPassFileName , FM3Globals . ExprsFileSuffix ) 
    ; LWrT := FileWr . Open ( LExprsFileFullName )
    ; LExprMap := UnitTRef ^ . UttUnitRef ^ . UntExprMap
    ; IF LExprMap = NIL
      THEN
        Wr . PutText ( LWrT , "<No expression map>" )
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ELSE
        LExprNosDumped := IntSets . Empty ( ) 
      ; FOR RExprNo
            := VarArray_Int_Refany . TouchedRange ( LExprMap ) . Lo
            TO  VarArray_Int_Refany . TouchedRange ( LExprMap ) . Hi
        DO IF NOT IntSets . IsElement ( RExprNo , LExprNosDumped )
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
              ; FM3Exprs . DumpExpr ( TExpr , LWrT , (*IN OUT*) LExprNosDumped )

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
; PROCEDURE CleanPassFilesAndCopies ( UnitTRef : FM3Units . UnitTRefTyp )
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
        IF FM3CLOptions . PassNo2
           IN UnitTRef ^ . UttUnitRef ^ . UntPassNosDisAsmed
        THEN
          LPassFileFullName
            := Pathname . Join
                 ( UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath
                 , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName 
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
; PROCEDURE CompileCLUnits ( PkgFileNameList : REF ARRAY OF TEXT )
  (* Compile a list of units specified on the command line. *) 

  = VAR LUnitTRef : FM3Units . UnitTRefTyp 
  ; VAR LCt : INTEGER

  ; BEGIN
      IF PkgFileNameList = NIL THEN RETURN END(*IF*)
    ; LCt := NUMBER ( PkgFileNameList ^ ) 
    ; IF LCt <= 0 THEN RETURN END(*IF*)
    ; FOR RI := 0 TO LCt - 1
      DO
        LUnitTRef := GetUnitTRefOfFileName ( PkgFileNameList ^ [ RI ] )
      ; AcquireUnit ( LUnitTRef , FM3Base . PositionNull , Uttr . UttrCL ) 
      END (*FOR*) 

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

(*EXPORTED.*)
; PROCEDURE WriteSemFile ( UnitTRef : FM3Units . UnitTRefTyp )
  (* Create or overlay. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LSemFileFullName : TEXT
  ; VAR LSemWrT : Wr .T
  ; VAR LPickleWriter : Pickle . Writer 

  ; <* FATAL Wr . Failure , Thread . Alerted *>
    BEGIN (*WriteSemFile*)
      LUnitRef := UnitTRef ^ . UttUnitRef
    ; LSemFileFullName
        := Pathname . Join
             ( LUnitRef ^ . UntBuildDirPath 
             , LUnitRef ^ . UntSrcFileSimpleName
             , FM3Globals . SemFileSuffix
             )
    ; LSemWrT := FileWr . Open ( LSemFileFullName )
    ; Wr . PutText
        ( LSemWrT
        , FM3SharedUtils . FilePrefixT
            ( FM3SharedGlobals . FM3FileKindUnit
            , FM3SharedGlobals . FM3FileVersion0
            )
        )
    ; LPickleWriter := NEW ( Pickle . Writer , wr := LSemWrT )  

    (* sem file hash and src file time are copies brought out front so a
       sem file's relevance can be checked w/o unpickling the whole thing.
    *)
    
    ; PickleStubs . OutLongint ( LPickleWriter , LUnitRef ^ . UntHash ) 
    ; PickleStubs . OutLongreal ( LPickleWriter , LUnitRef . UntSrcTime ) 

    ; LPickleWriter . write ( LUnitRef ) 
    ; Pickle . Write ( LSemWrT , LUnitRef , write16BitWidechar := FALSE )

    (* It appears one can just abandon LPickleWriter with no close actions. *) 
    ; Wr . Close ( LSemWrT )
    END WriteSemFile 

  ; PROCEDURE EmitSemFailureMsg
      ( FileName , Reason : TEXT
      ; Position : FM3Base . tPosition
      ; ReqKind : FM3Units . UnitReqKindTyp
      )
    (* Boy, are there a lot of things that could go wrong here. *) 
    = BEGIN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Unable to load ."
              , FM3Globals . SemFileSuffix 
              , " file \" " 
              , FileName
              , "\" (" 
              , Reason
              , ")"
              } 
          , Position 
          )
      END EmitSemFailureMsg
      
 ; PROCEDURE ReadSemHeadInfo
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; SemFileT : File . T 
    ; ReqPosition : FM3Base . tPosition
    ; ReqKind : FM3Units . UnitReqKindTyp
    ; VAR (*OUT*) PickleRd : Pickle . Reader 
    ; VAR (*OUT*) Hash : FM3Utils . HashTyp
    ; VAR (*OUT*) Time : Time . T
    )
  : BOOLEAN (* Success.*) 
  (* PRE: SemFileT is open for read. *)
  (* Get started reading the few things at the front of the
     sem file, but leave the UnitTyp pickle for later.
  *) 
  
  = VAR LFileRdT : FileRd .T
  ; VAR LKind : FM3SharedGlobals . FileKindTyp
  ; VAR LVersion : FM3SharedGlobals . FileVersionTyp 
  ; VAR LPrefixIsOK : BOOLEAN

  ; BEGIN (*ReadSemHeadInfo*)
      LFileRdT := NEW ( FileRd . T ) 
    ; TRY EVAL LFileRdT . init ( SemFileT )
(* TODO: Close SemFileT and LfileRdT. *) 
      EXCEPT ELSE
        EmitSemFailureMsg
          ( UnitTRef ^ . UttSrcFilePath
          , "FileRd.Init"
          , ReqPosition
          , ReqKind 
          ) 
      ; UnitTRef ^ . UttState := Utts . UttsNotLoadable 
      ; RETURN FALSE 
      END (*EXCEPT*)
    ; PickleRd := NEW ( Pickle . Reader , rd := LFileRdT )  

    ; FM3SharedUtils . ReadPrefixR
        ( LFileRdT
        , (*OUT*) LKind  
        , (*OUT*) LVersion 
        , (*OUT*) LPrefixIsOK 
        )
    ; TRY
        FM3SharedUtils .CheckPrefix
         ( LPrefixIsOK
         , LKind , FM3SharedGlobals . FM3FileKindSem
         , LVersion , FM3SharedGlobals . FM3FileVersion0
         , "Compiled ." & FM3Globals . SemFileSuffix & " file " 
         , Pathname . Last ( UnitTRef ^ . UttSrcFilePath )
         )
      EXCEPT FM3SharedUtils . FatalError
      =>  UnitTRef ^ . UttState := Utts . UttsNotLoadable 
      ;   RETURN FALSE 
      END (*EXCEPT*)

    (* sem file hash and src file time are copies brought out front so a
       sem file's relevence can be checked w/o unpickling the whole thing.
    *)
    ; Hash := PickleStubs . InLongint ( PickleRd )
    ; Time := PickleStubs . InLongreal ( PickleRd )
    ; RETURN TRUE 
    END ReadSemHeadInfo

; PROCEDURE ReadSemFileRemainder
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; PickleRd : Pickle . Reader 
    ; ReqPosition : FM3Base . tPosition
    ; ReqKind : FM3Units . UnitReqKindTyp 
    )
  : BOOLEAN (* Success. *)
  (*PRE: ReadSemHeadInfo has happened successfully, thus
         SemFileT and PickleRd are open.
  *)
  (* Read the UnitTyp pickle and connect it to the UnitTTyp. *) 

  = VAR LRefany : REFANY
  ; VAR LResult : BOOLEAN 

  ; BEGIN (*ReadSemFileRemainder*)
      TRY LRefany := PickleRd . read ( ) 
      EXCEPT ELSE
        EmitSemFailureMsg
          ( UnitTRef ^ . UttSrcFilePath , "unpickling" , ReqPosition , ReqKind )  
      ; UnitTRef ^ . UttState := Utts . UttsNotLoadable
      ; LResult := FALSE 
      END (*EXCEPT*) 
    ; TYPECASE LRefany OF
      | NULL
      =>  EmitSemFailureMsg
            ( UnitTRef ^ . UttSrcFilePath , "NIL pickle" , ReqPosition , ReqKind )
        ; UnitTRef ^ . UttState := Utts . UttsNotLoadable 
        ; LResult := FALSE 
            
      | FM3Units . UnitRefTyp ( TUnitRef )
      =>  UnitTRef ^ . UttUnitRef := TUnitRef
        ; UnitTRef ^ . UttState := Utts . UttsLoaded
        ; LResult := TRUE 
      
      ELSE
        EmitSemFailureMsg
          ( UnitTRef ^ . UttSrcFilePath
          , "wrongly typed pickle"
          , ReqPosition
          , ReqKind 
          )  
      ; UnitTRef ^ . UttState := Utts . UttsNotLoadable 
      ; LResult := FALSE 
      END (*TYPECASE*)
    ; UnitTRef . UttState := Utts . UttsCompiled
    (* It appears one can just abandon PickleRd with no close actions. *) 
    ; Rd . Close ( PickleRd . rd ) 
    ; RETURN LResult  
    END ReadSemFileRemainder

; PROCEDURE EnsureBuildDirExists
    ( UnitTRef : FM3Units . UnitTRefTyp ; ReqPosition : FM3Base . tPosition )
    (* PRE: UnitTRef ^ . UttUnitRef # NIL. *) 

  = VAR LBuildDirPath : TEXT

  ; BEGIN (*EnsureBuildDirExists*)
      LBuildDirPath
        := Pathname . Join
             ( FM3CLOptions . PkgDirName , FM3CLOptions . BuildDirRelPath ) 
    ; UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath := LBuildDirPath 
    ; TRY FS . CreateDirectory ( LBuildDirPath )
      EXCEPT
      | OSError . E ( EAtoms ) 
      => IF EAtoms . tail = NIL
            AND EAtoms . head # NIL
            AND Atom . ToText ( EAtoms . head ) # NIL
            AND Text . Equal ( Atom . ToText ( EAtoms . head ) , "errno=17" )
(* TODO: There has to be a more graceful (and almost OS-independent) way to
         detect this, but it looks like libm3 is letting us down here.
*) 
         THEN (* The directory already exists. We expect this sometimes. *)
           EVAL EAtoms (* Debug. *)  
         ELSE
           FM3Messages . ErrorArr
             ( ARRAY OF REFANY 
                 { "Exception trying to create build directory \"" 
                 , LBuildDirPath  
                 , "\": "
                 , FM3Messages . AtomListToOSError ( EAtoms )  
                 , FM3Messages . NLIndent 
                 , "Forging ahead, assuming it already exists." 
                 }
             , ReqPosition
             ) 
         END (*IF*)
      END (*EXCEPT*) 
    END EnsureBuildDirExists

; PROCEDURE CompileUnit
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; SrcFileT : File . T
   (* ^PRE: This is open for read. *) 
    ; StdTok : FM3SrcToks . TokTyp 
    ; ReqPosition : FM3Base . tPosition
    )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LUniRdT : UniRd . T
  ; VAR LUnitLogSimpleName : TEXT
  ; VAR LUnitLogFullName : TEXT
  
  ; <* FATAL Wr . Failure , Thread . Alerted *>
    BEGIN (*CompileUnit*)
      LUnitRef := FM3Units . NewUnitRef ( ) 
    ; UnitTRef ^ . UttUnitRef := LUnitRef 
    ; LUnitRef ^ . UntSrcFilePath := UnitTRef ^ . UttSrcFilePath
    ; LUnitRef ^ . UntSrcFileSimpleName
        := Pathname . Last ( UnitTRef ^ . UttSrcFilePath )  
    ; LUnitRef ^ . UntStdTok := StdTok 
    ; LUniRdT := FM3Files . OpenUniRd ( SrcFileT )
    ; UnitTRef ^ . UttSrcUniRdT := LUniRdT 
    ; IF LUniRdT = NIL
      THEN 
        UnitTRef ^ . UttState := Utts . UttsNotUsable
      ; RETURN 
      END (*IF*)

    (* Create the unit log output file. A pure text file. *)
    ; LUnitLogFullName
        := Pathname . Join
             ( NIL 
             , FM3SharedUtils . AbsFileName ( UnitTRef ^ . UttSrcFilePath ) 
             , FM3Globals . UnitLogSuffix 
             )
    ; IF FM3CLToks . CltUnitLog IN FM3CLOptions . OptionTokSet
      THEN 
        TRY UnitTRef ^ . UttLogWrT := FileWr . Open ( LUnitLogFullName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => (* Couldn't create a unit log, so have to use Stdio. *) 
           <*FATAL Thread . Alerted , Wr . Failure *>
           BEGIN
             Wr . PutText ( Stdio . stderr , "Unable to open unit log file " ) 
           ; Wr . PutText ( Stdio . stderr , LUnitLogSimpleName ) 
           ; Wr . PutText ( Stdio . stderr , ": " ) 
           ; Wr . PutText
               ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
           ; Wr . PutText ( Stdio . stderr , FM3Messages . NLIndent ) 
           ; Wr . PutText ( Stdio . stderr , "Will proceed without it." ) 
           ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
           ; Wr . Flush ( Stdio . stderr )
           END (*Block.*) 
        ; FM3SharedUtils . DeleteFile ( LUnitLogFullName ) (* Leftover? *) 
        ; UnitTRef ^ . UttLogWrT := NIL
        END (*EXCEPT*) 
      ELSE (* Remove any leftover unit log file. *) 
        FM3SharedUtils . DeleteFile ( LUnitLogFullName ) (* Leftover? *) 
      ; UnitTRef ^ . UttLogWrT := NIL
      END (*IF*)
    ; FM3Messages . SetUnitLog ( UnitTRef ^ . UttLogWrT )

    ; EnsureBuildDirExists ( UnitTRef , ReqPosition ) 

    ; FM3Units . PushUnitT ( UnitTRef )
    ; FM3Units . CacheTopUnitValues ( )

    ; UnitTRef ^ . UttState := Utts . UttsExporting
(*TODO ^ What is the right value here for cl and import units. *) 
    ; FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Getting dependencies of "
            , FM3Messages . NLIndent
            , "  " 
            , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
            , " ..."
            }
        )
    ; FM3Pass1 . RunPass1 ( )
    ; IF UnitTRef ^ . UttUnitRef ^ . UntParseResult <= 0
      THEN FM3Pass2 . RunPass2 ( )
      END (*IF*) 

    ; CleanPassFilesAndCopies ( UnitTRef ) 
    ; FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "... finished compiling "
            , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName , "."
            }
        )
    ; Wr . Close ( UnitTRef ^ . UttLogWrT ) 

    ; <* ASSERT FM3Units . UnitTStackTopRef = UnitTRef *>
      FM3Files . CloseUniRd ( UnitTRef ^ . UttSrcUniRdT )
    ; WriteSemFile ( UnitTRef ) 
    ; EVAL FM3Units . PopUnitT ( ) 
    ; FM3Messages . SetUnitLog ( UnitTRef ^ . UttLogWrT )
      (* of the deeper, now current unit. *) 
    ; FM3Units . CacheTopUnitValues ( ) 
    ; UnitTRef . UttState := Utts . UttsCompiled
    END CompileUnit

(*EXPORTED.*)
; PROCEDURE AcquireUnit
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; ReqPosition : FM3Base . tPosition
    ; ReqKind : FM3Units . UnitReqKindTyp 
    )
  (* PRE: UnitTRef ^ exists. *) 
  (* Load it, compile it, mark it something bad, whatever. *) 
    
  = VAR LSrcFileSimpleName : TEXT
  ; VAR LSemFileSimpleName : TEXT
  ; VAR LPkgDirName : TEXT
  ; VAR LSemPkgDirName : TEXT
  ; VAR LSemFileT : File . T 
  ; VAR LUnitHash : FM3Utils . HashTyp 
  ; VAR LUnitTime : Time . T
  ; VAR LSrcTime : Time . T
  ; VAR LPickleRd : Pickle . Reader 
  ; VAR LSrcPkgDirName : TEXT
  ; VAR LSrcFileT : File . T 
  ; VAR LStdTok : FM3SrcToks . TokTyp 

  ; BEGIN (*AcquireUnit*)
      IF UnitTRef = NIL THEN RETURN END (*IF*) 
    ; CASE UnitTRef ^ . UttState OF
      | Utts . UttsNull
      , Utts . UttsNotFound
      , Utts . UttsNotLoadable 
      => (* Nothing can be done. *)
      
      | Utts . UttsCompiled
      , Utts . UttsLoaded
      =>  (* Nothing needs to be done. *)

      | Utts . UttsExporting 
      , Utts . UttsImporting 
      , Utts . UttsCompiling
      =>  (* Shouldn't happen.. *)
(* Do we even need these states? *) 
(*FIXME: What to do? *) 
      
      | Utts . UttsNew (* Need to get something from file(s). *)
      =>  (* Get info about a possible sem file in build dir of compile pkg. *)
          LSemPkgDirName := NIL
        ; LSemFileT := NIL 
        ; LUnitHash := FM3Base . HashNull
        ; LUnitTime := 0.0D0
        
        ; LSrcFileSimpleName := Pathname . Last ( UnitTRef ^ . UttSrcFilePath )
        ; LSemFileSimpleName
            := Pathname . Join
                 ( NIL , LSrcFileSimpleName , FM3Globals . SemFileSuffix ) 
        ; LStdTok := StdUnitTok ( LSrcFileSimpleName )
        ; IF LStdTok # FM3Base . TokNull 
          THEN LPkgDirName := FM3CLOptions . ResourceDirName
          ELSE LPkgDirName := FM3CLOptions . PkgDirName
          END (*IF*)
        ; IF FM3Files . FindAndOpenRdFileT
               ( ARRAY OF TEXT { LPkgDirName }
               , FM3Globals . BuildDirName 
               , LSemFileSimpleName
               , (*OUT*) LSemPkgDirName 
               , (*OUT*) LSemFileT  
               )
          THEN (* Found sem file in comp pkg dir and opened LSemFileT on it. *)
            EVAL ReadSemHeadInfo
                   ( UnitTRef
                   , LSemFileT
                   , ReqPosition 
                   , ReqKind 
                   , (*OUT*) LPickleRd 
                   , (*OUT*) LUnitHash
                   , (*OUT*) LUnitTime
                   )
          END (*IF*) 

        ; IF ReqKind = Uttr . UttrCL
          THEN 
            (* Also get info about a possible source file in compile pkg dir. *)
            LSrcPkgDirName := NIL
          ; LSrcFileT := NIL
          ; LSrcTime := 0.0D0  
          ; IF FM3Files . FindAndOpenRdFileT
                 ( ARRAY OF TEXT { LPkgDirName }
                 , FM3Globals . SrcDirName 
                 , LSrcFileSimpleName
                 , (*OUT*) LSrcPkgDirName 
                 , (*OUT*) LSrcFileT
                 )
            THEN
              LSrcTime := LSrcFileT . status ( ) . modificationTime 
            END (*IF*)

          (* Decide whether to compile or load unit from compile package. *) 
          ; IF LSrcFileT # NIL
            THEN (* Source file exists insode pkg dir. *) 
              IF FM3CLToks . CltForceCompile IN FM3CLOptions . OptionTokSet 
                 OR LSemFileT = NIL
                 OR LSrcTime > LUnitTime 
              THEN (* Compile. *)
                CompileUnit ( UnitTRef , LSrcFileT , LStdTok , ReqPosition ) 
              ELSE (* Use the up-to-date sem file in comp pkg. *) 
                EVAL ReadSemFileRemainder
                  ( UnitTRef , LPickleRd , ReqPosition , ReqKind )  
              END (*IF*)
              
            ELSE (* No source file in compile pkg. *) 
              IF LSemFileT # NIL 
              THEN (* But a compiled sem file exists there. Use it. *) 
                EVAL ReadSemFileRemainder 
                       ( UnitTRef , LPickleRd , ReqPosition , ReqKind )  
              ELSE
                FM3Messages . FM3LogArr
                  ( ARRAY OF REFANY
                      { "No source or compiled file found in package directory \""
                      , LPkgDirName
                      , "\""
                      , FM3Messages . NLIndent
                      , "  " 
                      , "for command-line request \""
                      , LSrcFileSimpleName
                      , "\"." 
                      } 
                  )
              ; UnitTRef . UttState := Utts . UttsNotFound
              END (*IF*)
            END (*IF*)
            
          ELSE (* Export/import request *)
            IF LSrcFileT # NIL
               AND LSemFileT = NIL
                   OR ( LSrcTime > LUnitTime
                        AND FM3CLToks . CltRecompileImports
                            IN FM3CLOptions . OptionTokSet
                      ) 
            THEN
              CompileUnit ( UnitTRef , LSrcFileT , LStdTok , ReqPosition )
            ELSIF LSemFileT # NIL
            THEN (* Load sem file from compile pkg. *) 
              EVAL ReadSemFileRemainder 
                ( UnitTRef , LPickleRd , ReqPosition , ReqKind )  
            ELSE (* Nothing found in compile pkg, try import dirs. *) 
              IF LSemFileT # NIL THEN LSemFileT . close ( ) END (*IF*) 
            ; IF FM3Files . FindAndOpenRdFileT
                   ( FM3CLOptions . ImportPkgDirList ^
                   , FM3Globals . BuildDirName 
                   , LSemFileSimpleName
                   , (*OUT*) LSemPkgDirName 
                   , (*OUT*) LSemFileT  
                   )
              THEN (* Found and opened sem file in comp package dir load it. *) 
                EVAL ReadSemHeadInfo
                       ( UnitTRef
                       , LSemFileT
                       , ReqPosition 
                       , ReqKind 
                       , (*OUT*) LPickleRd 
                       , (*OUT*) LUnitHash
                       , (*OUT*) LUnitTime
                       )
              ; EVAL ReadSemFileRemainder 
                  ( UnitTRef , LPickleRd , ReqPosition , ReqKind )  

              ELSE (* Nothing found in import packages either. *) 
                FM3Messages . ErrorArr
                  ( ARRAY OF REFANY
                      { "No compiled file for import/export "
                      , "\""
                      , FM3Messages . NLIndent
                      , LSemFileSimpleName
                      , "\", in compile package nor any import package."
                      } 
                  , ReqPosition 
                  )
              ; UnitTRef . UttState := Utts . UttsNotFound
              END (*IF*) 
            END (*IF*) 
          END (*IF*)
        ; IF LSemFileT # NIL THEN LSemFileT . close ( ) END (*IF*) 
        ; IF LSrcFileT # NIL THEN LSrcFileT . close ( ) END (*IF*)   
      END (*CASE*) 
    END AcquireUnit

; BEGIN
    GSearchPathShown := FALSE 
  ; InitStdFileNames ( )
  END FM3Compile
.
