       
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE  FM3Compile

(* Overall build and compilation process. *) 

; IMPORT FileRd 
; IMPORT FileWr 
; IMPORT Fmt 
; IMPORT OSError 
; IMPORT Pathname
; IMPORT Pickle2 AS Pickle
; IMPORT Rd 
; IMPORT Text
; IMPORT Time 
; IMPORT UniRd
; IMPORT Wr

; IMPORT IntSets
; IMPORT IntRanges AS Ranges_Int 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_Text 
; IMPORT FM3Base
; IMPORT FM3CLOptions
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
; IMPORT FM3UnsafeUtils 
; IMPORT FM3Utils
; IMPORT RdBackFile
; IMPORT VarArray_Int_Refany

; TYPE Us = FM3Units . UnitStateTyp 

(*EXPORTED.*)
; PROCEDURE GetUnitTRefOfFileName
    ( SrcFileName : TEXT ) : FM3Units . UnitTRefTyp
  (* POST: Result, # NIL, references a UnitTTyp, whose source file is named in
           FM3Units . UnitsAtomDict, and has field UntSrcFileSimpleName set,
           using the file name taken from SrcFileName, which may include a
           path.  If it doesn't allready exist, Allocate the UnitTTyp and a
           UnitType and connect them.
  *) 

  = VAR LSimpleName : TEXT
  ; VAR LUnitTRef : FM3Units . UnitTRefTyp
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
    ; LUnitTRef 
        := VarArray_Int_Refany . Fetch ( FM3Units . UnitsTMap , LUnitNameAtom )
      (* ^Implied NARROW *)
    ; IF LUnitTRef = NIL
      THEN
        LUnitRef:= FM3Units . NewUnitRef ( ) 
      ; LUnitTRef := FM3Units . NewUnitTRef ( )
      ; LUnitTRef ^ . UttUnitRef := LUnitRef 
      ; LUnitTRef ^ . UttUnitRef ^. UntSrcFileSimpleName := LSimpleName 
      ; VarArray_Int_Refany . Assign
          ( FM3Units . UnitsTMap , LUnitNameAtom , LUnitTRef )
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
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; Adjective : TEXT
    ; ExpImpPosition : FM3Base . tPosition
    )
  : BOOLEAN (* Success *)
  (* POST: IF result, then the source file for UnitRef^ was found and opened,
           and fields UntSrcFilePath, UttSrcUniRd, and UntState are set.
  *) 

  = VAR LSrcDirList : REF ARRAY OF TEXT 
  ; VAR LSearchDir : TEXT 
  ; VAR LDirNumber : INTEGER
  ; VAR LDirSs : INTEGER
  ; VAR LStdUnit : FM3SrcToks . TokTyp 

  ; BEGIN
      IF UnitTRef = NIL THEN RETURN FALSE END (*IF*) 
    ; IF UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName = NIL
      THEN RETURN FALSE
      END (*IF*) 
    ; IF UnitTRef ^ . UttUnitRef ^ . UntState # Us . UsNull
      THEN RETURN FALSE
      END (*IF*)
    ; UnitTRef ^ . UttUnitRef ^ . UntStdTok
        := StdUnitTok ( UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName )
    ; IF UnitTRef ^ . UttUnitRef ^ . UntStdTok # FM3Base . TokNull 
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
                , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
                , SrcSearchPathOnce ( ) 
                }
            , ExpImpPosition 
            )
        ; UnitTRef ^ . UttSrcUniRd := NIL 
        ; UnitTRef ^ . UttUnitRef ^ . UntSrcFilePath := ""
        ; UnitTRef ^ . UttUnitRef ^ . UntSrcFileTime := 0.0D0
        ; UnitTRef ^ . UttUnitRef ^ . UntState := Us . UsNotUsable
        ; RETURN FALSE
        END (*IF*) 
      ; LSearchDir := FM3SharedUtils . AbsFileName ( LSrcDirList ^ [ LDirSs ] )
      ; TRY 
          FM3Files . OpenUniRd
            ( LSearchDir
            , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
            , (*OUT*) UnitTRef ^ . UttSrcUniRd 
            , (*OUT*) UnitTRef ^ . UttUnitRef ^ . UntSrcFileTime 
            )
        EXCEPT
        | OSError . E ( EMsg )
        => (* Not found in this dir, just loop. *)  
           INC ( LDirSs )
        END (*EXCEPT*)
      (* Found it. *) 
      ; UnitTRef ^ . UttUnitRef ^ . UntSrcFilePath := LSearchDir
      ; UnitTRef ^ . UttUnitRef ^ . UntState := Us . UsExporting
      ; RETURN TRUE 
      END (*LOOP*) 
    END FindAndOpenUnitSrcFile 
(* TODO: Move to FM3Files. *)
(*EXPORTED*) 
; PROCEDURE FindAndOpenRdFile
    ( DirNameList : REF ARRAY OF TEXT 
    ; FileSimpleName : TEXT 
    ; VAR (*OUT*) FoundInDirName : TEXT 
    ; VAR (*OUT*) ResultFile : File . T
    )

  = VAR LDirNumber : INTEGER
  ; VAR LDirSs : INTEGER
  ; VAR LLSimpleSearchDir : TEXT 
  ; VAR LAbsSearchDir : TEXT 
  ; VAR LFullFilePath : Pathname . T 

  ; BEGIN (* FindAndOpenRdFile *) 
      FoundInDirName := NIL
    ; ResultFile := NIL 
    ; IF UnitTRef = NIL THEN RETURN END (*IF*) 
    ; IF DirNameList = NIL THEN RETURN END (*IF*)
    ; IF FileSimpleName = NIL OR Text . Equal ( FileSimpleName , "" )
      THEN RETURN
      END (*IF*)
    ; LDirNumber := NUMBER ( DirNameList ^ ) 
    ; LDirSs := 0
    ; LOOP
        IF LDirSs >= LDirNumber THEN RETURN END (*IF*) 
      ; LSimpleSearchDir := DirNameList ^ [ LDirSs ] 
      ; LAbsSearchDir := FM3SharedUtils . AbsFileName ( LSimpleSearchDir )
      ; LFullFilePath
          := Pathname . Join ( LAbsSearchDir , FileSimpleName , NIL ) 
      ; TRY ResultFile := FS . OpenFileReadonly (*M3If79*) ( LFullFilePath )
        EXCEPT OSError . E ( EMsg )
        =>  ResultFile := NIL 
        END (*EXCEPT*)
      ; IF ResultFile = NIL 
        THEN INC ( LDirSs )
        ELSE 
          FoundInDirName := LSimpleSearchDir
        ; RETURN
        END (*IF*) 
      END (*LOOP*) 
    END FindAndOpenRdFile 
*)
(*EXPORTED*) 
; PROCEDURE CloseUnitSrcFile ( UnitTRef : FM3Units . UnitTRefTyp ) 

  = BEGIN
      IF UnitTRef = NIL THEN RETURN END (*IF*) 
    ; IF UnitTRef ^ . UttSrcUniRd = NIL THEN RETURN END (*IF*)
    ; UniRd . Close ( UnitTRef ^ . UttSrcUniRd )
    ; UnitTRef ^ . UttSrcUniRd := NIL 
    END CloseUnitSrcFile 

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
  
  ; BEGIN
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
    ( <*UNUSED*> Info : DumpInfoObj ; Ref : REFANY ; WrT : Wr . T )   
    (* Dispatched-to. *) 

  = BEGIN (*DpiMethNIL*) 
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
  
  ; BEGIN (*DumpMappedRecs*)
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
  ; VAR LDisAsmFileFullName : TEXT
  ; VAR LExprNosDumped : IntSets . T 
  ; VAR LExprMap : FM3Base . MapTyp
  ; VAR LWrT : Wr . T
  
  ; BEGIN
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
        IF FM3CLOptions . PassNo2 IN UnitTRef ^ . UttUnitRef ^ . UntPassNosDisAsmed
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
; PROCEDURE CompileUnitFromSrc ( UnitTRef : FM3Units . UnitTRefTyp )

  = BEGIN (*CompileUnitFromSrc*)
      FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Getting dependencies of "
            , FM3Messages . NLIndent
            , "  " 
            , Pathname . Join
                ( UnitTRef ^ . UttUnitRef ^ . UntSrcFilePath
                , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
                ) 
            , " ..."
            }
        )
    ; UnitTRef ^ . UttSkipStackBase
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi 
    ; FM3Pass1 . RunPass1 ( )
    ; IF UnitTRef ^ . UttUnitRef ^ . UntParseResult <= 0 THEN FM3Pass2 . RunPass2 ( ) END (*IF*) 

    ; RdBackFile . Close 
        ( UnitTRef ^ . UttPass2OutRdBack , - 1L (* Leave full length. *) )
      (* ^When the next pass is implemented, don't do this. *)

    ; CleanPassFilesAndCopies ( UnitTRef ) 
    ; <*ASSERT
          UnitTRef ^ . UttSkipStackBase 
            = VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
      *> 
      FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Finished compiling " , UnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName , "." }
        )
    ; Wr . Close ( UnitTRef ^ . UttLogWrT ) 
    END CompileUnitFromSrc

(*EXPORTED*)
; PROCEDURE CompileOrLoadCLUnit ( SrcFileName : TEXT )
  (* Compile or load the top unit, as named on the command line. *) 

  = VAR LUnitTRef : FM3Units . UnitTRefTyp
  
  ; BEGIN 
      LUnitTRef := GetUnitTRefOfFileName ( SrcFileName )
    ; IF LUnitTRef ^ . UttUnitRef ^ . UntState = Us . UsNull 
      THEN (* Haven't seen this unit yet. *)
      (* Compile it. *)
      (* Compare this to similar code in FM3ImpExp.Interface *) 
        IF FindAndOpenUnitSrcFile
             ( LUnitTRef 
             , Adjective := ""
             , ExpImpPosition := FM3Base . PositionNull
             )
        THEN 
          LUnitTRef ^ . UttUnitRef ^ . UntState := Us . UsExporting 
        ; FM3Units . PushUnit ( LUnitTRef )
        ; FM3Units . CacheTopUnitValues ( )
        (* SetUnitLog will have to wait until Pass1.InitPass1 has
             created the WrT. *) 
        ; CompileUnitFromSrc ( LUnitTRef )
        ; <* ASSERT FM3Units . UnitTStackTopRef = LUnitTRef *>
          EVAL FM3Units . PopUnit ( ) 
        ; FM3Messages . SetUnitLog ( LUnitTRef ^ . UttLogWrT ) 
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

(*EXPORTED.*)
; PROCEDURE WriteUnitFile ( UnitTRef : FM3Units . UnitTRefTyp )
  (* Create or overlay. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LUnitFileFullName : TEXT
  ; VAR LUnitWrT : Wr .T
  ; VAR LSrcTimeL : LONGINT (* Just a LOOPHOLE of a LONGREAL *)
  ; VAR LHashArray : FM3Utils . HashCharArrayTyp
  ; VAR LSrcTimeArray : FM3Utils . LongCharArrayTyp

  ; BEGIN (*WriteUnitFile*)
      LUnitRef := UnitTRef ^ . UttUnitRef
    ; LUnitRef ^ . UntUnitFileSimpleName
        := Pathname . Join
             ( NIL
             , LUnitRef ^ . UntSrcFileSimpleName
             , FM3Globals . UnitFileSuffix
             )
    ; LUnitFileFullName
        := Pathname . Join
             ( UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath
             , UnitTRef ^ . UttUnitRef ^ . UntUnitFileSimpleName 
             )
    ; LUnitWrT := FileWr . Open ( LUnitFileFullName )
    ; Wr . PutText
        ( LUnitWrT
        , FM3SharedUtils . FilePrefixT
            ( FM3SharedGlobals . FM3FileKindUnit
            , FM3SharedGlobals . FM3FileVersion0
            )
        )

    (* Unit file hash and src file time are copies brought out front so a
       unit file's relevance can be checked w/o unpickling the whole thing.
    *)
    ; LHashArray := FM3Utils . HashToChars ( LUnitRef ^ . UntHash )
    ; Wr . PutString ( LUnitWrT , LHashArray )

    ; LSrcTimeL := FM3UnsafeUtils . LongRealToLongInt ( LUnitRef . UntSrcTime )
    ; LSrcTimeArray := FM3Utils . LongToChars ( LSrcTimeL ) 
    ; Wr . PutString ( LUnitWrT , LSrcTimeArray )

    ; Pickle . Write ( LUnitWrT , LUnitRef , write16BitWidechar := FALSE ) 

    ; Wr . Close ( LUnitWrT )
    END WriteUnitFile

; EXCEPTION ReadUnitFailure
  (* In case caller of ReadUnitFile has an alternative. *) 

(*EXPORTED.*)
; PROCEDURE ReadUnitFile
    ( UnitTRef : FM3Units . UnitTRefTyp ; FileSimpleName : TEXT )
  RAISES { ReadUnitFailure } 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LUnitFileFullName : TEXT
  ; VAR LUnitRdT : Rd .T
  ; VAR LFrontTimeL : LONGINT 
  ; VAR LFrontTime : Time . T
  ; VAR LRefany : REFANY
  ; VAR LFrontHash : FM3Utils . HashTyp
  ; VAR LLen : CARDINAL 
  ; VAR LHashArray : FM3Utils . HashCharArrayTyp
  ; VAR LSrcTimeArray : FM3Utils . LongCharArrayTyp
  ; VAR LKind : FM3SharedGlobals . FileKindTyp
  ; VAR LVersion : FM3SharedGlobals . FileVersionTyp 
  ; VAR LPrefixIsOK : BOOLEAN

  ; PROCEDURE Failure ( Reason : TEXT ) 
    (* Boy, are there a lot of things that could go wrong here. *) 
    = BEGIN
        FM3Messages . InfoArr
          ( ARRAY OF REFANY
              { "Unable to  read unit file \" "
              , LUnitFileFullName
              , "\" (" 
              , Reason
              , ")"
              } 
          , UnitTRef . UttPositionOfImport 
          )
      ; RAISE ReadUnitFailure
      END Failure 

  ; BEGIN (*ReadUnitFile*)
      LUnitRef ^ . UntUnitFileSimpleName
        := Pathname . Join
             ( NIL
             , FileSimpleName
             , FM3Globals . UnitFileSuffix
             )
    ; LUnitFileFullName
        := Pathname . Join
             ( UnitTRef ^ . UttUnitRef ^ . UntBuildDirPath
             , UnitTRef ^ . UttUnitRef ^ . UntUnitFileSimpleName 
             )
    ; TRY LUnitRdT := FileRd . Open ( LUnitFileFullName )
      EXCEPT ELSE Failure ( "opening" )  
      END (*EXCEPT*) 
    ; FM3SharedUtils . ReadPrefixR
        ( LUnitRdT
        , (*OUT*) LKind  
        , (*OUT*) LVersion 
        , (*OUT*) LPrefixIsOK 
        )
    ; TRY
        FM3SharedUtils .CheckPrefix
         ( LPrefixIsOK
         , LKind , FM3SharedGlobals . FM3FileKindUnit 
         , LVersion , FM3SharedGlobals . FM3FileVersion0
         , "Compiled file"
         , LUnitFileFullName
         )
       EXCEPT FM3SharedUtils . FatalError
       => RAISE ReadUnitFailure
       END (*EXCEPT*)

    (* Unit file hash and src file time are copies brought out front so a
       unit file's relevance can be checked w/o unpickling the whole thing.
    *)
    ; TRY
        LLen := Rd . GetSub ( LUnitRdT , (*OUT*) LHashArray )
      ; LLen := Rd . GetSub ( LUnitRdT , (*OUT*) LSrcTimeArray )
      EXCEPT ELSE Failure ( "reading" )  
      END (*EXCEPT*) 
    ; LFrontHash := FM3Utils . CharsToHash ( LHashArray ) 
    ; LFrontTimeL := FM3Utils . CharsToLong ( LSrcTimeArray ) 
    ; LFrontTime := FM3UnsafeUtils . LongIntToLongReal ( LFrontTimeL )

(* Split here so can read just the hash and time and possibly skip
   reading the pickle, if it's outdated or inconsistent. 
*)


    ; TRY LRefany := Pickle . Read ( LUnitRdT )
      EXCEPT ELSE Failure ( "unpickling" )  
      END (*EXCEPT*) 
    ; TYPECASE LRefany OF
      | NULL => Failure ( "NIL pickle" )  
      | FM3Units . UnitRefTyp ( LUnitRef )
      => UnitTRef ^ . UttUnitRef:= LUnitRef 
      ELSE Failure ( "mistyped pickle" )  
      END (*TYPECASE*)

    ; IF LFrontHash # LUnitRef . UntHash
      THEN Failure ( "hash mismatch" )
      END (*IF*) 

    ; IF LFrontTime # LUnitRef . UntSrcTime 
      THEN Failure ( "time mismatch" )  
      END (*IF*) 

    ; TRY Rd . Close ( LUnitRdT ) 
      EXCEPT ELSE Failure ( "closing" )  
      END (*EXCEPT*) 
    END ReadUnitFile
    
; BEGIN
    GSearchPathShown := FALSE 
  ; InitStdFileNames ( )
  END FM3Compile
.
