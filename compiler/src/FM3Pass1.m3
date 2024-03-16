

(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Pass1

(* The first pass.
   1. Scan and Parse
   2. Replace identifers by numeric atoms. 
   3. Convert to a fully-delimited intermediate token stream, written
      to a file ready to be read backwards.

   5. Build a global table of atom-accessed Units.UnitTyp records for
      compilation units,
   6. For each unit, build atom-accessed records for identifiers, scopes,
      and declarations.
   7. For each scope, build a compact dictionary mapping identifier
      to decl atoms.
   8. Resolve some identifier occurrences to decl atoms and insert these
      into the token stream.
*)

; IMPORT Atom
; IMPORT Compiler 
; FROM File IMPORT Byte  
; IMPORT FileWr
; IMPORT Fmt 
; IMPORT FS 
; IMPORT IntSets 
; IMPORT OSError
; IMPORT Pathname
; IMPORT Stdio
; IMPORT Text 
; IMPORT Thread 
; IMPORT UniRd
; IMPORT Wr

; IMPORT Ranges_Int
; IMPORT IntIntVarArray 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; FROM FM3Base IMPORT tPosition 
; IMPORT FM3CLArgs
; IMPORT FM3Compile
; IMPORT FM3Compress
; FROM FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int
; IMPORT FM3DisAsm
; IMPORT FM3RTFailures 
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3IntToks AS Itk
; FROM FM3IntToks
    IMPORT LtToRt , LtToPatch , LtToOne , LtToOnePatch , LtToTwoPatch
           , LtToListSepPatch 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Parser
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks
; IMPORT FM3Units 
; IMPORT FM3Utils 
; IMPORT RdBackFile

; CONST PosImage = FM3Utils . PositionImage

; VAR FileTagVersion := VAL ( ORD ( '1' ) , Byte )  

; CONST LeftFileTagLen
    = BYTESIZE ( FM3SharedGlobals . FM3FileTagLt )
    + BYTESIZE ( FM3SharedGlobals . FM3FileKindRdBackLt )
    + BYTESIZE ( FileTagVersion )

; CONST ALOSE = FM3Messages . AtomListToOSError

; PROCEDURE PutBwd ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN
      <* ASSERT RdBack # FM3Globals . P2RdBack *> 
      IF TRUE 
      THEN
        TRY
          FM3Compress . PutBwd ( RdBack , ValueL ) 
        EXCEPT OSError . E ( EMsg )
        => FatalArr
             ( ARRAY OF REFANY
                 { "Unable to write to readback file: "
(*TODO: Give RdBackFile a "Filename" function,, then insert it here. *) 
                 , ALOSE ( EMsg ) , "."  
                 }
             ) 
        END (*EXCEPT*) 
      END (*IF*)
    END PutBwd

(*EXPORTED*) 
; PROCEDURE RunPass1 ( SrcFileName : TEXT ) 

  =  VAR LUnitRef : FM3Units . UnitRefTyp
 
  ; BEGIN (*RunPass1*)
      LUnitRef := InitPass1 ( SrcFileName ) 
    ; FM3Units . PushUnit ( LUnitRef )
    ; LUnitRef ^ . UntPassNosDisAsmed := FM3Base . PassNoSetEmpty 
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Compiling "
            , LUnitRef ^ . UntSrcFilePath
            , "/"
            , SrcFileName
            , "..."
            }
        )
    ; TranslatePass1 ( LUnitRef ) 
    ; FinishPass1 ( LUnitRef ) 
    END RunPass1

; PROCEDURE EnsureBuildDirectory
    ( UnitRef : FM3Units . UnitRefTyp ; SrcFilePath : TEXT )

  = BEGIN (*EnsureBuildDirectory*)
      UnitRef ^ . UntBuildDirPath
        := SrcFilePath & "/" & FM3Globals . BuildDirRelPath
    ; TRY
        FS . CreateDirectory ( UnitRef ^ . UntBuildDirPath )
      EXCEPT
      | OSError . E ( EAtoms ) 
      => IF EAtoms . tail = NIL
            AND EAtoms . head # NIL
            AND Atom . ToText ( EAtoms . head ) # NIL
            AND Text . Equal ( Atom . ToText ( EAtoms . head ) , "errno=17" )
(* TODO: There has to be a more graceful way to detect this, but it looks
         like libm3 is letting us down here.
*) 
         THEN (* The directory already exists. We expect this sometimes. *)
           EVAL EAtoms 
         ELSE 
           <*FATAL Thread . Alerted , Wr . Failure *>
           BEGIN
             Wr . PutText
               ( Stdio . stderr , "Unable to create build directory " ) 
           ; Wr . PutText ( Stdio . stderr , UnitRef ^ . UntBuildDirPath ) 
           ; Wr . PutText ( Stdio . stderr , ": " ) 
           ; Wr . PutText
               ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
           ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
           ; Wr . PutText
               ( Stdio . stderr , "Forging ahead, assuming it already exists." ) 
           ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
           ; Wr . Flush ( Stdio . stderr )
           END (*Block.*)
         END (*IF*) 
      END (*EXCEPT*) 
(* TODO: Use Pathname to construct paths, so this works in Windows too. *)  
(* CHECK^ Or would it be better to use FS.GetAbsolutePathname? *)  
    END EnsureBuildDirectory

; CONST UnitLogSuffix = ".log" 

(*EXPORTED.*)
; PROCEDURE InitPass1 ( SrcFileName : TEXT ) : FM3Units . UnitRefTyp

  = VAR LFullFileName : TEXT
  ; VAR LFullPass1OutName : TEXT 
  ; VAR LFullPatchStackName : TEXT 
  ; VAR LSimpleSrcFileName : TEXT 
  ; VAR LSrcFilePath : TEXT 
  ; VAR LUniRd : UniRd . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN (*InitPass1*)
    (* Open source file. *) 
      LSrcFilePath
        := Pathname . Prefix ( FM3Files . AbsFileName ( SrcFileName ) )
    ; LSimpleSrcFileName := Pathname . Last ( SrcFileName )
    ; LUniRd
        := FM3Files . OpenUniRd
             ( LSimpleSrcFileName , LSrcFilePath , "source file " , NIL ) 
    ; LUnitRef := FM3Units . NewUnitRef ( )
    ; LUnitRef ^ . UntSrcFileSimpleName := LSimpleSrcFileName 
    ; LUnitRef ^ . UntSrcFilePath := LSrcFilePath

    (* Create the build directory: *)
(* FIXME: FM3CLArgs wants a build directory to put a log file in, even before
          we get here.  Is this the right place for it?
*)
    ; EnsureBuildDirectory ( LUnitRef , LSrcFilePath ) 

    (* Create the unit log output file. A pure text file. *) 
    ; LUnitRef ^ . UntLogName := LUnitRef ^ . UntSrcFileSimpleName & UnitLogSuffix

    ; TRY LUnitRef ^ . UntLogWrT
            := FileWr . Open ( LUnitRef ^ . UntLogName ) 
      EXCEPT
      | OSError . E ( EAtoms )
      => <*FATAL Thread . Alerted , Wr . Failure *>
         BEGIN
           Wr . PutText ( Stdio . stderr , "Unable to open unit log file " ) 
         ; Wr . PutText ( Stdio . stderr , LUnitRef ^ . UntLogName ) 
         ; Wr . PutText ( Stdio . stderr , ": " ) 
         ; Wr . PutText
             ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
         ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
         ; Wr . PutText ( Stdio . stderr , "Will proceed without it." ) 
         ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
         ; Wr . Flush ( Stdio . stderr )
         END (*Block.*) 
      ; LUnitRef ^ . UntLogWrT := NIL 
      END (*EXCEPT*)
    ; FM3Messages . StartUnit
        ( LUnitRef ^ . UntSrcFileSimpleName , LUnitRef ^ . UntLogWrT ) 

    (* Create build files for the unit. *) 
    ; LUnitRef ^ . UntPatchStackName
        := LUnitRef ^ . UntSrcFileSimpleName & FM3Globals . PatchStackSuffix   
    ; LUnitRef ^ . UntPass1OutSimpleName
        := LUnitRef ^ . UntSrcFileSimpleName & FM3Globals . Pass1OutSuffix   
    ; TRY (*EXCEPT*)
        (* Heh, heh.  Code the exception handler only once for all files. *) 
        LFullPass1OutName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntPass1OutSimpleName
               , NIL
               )
      ; LFullFileName :=  LFullPass1OutName 
      ; LUnitRef ^ . UntPass1OutRdBack
          := RdBackFile . Create ( LFullPass1OutName , Truncate := TRUE )
          
      ; LFullPatchStackName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntPatchStackName
               , NIL
               ) 
      ; LFullFileName :=  LFullPatchStackName 
      ; LUnitRef ^ . UntPatchStackRdBack
          := RdBackFile . Create ( LFullPatchStackName , Truncate := TRUE )
      
      ; LFullPass1OutName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntPass1OutSimpleName
               , NIL
               )
      ; LFullFileName :=  LFullPass1OutName 
      ; LUnitRef ^ . UntPass1OutRdBack
          := RdBackFile . Create ( LFullPass1OutName , Truncate := TRUE )
      EXCEPT
      | OSError . E ( EMsg ) 
      => FatalArr
           ( ARRAY OF REFANY
               { "Unable to open build file \""
               , LFullFileName
               , "\": "
               , ALOSE ( EMsg)
               , "."
               } 
           ) 
      END (*EXCEPT*)

    (* Initialize the readback files. *)
(* COMPLETEME: See that RdBack Create adds FM3 file tags and lengths. *)
    ; PutBwd
        ( LUnitRef ^ . UntPass1OutRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwd
        ( LUnitRef ^ . UntPass1OutRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; LUnitRef ^ . UntPass1OutEmptyCoord
        := RdBackFile . LengthL ( LUnitRef ^ . UntPass1OutRdBack )
    ; LUnitRef ^ . UntMaxPass1OutDepth
        := LUnitRef ^ . UntPass1OutEmptyCoord

    ; PutBwd
        ( LUnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkBOF , LONGINT ) )
    ; PutBwd
        ( LUnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; LUnitRef ^ . UntPatchStackEmptyCoord
        := RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack )
    ; LUnitRef ^ . UntMaxPatchStackDepth
        := LUnitRef ^ . UntPatchStackEmptyCoord
    ; LUnitRef . UntPatchStackTopCoord := LUnitRef . UntPass1OutEmptyCoord

    (* Create unit data structures. *)
(* TODO: eliminate redundant initialization between here are FM3Units.NewUnit. *)
(* Check: Do we really need separate atom dictionaries for identifiers,
          numbers, and CHAR literasl? *) 
    ; LUnitRef ^ . UntIdentAtomDict
        := FM3Atom_OAChars . New
             ( FM3Globals . IdentAtomInitSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAChars
             , DoReverseMap := TRUE 
             )

    ; LUnitRef ^ . UntNumberAtomDict
        := FM3Atom_OAChars . New
             ( FM3Globals . NumberAtomInitSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAChars
             , DoReverseMap := TRUE 
             )

    ; LUnitRef ^ . UntCharsAtomDict (* For CHAR literals. *) 
        := FM3Atom_OAChars . New
             ( FM3Globals . CharsAtomInitSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAChars
             , DoReverseMap := TRUE 
             )

    ; LUnitRef ^ . UntWCharsAtomDict
        := FM3Atom_OAWideChars . New
             ( FM3Globals . WideCharsAtomInitSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAWChars
             , DoReverseMap := TRUE 
             )

    ; LUnitRef ^ . UntScopeMap
        := FM3Scopes . NewScopeMap ( FM3Globals . InitScopeCtPerUnit )  
    ; LUnitRef ^ . UntDeclMap
        := FM3Decls . NewDeclMap ( FM3Globals . InitDeclCtPerUnit )

      (* Initialize Scanner for unit. *)
    ; FM3Scanner . PushState ( LUniRd , LUnitRef )
(* CHECK: ? *)

    ; FM3Globals . SkipNoStack 
        := IntIntVarArray . New
             ( FIRST ( INTEGER )
             , Ranges_Int . RangeTyp
                 {  0 , FM3Globals . InitSkipStackCt - 1 }
             )
    ; IntIntVarArray . Touch (* It needs a lower bound. *) 
        ( FM3Globals . SkipNoStack , Ranges_Int . RangeTyp { 0 , 0 } )   
    ; FM3Globals . NextSkipNo := 1 (* But don't use element 0. *) 
    ; RETURN LUnitRef 
    END InitPass1

; PROCEDURE DisAsmx
    ( UnitRef : FM3Units . UnitRefTyp ; RdBackFileName : TEXT )
  (*PRE: RdBackFile.Copy is closed. *) 
  (*POST: RdBackFile.Copy is reclosed. *) 

  = VAR LFullDisAsmFileName : TEXT
  ; VAR LFullRdBackFileName : TEXT
  ; VAR LDisAsmWrT : Wr . T
  ; VAR LRdBack : RdBackFile . T
  
  ; BEGIN
      LFullDisAsmFileName
        := Pathname . Join
             ( NIL , RdBackFileName , FM3Globals . DisAsmFileSuffix ) 
    ; LFullRdBackFileName
        := Pathname . Join
             ( NIL , RdBackFileName , FM3Globals . CopyFileSuffix )
    ; LRdBack := RdBackFile . Open ( LFullRdBackFileName )
    ; LDisAsmWrT := FileWr . Open ( LFullDisAsmFileName )
    ; FM3DisAsm . DisAsmWOperandsBwd ( LRdBack , LDisAsmWrT )
    ; RdBackFile . Close ( LRdBack , - 1L )      
    ; Wr . Close ( LDisAsmWrT ) 
    END DisAsmx

; PROCEDURE TranslatePass1 ( UnitRef : FM3Units . UnitRefTyp )

  = VAR LPass1LengthL : LONGINT
  ; VAR LExceptionName : TEXT 
  ; VAR LExceptionLoc : TEXT 
  
  ; BEGIN (*TranslatePass1*)
      TRY

      (* Run the translation part of pass 1. *) 
        UnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *)

      (* Finish Pass 1 output file before running Pass2. *) 
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkRightEnd , LONGINT )
          )
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      ; LPass1LengthL := RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack )
      ; LExceptionName := NIL (* Succeeded. *) 
      ; FM3Parser . CloseFM3Parser ( )
(*TODO ^ Do this sometime later. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE
        LPass1LengthL := RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack )
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkRightEndIncomplete , LONGINT )
          )

      ; LExceptionName
          := FM3RTFailures . ExcNameFromAddr ( Compiler . ThisException ( ) )  
      ; LExceptionLoc 
          := FM3RTFailures . ActivationLocationFromAddr
               ( Compiler . ThisException ( ) )  
      END (*EXCEPT *)
    END TranslatePass1

(*EXPORTED.*)
; PROCEDURE FinishPass1 ( UnitRef : FM3Units . UnitRefTyp ) 

(* TODO: Consistify "Depth" in identifiers with "Length" in RdBackFile. *)  
  = VAR LPass1LengthL : LONGINT
  ; VAR LLengthL : LONGINT
  ; VAR LPass1FullFileName , LPass1FullCopyName : TEXT 
  ; VAR LExceptionName , LExceptionLoc : TEXT 

  ; BEGIN (*FinishPass1*) 
      LPass1FullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPass1OutSimpleName
             , NIL
             )
    ; LPass1FullCopyName 
        := Pathname . Join
             ( NIL , LPass1FullFileName , FM3Globals . CopyFileSuffix ) 
    ; RdBackFile . Copy 
        ( UnitRef ^ . UntPass1OutRdBack , LPass1FullCopyName , - 1L )
    ; IF LExceptionName # NIL OR FM3CLArgs . DoDisAsmPass1
      THEN (* Disassemble pass 1 output now. *) 
        FM3Compile . DisAsmPassFile ( UnitRef , LPass1FullFileName )
      ; TRY FS . DeleteFile ( LPass1FullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF LExceptionName # NIL
      THEN 
        FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure writing pass 1 output file at depth "
               , Fmt . LongInt
                   ( RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack ) )
               , FM3Messages . NLIndent 
               , "Exception "
               , LExceptionName
               , ","
               , FM3Messages . NLIndent 
               , "raised at "
               , LExceptionLoc
               , "."
               }
           )
      ; RAISE FM3SharedUtils . FatalError
                ( "Failure writing pass 1 output file." )
      END (*IF*) 

    (* Finish with pass 1 output file. *) 
    ; UnitRef ^ . UntMaxPass1OutDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPass1OutRdBack )
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Pass 1 output file "
            , UnitRef ^ . UntPass1OutSimpleName
            , " has "
            , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPass1OutDepth )
            , " bytes."
            } 
        )
    ; LLengthL := RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack )
    ; RdBackFile . Close 
        (  UnitRef ^ . UntPass1OutRdBack , - 1L (* Leave full length. *) )
    ; IF LLengthL # UnitRef ^ . UntPass1OutEmptyCoord 
      THEN
        UnitRef . UntPass2Result := FM3CLArgs . CcPass1OutNotEmpty  
      ; FM3LogArr
          ( ARRAY OF REFANY
              { "Pass 1 output file "
              , UnitRef ^ . UntPass1OutSimpleName
              , " final size = "
              , FM3Base . Int64Image ( LLengthL )
              , " bytes."
              } 
          )
      ; IF NOT FM3CLArgs . DoDisAsmPass1
        THEN 
          FM3Compile . DisAsmPassFile ( UnitRef , LPass1FullFileName )
        ; TRY FS . DeleteFile ( LPass1FullCopyName )
          EXCEPT OSError . E => (* It didn't exist. *) 
          END (*EXCEPT*)
        END (*IF*) 
      ; FatalArr
          ( ARRAY OF REFANY
              { "Pass 1 output file is not sufficiently empty, should be "
              , FM3Base . Int64Image ( UnitRef ^ . UntPass1OutEmptyCoord )
              , "."
              }
          )
      ; RAISE FM3SharedUtils . FatalError
                ( "Pass 1 output file is not sufficiently empty." )
      END (*IF*) 
    ; IF NOT FM3CLArgs . DoDisAsmPass1  
      THEN
        TRY FS . DeleteFile ( LPass1FullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF NOT FM3CLArgs . DoKeep
      THEN 
        TRY FS . DeleteFile ( UnitRef ^ . UntPass1OutSimpleName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)




(* TODO: display code point counts. *)

    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Finished compiling " , UnitRef ^ . UntSrcFileSimpleName , "." }
        )
    END FinishPass1 
    
; PROCEDURE UnitId 
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition
    ; VAR (*OUT*) IdText : TEXT
    ; VAR (*OUT*) NameFromFileName : TEXT 
    )

  = BEGIN (* UnitId *) 
      IF UnitRef = NIL THEN RETURN END (*IF*)
    ; IF IdAtom = FM3Base . AtomNull THEN RETURN END (*IF*)
    ; IF UnitRef ^ . UntSrcFileSimpleName = NIL THEN RETURN END (*IF*) 
    ; UnitRef ^ . UntUnitIdentAtom := IdAtom 
    ; IdText := FM3Units . TextOfIdAtom ( IdAtom )
    ; UnitRef ^ . UntUnitIdentAtom := IdAtom 
    ; UnitRef ^ . UntUnitIdentPos := Position 
    ; NameFromFileName
        := FM3Files . RemoveSuffix ( UnitRef ^ . UntSrcFileSimpleName ) 
    END UnitId 

(*EXPORTED:*)
; PROCEDURE InterfaceId
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition 
    )

  = VAR LIdText : TEXT
  ; VAR LNameFromFileName : TEXT

  ; BEGIN (* InterfaceId *)
      UnitId
        ( UnitRef , IdAtom , Position
        , (*OUT*) LIdText , (*OUT*) LNameFromFileName
        ) 
    ; IF NOT Text . Equal ( LIdText , LNameFromFileName)
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Interface name \""
              , LIdText
              , "\" does not match file name \""
              , UnitRef ^ . UntSrcFileSimpleName
              , "\"" 
              , FM3Messages . NLIndent
              , "Changing interface name to \""
              , LNameFromFileName 
              , "\"." 
              }
          , Position
          )
      ; UnitRef ^ . UntSrcFileSimpleName := LNameFromFileName 
      END (*IF*)
    END InterfaceId 

(*EXPORTED:*)
; PROCEDURE ModuleId
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition 
    )

  = VAR LIdText : TEXT
  ; VAR LNameFromFileName : TEXT

  ; BEGIN (* ModuleId *) 
      UnitId
        ( UnitRef , IdAtom , Position
        , (*OUT*) LIdText , (*OUT*) LNameFromFileName
        ) 
    ; IF NOT Text . Equal ( LIdText , LNameFromFileName)
      THEN
        FM3Messages . InfoArr
          ( ARRAY OF REFANY
              { "Module name \""
              , LIdText
              , "\" does not match file name \""
              , UnitRef ^ . UntSrcFileSimpleName
              , "\"." 
              }
         , Position 
         ) 
      END (*IF*)
    END ModuleId 

(*EXPORTED:*)
; PROCEDURE CheckUnitFinalId
    ( UnitRef : FM3Units . UnitRefTyp
    ; EndIdAtom : FM3Base . AtomTyp 
    ; UnitKind : FM3Units . UnitKindTyp
    ; Position : FM3Base . tPosition 
    )
    
  = BEGIN (* CheckUnitFinalId *)
      IF UnitRef ^ . UntUnitIdentAtom # FM3Base . AtomNull
         AND EndIdAtom # FM3Base . AtomNull
         AND EndIdAtom # UnitRef ^ . UntUnitIdentAtom  
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY 
              { "Identifier at end of "
              , FM3Units . UnitKindImage ( UnitKind )
              , " \""
              , FM3Units . TextOfIdAtom ( UnitRef ^ . UntUnitIdentAtom )
              , "\", at " 
              , PosImage ( UnitRef ^ . UntUnitIdentPos )  
              , " must repeat its name ("
              , FM3Units . UnitKindSectionNo ( UnitKind )
              , ")." 
              } 
          , Position
          )
      END (*IF*) 
    END CheckUnitFinalId

(* ------------------------ Pass 1 output file --------------------- *) 

(*EXPORTED:*)
; PROCEDURE Coord ( ) : LONGINT
  (* Current coordinate of the current unit. *)
  
  = BEGIN
      RETURN RdBackFile . LengthL
               ( FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack ) 
    END Coord

(*EXPORTED:*)
; PROCEDURE PutBwd_Attribute ( READONLY ParsAttr : tParsAttribute )
   
  = BEGIN
(*FIXME: Don't push unnest if skipping.. *) 
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO
(* Keep DumpWork.DumpNumericBwd consistent with this:*) 
        CASE ParsAttr . Scan . SaTok OF (* Optional varterm-specific value info: *) 
     (* | FM3SrcToks . StkIdent
          => Ident spelling? Probably not.
             PushOACharsBwd ( WRdBack , ParsAttr . Scan . SaChars )
     *) 
        | FM3SrcToks . StkIntLit 
        , FM3SrcToks . StkLongIntLit 
        , FM3SrcToks . StkBasedLit 
        , FM3SrcToks . StkLongBasedLit
        , FM3SrcToks . StkRealLit 
        , FM3SrcToks . StkLongRealLit 
        , FM3SrcToks . StkExtendedLit 
        , FM3SrcToks . StkTextLit 
          => PushOACharsBwd ( WRdBack , ParsAttr . Scan . SaChars )
        | FM3SrcToks . StkWideTextLit 
          => PushOAWideCharsBwd
               ( WRdBack , ParsAttr . Scan . SaWideChars )
        | FM3SrcToks . StkCharLit 
        , FM3SrcToks . StkWideCharLit 
          => PutBwd
               ( WRdBack
               , VAL ( ORD ( ParsAttr . Scan . SaWCh ) , LONGINT ) 
               )
     (* | FM3SrcToks . StkLexErrChars => Throw these away, for now. *) 
        ELSE
        END (*CASE*) 

      ; CASE ParsAttr . Scan . SaTok OF (* All varterms. *)
        | FM3SrcToks . StkLexErrChars => (* Throw these away, for now. *) 
        | FM3SrcToks . StkIdent .. FM3SrcToks . StkWideCharLit
        => PutBwd
             ( WRdBack
             , VAL ( ParsAttr . Scan . Position . Column , LONGINT )
             )
         ; PutBwd
             ( WRdBack
             , VAL ( ParsAttr . Scan . Position . Line , LONGINT )
             )
         ; PutBwd
             ( WRdBack , VAL ( ParsAttr . Scan . SaAtom , LONGINT ) )
         ; PutBwd
             ( WRdBack , VAL ( ParsAttr . Scan . SaTok , LONGINT ) )
         ELSE 
         END (*CASE*) 
      END (*WITH*)
    END PutBwd_Attribute

; PROCEDURE PushOACharsBwd
    ( RdBack : RdBackFile . T ; Chars : REF ARRAY OF CHAR )

  = VAR LNumber : INTEGER

  ; BEGIN
      LNumber := NUMBER ( Chars ^ ) 
    ; FOR RI := LNumber - 1 TO 0 BY - 1 
      DO PutBwd
           ( RdBack , VAL ( ORD ( Chars ^ [ RI ] ) , LONGINT ) )
      END (*FOR*)
    ; PutBwd ( RdBack , VAL ( LNumber , LONGINT ) )
    END PushOACharsBwd 
    
; PROCEDURE PushOAWideCharsBwd
    ( RdBack : RdBackFile . T ; Chars : REF ARRAY OF WIDECHAR )

  = VAR LNumber : INTEGER

  ; BEGIN
      LNumber := NUMBER ( Chars ^ ) 
    ; FOR RI := LNumber - 1 TO 0 BY - 1 
      DO PutBwd
           ( RdBack , VAL ( ORD ( Chars ^ [ RI ] ) , LONGINT ) ) 
      END (*FOR*)
    ; PutBwd ( RdBack , VAL ( LNumber , LONGINT ) )
    END PushOAWideCharsBwd 

(*EXPORTED:*)
; PROCEDURE PutBwdInt ( Value : INTEGER )

  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
        , VAL ( Value , LONGINT ) 
        )

    END PutBwdInt

(*EXPORTED:*)
; PROCEDURE PutBwdLong ( Value : LONGINT )
  
  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack , Value )
    END PutBwdLong

(*EXPORTED:*)
; PROCEDURE PutBwd_ListSepPatchPos
    ( ListTokLt : Itk . TokTyp
    ; C : LONGINT
    ; ElemNo : INTEGER
    ; READONLY Position : FM3Base . tPosition
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ElemNo , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd
          ( WRdBack , VAL ( ListTokLt + Itk . LtToListSepPatch , LONGINT ) )
      END (*WITH*) 
    END PutBwd_ListSepPatchPos 

(*EXPORTED:*)
; PROCEDURE PutBwd_L ( T : Itk . TokTyp )

  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
        , VAL ( T , LONGINT ) 
        )
    END PutBwd_L

(*EXPORTED:*)
; PROCEDURE PutBwd_LP
    ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LP

(*EXPORTED:*)
; PROCEDURE PutBwd_LP_rp
    ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + Itk . LtToRt, LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LP_rp

(*EXPORTED:*)
; PROCEDURE PutBwd_RP
    ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + Itk . LtToRt , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_RP

(*EXPORTED:*)
; PROCEDURE PutBwd_LI ( T : Itk . TokTyp ; I : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LI

(*EXPORTED:*)
; PROCEDURE PutBwd_LIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LIP

(*EXPORTED:*)
; PROCEDURE PutBwd_LIP_rip
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + Itk . LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LIP_rip

(*EXPORTED:*)
; PROCEDURE PutBwd_EIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOne , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_EIP

(*EXPORTED:*)
; PROCEDURE PutBwd_ECIP
    ( T : Itk . TokTyp
    ; Coord : LONGINT
    ; I : INTEGER
    ; READONLY Position : tPosition
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , Coord ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToListSepPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_ECIP

(*EXPORTED:*)
; PROCEDURE PutBwd_LCr ( T : Itk . TokTyp ; C : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCr

(*EXPORTED:*)
; PROCEDURE PutBwd_LCP_rp
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCP_rp

(*EXPORTED:*)
; PROCEDURE PutBwd_LCP_eCp_rp
   ( T : Itk . TokTyp
   ; C1 : LONGINT
   ; READONLY Position : tPosition
   ; C2 : LONGINT
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C2 ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C1 ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCP_eCp_rp

(*EXPORTED:*)
; PROCEDURE PutBwd_LCPI_rpi
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; READONLY Position : tPosition 
    ; I : INTEGER 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCPI_rpi

(*EXPORTED:*)
; PROCEDURE PutBwd_LCIP_rip
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I : INTEGER 
    ; READONLY Position : tPosition 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP_rip

(*EXPORTED:*)
; PROCEDURE PutBwd_LCIP_eCiP_riP
    ( T : Itk . TokTyp 
    ; LC : LONGINT 
    ; I : INTEGER 
    ; READONLY LPos : tPosition
    ; EC : LONGINT 
    ; READONLY EPos : tPosition
    ; READONLY RPos : tPosition
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( RPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( RPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( EPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( EPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , EC ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( LPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( LPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , LC ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP_eCiP_riP

(*EXPORTED:*)
; PROCEDURE PutBwd_LCP_eCP_rP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionLt : tPosition
   ; CEins : LONGINT
   ; READONLY PositionEins : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionRt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionRt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionEins . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionEins . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CEins ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionLt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionLt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCP_eCP_rP

(*EXPORTED:*)
; PROCEDURE PutBwd_LCP_eCP_zCP_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; READONLY PL : tPosition
   ; Ce : LONGINT
   ; READONLY Pe : tPosition
   ; Cz : LONGINT
   ; READONLY Pz : tPosition
   ; READONLY Pr : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Pr . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pr . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( Pz . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pz. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , Cz ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToTwoPatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( Pe . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pe. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ce ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToOnePatch , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( PL . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PL . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CL ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCP_eCP_zCP_rP 

(*EXPORTED:*)
; PROCEDURE PutBwd_LCP_eCPB_zCP_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; READONLY PL : tPosition
   ; Ce : LONGINT
   ; READONLY Pe : tPosition
   ; Be : BOOLEAN 
   ; Cz : LONGINT
   ; READONLY Pz : tPosition
   ; READONLY Pr : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Pr . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pr . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( Pz . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pz. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , Cz ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToTwoPatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( ORD ( Be ) , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pe . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pe. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ce ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToOnePatch , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( PL . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PL . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CL ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCP_eCPB_zCP_rP 

(*EXPORTED:*)
; PROCEDURE PutBwd_LCPeCprp
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; CInfix : LONGINT 
   ; READONLY PositionInfix : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionInfix . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionInfix . Line , LONGINT ) )
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( PositionInfix . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionInfix . Line , LONGINT ) )
      ; PutBwd ( WRdBack , CInfix ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( PositionInfix . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionInfix . Line , LONGINT ) )
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCPeCprp

(*EXPORTED:*)
; PROCEDURE PutBwd_ECPrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionRt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionRt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( PositionOne . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionOne . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_ECPrP

(*EXPORTED:*)
; PROCEDURE PutBwd_ECIP_riP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; I : INTEGER 
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionRt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionRt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( PositionOne . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionOne . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_ECIP_riP

(*EXPORTED:*)
; PROCEDURE PutBwd_LCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( ORD ( B ) , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCBr

(*EXPORTED:*)
; PROCEDURE PutBwd_LCI_ri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCI_ri

(*EXPORTED:*)
; PROCEDURE PutBwd_LI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I0 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LI3

(*EXPORTED:*)
; PROCEDURE PutBwd_LI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I5 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I4 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I3 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I0 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LI6

(*EXPORTED:*)
; PROCEDURE PutBwd_LCeCr ( T : Itk . TokTyp ; Ct , Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) )
      END (*WITH*) 
    END PutBwd_LCeCr

(*EXPORTED:*)
; PROCEDURE PutBwd_LCIeCri
    ( T : Itk . TokTyp ; Ct : LONGINT ; I : INTEGER ; Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIeCri

(*EXPORTED:*)
; PROCEDURE Pop4 ( )

  = BEGIN (*Pop4*)
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        EVAL GetBwd ( WRdBack ) 
      ; EVAL GetBwd ( WRdBack ) 
      ; EVAL GetBwd ( WRdBack )
      ; EVAL GetBwd ( WRdBack ) 
      END (*WITH*) 
    END Pop4
      
(*EXPORTED:*)
; PROCEDURE Pop8 ( )

  = BEGIN (*Pop4*)
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        EVAL GetBwd ( WRdBack ) 
      ; EVAL GetBwd ( WRdBack ) 
      ; EVAL GetBwd ( WRdBack )
      ; EVAL GetBwd ( WRdBack ) 
      ; EVAL GetBwd ( WRdBack )
      ; EVAL GetBwd ( WRdBack ) 
      ; EVAL GetBwd ( WRdBack )
      ; EVAL GetBwd ( WRdBack ) 
      END (*WITH*) 
    END Pop8

(*EXPORTED:*)
; PROCEDURE MakeConstruct
    ( PatchCoord : LONGINT ; TokLt : Itk . TokTyp )
  (* Fixed shape construct, with left and right tokens only. *) 

  = BEGIN
    (* Right token: *) 
      PutBwdInt ( TokLt + Itk . LtToRt )
    (* Left token, to be moved leftward: *) 
    ; PutBwdLong ( PatchCoord ) (*Patch*)
    ; PutBwdInt ( TokLt + Itk . LtToPatch )
    END MakeConstruct

; TYPE Dkt = FM3Decls . DeclKindTyp
; VAR VarLabel := ARRAY Dkt OF TEXT { NIL , .. }  
; VAR VarSection := ARRAY Dkt OF TEXT { NIL , .. }

; PROCEDURE InitVarInfo ( )

  = BEGIN
      VarLabel [ Dkt . DkVar ] := "Variable"
    ; VarLabel [ Dkt . DkVALUEFormal ] := "VALUE formal"
    ; VarLabel [ Dkt . DkVARFormal ] := "VAR formal"
    ; VarLabel [ Dkt . DkROFormal ] := "READONLY formal"
    ; VarLabel [ Dkt . DkRecField ] := "Record field"
    ; VarLabel [ Dkt . DkObjField ] := "Object field"

    ; VarSection [ Dkt . DkVar ] := "(2.4.3)"
    ; VarSection [ Dkt . DkVALUEFormal ] := "(2.2.8)"
    ; VarSection [ Dkt . DkVARFormal ] := "(2.2.8)"
    ; VarSection [ Dkt . DkROFormal ] := "(2.2.8)"
    ; VarSection [ Dkt . DkRecField ] := "(2.2.4)"
    ; VarSection [ Dkt . DkObjField ] := "(2.2.9)"
    END InitVarInfo 

(*EXPORTED.*)
; PROCEDURE RequireTypeAndOrValue 
    ( READONLY Position : tPosition
    ; HasType : BOOLEAN 
    ; HasValue : BOOLEAN
    )
  : BOOLEAN (* OK *) 

  (* Anything that requires a type and/or value: variable , formal, field. 
     Check and maybe emit message. 
     Gets DeclKind from FM3Decls.TopDeclInfo. 
  *) 

  = BEGIN 
     IF NOT  HasType AND NOT HasValue 
     THEN
       WITH WDeclInfo = FM3Decls . TopDeclInfo ( )
       DO 
         FM3Messages . ErrorArr
           ( ARRAY OF REFANY 
               { VarLabel [ WDeclInfo . DiKind ] 
               , " must have a type and/or an initial value. "
               , VarSection [ WDeclInfo . DiKind  ]
               } 
           , Position
           )
       ; RETURN FALSE 
       END (*WITH*)
     ELSE RETURN TRUE 
     END (*IF*) 
    END RequireTypeAndOrValue 

(*EXPORTED:*)
; PROCEDURE MakeElem
    ( VAR LHSAttr : tParsAttribute
    ; PatchCoord : LONGINT
    ; TokLt : Itk . TokTyp
    ; ElemNo : INTEGER 
    )
(* Rework or eliminate: *) 
  (* Left and right tokens surrounding a numbered element of a list. *)
  
  = BEGIN
      LHSAttr . PaInt := ElemNo 
    (* Right token: *) 
    ; PutBwdInt ( ElemNo )
    ; PutBwdInt ( TokLt + Itk . LtToRt )
    (* Left token, to be moved leftward: *) 
    ; PutBwdInt ( ElemNo )
    ; PutBwdLong ( PatchCoord ) (*Patch*)
    ; PutBwdInt ( TokLt + Itk . LtToPatch )
    END MakeElem

(*EXPORTED:*)
; PROCEDURE MakeListEmpty
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY Position : tPosition
    )

  = BEGIN
      LHSAttr . PaInt := 0 (* Valid element count. *) 
    ; PutBwdInt ( Position . Column ) 
    ; PutBwdInt ( Position . Line ) 
    ; PutBwdLong ( 0L )
    ; PutBwdInt ( TokLt  )

    ; PutBwdInt ( Position . Column ) 
    ; PutBwdInt ( Position . Line ) 
    ; PutBwdLong ( 0L )
    ; PutBwdInt ( TokLt + Itk . LtToRt )
    
    END MakeListEmpty 

(*EXPORTED:*)
; PROCEDURE MakeListPos
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY Position : tPosition
    ; READONLY ElemsAttr : tParsAttribute 
    )

  = BEGIN
      LHSAttr . PaInt := ElemsAttr . PaInt (* Valid element count. *) 
    ; LHSAttr . PaPass1Coord := ElemsAttr . PaPass1Coord (* Redundant? used? *)
    ; PutBwdInt ( Position . Column ) 
    ; PutBwdInt ( Position . Line ) 
    ; PutBwdInt ( ElemsAttr . PaInt ) (* Elem Ct. *)
    ; PutBwdInt ( TokLt + Itk . LtToRt )
    ; PutBwdInt ( Position . Column ) 
    ; PutBwdInt ( Position . Line ) 
    ; PutBwdInt ( ElemsAttr . PaInt )
    ; PutBwdLong ( ElemsAttr . PaPass1Coord ) 
    ; PutBwdInt ( TokLt + Itk . LtToPatch )
    END MakeListPos 

(*EXPORTED:*)
; PROCEDURE ImportsLt (  )

  = BEGIN (*ImportsLt*)
    END ImportsLt
      
(*EXPORTED:*)
; PROCEDURE ImportsRt (  )

  = BEGIN (*ImportsRt*)
    END ImportsRt
      
(*EXPORTED:*)
; PROCEDURE Import
    ( Atom : FM3Base . AtomTyp ; READONLY Position : tPosition ) 

  = BEGIN (*Import*)
    END Import
      
(*EXPORTED:*)
; PROCEDURE FromImport
    ( IntfAtom : FM3Base . AtomTyp
    ; READONLY InftPos : tPosition
    ; DeclAtom : FM3Base . AtomTyp
    ; READONLY DeclPos : tPosition
    )

  = BEGIN (*FromImport*)
    END FromImport

(*EXPORTED:*)
; PROCEDURE BeginBlock ( ) : FM3Base . ScopeNoTyp (* Created. *) 

  = BEGIN (*BeginBlock*)
    END BeginBlock

(*EXPORTED:*)
; PROCEDURE EndBlock ( )

  = BEGIN (*EndBlock*)
    END EndBlock

(* Insert tok *) 

(* ----------------------------- Scopes ---------------------------- *)

(* These are called by the parser: *) 

(*EXPORTED.*)
; PROCEDURE ScopeEmpty
    ( ScopeKind : FM3Scopes . ScopeKindTyp ; Position : FM3Base . tPosition )
  : FM3Scopes . ScopeRefTyp 

  = BEGIN (*ScopeEmpty*)
      RETURN
        FM3Scopes . NewScopeRef
          ( FM3Units . UnitStackTopRef , ScopeKind , Position ) 
    END ScopeEmpty

(* Left-to-right scope handling.  These are called by the parser. *)

(*EXPORTED.*)
; PROCEDURE DeclIdL2R 
    ( DeclIdTok : Itk . TokTyp
    ; DeclKind : Dkt 
    ; READONLY IdAttribute : tParsAttribute
    ; SepTok : Itk . TokTyp := Itk . ItkNull
                            (* ^Implies single decl id, not in a list. *)  
    ; READONLY SepPosition : tPosition := FM3Base . PositionNull 
    ; PriorIdCt : INTEGER := 0 (* Number of ids to left of this one. *)
    )
  : BOOLEAN (* Use this declared id.  (It's not predefined and not a duplicate
               in current scope.) *)
  (* PRE: IdAttribute is for an identifier in a declaration context. *) 

  = VAR LTokToPut : Itk . TokTyp
  ; VAR LResult : BOOLEAN

  ; BEGIN (*DeclIdL2R*)
      WITH WScope = FM3Scopes . DeclScopeStackTopRef ^
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack 
      DO
        IF IdAttribute . Scan . SaIsReservedId
        THEN
          ErrorArr
            ( ARRAY OF REFANY
                { PosImage ( IdAttribute . Scan . Position )
                , " Identifier \""
                , FM3SrcToks . Image ( IdAttribute . Scan . SaAtom )
                , "\" is predefined and cannot be redeclared (2.8.2)."
                } 
            )
          (* No output. *) 
        ; RETURN FALSE (* Caller, Don't use this Id. *) 
        ELSE
          IF NOT IntSets . IsElement
                   ( IdAttribute . Scan . SaAtom , WScope . ScpDeclIdSet )
          THEN  (* 1st declaration of Ident in scope . *) 
            WScope . ScpDeclIdSet
              := IntSets . Include
                   ( WScope . ScpDeclIdSet , IdAttribute . Scan . SaAtom )
          (* Maybe push Separator token: *)
          ; IF SepTok # Itk . ItkNull AND PriorIdCt > 0
            THEN 
              PutBwd ( WunRdBack , VAL ( SepPosition . Column , LONGINT ) ) 
            ; PutBwd ( WunRdBack , VAL ( SepPosition . Line , LONGINT ) )
            ; PutBwd ( WunRdBack , VAL ( PriorIdCt , LONGINT ) )
            ; PutBwd ( WunRdBack , VAL ( SepTok , LONGINT ) )
            END (*IF*) 
          (* Id is valid. Plan to push Ident token: *)
          ; LTokToPut := DeclIdTok
(*FIXME ^ This should always be passed in as ItkDeclId, so use it. *) 
          ; LResult := TRUE (* Caller, Use this decl id. *)
          ELSE (* A Duplicate declaration of SaAtom in current scope. *)
            WScope . ScpDuplDeclIdSet
              := IntSets . Include
                   ( WScope . ScpDuplDeclIdSet , IdAttribute . Scan . SaAtom )
(* CHECK^ Do we need ScpDuplDeclIdSet? *) 
          (* Plan to push duplicate Ident token.  The only effect will be to
             emit an error later, during R2L, when the position of the original
             declaring occurence is known. *) 
          ; LTokToPut := Itk . ItkDuplDeclId
          ; LResult := FALSE (* Caller, Don't use this Id. *)
          END (*IF*) 
        ; PutBwd
            ( WunRdBack
            , VAL ( IdAttribute . Scan . Position . Column , LONGINT )
            ) 
        ; PutBwd
            ( WunRdBack
            , VAL ( IdAttribute . Scan . Position . Line , LONGINT )
            )
        ; PutBwd
            ( WunRdBack
            , VAL ( IdAttribute . Scan . SaAtom , LONGINT )
            )
        ; IF LTokToPut = Itk . ItkDeclId 
          THEN PutBwd ( WunRdBack , VAL ( ORD ( DeclKind ) , LONGINT ) )
(* TODO: Make up your mind and either make all the decl ids ItkDeclId,
         (which has a declKind), or always use the passed-in
         kind-distinguished Id token, (which does not) everywhere.
*) 
          END (*IF*) 
        ; PutBwd ( WunRdBack , VAL ( LTokToPut , LONGINT ) )
        ; RETURN LResult
        END (*IF*)
      END (*WITH*) 
    END DeclIdL2R

(*EXPORTED.*)
; PROCEDURE IdentRefL2R ( READONLY StkIdAttribute : tParsAttribute )
  (* Including a reserved Id. *) 

  = VAR LTokToPut : Itk . TokTyp

  ; BEGIN (*IdentRefL2R*)
      WITH WScan = StkIdAttribute . Scan
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack 
      DO IF WScan . SaIsReservedId 
        THEN LTokToPut := Itk . ItkReservedId 
        ELSE
          WITH WIdentRefSet = FM3Scopes . LookupScopeStackTopRef ^ . ScpRefIdSet
          DO WIdentRefSet
               := IntSets . Include
                    ( WIdentRefSet , StkIdAttribute . Scan . SaAtom )
          END (*WITH*) 
        ; LTokToPut := Itk . ItkIdRefAtom 
        END (*IF*) 
      ; PutBwd ( WunRdBack , VAL ( WScan . Position . Column , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( WScan . Position . Line , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( WScan . SaAtom , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( LTokToPut , LONGINT ) ) 
      END (*WITH*) 
    END IdentRefL2R

(*EXPORTED.*)
; PROCEDURE OverrideIdentRefL2R ( READONLY StkIdAttribute : tParsAttribute )
  : BOOLEAN (* It's OK so far. *) 
  (* Disallows reserved Id. *) 

  = BEGIN (*OverrideIdentRefL2R*)
      WITH WScan = StkIdAttribute . Scan
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack 
      DO IF WScan . SaIsReservedId 
        THEN RETURN FALSE 
        ELSE
          PutBwd ( WunRdBack , VAL ( WScan . Position . Column , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( WScan . Position . Line , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( WScan . SaAtom , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkIdRefAtom , LONGINT ) )
        ; RETURN TRUE 
        END (*IF*) 
      END (*WITH*) 
    END OverrideIdentRefL2R

; PROCEDURE CheckQualNotReserved ( READONLY StkIdAttribute : tParsAttribute )
  : BOOLEAN (* It's OK. *) 

  = BEGIN
      IF StkIdAttribute . Scan . SaIsReservedId 
      THEN
        ErrorArr
          ( ARRAY OF REFANY
              { PosImage ( StkIdAttribute . Scan . Position )
              , " Identifier \""
              , FM3SrcToks . Image ( StkIdAttribute . Scan . SaAtom )
              , "\" is predefined and cannot be used"
              , " in a qualified identifier (2.10)."
              } 
          )
      ; RETURN FALSE 
      ELSE RETURN TRUE
      END (*IF*)
    END CheckQualNotReserved 

(*EXPORTED.*)
; PROCEDURE QualIdentL2R
    (  READONLY StkLtIdAttribute , StkRtIdAttribute : tParsAttribute )
  (* Handles either/both idents reserved (error msg). *) 

  = BEGIN (*QualIdentL2R*)
      WITH WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO IF CheckQualNotReserved ( StkLtIdAttribute )
            AND CheckQualNotReserved ( StkRtIdAttribute )
        THEN (* All OK. *) 
          WITH WIdentRefSet = FM3Scopes . DeclScopeStackTopRef ^ . ScpRefIdSet
          DO WIdentRefSet
               := IntSets . Include
                    ( WIdentRefSet , StkLtIdAttribute . Scan . SaAtom )
          END (*WITH*) 
        ; PutBwd
            ( WunRdBack
            , VAL ( StkRtIdAttribute . Scan . Position . Column , LONGINT )
            )
        ; PutBwd
            ( WunRdBack
            , VAL ( StkRtIdAttribute . Scan . Position . Line , LONGINT ) 
            ) 
        ; PutBwd
            ( WunRdBack
            , VAL ( StkLtIdAttribute . Scan . Position . Column , LONGINT )
            ) 
        ; PutBwd
            ( WunRdBack
            , VAL ( StkLtIdAttribute . Scan . Position . Line , LONGINT ) 
            )
        ; PutBwd
            ( WunRdBack , VAL ( StkRtIdAttribute . Scan . SaAtom , LONGINT ) ) 
        ; PutBwd
            ( WunRdBack , VAL ( StkLtIdAttribute . Scan . SaAtom , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkQualIdAtoms , LONGINT ) )
        ELSE 
(* TODO: Error message here? *) 
          PutBwd 
            ( WunRdBack
            , VAL ( StkLtIdAttribute . Scan . Position . Column , LONGINT )
            )
        ; PutBwd 
            ( WunRdBack
            , VAL ( StkLtIdAttribute . Scan . Position . Line , LONGINT )
            )
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkInvalidRef , LONGINT ) )
        END (*IF*)
      END (*WITH*)
    END QualIdentL2R

(*EXPORTED.*)
; PROCEDURE DeclScopeRtL2R ( ScopeRef : FM3Scopes . ScopeRefTyp )
  (* Create an IdAtom-to-declNo, fixed-size dictionary for the scope, of
     exactly the needed size, and load it up with mappings of the idents
     declared in the scope, using a contiguously-numbered range of DeclNos.
  *) 

  = VAR SrtDeclNo : INTEGER 

  ; BEGIN (*DeclScopeRtL2R*)
      VAR LDeclCt : INTEGER
    ; VAR LExpectedToDeclNo : INTEGER 

    ; PROCEDURE SrtVisit ( DeclIdAtomI : INTEGER )
      (* PRE: DeclIdAtomI IN FM3Base . AtomTyp. *)
      = BEGIN
          FM3Dict_Int_Int . InsertFixed
            ( ScopeRef ^ . ScpDeclDict
            , DeclIdAtomI
            , FM3Base . HashNull
            , SrtDeclNo
            )
        ; INC ( SrtDeclNo ) 
        END SrtVisit

    ; BEGIN (* Block. *)
        <* ASSERT ScopeRef = FM3Scopes . DeclScopeStackTopRef *> 
        IF ScopeRef ^ . ScpKind = FM3Scopes . ScopeKindTyp . SkExports
        THEN
(*COMPLETEME: Handle getting exported interfaces here. *) 
        END (*IF*)
      ; LDeclCt := IntSets . Card ( ScopeRef ^ . ScpDeclIdSet )
      (* LDeclCt is exactly the needed dictionary size. *)
      ; ScopeRef ^ . ScpDeclDict 
          := FM3Dict_Int_Int . NewFixed 
               ( LDeclCt , FM3SharedUtils . IntHash )
      ; SrtDeclNo := FM3Units . AllocateDeclNos ( LDeclCt )
      ; LExpectedToDeclNo := SrtDeclNo + LDeclCt 
      ; IntSets . ForAllDo ( ScopeRef ^ . ScpDeclIdSet , SrtVisit )
      ; <*ASSERT SrtDeclNo = LExpectedToDeclNo *> 
        TRY FM3Dict_Int_Int . FinalizeFixed ( ScopeRef ^ . ScpDeclDict )
        EXCEPT FM3Dict_Int_Int . Error ( EMsg )
        => FatalArr
             ( ARRAY OF REFANY
               { "Finalizing Scope at "
               , PosImage ( ScopeRef ^ . ScpPosition )
               , EMsg
               , "." 
               }
             ) 
        END (*EXCEPT*)
      END (*Block*)
    END DeclScopeRtL2R

(* ----------------------- Procedure signatures --------------------- *)


; BEGIN (*FM3Pass2*)
    InitVarInfo ( )
  END FM3Pass1
.

