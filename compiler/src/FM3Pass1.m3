 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
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

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; FROM FM3Base IMPORT tPosition
; IMPORT FM3Builtins 
; IMPORT FM3CLArgs
; IMPORT FM3CLOptions
; IMPORT FM3CLToks AS Clt 
; IMPORT FM3Compile
; IMPORT FM3Compress
; FROM FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int
; IMPORT FM3DisAsm
; IMPORT FM3ExpImp 
; IMPORT FM3RTFailures 
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3IntToks AS Itk; FROM FM3IntToks
    IMPORT LtToRt , LtToPatch , LtToOne , LtToTwo , LtToOnePatch , LtToTwoPatch
           , LtToListSepPatch 
; IMPORT FM3Messages 
; IMPORT FM3Parser
; IMPORT FM3PgToks
; IMPORT FM3Std 
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Units 
; IMPORT FM3Utils 
; IMPORT RdBackFile

; CONST PosImage = FM3Utils . PositionImage

; TYPE Dkt = FM3Decls . DeclKindTyp 
; TYPE Skt = FM3Scopes . ScopeKindTyp 
; TYPE Ukt = FM3Units . UnitKindTyp 

; VAR FileTagVersion := VAL ( ORD ( '1' ) , Byte )  

; CONST ALOSE = FM3Messages . AtomListToOSError

(*TODO: Consistify formal names of PutBwd* procs, including Pos for Position. *)
(*TODO: Move PutBwd procs to a separate package. *)

; PROCEDURE PutBwd ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN
      <* ASSERT RdBack # FM3Globals . P2RdBack *>
      TRY
        FM3Compress . PutBwd ( RdBack , ValueL ) 
      EXCEPT OSError . E ( EMsg )
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to write to readback file \""
               , RdBackFile . FileName ( RdBack )
               , "\", " 
               , ALOSE ( EMsg ) , "."  
               }
           ) 
      END (*EXCEPT*) 
    END PutBwd
(*
(*EXPORTED.*)
; PROCEDURE PutBwdTok ( RdBack : RdBackFile . T ; ValueL : LONGINT )

  = BEGIN (*PutBwdTok*)
      <* ASSERT RdBack ^ . RbCheckArgCt = 0 *>
      RdBack ^ . RbCheckTTok := ValueL
    ; PutBwd ( RdBack , ValueL ) 
    END PutBwdTok
  *)     

(*EXPORTED*) 
; PROCEDURE RunPass1 ( ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; BEGIN (*RunPass1*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; InitPass1 ( LUnitRef )
    ; LUnitRef ^ . UntPassNosDisAsmed := FM3CLOptions . PassNoSetEmpty 
    ; TranslatePass1 ( LUnitRef ) 
    ; FinishPass1 ( LUnitRef ) 
    END RunPass1

; PROCEDURE EnsureBuildDirectory
    ( UnitRef : FM3Units . UnitRefTyp ; SrcFilePath : TEXT )

  = BEGIN (*EnsureBuildDirectory*)
      UnitRef ^ . UntBuildDirPath
        := SrcFilePath & "/" & FM3CLOptions . BuildDirRelPath
    ; TRY
        FS . CreateDirectory ( UnitRef ^ . UntBuildDirPath )
      EXCEPT
      | OSError . E ( EAtoms ) 
      => IF EAtoms . tail = NIL
            AND EAtoms . head # NIL
            AND Atom . ToText ( EAtoms . head ) # NIL
            AND Text . Equal ( Atom . ToText ( EAtoms . head ) , "errno=17" )
(* TODO: There has to be a more graceful (and OS-independent) way to detect
         this, but it looks like libm3 is letting us down here.
*) 
         THEN (* The directory already exists. We expect this sometimes. *)
           EVAL EAtoms (* Debug. *)  
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

(*EXPORTED*) 
; PROCEDURE DisAsmPass1 ( UnitRef : FM3Units . UnitRefTyp )
  RAISES { RdBackFile . BOF }

  = BEGIN (*DisAsmPass1*)
      IF NOT FM3CLOptions . PassNo1 IN UnitRef ^ . UntPassNosDisAsmed 
      THEN (* Disassembly file is not already written. *) 
        FM3Compile . DisAsmPassFile
          ( UnitRef , FM3Globals . Pass1OutSuffix , L2R := FALSE )
      ; FM3CLOptions . InclPassNo
          ( UnitRef ^ . UntPassNosDisAsmed , FM3CLOptions . PassNo1 ) 
      END (*IF*) 
    END DisAsmPass1

; CONST UnitLogSuffix = ".log" 

(*EXPORTED.*)
; PROCEDURE InitPass1 ( UnitRef : FM3Units . UnitRefTyp ) 

  = VAR LFullFileName : TEXT
  ; VAR LFullPass1OutName : TEXT 
  ; VAR LFullPatchStackName : TEXT 
  ; VAR LUnitLogFullName : TEXT 

  ; BEGIN (*InitPass1*)
  
    (* Create the build directory: *)
(* FIXME: FM3CLArgs wants a build directory to put a log file in, even before
          we get here.  Is this the right place for it?
*)
      EnsureBuildDirectory ( UnitRef , UnitRef ^ . UntSrcFilePath ) 

    (* Create the unit log output file. A pure text file. *)
    ; UnitRef ^ . UntLogSimpleName
        := Pathname . Join
             ( NIL
             , UnitRef ^ . UntSrcFileSimpleName
             , FM3Globals . UnitLogSuffix
             ) 
    ; LUnitLogFullName
        := Pathname . Join
             ( UnitRef ^ . UntSrcFilePath , UnitRef ^ . UntLogSimpleName , NIL )
    ; IF Clt . CltUnitLog IN FM3CLOptions . OptionTokSet
      THEN 
        TRY UnitRef ^ . UntLogWrT := FileWr . Open ( LUnitLogFullName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => <*FATAL Thread . Alerted , Wr . Failure *>
           BEGIN
             Wr . PutText ( Stdio . stderr , "Unable to open unit log file " ) 
           ; Wr . PutText ( Stdio . stderr , UnitRef ^ . UntLogSimpleName ) 
           ; Wr . PutText ( Stdio . stderr , ": " ) 
           ; Wr . PutText
               ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
           ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
           ; Wr . PutText ( Stdio . stderr , "Will proceed without it." ) 
           ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
           ; Wr . Flush ( Stdio . stderr )
           END (*Block.*) 
        ; UnitRef ^ . UntLogWrT := NIL
        ELSE (* Remove any leftover unit log file. *) 
          FM3SharedUtils . DeleteFile ( LUnitLogFullName ) 
        END (*IF*) 
      END (*EXCEPT*)
    ; FM3Messages . SetUnitLog ( UnitRef ^ . UntLogWrT )

    (* Create build files for the pass. *) 
    ; UnitRef ^ . UntPass1OutSimpleName
        := Pathname . Join
             ( NIL
             , UnitRef ^ . UntSrcFileSimpleName
             , FM3Globals . Pass1OutSuffix
             )
    ; UnitRef ^ . UntPatchStackSimpleName
        := Pathname . Join
             ( NIL
             , UnitRef ^ . UntSrcFileSimpleName
             , FM3Globals . PatchStackSuffix
             )
    ; TRY (*EXCEPT*)
        (* Heh, heh.  Code the exception handler only once for both files. *) 
        LFullPass1OutName
          := Pathname . Join
               ( UnitRef ^ . UntBuildDirPath 
               , UnitRef ^ . UntPass1OutSimpleName
               , NIL
               )
      ; LFullFileName :=  LFullPass1OutName 
      ; UnitRef ^ . UntPass1OutRdBack
          := RdBackFile . Create ( LFullPass1OutName ) 
      ; FM3Globals . P1RdBack := UnitRef ^ . UntPass1OutRdBack
        (* ^Cache for faster access. *)

      ; LFullPatchStackName
          := Pathname . Join
               ( UnitRef ^ . UntBuildDirPath 
               , UnitRef ^ . UntPatchStackSimpleName
               , NIL
               ) 
      ; LFullFileName :=  LFullPatchStackName 
      ; UnitRef ^ . UntPatchStackRdBack
          := RdBackFile . Create ( LFullPatchStackName )
      ; FM3Globals . PatchRdBack := UnitRef ^ . UntPatchStackRdBack 
        (* ^Cache for faster access. *) 
      
      EXCEPT
      | OSError . E ( EMsg ) 
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to create build file \""
               , LFullFileName
               , "\": "
               , ALOSE ( EMsg)
               , "."
               } 
           ) 
      END (*EXCEPT*)

    (* Initialize the readback files. *)

    ; PutBwd
        ( UnitRef ^ . UntPass1OutRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwd
        ( UnitRef ^ . UntPass1OutRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; UnitRef ^ . UntPass1OutEmptyCoord
        := RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack )
    ; UnitRef ^ . UntMaxPass1OutLength
        := UnitRef ^ . UntPass1OutEmptyCoord
        
    (* Write initial tokens to output files. *) 

    ; PutBwd
        ( UnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkBOF , LONGINT ) )
    ; PutBwd
        ( UnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; UnitRef ^ . UntPatchStackEmptyCoord
        := RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack )
    ; UnitRef ^ . UntMaxPatchStackDepth
        := UnitRef ^ . UntPatchStackEmptyCoord
    ; UnitRef . UntPatchStackTopCoord := UnitRef . UntPass1OutEmptyCoord
    ; PutBwd
        ( UnitRef ^ . UntPatchStackRdBack
        , FM3Globals . PatchStackEmptySentinel
        )
      (* ^A sentinel for when the patch stack is empty of actual tokens. 
         It's a pseudo patch-coordinate atop the initial non-existent token
      *)

    (* Create unit data structures. *)
    
(* Check: Do we really need separate atom dictionaries for identifiers, *) 

    (* Initialize Scanner for unit. *)
      
    ; FM3Scanner . PushState ( UnitRef ^ . UntSrcUniRd , UnitRef )
(* CHECK: ? *)

    END InitPass1

; PROCEDURE TranslatePass1 ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN (*TranslatePass1*)
      TRY

      (* Run the translation part of pass 1. *)
        UnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *)

      (* Write final successful Pass 1 output file tokens. *)
      ; UnitRef ^ . UntPass1OutDataLength
          := RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack )
        (* ^Not including the boilerplate stuff below. *) 
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkRightEnd , LONGINT )
          )
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      ; RdBackFile . Flush ( UnitRef ^ . UntPass1OutRdBack ) 
      
      ; FM3Parser . CloseFM3Parser ( )
(*TODO ^ Do this sometime later? *)

      (* Prepare for possible disassembly later. *)
(* ** Not needed after rework of RdBackFile. **
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass1OutSuffix
          , UnitRef ^ . UntPass1OutRdBack
          )
        (*^ This copy may be used by disassembly called for by command-line
            option, a later pass failure, or not at all. *)
** *)

      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE

      (* Writing of pass 1 output file failed. *)
        PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkRightEndIncomplete , LONGINT )
          )

      ; RdBackFile . Flush ( UnitRef ^ . UntPass1OutRdBack ) 
(* ** Not needed after rework of RdBackFile. **
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass1OutSuffix
          , UnitRef ^ . UntPass1OutRdBack
          )
        (* ^This copy will be used immediately to disassemble
            what there is of the failed file. *)
** *)
      ; DisAsmPass1 ( UnitRef )

      ; FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure writing pass 1 output file at length "
               , Fmt . LongInt
                   ( RdBackFile . LengthL ( UnitRef ^ . UntPass1OutRdBack ) )
               , FM3Messages . NLIndent 
               , "Exception "
               , FM3RTFailures . ExcNameFromAddr
                   ( Compiler . ThisException ( ) )
               , ","
               , FM3Messages . NLIndent 
               , "raised at "
               , FM3RTFailures . ActivationLocationFromAddr
                   ( Compiler . ThisException ( ) )
               , "."
               }
           )
      END (*EXCEPT *)
    END TranslatePass1

(*EXPORTED.*)
; PROCEDURE FinishPass1 ( UnitRef : FM3Units . UnitRefTyp ) 

  = VAR LPass1LengthL : LONGINT
  ; VAR LLengthL : LONGINT
  ; VAR LPass1FullFileName , LPass1FullCopyName : TEXT 
  ; VAR LExceptionName , LExceptionLoc : TEXT 

  ; BEGIN (*FinishPass1*)

    (* Report size and maybe disassemble pass 1 output file. *) 
      LPass1FullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPass1OutSimpleName
             , NIL
             )

    ; UnitRef ^ . UntMaxPass1OutLength
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPass1OutRdBack )
    ; FM3Messages . InfoArr
        ( ARRAY OF REFANY
            { "Pass 1 output file "
            , UnitRef ^ . UntPass1OutSimpleName
            , " has "
            , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPass1OutLength )
            , " bytes."
            } 
        )
    ; IF FM3CLOptions . PassNo1 IN FM3CLOptions . PassNosToDisAsm 
      THEN DisAsmPass1 ( UnitRef )
      END (*IF*)

(* Close source file. *) 

(* TODO: display code point counts. *)

    END FinishPass1 
    
(*
; PROCEDURE UnitId 
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdScanAttr : FM3Scanner . tScanAttribute 
    ; VAR (*OUT*) NameFromFileName : TEXT 
    )

  = BEGIN (* UnitId *)
      NameFromFileName := NIL 
    ; IF UnitRef # NIL
      THEN 
        UnitRef ^ . UntUnitIdentPos := IdScanAttr . Position 
      ; UnitRef ^ . UntUnitIdent := IdText 
      ; IF UnitRef ^ . UntSrcFileSimpleName # NIL
        THEN
          NameFromFileName
            := FM3Files . RemoveSuffix ( UnitRef ^ . UntSrcFileSimpleName )
        END (*IF*) 
      END (*IF*) 
    END UnitId 

; PROCEDURE NameFromFileName 
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdScanAttr : FM3Scanner . tScanAttribute 
    )
  : REF ARRAY OF CHAR
  (* If the unit name has a suffix of the form .c*, where c is any non-dot,
     remove it.  NIL if no unit name exists. 
  *) 

  = VAR LOACharsRef : REF ARRAY OF CHAR
  l VAR LResult : REF ARRAY OF CHAR
  ; VAR LNumber : INTEGER
  ; VAR LSs : INTEGER 

  ; BEGIN (* UnitId *)
      IF UnitRef = NIL THEN RETURN NIL END (*IF*) 
    ; LOACharsRef := UnitRef ^ . UntUnitIdentOA
    ; IF LOACharsRef = NIL THEN RETURN NIL END (*IF*)
    ; LNumber := NUMBER ( LOACharsRef ^ )
    ; LSs := LNumber - 1
    ; LOOP
        IF LSs < 0 THEN RETURN LOACharsRef 
        ELSIF LOACharsRef ^ [ LSs ] = '.' 
        THEN
          LResult := NEW ( REF ARRAY OF CHAR , LSs )
        ; LResult ^ := SUBARRAY ( LOACharsRef , 0 , LSs )
        ; RETURN LResult 
        ELSE DEC ( LSs ) 
      END (*LOOP*) 
    END NameFromFileName
*) 

; PROCEDURE UnitNameTFromFileName 
    ( UnitRef : FM3Units . UnitRefTyp ) : TEXT 
  (*PRE: UnitRef # NIL *) 
  (* If the unit name has a suffix of the form .c*, where c is any non-dot,
     remove it.  NIL if no unit name exists. 
  *) 

  = VAR LFileNameT : TEXT
  ; VAR LBase : TEXT 

  ; BEGIN (* UnitNameTFromFileName *)
      LFileNameT := UnitRef ^ . UntSrcFileSimpleName 
    ; IF LFileNameT = NIL THEN RETURN NIL END (*IF*)
    ; LBase := Pathname . Base ( LFileNameT )
    ; RETURN LBase 
    END UnitNameTFromFileName 

(*EXPORTED:*)
; PROCEDURE InterfaceId
    ( UnitRef : FM3Units . UnitRefTyp
    ; READONLY IdScanAttr : FM3Scanner . tScanAttribute 
    )
  (*PRE: UnitRef # NIL *) 

  = VAR LNameFromFileName : TEXT

  ; BEGIN (* InterfaceId *)
      UnitRef ^ . UntUnitIdent := IdScanAttr . SaChars 
    ; UnitRef ^ . UntUnitIdentPos := IdScanAttr . Position
    ; <* ASSERT UnitRef ^ . UntStdTok = IdScanAttr . SaBuiltinTok
         , "Std unit Toks for filename and interface name disagree."
      *>
      IF UnitRef ^ . UntUnitIdent = NIL THEN RETURN END (*IF*) 
    ; LNameFromFileName := UnitNameTFromFileName ( UnitRef ) 
    ; IF LNameFromFileName = NIL THEN RETURN END (*IF*) 
    ; IF NOT Text . Equal
             ( Text . FromChars ( UnitRef ^ . UntUnitIdent ^ )
             , LNameFromFileName
             ) 
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Interface name \""
              , UnitRef ^ . UntUnitIdent 
              , "\" does not agree with file name \""
              , UnitRef ^ . UntSrcFileSimpleName
              , "\" (FM3 requirement)." 
              }
          , IdScanAttr . Position
          )
      END (*IF*)
    END InterfaceId 

(*EXPORTED:*)
; PROCEDURE ModuleId
    ( UnitRef : FM3Units . UnitRefTyp
    ; READONLY IdScanAttr : FM3Scanner . tScanAttribute 
    )
  (*PRE: UnitRef # NIL *) 

  = VAR LNameFromFileName : TEXT

  ; BEGIN (* ModuleId *) 
      UnitRef ^ . UntUnitIdent := IdScanAttr . SaChars 
    ; UnitRef ^ . UntUnitIdentPos := IdScanAttr . Position
    ; IF UnitRef ^ . UntUnitIdent = NIL THEN RETURN END (*IF*) 
    ; LNameFromFileName := UnitNameTFromFileName ( UnitRef ) 
    ; IF LNameFromFileName = NIL THEN RETURN END (*IF*) 
    ; IF NOT Text . Equal
               ( Text . FromChars ( UnitRef ^ . UntUnitIdent ^ )
               , LNameFromFileName
               ) 
      THEN
        FM3Messages . InfoArr
          ( ARRAY OF REFANY
              { "Module name \""
              , UnitRef ^ . UntUnitIdent  
              , "\" is not consistent with file name \""
              , UnitRef ^ . UntSrcFileSimpleName
              , "\"." 
              }
         , IdScanAttr . Position 
         ) 
      END (*IF*)
    END ModuleId 

(*EXPORTED:*)
; PROCEDURE CheckUnitFinalId
    ( UnitRef : FM3Units . UnitRefTyp
    ; READONLY EndIdScanAttr : FM3Scanner . tScanAttribute 
    ; UnitKind : FM3Units . UnitKindTyp
    )
    
  = BEGIN (* CheckUnitFinalId *)
      IF UnitRef = NIL THEN RETURN END (*IF*) 
    ; IF UnitRef ^ . UntUnitIdent = NIL THEN RETURN END (*IF*) 
    ; IF EndIdScanAttr . SaChars = NIL THEN RETURN END (*IF*)
    ; IF EndIdScanAttr . SaChars ^ # UnitRef ^ . UntUnitIdent ^   
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY 
              { "Identifier \""
              , EndIdScanAttr . SaChars 
              , "\" at end of "
              , FM3Units . UnitKindImage ( UnitKind )
              , " named \""
              , UnitRef ^ . UntUnitIdent 
              , "\", at " 
              , PosImage ( UnitRef ^ . UntUnitIdentPos )  
              , ", must repeat its name ("
              , FM3Units . UnitKindSectionNo ( UnitKind )
              , ")." 
              } 
          , EndIdScanAttr . Position
          )
      END (*IF*) 
    END CheckUnitFinalId

(*EXPORTED:*)
; PROCEDURE CheckStdUnitPragma ( UnitRef : FM3Units . UnitRefTyp )
    
  = BEGIN (* CheckStdUnitPragma *)
      IF UnitRef = NIL THEN RETURN END (*IF*)
    ; IF UnitRef ^ . UntKind # Ukt . UkInterface THEN RETURN END (*IF*)
    ; <* ASSERT UnitRef ^ . UntHasStdUnitPragma
                = ( UnitRef ^ . UntStdTok # FM3Base . TokNull ) 
      , "HasStdUnitPragma disagrees with UntStdTok for "
        & UnitRef ^ . UntSrcFileSimpleName
      *> 
    END CheckStdUnitPragma

(* ------------------------ Pass 1 output file --------------------- *) 

(*EXPORTED:*)
; PROCEDURE Coord ( ) : LONGINT
  (* Current coordinate of the current unit. *)
  
  = BEGIN
      RETURN RdBackFile . LengthL
               ( FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack ) 
    END Coord

(*EXPORTED:*)
; PROCEDURE PutBwd_TextLit ( VAR(*READONLY*) ParsAttr : tParsAttribute )

  = VAR LNumber : INTEGER

  ; BEGIN (*PutBwd_TextLit*)
      LNumber := NUMBER ( ParsAttr . Scan . SaChars ^ )
    ; WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Column , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Line , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . SaAtom , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . SaTok , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( Itk . ItkLiteral , LONGINT ) )
(* Don't put the chars in the stream.  An atom arg instead.
      ; PutBwd ( WRdBack , VAL ( LNumber , LONGINT ) )
      ; FOR RI := LNumber - 1 TO 0 BY - 1 (* LM Char near EOF. *) 
        DO
          PutBwd
            ( WRdBack
            , VAL ( ORD ( ParsAttr . Scan . SaChars ^ [ RI ] ) , LONGINT )
            )
        END (*FOR*) 
      ; PutBwd ( WRdBack , VAL ( LNumber , LONGINT ) )

      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Column , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Line , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . SaAtom , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( Itk . ItkTextLitRt , LONGINT ) )
*)
      END (*WITH*) 
    END PutBwd_TextLit 
    
(*EXPORTED:*)
; PROCEDURE PutBwd_WideTextLit ( VAR(*READONLY*) ParsAttr : tParsAttribute )

  = VAR LNumber : INTEGER

  ; BEGIN (*PutBwd_WideTextLit*)
      LNumber := NUMBER ( ParsAttr . Scan . SaWideChars ^ )
    ; WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Column , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Line , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . SaAtom , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . SaTok , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( Itk . ItkLiteral , LONGINT ) )
          
(* Don't put the chars in the stream.  An atom arg instead. 
      ; PutBwd ( WRdBack , VAL ( LNumber , LONGINT ) )
      ; FOR RI := LNumber - 1 TO 0 BY - 1 (* LM Char near EOF. *)
        DO
          PutBwd
            ( WRdBack
            , VAL ( ORD ( ParsAttr . Scan . SaWideChars ^ [ RI ] ) , LONGINT )
            ) 
        END (*FOR*) 
      ; PutBwd ( WRdBack , VAL ( LNumber , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Column , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . Position . Line , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( ParsAttr . Scan . SaAtom , LONGINT ) )
      ; PutBwd
          ( WRdBack , VAL ( Itk . ItkWideTextLitRt , LONGINT ) )
*)
      END (*WITH*) 
    END PutBwd_WideTextLit 
    
(*EXPORTED:*)
; PROCEDURE PutBwd_Attribute ( READONLY ParsAttr : tParsAttribute )
(* TODO: Probably phase this out. *) 
   
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
; PROCEDURE PutBwd_LNP
    ( T : Itk . TokTyp ; N : LONGINT ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , N ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LNP

(*EXPORTED:*)
; PROCEDURE PutBwd_LNNP
    ( T : Itk . TokTyp ; N1 , N2 : LONGINT ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , N2 ) 
      ; PutBwd ( WRdBack , N1 ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LNNP

(*EXPORTED:*)
; PROCEDURE PutBwd_LINP
    ( T : Itk . TokTyp
    ; I : INTEGER
    ; N : LONGINT
    ; READONLY Position : tPosition
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , N ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LINP

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
; PROCEDURE PutBwd_TI ( T : Itk . TokTyp ; I : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_TI

(*EXPORTED:*)
; PROCEDURE PutBwd_TIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_TIP

(*EXPORTED:*)
; PROCEDURE PutBwd_LIIP
    ( T : Itk . TokTyp ; I1 , I2 : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LIIP

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
; PROCEDURE PutBwd_EP ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOne , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_EP

(*EXPORTED:*)
; PROCEDURE PutBwd_ZP ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToTwo , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_ZP

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
; PROCEDURE PutBwd_LCBP_rbp
   ( T : Itk . TokTyp
   ; C : LONGINT
   ; B : BOOLEAN 
   ; READONLY Position : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ORD ( B ) , LONGINT ) )  
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )  
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ORD ( B ) , LONGINT ) )   
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCBP_rbp

(*EXPORTED:*)
; PROCEDURE PutBwd_LCBP_eCP_rbP
   ( T : Itk . TokTyp
   ; C1 : LONGINT
   ; Be : BOOLEAN 
   ; READONLY Position : tPosition
   ; C2 : LONGINT
   ; READONLY Position1 : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionRt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionRt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ORD ( Be ) , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( Position1 . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position1 . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C2 ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) )
      ; PutBwd ( WRdBack , VAL ( ORD ( Be ) , LONGINT ) ) 
      ; PutBwd ( WRdBack , C1 ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCBP_eCP_rbP

(*EXPORTED:*)
; PROCEDURE PutBwd_LCPI_rpi
(* REVIEW Why the rpi order? why not rip? *) 
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
; PROCEDURE PutBwd_LCIP
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
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP

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
;
(*EXPORTED:*)
 PROCEDURE PutBwd_LCIP_eCp_rip
    ( T : Itk . TokTyp 
    ; CLt : LONGINT 
    ; I : INTEGER 
    ; READONLY Position : tPosition
    ; COne : LONGINT
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
      ; PutBwd ( WRdBack , COne ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP_eCp_rip

(*EXPORTED:*)
; PROCEDURE PutBwd_LCIP_eCP_rip
    ( T : Itk . TokTyp 
    ; CLt : LONGINT 
    ; I : INTEGER 
    ; READONLY PositionLt : tPosition
    ; COne : LONGINT
    ; READONLY PositionOne : tPosition 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionLt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionLt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( PositionOne . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionOne . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , COne ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( PositionLt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionLt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP_eCP_rip

(*EXPORTED:*)
; PROCEDURE PutBwd_LCIIP_riip
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I1 , I2 : INTEGER 
    ; READONLY Position : tPosition 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIIP_riip

(*EXPORTED:*)
; PROCEDURE PutBwd_LCNP_rnp
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; N : LONGINT 
    ; READONLY Position : tPosition 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , N ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , N ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCNP_rnp

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
; PROCEDURE PutBwd_LCIP_eCip_rip
    ( T : Itk . TokTyp 
    ; LC : LONGINT 
    ; I : INTEGER 
    ; READONLY LPos : tPosition
    ; EC : LONGINT 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( LPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( LPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( LPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( LPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , EC ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( LPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( LPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , LC ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP_eCip_rip

(*EXPORTED:*)
; PROCEDURE PutBwd_LCIP_eCiP_rip
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
    END PutBwd_LCIP_eCiP_rip

(*EXPORTED:*)
; PROCEDURE PutBwd_TP ( T : Itk . TokTyp ; READONLY P : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( P . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( P. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) )
      END (*WITH*) 
    END PutBwd_TP

(*EXPORTED:*)
; PROCEDURE PutBwd_TBP
    ( T : Itk . TokTyp ; B : BOOLEAN ; READONLY P : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( P . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( P. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ORD ( B ) , LONGINT ) )
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) )
      END (*WITH*) 
    END PutBwd_TBP

(*EXPORTED:*)
; PROCEDURE PutBwd_TCP
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY P : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( P . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( P. Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) )
      END (*WITH*) 
    END PutBwd_TCP

(*EXPORTED:*)
; PROCEDURE PutBwd_LCP_eCP_rP
   ( T : Itk . TokTyp
   ; LC : LONGINT
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
      ; PutBwd ( WRdBack , LC ) 
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

; PROCEDURE PutBwd_LCP_eCPB_zCPB_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; READONLY PL : tPosition
   ; Ce : LONGINT
   ; READONLY Pe : tPosition
   ; Be : BOOLEAN 
   ; Cz : LONGINT
   ; READONLY Pz : tPosition
   ; Bz : BOOLEAN 
   ; READONLY Pr : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Pr . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Pr . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( L + LtToRt , LONGINT ) )

      ; PutBwd ( WRdBack , VAL ( ORD ( Bz ) , LONGINT ) ) 
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
    END PutBwd_LCP_eCPB_zCPB_rP 

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
; PROCEDURE PutBwd_ECP
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_ECP

(*EXPORTED:*)
; PROCEDURE PutBwd_ZCP
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToTwoPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_ZCP

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
; PROCEDURE PutBwd_LC_eC_r ( T : Itk . TokTyp ; Ct , Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) )
      END (*WITH*) 
    END PutBwd_LC_eC_r

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
        TRY 
          EVAL GetBwd ( WRdBack ) 
        ; EVAL GetBwd ( WRdBack ) 
        ; EVAL GetBwd ( WRdBack )
        ; EVAL GetBwd ( WRdBack )
        EXCEPT RdBackFile . BOF
        => FM3Messages . FatalArr
            ( ARRAY OF REFANY
                { "Pass 1 output file popped when already empty. " } 
            , FM3Scanner . Attribute . Position 
            ) 
        END (*EXCEPT*) 
      END (*WITH*) 
    END Pop4
      
(*EXPORTED:*)
; PROCEDURE Pop8 ( )

  = BEGIN (*Pop4*)
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        TRY 
          EVAL GetBwd ( WRdBack ) 
        ; EVAL GetBwd ( WRdBack ) 
        ; EVAL GetBwd ( WRdBack )
        ; EVAL GetBwd ( WRdBack ) 
        ; EVAL GetBwd ( WRdBack )
        ; EVAL GetBwd ( WRdBack ) 
        ; EVAL GetBwd ( WRdBack )
        ; EVAL GetBwd ( WRdBack ) 
        EXCEPT RdBackFile . BOF
        => FM3Messages . FatalArr
            ( ARRAY OF REFANY
                { "Pass 1 output file popped when already empty. " } 
            , FM3Scanner . Attribute . Position 
            ) 
        END (*EXCEPT*) 
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

; PROCEDURE SkipFrom ( Coord : LONGINT )

  = BEGIN 
      PutBwd_LCI_ri ( Itk . ItkSkipLt , Coord , FM3Globals . NextSkipNo ) 
    ; INC ( FM3Globals . NextSkipNo )
    END SkipFrom 
 
(*EXPORTED.*)
; PROCEDURE RequireTypeAndOrValue 
    ( READONLY Position : tPosition
    ; HasType : BOOLEAN 
    ; HasValue : BOOLEAN
    )
  : BOOLEAN (* OK *) 

  (* Anything that requires a type and/or value: variable , formal, field. 
     Check and maybe emit message. 
     Gets DeclKind from FM3Decls.TopDeclParseInfo. 
  *) 

  = VAR LValueTag : TEXT

  ; BEGIN 
     IF NOT HasType AND NOT HasValue 
     THEN
       WITH WDeclParseInfo = FM3Decls . TopDeclParseInfo ( )
       DO
         IF WDeclParseInfo . DiKind = Dkt . DkVar 
         THEN LValueTag := "an initial value "
         ELSE LValueTag := "a default "
         END (*IF*) 
       ; FM3Messages . ErrorArr
           ( ARRAY OF REFANY 
               { VarLabel [ WDeclParseInfo . DiKind ] 
               , " must have a type and/or "
               , LValueTag 
               , VarSection [ WDeclParseInfo . DiKind  ]
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
    ; PutBwd_LCIP_rip
        ( TokLt
        , ElemsAttr . PaPass1Coord
        , ElemsAttr . PaInt
        , Position
        ) 
    END MakeListPos 

(* REMOVEME:*) 
(*EXPORTED:*)
; PROCEDURE ImportsLt (  )

  = BEGIN (*ImportsLt*)
    END ImportsLt
      
(* REMOVEME:*) 
(*EXPORTED:*)
; PROCEDURE ImportsRt (  )

  = BEGIN (*ImportsRt*)
    END ImportsRt
      
(* REMOVEME:*) 
(*EXPORTED:*)
; PROCEDURE Import
    ( Atom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ) 

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
; PROCEDURE BeginBlock ( ) : FM3Globals . ScopeNoTyp (* Created. *) 

  = BEGIN (*BeginBlock*)
    END BeginBlock

(*EXPORTED:*)
; PROCEDURE EndBlock ( )

  = BEGIN (*EndBlock*)
    END EndBlock

(* Insert tok *) 

(* ----------------------------- Scopes ---------------------------- *)

(* These are called by the parser: *) 

; PROCEDURE ScopeEmpty 
    ( ScopeKind : FM3Scopes . ScopeKindTyp ; Position : FM3Base . tPosition )
  : FM3Scopes . ScopeRefTyp 

  = BEGIN (*ScopeEmpty*)
      RETURN
        FM3Scopes . NewScopeRef
          ( FM3Units . UnitStackTopRef , ScopeKind , Position ) 
    END ScopeEmpty
    
(*EXPORTED.*)
; PROCEDURE ProcBodyFormalsScope ( ScopeRef : FM3Scopes . ScopeRefTyp )
    : FM3Scopes . ScopeRefTyp
    (* NIL if ScopeRef is not a proc body scope.  May assert fail if
       it's not valid.  Else the formals scope of the proc body.
    *) 

  = BEGIN
      IF ScopeRef = NIL THEN RETURN NIL END (*IF*) 
    ; IF ScopeRef ^ . ScpKind # Skt . SkProcBody 
      THEN
        IF ScopeRef ^ . ScpFormalsScopeRef # NIL
        THEN <* ASSERT FALSE , "Non-proc-body scope has formals scope."*>
        ELSE RETURN NIL 
        END (*IF*) 
      END (*IF*) 
    ; IF ScopeRef ^ . ScpFormalsScopeRef = NIL 
      THEN <* ASSERT FALSE , "Proc body scope has no formals scope."*> 
      END (*IF*) 
    ; IF ScopeRef ^ . ScpFormalsScopeRef ^ . ScpKind # Skt . SkFormals 
      THEN <* ASSERT FALSE , "Formals scope of proc body has wrong kind."*> 
      END (*IF*)
    ; RETURN ScopeRef ^ . ScpFormalsScopeRef 
    END ProcBodyFormalsScope
 
(* Left-to-right scope handling.  These are called by the parser. *)

; PROCEDURE AtomOfStdId
    ( READONLY IdAttr : tParsAttribute ) : FM3Base . AtomTyp

  = BEGIN
      RETURN FM3Atom_OAChars . MakeAtom 
                ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                , IdAttr . Scan . SaChars 
                , IdAttr . Scan . SaHash 
                ) 
    END AtomOfStdId

(*EXPORTED.*)
; PROCEDURE VerifyIdentNotReserved
    ( READONLY IdAttr : tParsAttribute
    ; Position : tPosition 
    ; IllegalPastParticiple : TEXT
    )
  : BOOLEAN (* It's OK. *)
  (* POST: FALSE result => Error message has been generated. *)  

  = BEGIN (*VerifyIdentNotReserved*)
      IF IntSets . IsElement
           ( IdAttr . Scan . SaBuiltinTok , FM3Std . ReservedIdSet ) 
      THEN 
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Reserved identifier \""
              , FM3SrcToks . Image ( IdAttr . Scan . SaBuiltinTok )
              , "\" cannot "
              , IllegalPastParticiple 
              , " (2.8.2)." 
              }
          , Position 
          )
      ; RETURN FALSE (* Not OK. *)
      ELSE RETURN TRUE (* It's OK *) 
      END (*IF*) 
    END VerifyIdentNotReserved

(*EXPORTED.*)
; PROCEDURE DeclIdL2R 
    ( DeclKind : Dkt 
    ; READONLY IdAttr : tParsAttribute
    ; SepTok : Itk . TokTyp := Itk . ItkNull
                            (* ^Implies single decl id, not in a list. *)  
    ; READONLY SepPosition : tPosition := FM3Base . PositionNull 
    ; PriorIdCt : INTEGER := 0 (* Number of ids to left of this one. *)
    )
  : BOOLEAN (* Use this declared id.  (It's not reserved and not a duplicate
               in current scope.)
            *)
  (* PRE: IdAttr is for an identifier in a declaring context. *) 

  = VAR LFormalsScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LPriorDeclScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LAtom : FM3Base . AtomTyp

  ; BEGIN (*DeclIdL2R*)
      IF NOT VerifyIdentNotReserved
               ( IdAttr , IdAttr . Scan . Position , "be declared" )
      THEN (* Reserved Ident. Message already emitted. *)
        RETURN FALSE
      END (*IF*) 
    ; LAtom := IdAttr . Scan . SaAtom 
    ; WITH WDeclScopeRef = FM3Scopes . DeclScopeStackTopRef 
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack 
      DO 
        IF WDeclScopeRef . ScpOwningUnitRef = FM3Units . UnitStackTopRef 
           AND NOT FM3ExpImp . CheckDuplicateExpImp
                     ( FM3Units . UnitStackTopRef
                     , LAtom
                     , IdAttr . Scan . Position
                     , "declaration"
                     )
        THEN (* LAtom duplicates export or import. Message already emitted. *)
          RETURN FALSE 
        ELSE
          LFormalsScopeRef := ProcBodyFormalsScope ( WDeclScopeRef ) 
        ; IF LFormalsScopeRef # NIL (* WDeclScopeRef is for a proc body. *) 
             AND IntSets . IsElement
                   ( LAtom , LFormalsScopeRef ^ . ScpDeclIdSet )
          THEN  (* LAtom duplicates a previously declared formal. *)
            LPriorDeclScopeRef := LFormalsScopeRef
          ELSIF IntSets . IsElement
                  ( LAtom , WDeclScopeRef ^ . ScpDeclIdSet )
          THEN (* LAtom duplicates a previously declared id in this scope. *) 
            LPriorDeclScopeRef := WDeclScopeRef
          ELSE 
            LPriorDeclScopeRef := NIL
          END (*IF*)

        ; IF LPriorDeclScopeRef # NIL  
          THEN (* Write a duplicate Ident token.  The only effect will be to
                  emit an error later, during Pass2, when the position of the
                  original declaring occurence is known.
               *) 
            PutBwd
              ( WunRdBack
              , VAL ( IdAttr . Scan . Position . Column , LONGINT )
              ) 
          ; PutBwd
              ( WunRdBack
              , VAL ( IdAttr . Scan . Position . Line , LONGINT )
              )
          ; PutBwd
              ( WunRdBack
              , VAL ( LPriorDeclScopeRef . ScpSelfScopeNo , LONGINT )
              ) 
          ; PutBwd ( WunRdBack , VAL ( LAtom , LONGINT ) ) 
          ; PutBwd ( WunRdBack , VAL ( Itk . ItkDuplDeclId , LONGINT ) )
          ; RETURN FALSE (* Caller, Don't use this Id. *)
          ELSE (* 1st declaration of Ident in scope(s) . *) 
            WDeclScopeRef . ScpDeclIdSet
              := IntSets . Include ( WDeclScopeRef . ScpDeclIdSet , LAtom )

          (* Maybe write Separator token: *)
          ; IF SepTok # Itk . ItkNull AND PriorIdCt > 0
            THEN 
              PutBwd ( WunRdBack , VAL ( SepPosition . Column , LONGINT ) ) 
            ; PutBwd ( WunRdBack , VAL ( SepPosition . Line , LONGINT ) )
            ; PutBwd ( WunRdBack , VAL ( PriorIdCt , LONGINT ) )
            ; PutBwd ( WunRdBack , VAL ( SepTok , LONGINT ) )
            END (*IF*)

          (* Id is valid.  Write decl Ident token: *)
          ; PutBwd
              ( WunRdBack 
              , VAL ( IdAttr . Scan . Position . Column , LONGINT ) 
              ) 
          ; PutBwd
              ( WunRdBack 
              , VAL ( IdAttr . Scan . Position . Line , LONGINT ) 
              )
          ; PutBwd 
              ( WunRdBack , VAL ( IdAttr . Scan . SaBuiltinTok , LONGINT ) ) 
          ; PutBwd ( WunRdBack , VAL ( LAtom , LONGINT ) ) 
          ; PutBwd ( WunRdBack , VAL ( ORD ( DeclKind ) , LONGINT ) )
          ; PutBwd ( WunRdBack , VAL ( Itk . ItkDeclId , LONGINT ) )
          ; RETURN TRUE (* Caller, Use this decl id. *)
          END (*IF*)
        END (*IF*)
      END (*WITH*) 
    END DeclIdL2R

(*EXPORTED.*)
; PROCEDURE IdentRefLone ( READONLY IdAttr : tParsAttribute )
  (* Possibly is a reserved Id, possibly legally. *) 
  (* PRE: The ident is not followed by actuals, subscripte, or a deref. *) 

  = VAR LReqdActualsCt : INTEGER 

  ; BEGIN (*IdentRefLone*)
      WITH WScan = IdAttr . Scan
      DO IF WScan . SaAtom = FM3Base . AtomNull 
        THEN (* Reserved Ident. *)
          LReqdActualsCt := BuiltinActualCt ( WScan . SaBuiltinTok ) 
        ; IF LReqdActualsCt > 0
             OR LReqdActualsCt = FM3Builtins . ActualsCtAtLeastOne 
          THEN
            FM3Messages . ErrorArr
              ( ARRAY OF REFANY
                  { "Reserved identifier \""
                  , FM3SrcToks . Image ( WScan . SaBuiltinTok )
                  , "\" is a builtin function and must have a parameter list." 
                  }
              , WScan . Position 
              )
          ; PutNotUsable ( FM3Base . AtomNull , WScan . Position )
          ; RETURN 
          END (*IF*)  
        END (*IF*)
      ; IdentRefL2R ( IdAttr ) 
      END (*WITH*)  
    END IdentRefLone

(*EXPORTED.*)
; PROCEDURE IdentRefL2R ( READONLY IdAttr : tParsAttribute )
  (* Possibly is a reserved Id, possibly legally. *) 
  (* PRE: The ident is followed by actuals, subscripte, or a deref. *) 

  = BEGIN (*IdentRefL2R*)
      WITH WScan = IdAttr . Scan
      DO IF WScan . SaAtom = FM3Base . AtomNull 
        THEN (* Reserved Ident.  Now that we know from syntax that it's a
                reference, distinguish reserved from non-reserved ident.
             *)
          PutBwd_TIP
            ( Itk . ItkReservedIdRef , WScan . SaBuiltinTok , WScan . Position )
        ELSE 
          WITH WIdentRefSet = FM3Scopes . OpenScopeStackTopRef ^ . ScpRefIdSet
          DO WIdentRefSet := IntSets . Include ( WIdentRefSet , WScan . SaAtom )
          END (*WITH*) 
        ; PutBwd_TIP ( Itk . ItkIdRefAtom , WScan . SaAtom , WScan . Position ) 
        END (*IF*) 
      END (*WITH*) 
    END IdentRefL2R

(*EXPORTED.*)
; PROCEDURE RecognizedPragma ( READONLY PragmaAttr : tParsAttribute )

  = BEGIN
      CASE PragmaAttr . Scan . SaBuiltinTok OF
      | FM3PgToks . PgFM3StdUnit
       => FM3Units . UnitStackTopRef ^ . UntHasStdUnitPragma := TRUE 
      ELSE
        FM3Messages . WarningArr
          ( ARRAY OF REFANY
              { "FM3 unimplemented pragma: \""
              , FM3PgToks . Image ( PragmaAttr . Scan . SaBuiltinTok )
              , "\"."
              }
          , PragmaAttr . Scan . Position 
          ) 
      END (*CASE*) 
    END RecognizedPragma

(*EXPORTED.*)
; PROCEDURE UnrecognizedPragma ( READONLY IdAttr : tParsAttribute )

  = BEGIN
      FM3Messages . WarningArr
        ( ARRAY OF REFANY
            { "FM3 unrecognized pragma: \""
            , FM3Units . CharsOfIdentAtom
                ( FM3Units . UnitStackTopRef , IdAttr . Scan . SaAtom )
            , "\"."
            }
        , IdAttr . Scan . Position 
        ) 
    END UnrecognizedPragma

(*EXPORTED.*)
; PROCEDURE PutNotUsable 
    ( IdentRefAtom : FM3Base . AtomTyp
    ; READONLY Position : FM3Base . tPosition
    )

  = BEGIN 
      WITH Wp1RdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO 
        PutBwd ( Wp1RdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( Wp1RdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( Wp1RdBack , VAL ( IdentRefAtom , LONGINT ) ) 
      ; PutBwd ( Wp1RdBack , VAL ( Itk . ItkIdRefAtomNotUsable , LONGINT ) )
      END (*WITH*)
    END PutNotUsable 

(*EXPORTED.*)
; PROCEDURE OverrideIdentRefL2R ( READONLY IdAttr : tParsAttribute )
  : BOOLEAN (* It's OK so far. *) 
  (* Disallows reserved Id. *) 

  = BEGIN (*OverrideIdentRefL2R*)
      WITH WScan = IdAttr . Scan
      DO IF WScan . SaAtom < 0
            AND IntSets . IsElement
                  ( - WScan . SaAtom , FM3Std . ReservedIdSet ) 
        THEN
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Identifier \""
                , FM3SrcToks . Image ( - WScan . SaAtom ) 
                , "\" is reserved and cannot denote an overridable method." 
             (* , SectionOfBuiltin ( IdAttr . Scan . SaBuiltinTok ) *)  
                , "(2.10)."
                } 
            , IdAttr . Scan . Position 
            )
        ; PutNotUsable ( WScan . SaAtom , WScan . Position )  
        ; RETURN FALSE 
        ELSE
          PutBwd_TIP ( Itk . ItkOverrideIdAtom , WScan . SaAtom , WScan . Position ) 
        ; RETURN TRUE 
        END (*IF*) 
      END (*WITH*) 
    END OverrideIdentRefL2R

(*EXPORTED.*)
; PROCEDURE AttrIsReservedId
    ( READONLY IdAttr : tParsAttribute ; contextTag := "in this context" )
  : BOOLEAN (* It's OK. *) 

  = BEGIN
      IF IdAttr . Scan . SaTok # Stk . StkIdent THEN RETURN FALSE END (*IF*)
    ; IF IdAttr . Scan . SaAtom # FM3Base . AtomNull THEN RETURN FALSE END (*IF*)
         (* ^Either standard or declared. *) 
    ; <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaBuiltinTok , FM3Std . ReservedIdSet )
      *>
      RETURN TRUE 
    END AttrIsReservedId

(*EXPORTED.*)
; PROCEDURE QualIdentRefL2R
    ( READONLY LtIdAttr , RtIdAttr : tParsAttribute )
  (* Handles either/both idents reserved. *) 

  = VAR LIsLegal : BOOLEAN

  ; BEGIN (*QualIdentRefL2R*)
      WITH WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO LIsLegal
           := VerifyIdentNotReserved
                ( LtIdAttr , LtIdAttr . Scan . Position , "have a qualifier." )
      ; LIsLegal
          := VerifyIdentNotReserved
               ( RtIdAttr , RtIdAttr . Scan . Position , "be a qualifier." )
             AND LIsLegal
      ; IF NOT LIsLegal 
        THEN 
          PutBwd 
            ( WunRdBack , VAL ( LtIdAttr . Scan . Position . Column , LONGINT ) )
        ; PutBwd 
            ( WunRdBack , VAL ( LtIdAttr . Scan . Position . Line , LONGINT ) )
        ; PutBwd 
            ( WunRdBack , VAL ( LtIdAttr . Scan . SaAtom , LONGINT ) )
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkIdRefAtomNotUsable , LONGINT ) ) 
        ELSE (* Neither ident is reserved. *) 
          WITH WIdentRefSet = FM3Scopes . DeclScopeStackTopRef ^ . ScpRefIdSet
          DO WIdentRefSet
               := IntSets . Include ( WIdentRefSet , LtIdAttr . Scan . SaAtom )
          END (*WITH*) 
        ; PutBwd
            ( WunRdBack , VAL ( RtIdAttr . Scan . Position . Column , LONGINT ) )
        ; PutBwd
            ( WunRdBack , VAL ( RtIdAttr . Scan . Position . Line , LONGINT ) ) 
        ; PutBwd
            ( WunRdBack , VAL ( LtIdAttr . Scan . Position . Column , LONGINT ) )
        ; PutBwd
            ( WunRdBack , VAL ( LtIdAttr . Scan . Position . Line , LONGINT ) )
        ; PutBwd ( WunRdBack , VAL ( RtIdAttr . Scan . SaAtom , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( LtIdAttr . Scan . SaAtom , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkQualIdAtoms , LONGINT ) )
        END (*IF*) 
      END (*WITH*)
    END QualIdentRefL2R

(*EXPORTED.*)
; PROCEDURE BuiltinNoSelectorAllowed
    ( READONLY IdAttr , SelectorAttr : tParsAttribute
    ; SelectedTag : TEXT
    )

  = BEGIN
      FM3Messages . ErrorArr
        ( ARRAY OF REFANY
            { "Identifier \""
            , FM3SrcToks . Image ( IdAttr . Scan . SaBuiltinTok )
            , "\" is reserved and cannot be "
            , SelectedTag
         (* , SectionOfBuiltin ( IdAttr . Scan . SaBuiltinTok ) *)  
            , " (2.10)."
(* FIXME --------- ^ *) 
            }
        , IdAttr . Scan . Position 
        )
    ; SkipFrom ( IdAttr . PaPass1Coord )
    ; PutBwd_TIP
        ( Itk . ItkIdRefAtomNotUsable
        , FM3Base . AtomNull
        , IdAttr . Scan . Position
        )
    END BuiltinNoSelectorAllowed 

; PROCEDURE CheckBuiltinProcActualsCt
    ( READONLY IdAttr : tParsAttribute 
    ; READONLY ActualsAttr : tParsAttribute
    ; ExpectedCt : INTEGER
    )
  : BOOLEAN (* It's OK and taken care of. *)
  (* PRE: IdAttr is a builtin ident of a procedure/function that
          requires ExpectedCt actual parameters. *) 

  = BEGIN
      SkipFrom ( IdAttr . PaPass1Coord ) 
    ; IF ActualsAttr . PaInt (* Actual count *) # ExpectedCt 
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "\""
              , FM3SrcToks . Image ( IdAttr . Scan . SaBuiltinTok )
              , "\" is a builtin procedure requiring "
              , Fmt . Int ( ExpectedCt ) 
              , " actual parameter"
              , FM3SharedUtils . PluralSuffix ( ExpectedCt )
              , ", not "
              , Fmt . Int ( ActualsAttr . PaInt )
              , " " 
           (* , SectionOfBuiltin ( IdAttr . Scan . SaBuiltinTok ) *)  
              , " (2.10)."
(* FIXME ----------- ^ *) 
              }
          , ActualsAttr . Scan . Position 
          )
      ; PutBwd_TIP
          ( Itk . ItkIdRefAtomNotUsable
          , FM3Base . AtomNull
          , IdAttr . Scan . Position
          )
      ; RETURN FALSE
      ELSE
        PutBwd_LCIIP_riip
          ( Itk . ItkBuiltinCallLt
          , IdAttr . PaPass1Coord 
          , IdAttr . Scan . SaBuiltinTok
          , ExpectedCt
          , ActualsAttr . Scan . Position
          ) 
      ; RETURN TRUE
      END (*IF*)
    END CheckBuiltinProcActualsCt

; PROCEDURE ActualsCtImage ( Ct : INTEGER ) : TEXT

  = BEGIN (*ActualsCtImage*)
      CASE Ct OF
      | - 1 => RETURN "absence of"  (* Shouldn't happen. *)  
      | 0 => RETURN "zero"
      | 1 => RETURN "one"
      | 2 => RETURN "two"
      | 3 => RETURN "three"
      | 4 => RETURN "four"
      | 5 => RETURN "one or more"
      ELSE RETURN Fmt . Int ( Ct )
      END (*CASE*) 
    END ActualsCtImage
    
(*EXPORTED.*)
; PROCEDURE PluralSuffix ( Int : INTEGER ) : TEXT 

  = BEGIN
      IF Int = 1 THEN RETURN ""
      ELSE RETURN "s"
      END (*IF*) 
    END PluralSuffix

(*EXPORTED.*)
; PROCEDURE BuiltinActualCt ( Tok : FM3SrcToks . TokTyp ) : INTEGER
  (* PRE: Tok is a reserved or standard builtin, not necessarily a function. *) 
  (* Number of actual parameters Tok requires. *)
  (* POST: 5 means one or more. -1 means not a function=>not callable. *)

  = BEGIN (*BuiltinActualCt*)
(*TODO: Make this result an integer value in FM3Std. *) 
      IF IntSets . IsElement ( Tok , FM3Std . OneParamSet )
      THEN RETURN 1 
      ELSIF IntSets . IsElement ( Tok , FM3Std . TwoParamSet )
      THEN RETURN 2
      ELSIF IntSets . IsElement ( Tok , FM3Std . ThreeParamSet )
      THEN RETURN 3
      ELSIF IntSets . IsElement ( Tok , FM3Std . OneOrMoreParamSet )
      THEN RETURN 5
      ELSE RETURN - 1 (* Means not a function. *)  
      END (*IF*)
    END BuiltinActualCt


(*EXPORTED.*)
; PROCEDURE CheckReservedActualsCt
    ( READONLY ActualsAttr : tParsAttribute
    ; READONLY TokAttr : tParsAttribute
    )
  : BOOLEAN (* Nothing illegal.  Nothing done.
               Otherwise message emitted and token stream modified. *)

  = VAR LReqdActualsCt : INTEGER
  ; VAR LBuiltinTok : FM3SrcToks . TokTyp 

  ; BEGIN
      LBuiltinTok := TokAttr . Scan . SaBuiltinTok
    ; IF NOT IntSets . IsElement
               ( LBuiltinTok , FM3Std . ReservedIdSet )
      THEN RETURN TRUE 
      END (*IF*) 
    ; LReqdActualsCt := BuiltinActualCt ( LBuiltinTok )
    ; IF LReqdActualsCt < 0 
      THEN 
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Reserved identifier \""
              , FM3SrcToks . Image ( LBuiltinTok )
              , "\" is not a function and cannot be called "
           (* , SectionOfBuiltin ( LBuiltinTok ) *)  
              }
          , ActualsAttr . Scan . Position 
          )
        (* And fall thru' to below. *) 
      ELSIF ( LReqdActualsCt >= 5 AND ActualsAttr . PaInt >= 1 )
            OR ( ActualsAttr . PaInt = LReqdActualsCt ) 
      THEN (* Actuals count of reserved id function is OK *) 
        RETURN TRUE
      ELSE 
      (* The actuals count is wrong. *) 
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Reserved identifier \""
              , FM3SrcToks . Image ( LBuiltinTok )
              , "\" requires "
              , ActualsCtImage ( LReqdActualsCt )  
              , " actual parameter"
              , PluralSuffix ( LReqdActualsCt )  
           (* , SectionOfBuiltin ( LBuiltinTok ) *)  
              }
          , ActualsAttr . Scan . Position 
          )
      END (*IF*) 
    ; SkipFrom ( TokAttr . PaPass1Coord ) (* Skip the reserved id. *)
    ; PutNotUsable ( TokAttr . Scan . SaAtom , TokAttr . Scan . Position )
(* Duplicates the above^.
    ; PutBwd_TP
        ( Itk . ItkDeclValAbsent , ActualsAttr . Scan . Position )
        (* ^ Could be ItkDeclTypeAbsent, but we can't tell without a
           lot of work, and its only for debugging token streams.
        *)
*) 
    ; RETURN FALSE 
    END CheckReservedActualsCt 

; PROCEDURE BuiltinIdentActualsL2R
    ( READONLY IdAttr : tParsAttribute ; READONLY ActualsAttr : tParsAttribute )
  (* PRE: IntSets . IsElement
            ( IdAttr . Scan . SaBuiltinTok , FM3Std . ReservedIdSet ).
  *) 
  (* PRE: IdAttr is for the builtin ident only, not the actuals. *) 
  (* PRE: ActualsAttr is for an actual parameter list. *) 

  = BEGIN 
      <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaBuiltinTok , FM3Std . ReservedIdSet )
      *>
      IF IntSets . IsElement
           ( IdAttr . Scan . SaBuiltinTok , FM3Std . OneParamSet )
      THEN 
        EVAL CheckBuiltinProcActualsCt ( IdAttr , ActualsAttr , 1 )
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaBuiltinTok , FM3Std . TwoParamSet )
      THEN 
        EVAL CheckBuiltinProcActualsCt ( IdAttr , ActualsAttr , 2 )
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaBuiltinTok , FM3Std . ThreeParamSet )
      THEN 
        EVAL CheckBuiltinProcActualsCt ( IdAttr , ActualsAttr , 3 )

(* COMPLETE: One or more params (NEW) *) 
      ELSE (* Others take no parameter list, not even parentheses. *)
        BuiltinNoSelectorAllowed ( IdAttr , ActualsAttr , "called" ) 
      END (*CASE*) 
    END BuiltinIdentActualsL2R

(*EXPORTED.*)
; PROCEDURE BuiltinOtherSelector
    ( READONLY IdAttr , SelectorAttr : tParsAttribute ; Tag : TEXT )
  (* A builtin id with either a dot-selection or subscript(s).
     There is no builtin that allows either of these. *)
  (* PRE: IntSets . IsElement
            ( IdAttr . Scan . SaBuiltinTok , FM3Std . ReservedIdSet ).
  *) 

  = BEGIN
      <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaBuiltinTok , FM3Std . ReservedIdSet )
      *>
      BuiltinNoSelectorAllowed ( IdAttr , SelectorAttr , Tag )
    ; SkipFrom ( IdAttr . PaPass1Coord )
    ; PutBwd_TIP
        ( Itk . ItkIdRefAtomNotUsable
        , FM3Base . AtomNull
        , IdAttr . Scan . Position
        ) 
    END BuiltinOtherSelector 

(*EXPORTED.*)
; PROCEDURE DeclScopeRtL2R ( ScopeRef : FM3Scopes . ScopeRefTyp )
  (* Create an IdAtom-to-declNo, fixed-size dictionary for the scope, of
     exactly the needed size, and load it up with mappings of the idents
     declared in the scope, onto a contiguously-numbered range of DeclNos.
  *) 

  = VAR SrtDeclNo : INTEGER 

  ; BEGIN (*DeclScopeRtL2R*)
      VAR LUnitRef : FM3Units . UnitRefTyp
    ; VAR LContainingScopeRef : FM3Scopes . ScopeRefTyp
    ; VAR LEscapingRefSet : IntSets . T 
    ; VAR LDeclCt : INTEGER
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
(*      <* ASSERT ScopeRef = FM3Scopes . DeclScopeStackTopRef *> *)
        LUnitRef := ScopeRef ^ . ScpOwningUnitRef
      (* Move Idents ref'd but to decls in this scope out to 
         containing lookup scope.
      *)
      ; IF ScopeRef = FM3Scopes . OpenScopeStackTopRef
        THEN (* A ref herein can refer to a decl herein. *) 
          LEscapingRefSet
            := IntSets . Difference 
                 ( ScopeRef ^ . ScpRefIdSet , ScopeRef ^ . ScpDeclIdSet ) 
        ; ScopeRef ^ . ScpRefIdSet 
            := IntSets . Intersection
                 ( ScopeRef ^ . ScpRefIdSet , ScopeRef ^ . ScpDeclIdSet )
        ; LContainingScopeRef := ScopeRef ^ . ScpOpenScopeStackLink
        ; IF Clt . CltRemoveUnusedDecls IN FM3CLOptions . OptionTokSet
             AND LUnitRef ^ . UntScopeRef = ScopeRef
             AND FM3Units . CurrentUnitIsModule ( ) 
          THEN (* It's a module. *) 
            ScopeRef ^ . ScpDeclIdSet 
              := IntSets . Difference
                   ( ScopeRef ^ . ScpDeclIdSet , LEscapingRefSet )
          END (*IF*)
        ELSE (* refs never refer to decls in ScopeRef. *)
          LEscapingRefSet := ScopeRef ^ . ScpRefIdSet
        ; ScopeRef ^ . ScpRefIdSet := IntSets . Empty ( ) 
        ; LContainingScopeRef := FM3Scopes . OpenScopeStackTopRef  
        END (*IF*) 
      ; IF LContainingScopeRef # NIL
        THEN
          LContainingScopeRef ^ . ScpRefIdSet
            := IntSets . Union
                 ( LContainingScopeRef ^ . ScpRefIdSet , LEscapingRefSet ) 
        END (*IF*)

      ; LDeclCt := IntSets . Card ( ScopeRef ^ . ScpDeclIdSet )
      (* Duplicate declared idents not included. *) 
      (* LDeclCt is exactly the needed dictionary size. *)
      ; ScopeRef ^ . ScpDeclCt := LDeclCt 
      ; ScopeRef ^ . ScpDeclDict 
          := FM3Dict_Int_Int . NewFixed 
               ( LDeclCt , FM3SharedUtils . IntHash )
      ; SrtDeclNo := FM3Units . AllocateDeclNos ( LDeclCt )
      ; ScopeRef ^ . ScpMinDeclNo := SrtDeclNo 
      ; LExpectedToDeclNo := SrtDeclNo + LDeclCt

      ; IntSets . ForAllDo ( ScopeRef ^ . ScpDeclIdSet , SrtVisit )

      ; <*ASSERT SrtDeclNo = LExpectedToDeclNo *> 
        TRY FM3Dict_Int_Int . FinalizeFixed ( ScopeRef ^ . ScpDeclDict )
        EXCEPT FM3Dict_Int_Int . Error ( EMsg )
        => FM3Messages . FatalArr
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

(* ---------------------------- Object types ------------------------ *)

(*EXPORTED.*)
; PROCEDURE ObjTypeLtL2R
    ( VAR LHSAttr : tParsAttribute
    ; BrandKind : FM3Base . Card8Typ (* FM3Parser . BrandKindTyp. *) 
    ; Position : tPosition
    ) 

  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LResult : INTEGER 
  
  ; BEGIN 
      LHSAttr . PaDeclDepth (* DeclParseInfoDepth *) 
        := FM3Decls . PushDeclParseInfo
             ( FM3Decls . DeclParseInfoTyp
                 { DiDeclTok := Itk . ItkFieldDeclLt
                 , DiIdListTok := Itk . ItkFieldDeclIdListLt
                 , DiIdSepTok := Itk . ItkFieldDeclIdListLt + Itk . LtToListSep
                 , DiKind := Dkt . DkObjField 
                 }
             ) 
    ; LHSAttr . PaByte := BrandKind
      (* Need the scope now, to collect decl id atoms. *) 
    ; LScopeRef 
        := FM3Scopes . NewScopeRef
             ( FM3Units . UnitStackTopRef
             , Skt . SkObj
             , Position
             ) 
    ; LHSAttr . PaInt := LScopeRef ^ . ScpSelfScopeNo  
    ; FM3Scopes . PushDeclScopeRef ( LScopeRef ) 
    ; PutBwd_TI ( Itk . ItkDeclScopeLt , LScopeRef ^ . ScpSelfScopeNo  ) 
    END ObjTypeLtL2R  

; BEGIN (*FM3Pass1*)
    InitVarInfo ( )
  END FM3Pass1
.

