
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
; IMPORT FM3IntToks AS Itk
; FROM FM3IntToks
    IMPORT LtToRt , LtToPatch , LtToOne , LtToOnePatch , LtToTwoPatch
           , LtToListSepPatch 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Parser
; IMPORT FM3Predefs 
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
                 { "Unable to write to readback file \""
                 , RdBackFile . FileName ( RdBack )
                 , "\", " 
                 , ALOSE ( EMsg ) , "."  
                 }
             ) 
        END (*EXCEPT*) 
      END (*IF*)
    END PutBwd

(*EXPORTED*) 
; PROCEDURE RunPass1 ( ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; BEGIN (*RunPass1*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; InitPass1 ( LUnitRef )
    ; LUnitRef ^ . UntPassNosDisAsmed := FM3CLOptions . PassNoSetEmpty 
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Compiling "
            , Pathname . Join
                ( LUnitRef ^ . UntSrcFilePath
                , LUnitRef ^ . UntSrcFileSimpleName
                ) 
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
    ; FM3Messages . StartUnit ( UnitRef ^ . UntSrcFileSimpleName ) 

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
          := RdBackFile . Create ( LFullPass1OutName , Truncate := TRUE ) 
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
          := RdBackFile . Create ( LFullPatchStackName , Truncate := TRUE )
      ; FM3Globals . PatchRdBack := UnitRef ^ . UntPatchStackRdBack 
        (* ^Cache for faster access. *) 
      
      EXCEPT
      | OSError . E ( EMsg ) 
      => FatalArr
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

(* COMPLETEME: See that RdBack Create adds FM3 file tags and lengths. *)
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
        ( UnitRef ^ . UntPatchStackRdBack , FM3Globals . PatchSackEmptySentinal )
      (* ^A sentinal for when the patch stack is empty of actual tokens. *) 

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
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkRightEnd , LONGINT )
          )
      ; PutBwd
          ( UnitRef ^ . UntPass1OutRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      
      ; FM3Parser . CloseFM3Parser ( )
(*TODO ^ Do this sometime later? *)

      (* Prepare for possible disassembly later. *) 
      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass1OutSuffix
          , UnitRef ^ . UntPass1OutRdBack
          )
        (*^ This copy may be used by disassembly called for by command-line
            option, a later pass failure, or not at all. *) 

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

      ; FM3Compile . MakePassFileCopy
          ( UnitRef
          , FM3Globals . Pass1OutSuffix
          , UnitRef ^ . UntPass1OutRdBack
          )
        (* ^This copy will be used immediately to disassemble
            what there is of the failed file. *)
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

    (* Report size and maybe disassemble pass 1 outputfile. *) 
      LPass1FullFileName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath 
             , UnitRef ^ . UntPass1OutSimpleName
             , NIL
             )

    ; UnitRef ^ . UntMaxPass1OutLength
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPass1OutRdBack )
    ; FM3LogArr
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
    ; IdScanAttr : FM3Scanner . tScanAttribute 
    )
  (*PRE: UnitRef # NIL *) 

  = VAR LNameFromFileName : TEXT

  ; BEGIN (* InterfaceId *)
      UnitRef ^ . UntUnitIdent := IdScanAttr . SaChars 
    ; UnitRef ^ . UntUnitIdentPos := IdScanAttr . Position
    ; UnitRef ^ . UntPredefTok := IdScanAttr . SaPredefTok 
    ; IF UnitRef ^ . UntUnitIdent = NIL THEN RETURN END (*IF*) 
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
    ; IdScanAttr : FM3Scanner . tScanAttribute 
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
    ; EndIdScanAttr : FM3Scanner . tScanAttribute 
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
          ( WRdBack , VAL ( Itk . ItkTextLitLt , LONGINT ) )

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
          ( WRdBack , VAL ( Itk . ItkWideTextLitLt , LONGINT ) )

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
; PROCEDURE PutBwd_LCIP_eCP_rip
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
      ; PutBwd ( WRdBack , EC ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) )
      
      ; PutBwd ( WRdBack , VAL ( LPos . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( LPos . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , LC ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END PutBwd_LCIP_eCP_rip

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

  = BEGIN 
     IF NOT  HasType AND NOT HasValue 
     THEN
       WITH WDeclParseInfo = FM3Decls . TopDeclParseInfo ( )
       DO 
         FM3Messages . ErrorArr
           ( ARRAY OF REFANY 
               { "Declared "
               , VarLabel [ WDeclParseInfo . DiKind ] 
               , " must have a type and/or an initial value. "
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

; PROCEDURE AtomOfPredefId
    ( READONLY IdAttr : tParsAttribute ) : FM3Base . AtomTyp

  = BEGIN
      RETURN FM3Atom_OAChars . MakeAtom 
                ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
                , IdAttr . Scan . SaChars 
                , IdAttr . Scan . SaHash 
                ) 
    END AtomOfPredefId 

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
               in current scope.) *)
  (* PRE: IdAttr is for an identifier in a declaring context. *) 

  = VAR LAtom : FM3Base . AtomTyp 

  ; BEGIN (*DeclIdL2R*)
      IF IntSets . IsElement
           ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet ) 
      THEN 
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Identifier \""
              , FM3SrcToks . Image ( IdAttr . Scan . SaPredefTok )
              , "\" is reserved and cannot be declared (2.8.2)." 
              }
          , IdAttr . Scan . Position 
          )
      ; RETURN FALSE (* Caller, Don't use this Id. *) 
      END (*IF*) 
    ; LAtom := IdAttr . Scan . SaAtom 
    ; WITH WScope = FM3Scopes . DeclScopeStackTopRef ^
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack 
      DO 
        IF WScope . ScpOwningUnitRef = FM3Units . UnitStackTopRef 
           AND NOT FM3ExpImp . CheckDuplicateExpImp
                     ( FM3Units . UnitStackTopRef
                     , LAtom
                     , IdAttr . Scan . Position
                     , "declaration"
                     )
        THEN (* It duplicates an export or import. *) 
          RETURN FALSE 
        ELSIF IntSets . IsElement ( LAtom , WScope . ScpDeclIdSet )
        THEN (* It duplicates an earlier decl in this scope. *) 
          WScope . ScpDuplDeclIdSet
            := IntSets . Include ( WScope . ScpDuplDeclIdSet , LAtom )
(* CHECK^ Do we need ScpDuplDeclIdSet? *) 
        (* Write a duplicate Ident token.  The only effect will be to
           emit an error later, during Pass2, when the position of the original
           declaring occurence is known. *) 
        ; PutBwd
            ( WunRdBack
            , VAL ( IdAttr . Scan . Position . Column , LONGINT )
            ) 
        ; PutBwd
            ( WunRdBack
            , VAL ( IdAttr . Scan . Position . Line , LONGINT )
            )
        ; PutBwd ( WunRdBack , VAL ( LAtom , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL (  Itk . ItkDuplDeclId , LONGINT ) )
        ; RETURN FALSE (* Caller, Don't use this Id. *)

        ELSE  (* 1st declaration of Ident in scope . *) 
          WScope . ScpDeclIdSet
            := IntSets . Include ( WScope . ScpDeclIdSet , LAtom )

        (* Maybe write Separator token: *)
        ; IF SepTok # Itk . ItkNull AND PriorIdCt > 0
          THEN 
            PutBwd ( WunRdBack , VAL ( SepPosition . Column , LONGINT ) ) 
          ; PutBwd ( WunRdBack , VAL ( SepPosition . Line , LONGINT ) )
          ; PutBwd ( WunRdBack , VAL ( PriorIdCt , LONGINT ) )
          ; PutBwd ( WunRdBack , VAL ( SepTok , LONGINT ) )
          END (*IF*)

        (* Id is valid.  Write Ident token: *)
        ; PutBwd
            ( WunRdBack , VAL ( IdAttr . Scan . Position . Column , LONGINT ) ) 
        ; PutBwd
            ( WunRdBack , VAL ( IdAttr . Scan . Position . Line , LONGINT ) )
        ; PutBwd ( WunRdBack , VAL ( IdAttr . Scan . SaPredefTok , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( LAtom , LONGINT ) ) 
        ; PutBwd ( WunRdBack , VAL ( ORD ( DeclKind ) , LONGINT ) )
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkDeclId , LONGINT ) )
        ; RETURN TRUE (* Caller, Use this decl id. *)
        END (*IF*)
      END (*WITH*) 
    END DeclIdL2R

(*EXPORTED.*)
; PROCEDURE IdentRefL2R ( READONLY IdAttr : tParsAttribute )
  (* Possibly is a reserved Id, possibly legally. *) 

  = BEGIN (*IdentRefL2R*)
      WITH WScan = IdAttr . Scan
      DO IF WScan . SaAtom = FM3Base . AtomNull  
        THEN (* Reserved Ident.  Now that we know it's a reference,
                distinguish reserved from non-.
             *) 
          PutBwd_LIP
            ( Itk . ItkReservedIdRef , WScan . SaPredefTok, WScan . Position )
        ELSE 
          WITH WIdentRefSet = FM3Scopes . OpenScopeStackTopRef ^ . ScpRefIdSet
          DO WIdentRefSet := IntSets . Include ( WIdentRefSet , WScan . SaAtom )
          END (*WITH*) 
        ; PutBwd_LIP ( Itk . ItkIdRefAtom , WScan . SaAtom , WScan . Position ) 
        END (*IF*) 
      END (*WITH*) 
    END IdentRefL2R

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
                  ( - WScan . SaAtom , FM3Predefs . ReservedIdSet ) 
        THEN
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Identifier \""
                , FM3SrcToks . Image ( - WScan . SaAtom ) 
                , "\" is reserved and cannot denote an overridable method." 
             (* , SectionOfBuiltin ( IdAttr . Scan . SaPredefTok ) *)  
                , "(2.10)."
                } 
            , IdAttr . Scan . Position 
            )
        ; PutNotUsable ( WScan . SaAtom , WScan . Position )  
        ; RETURN FALSE 
        ELSE
         PutBwd_LIP ( Itk . ItkIdRefAtom , WScan . SaAtom , WScan . Position ) 
        ; RETURN TRUE 
        END (*IF*) 
      END (*WITH*) 
    END OverrideIdentRefL2R

(*EXPORTED.*)
; PROCEDURE AttrIsReservedId
    ( READONLY IdAttr : tParsAttribute ; contextTag := "in this context" )
  : BOOLEAN (* It's OK. *) 

  = BEGIN
      IF IdAttr . Scan . SaTok #  Stk . StkIdent THEN RETURN FALSE END (*IF*) 
    ; IF IdAttr . Scan . SaAtom # FM3Base . AtomNull
      THEN RETURN FALSE
      END (*IF*)
    ; <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet )
      *>
      RETURN TRUE 
    END AttrIsReservedId

(*EXPORTED.*)
; PROCEDURE FlagReservedIdent
    ( READONLY IdAttr : tParsAttribute ; ContextTag := "used in this context" )

  = BEGIN
      IF IdAttr . Scan . SaAtom # FM3Base . AtomNull
      THEN 
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Identifier \""
              , FM3SrcToks . Image ( IdAttr . Scan . SaPredefTok )
              , "\" is reserved and cannot be "
              , ContextTag 
              , "(2.8.2)."
              }
          , IdAttr . Scan . Position 
          )
      END (*IF*) 
    END FlagReservedIdent

(*EXPORTED.*)
; PROCEDURE QualIdentL2R
    ( READONLY LtIdAttr , RtIdAttr : tParsAttribute )
  (* Handles either/both idents reserved. *) 

  = BEGIN (*QualIdentL2R*)
      WITH WunRdBack = FM3Units . UnitStackTopRef ^ . UntPass1OutRdBack
      DO IF AttrIsReservedId ( LtIdAttr )
         OR AttrIsReservedId ( RtIdAttr )
        THEN 
          PutBwd 
            ( WunRdBack , VAL ( LtIdAttr . Scan . Position . Column , LONGINT ) )
        ; PutBwd 
            ( WunRdBack , VAL ( LtIdAttr . Scan . Position . Line , LONGINT ) )
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkInvalidRef , LONGINT ) ) 
        ELSE (* Neither is reserved. *) 
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
    END QualIdentL2R

(*EXPORTED.*)
; PROCEDURE BuiltinNoSelectorAllowed
    ( READONLY IdAttr , SelectorAttr : tParsAttribute
    ; SelectedTag : TEXT
    )

  = BEGIN
      FM3Messages . ErrorArr
        ( ARRAY OF REFANY
            { "Identifier \""
            , FM3SrcToks . Image ( IdAttr . Scan . SaPredefTok )
            , "\" is reserved and cannot be "
            , SelectedTag
         (* , SectionOfBuiltin ( IdAttr . Scan . SaPredefTok ) *)  
            , " (2.10)."
(* FIXME --------- ^ *) 
            }
        , IdAttr . Scan . Position 
        )
    ; SkipFrom ( IdAttr . PaPass1Coord )
    ; PutBwd_LIP
        ( Itk . ItkIdRefAtomNotUsable
        , FM3Base . AtomNull
        , IdAttr . Scan . Position
        )
    END BuiltinNoSelectorAllowed 

; PROCEDURE CheckBuiltinProcActualsCt
    ( READONLY IdAttr , ActualsAttr : tParsAttribute
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
              , FM3SrcToks . Image ( IdAttr . Scan . SaPredefTok )
              , "\" is a function requiring "
              , Fmt . Int ( ExpectedCt ) 
              , " actual parameter"
              , FM3SharedUtils . PluralSuffix ( ExpectedCt )
              , ", not "
              , Fmt . Int ( ActualsAttr . PaInt )
              , " " 
           (* , SectionOfBuiltin ( IdAttr . Scan . SaPredefTok ) *)  
              , " (2.10)."
(* FIXME ----------- ^ *) 
              }
          , IdAttr . Scan . Position 
          )
      ; PutBwd_LIP
          ( Itk . ItkIdRefAtomNotUsable
          , FM3Base . AtomNull
          , IdAttr . Scan . Position
          )
      ; RETURN FALSE
      ELSE
        PutBwd_LCIIP_riip
          ( Itk . ItkBuiltinCallLt
          , IdAttr . PaPass1Coord 
          , IdAttr . Scan . SaPredefTok
          , ExpectedCt
          , ActualsAttr . Scan . Position
          ) 
      ; RETURN TRUE
      END (*IF*)
    END CheckBuiltinProcActualsCt

(*EXPORTED.*)
; PROCEDURE BuiltinWithNoSelector ( READONLY IdAttr : tParsAttribute )
  (* PRE: IntSets . IsElement
            ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet ).
  *) 
  (* Builtin ident that has no selector in source code. *) 

  = VAR LParamTag : TEXT
  ; VAR LPluralSuffix : TEXT 

  ; BEGIN
      <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet )
      *>
      LPluralSuffix := "s "
    ; IF IntSets . IsElement
           ( IdAttr . Scan . SaPredefTok , FM3Predefs . OneParamSet )
      THEN
        LParamTag := "one"
      ; LPluralSuffix := " "
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaPredefTok , FM3Predefs . TwoParamSet )
      THEN LParamTag := "two" 
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaPredefTok , FM3Predefs . ThreeParamSet )
      THEN LParamTag :="three"
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaPredefTok , FM3Predefs . OneOrMoreParamSet )
      THEN LParamTag := "one or more" 
      ELSE (* It's OK. Convert to ItkBuiltinIdRef *)
        PutBwd_LIP
          ( Itk . ItkBuiltinIdRef
          , IdAttr . Scan . SaPredefTok
          , IdAttr . Scan . Position
          ) 
      ; RETURN 
      END (*IF*)

    (* It's bad. *) 
    ; FM3Messages . ErrorArr
        ( ARRAY OF REFANY
            { "Reserved identifier \""
            , FM3SrcToks . Image ( IdAttr . Scan . SaPredefTok )
            , "\" requires a list of "
            , LParamTag
            , " parameter"
            , LPluralSuffix 
         (* , SectionOfBuiltin ( IdAttr . Scan . SaPredefTok ) *)  
            }
        , IdAttr . Scan . Position 
        )
    ; SkipFrom ( IdAttr . PaPass1Coord ) (* Skip the reserved id. *)
    ; PutBwd_LIP
        ( Itk . ItkIdRefAtomNotUsable
        , FM3Base . AtomNull
        , IdAttr . Scan . Position
        ) 
    END BuiltinWithNoSelector 

(*EXPORTED.*)
; PROCEDURE BuiltinIdentActualsL2R
    ( READONLY IdAttr , ActualsAttr : tParsAttribute )
  (* PRE: IntSets . IsElement
            ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet ).
  *) 
  (* PRE: IdAttr is for the builtin ident only, not the actuals. *) 
  (* PRE: ActualsAttr is for an actual parameter list. *) 

  = BEGIN 
      <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet )
      *>
      IF IntSets . IsElement
           ( IdAttr . Scan . SaPredefTok , FM3Predefs . OneParamSet )
      THEN 
        EVAL CheckBuiltinProcActualsCt ( IdAttr , ActualsAttr , 1 )
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaPredefTok , FM3Predefs . TwoParamSet )
      THEN 
        EVAL CheckBuiltinProcActualsCt ( IdAttr , ActualsAttr , 2 )
      ELSIF IntSets . IsElement
              ( IdAttr . Scan . SaPredefTok , FM3Predefs . ThreeParamSet )
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
            ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet ).
  *) 

  = BEGIN
      <* ASSERT IntSets . IsElement
                  ( IdAttr . Scan . SaPredefTok , FM3Predefs . ReservedIdSet )
      *>
      BuiltinNoSelectorAllowed ( IdAttr , SelectorAttr , Tag )
    ; SkipFrom ( IdAttr . PaPass1Coord )
    ; PutBwd_LIP
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
    ; VAR LParentScopeRef : FM3Scopes . ScopeRefTyp
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
        <* ASSERT ScopeRef = FM3Scopes . DeclScopeStackTopRef *>
      (* Move Idents ref'd but not declared here out to parent lookup scope. *)
        LEscapingRefSet
          := IntSets . Difference 
               ( ScopeRef ^ . ScpRefIdSet , ScopeRef ^ . ScpDeclIdSet ) 
      ; ScopeRef ^ . ScpRefIdSet 
          := IntSets . Intersection
               ( ScopeRef ^ . ScpRefIdSet , ScopeRef ^ . ScpDeclIdSet )
      ; LParentScopeRef := ScopeRef ^ . ScpOpenScopeStackLink
      ; IF LParentScopeRef # NIL
        THEN
          LParentScopeRef ^ . ScpRefIdSet
            := IntSets . Union
                 ( LParentScopeRef ^ . ScpRefIdSet , LEscapingRefSet ) 
        END (*IF*)
      ; LUnitRef := ScopeRef ^ . ScpOwningUnitRef
      ; IF Clt . CltRemoveUnusedDecls IN FM3CLOptions . OptionTokSet
           AND LUnitRef ^ . UntScopeRef = ScopeRef
           AND FM3Units . CurrentUnitIsModule ( ) 
        THEN ScopeRef ^ . ScpDeclIdSet := ScopeRef ^ . ScpRefIdSet
        END (*IF*)

      ; LDeclCt := IntSets . Card ( ScopeRef ^ . ScpDeclIdSet ) 
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


; BEGIN (*FM3Pass1*)
    InitVarInfo ( )
  END FM3Pass1
.

