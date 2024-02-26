
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3ParsePass

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
; IMPORT RangeUtils 
; IMPORT IntIntVarArray 
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; FROM FM3Base IMPORT tPosition 
; IMPORT FM3CLArgs
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
; FROM FM3StreamUtils
    IMPORT GetBwdInt , GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT FatalArr , ErrorArr , FM3LogArr
; IMPORT FM3Parser
; IMPORT FM3Predefined
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Units 
; IMPORT FM3Utils 
; IMPORT RdBackFile


; TYPE Skt = FM3Scopes . ScopeKindTyp 

; CONST PosImage = FM3Utils . PositionImage

; VAR FileTagVersion := VAL ( ORD ( '1' ) , Byte )  

; CONST LeftFileTagLen
    = BYTESIZE ( FM3SharedGlobals . FM3FileTagLt )
    + BYTESIZE ( FM3SharedGlobals . FM3FileKindRdBackLt )
    + BYTESIZE ( FileTagVersion )

; CONST ALOSE = FM3Messages . AtomListToOSError

(*EXPORTED*)
; PROCEDURE StartSkipping0 ( ) : CARDINAL (* depth after. *)

  = BEGIN
      INC ( SkipDepth )
    ; RETURN SkipDepth 
    END StartSkipping0

(*EXPORTED*)
; PROCEDURE StopSkipping0
    ( ) : CARDINAL (* depth before. *)
  (* Let's leave it to callers to check expected depth,
     so a failure will be detected at the call site.
  *)

  = VAR LDepth : CARDINAL

  ; BEGIN
      LDepth := SkipDepth
    ; IF LDepth > 0 THEN DEC ( SkipDepth ) END (*IF*)  
    ; RETURN LDepth 
    END StopSkipping0

; PROCEDURE PutBwd ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN
      <* ASSERT RdBack # FM3Globals . P2RdBack *> 
      IF TRUE OR SkipDepth <= 0
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

; PROCEDURE PutBwdP2 ( RdBack : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN (*PutBwdP2*) 
      <* ASSERT RdBack = FM3Globals . P2RdBack *> 
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0 
      THEN (* We are skipping output. *) RETURN
      END (*IF*) 
    ; TRY
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
    END PutBwdP2

(* 
; PROCEDURE FindFilePrefix ( FileName : TEXT ; Why : TEXT ) : TEXT

  = VAR LResult : TEXT
  
  ; BEGIN
      TRY
        LResult := Pathname . Prefix  ( FileName )
      EXCEPT OSError . E ( EMsg )
      => FatalArr
           ( ARRAY OF REFANY
               { "Unable to get absolute path for "
               , FileName , ", " , Why , ": " , ALOSE ( EMsg ) , "."  
               }
           ) 
      END (*EXCEPT*) 
    ; RETURN LResult 
    END FindFilePrefix
*)

; CONST UnitLogSuffix = ".log" 

; PROCEDURE OpenUnit ( SrcFileName : TEXT ) : FM3Units . UnitRefTyp

  = VAR LFullFileName : TEXT
  ; VAR LFullUnnestStackName : TEXT 
  ; VAR LFullPatchStackName : TEXT 
  ; VAR LFullParsePassName : TEXT 
  ; VAR LSimpleSrcFileName : TEXT 
  ; VAR LSrcFilePath : TEXT 
  ; VAR LUniRd : UniRd . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN
    (* Open source file. *) 
      LSrcFilePath
        := Pathname . Prefix ( FM3Files . AbsFileName ( SrcFileName ) )
    ; LSimpleSrcFileName := Pathname . Last ( SrcFileName )
    ; LUniRd
        := FM3Files . OpenUniRd
             ( LSimpleSrcFileName , LSrcFilePath , "source file " , NIL ) 
    ; LUnitRef := FM3Units . NewUnitRef ( )
    ; LUnitRef ^ . UntSrcFileName := LSimpleSrcFileName 
    ; LUnitRef ^ . UntSrcFilePath := LSrcFilePath

    (* Create the build directory: *)
(* FIXME: FM3CLArgs wants a build directory to put a log file in, even before
          we get here.  Is this the right place for it?
*) 
    ; LUnitRef ^ . UntBuildDirPath
        := LSrcFilePath & "/" & FM3Globals . BuildDirRelPath
    ; TRY
        FS . CreateDirectory ( LUnitRef ^ . UntBuildDirPath )
      EXCEPT
      | OSError . E ( EAtoms ) 
      => IF EAtoms . tail = NIL
            AND EAtoms . head # NIL
            AND Atom . ToText ( EAtoms . head ) # NIL
            AND Text . Equal ( Atom . ToText ( EAtoms . head ) , "errno=17" )
(* TODO: There has to be a more graceful way to detect this, but it looks
         like libm3 is letting us down here.
*) 
         THEN (* We expect this.  The directory already exists. *)
           EVAL EAtoms 
         ELSE 
           <*FATAL Thread . Alerted , Wr . Failure *>
           BEGIN
             Wr . PutText
               ( Stdio . stderr , "Unable to create build directory " ) 
           ; Wr . PutText ( Stdio . stderr , LUnitRef ^ . UntBuildDirPath ) 
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

    (* Create the log output file. A pure text file. *) 
    ; LUnitRef ^ . UntLogName := SrcFileName & UnitLogSuffix

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
    ; FM3Messages . StartUnit ( SrcFileName , LUnitRef ^ . UntLogWrT ) 

    (* Create build files for the unit. *) 
    ; LUnitRef ^ . UntPatchStackName
        := SrcFileName & FM3Globals . PatchStackSuffix   
    ; LUnitRef ^ . UntUnnestStackName
        := SrcFileName & FM3Globals . UnnestStackSuffix   
    ; LUnitRef ^ . UntParsePassName
        := SrcFileName & FM3Globals . ParsePassSuffix   
    ; TRY (*EXCEPT*)
        (* Heh, heh.  Code the exception handler only once for all files. *) 
        LFullUnnestStackName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntUnnestStackName
               , NIL
               )
      ; LFullFileName :=  LFullUnnestStackName 
      ; LUnitRef ^ . UntUnnestStackRdBack
          := RdBackFile . Create ( LFullUnnestStackName , Truncate := TRUE )
          
      ; LFullPatchStackName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntPatchStackName
               , NIL
               ) 
      ; LFullFileName :=  LFullPatchStackName 
      ; LUnitRef ^ . UntPatchStackRdBack
          := RdBackFile . Create ( LFullPatchStackName , Truncate := TRUE )
      
      ; LFullParsePassName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntParsePassName
               , NIL
               )
      ; LFullFileName :=  LFullParsePassName 
      ; LUnitRef ^ . UntParsePassRdBack
          := RdBackFile . Create ( LFullParsePassName , Truncate := TRUE )
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
        ( LUnitRef ^ . UntUnnestStackRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwd
        ( LUnitRef ^ . UntUnnestStackRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; LUnitRef ^ . UntUnnestStackEmptyCoord
        := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
    ; LUnitRef ^ . UntMaxUnnestStackDepth
        := LUnitRef ^ . UntUnnestStackEmptyCoord

    ; PutBwd
        ( LUnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkBOF , LONGINT ) )
    ; PutBwd
        ( LUnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; LUnitRef ^ . UntPatchStackEmptyCoord
        := RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack )
    ; LUnitRef ^ . UntMaxPatchStackDepth
        := LUnitRef ^ . UntPatchStackEmptyCoord
    ; LUnitRef . UntPatchStackTopCoord := LUnitRef . UntUnnestStackEmptyCoord

    ; PutBwd
        ( LUnitRef ^ . UntParsePassRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwd
        ( LUnitRef ^ . UntParsePassRdBack , VAL ( Itk . ItkRightEnd , LONGINT ) )
    ; LUnitRef ^ . UntParsePassEmptyCoord
        := RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack )  

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
    ; RETURN LUnitRef 
    END OpenUnit 

; PROCEDURE DisAsm
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
    END DisAsm

; PROCEDURE CompileUnit ( SrcFileName : TEXT )

  = VAR LUnitRef , LPoppedUnitRef : FM3Units . UnitRefTyp

(* TODO: Consistify "Depth" in identifiers with "Length" in RdBackFile. *)  
  ; VAR LUnnDepthL , LPpDepthL : LONGINT
  ; VAR LLengthL : LONGINT
  ; VAR LUnnestFullFileName , LUnnestFullCopyName : TEXT 
  ; VAR LPatchFullFileName : TEXT 
  ; VAR LParsePassFullFileName , LParsePassFullCopyName : TEXT 
  ; VAR LExceptionName , LExceptionLoc : TEXT 

  ; BEGIN (*CompileUnit*) 
      IF SkipDepth > 0 THEN RETURN END (*IF*) 
    ; LUnitRef := OpenUnit ( SrcFileName ) 
    ; FM3Units . PushUnit ( LUnitRef ) 
    ; FM3Units . UnitStackTopRef := LUnitRef
      (* ^Cached, for faster access. *) 
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Compiling " , LUnitRef ^ . UntSrcFilePath , SrcFileName , "..." }
        )
    ; TRY 
        LUnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *)

      (* Finish Unnest stack before building ParsePass file. *) 
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkRightEnd , LONGINT )
          )
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      ; LUnnDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
      ; LExceptionName := NIL (* Succeeded. *) 
      ; FM3Parser . CloseFM3Parser ( )
(*TODO ^ Do this sometime later. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
      ELSE
        LUnnDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkRightEndIncomplete , LONGINT )
          )

      ; LExceptionName
          := FM3RTFailures . ExcNameFromAddr ( Compiler . ThisException ( ) )  
      ; LExceptionLoc 
          := FM3RTFailures . ActivationLocationFromAddr
               ( Compiler . ThisException ( ) )  
      END (*EXCEPT *)
      
    ; LUnnestFullFileName
        := Pathname . Join
             ( LUnitRef ^ . UntBuildDirPath 
             , LUnitRef ^ . UntUnnestStackName
             , NIL
             )
    ; LUnnestFullCopyName 
        := Pathname . Join
             ( NIL , LUnnestFullFileName , FM3Globals . CopyFileSuffix ) 
    ; RdBackFile . Copy 
        ( LUnitRef ^ . UntUnnestStackRdBack , LUnnestFullCopyName , - 1L )
    ; IF LExceptionName # NIL OR FM3CLArgs . DoDisAsmUnnest
      THEN (* Disassemble it now. *) 
        DisAsm ( LUnitRef , LUnnestFullFileName )
      ; TRY FS . DeleteFile ( LUnnestFullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF LExceptionName # NIL
      THEN 
        FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure pushing unnest stack at depth "
               , Fmt . LongInt ( LUnnDepthL )
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
      ; RAISE FM3SharedUtils . FatalError ( "Failure pushing unnest stack." )
      END (*IF*) 

    (* Now build the ParsePass RdBack. *) 
    ; TRY 
        Unnest ( LUnitRef ^ . UntParsePassEmptyCoord ) 
      ; PutBwdP2
          ( LUnitRef ^ . UntParsePassRdBack
          , VAL ( Itk . ItkLeftEnd , LONGINT )
          )
      ; PutBwdP2
          ( LUnitRef ^ . UntParsePassRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      ; LPpDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack )
      ; LExceptionName := NIL (* Succeeded. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      | FM3SharedUtils . FatalError ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . FatalError ( Arg )  
     ELSE 
        LPpDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack )
      ; PutBwdP2
          ( LUnitRef ^ . UntParsePassRdBack
          , VAL ( Itk . ItkLeftEndIncomplete , LONGINT )
          )
      ; LExceptionName
          := FM3RTFailures . ExcNameFromAddr ( Compiler . ThisException ( ) )  
      ; LExceptionLoc 
          := FM3RTFailures . ActivationLocationFromAddr
               ( Compiler . ThisException ( ) )  
      ; IF LExceptionName = NIL AND NOT FM3CLArgs . DoDisAsmUnnest  
        THEN (* Didn't already disassemble unnest stack.  Do it now. *) 
          DisAsm ( LUnitRef , LUnnestFullFileName )
        ; TRY FS . DeleteFile ( LUnnestFullCopyName )
          EXCEPT OSError . E => (* It didn't exist. *) 
          END (*EXCEPT*) 
        END (*IF*) 
      END (*EXCEPT *)
    ; LParsePassFullFileName
        := Pathname . Join
             ( LUnitRef ^ . UntBuildDirPath 
             , LUnitRef ^ . UntParsePassName
             , NIL
             )
    ; LParsePassFullCopyName 
        := Pathname . Join
             ( NIL , LParsePassFullFileName , FM3Globals . CopyFileSuffix ) 
    ; RdBackFile . Copy 
        ( LUnitRef ^ . UntParsePassRdBack , LParsePassFullCopyName , - 1L )

    ; IF LExceptionName # NIL OR FM3CLArgs . DoDisAsmParsePass
      THEN (* Disassemble it now. *) 
        DisAsm ( LUnitRef , LParsePassFullFileName )
      ; TRY FS . DeleteFile ( LParsePassFullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF LExceptionName # NIL
      THEN 
        FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure pushing writing parse pass at depth "
               , Fmt . LongInt ( LPpDepthL )
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
      END (*IF*)

    (* Close first pass. *) 
    ; LUnitRef . UntParsePassResult := 0 
    ; EVAL FM3Scanner . PopState ( )
(* TODO^ Maybe do this elsewhere. *) 

    (* Finish with patch stack. *) 
    ; LUnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( LUnitRef ^ . UntPatchStackRdBack ) 
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Patch stack "
              , LUnitRef ^ . UntPatchStackName
              , " peak size = "
              , FM3Base . Int64Image  ( LUnitRef ^ . UntMaxPatchStackDepth )
              , " bytes."
            } 
        ) 
    ; IF NOT RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack )
             <= LUnitRef ^ . UntPatchStackEmptyCoord 
      THEN
        LUnitRef . UntParsePassResult := FM3CLArgs . CcPatchStackNotEmpty  
      ; FM3LogArr
          ( ARRAY OF REFANY
              { "Patch stack " 
              , LUnitRef ^ . UntPatchStackName
              , " final size = "
              , FM3Base . Int64Image
                  ( RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack ) )
              }
          ) 
      ; FatalArr
          ( ARRAY OF REFANY
              { "Patch stack is not sufficiently empty, should be "  
              , FM3Base . Int64Image ( LUnitRef ^ . UntPatchStackEmptyCoord )
              , "." 
              } 
          )
      ; RAISE FM3SharedUtils . FatalError ( "Nonempty patch stack." )
      END (*IF*)
    ; RdBackFile . Close (  LUnitRef ^ . UntPatchStackRdBack , 0L )
      (* No point in keeping the patch stack.  It has pogo-sticked and 
         now is empty. *)
    ; LPatchFullFileName
        := Pathname . Join
             ( LUnitRef ^ . UntBuildDirPath 
             , LUnitRef ^ . UntPatchStackName
             , NIL
             )
    ; TRY FS . DeleteFile ( LPatchFullFileName )
      EXCEPT OSError . E => (* It didn't exist. *) 
      END (*EXCEPT*) 

    (* Finish with unnest stack. *) 
    ; LUnitRef ^ . UntMaxUnnestStackDepth
        := RdBackFile . MaxLengthL ( LUnitRef ^ . UntUnnestStackRdBack )
    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Unnest stack "
            , LUnitRef ^ . UntUnnestStackName
            , " peak size = "
            , FM3Base . Int64Image  ( LUnitRef ^ . UntMaxUnnestStackDepth )
            , " bytes."
            } 
        )
    ; LLengthL := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
    ; RdBackFile . Close 
        (  LUnitRef ^ . UntUnnestStackRdBack , - 1L (* Leave full length. *) )
    ; IF LLengthL # LUnitRef ^ . UntUnnestStackEmptyCoord 
      THEN
        LUnitRef . UntParsePassResult := FM3CLArgs . CcUnnestStackNotEmpty  
      ; FM3LogArr
          ( ARRAY OF REFANY
              { "Unnest stack "
              , LUnitRef ^ . UntUnnestStackName
              , " final size = "
              , FM3Base . Int64Image ( LLengthL )
              , " bytes."
              } 
          )
      ; IF NOT FM3CLArgs . DoDisAsmUnnest
        THEN 
          DisAsm ( LUnitRef , LUnnestFullFileName )
        ; TRY FS . DeleteFile ( LUnnestFullCopyName )
          EXCEPT OSError . E => (* It didn't exist. *) 
          END (*EXCEPT*)
        END (*IF*) 
      ; FatalArr
          ( ARRAY OF REFANY
              { "Unnest stack is not sufficiently empty, should be "
              , FM3Base . Int64Image ( LUnitRef ^ . UntUnnestStackEmptyCoord )
              , "."
              }
          )
      ; RAISE FM3SharedUtils . FatalError
                ( "Unnest stack is not sufficiently empty." )
      END (*IF*) 
    ; IF NOT FM3CLArgs . DoDisAsmUnnest  
      THEN
        TRY FS . DeleteFile ( LUnnestFullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF NOT FM3CLArgs . DoKeep
      THEN 
        TRY FS . DeleteFile ( LUnitRef ^ . UntUnnestStackName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)

    (* Finish with parse pass RdBack, the output of this pass. *) 
    ; FM3LogArr
        ( ARRAY OF REFANY 
            { "Parse pass output file "
            , LUnitRef ^ . UntParsePassName 
            , " has " 
            , FM3Base . Int64Image 
                ( RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack ) ) 
            , " bytes."
            } 
        )
    ; PutBwdP2
        ( FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkLeftEnd , LONGINT ) 
        )
    ; PutBwdP2
        ( FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkEOF , LONGINT ) 
        )

    ; RdBackFile . Close 
        ( LUnitRef ^ . UntParsePassRdBack , - 1L (* Leave full length. *) )
    (* ^Don't, later when next pass is implemented. *) 

(* TODO: display code point counts. *)

    ; FM3LogArr
        ( ARRAY OF REFANY
            { "Finished compiling " , SrcFileName , "." }
        )
    ; LPoppedUnitRef := FM3Units . PopUnit ( )
    ; <* ASSERT LPoppedUnitRef = LUnitRef *> 
      FM3Messages . EndUnit ( SrcFileName ) 
    ; FM3Units . UnitStackTopRef := LUnitRef
    END CompileUnit
    
(*EXPORTED*)
; PROCEDURE Run ( )

  = VAR LSrcFileName : TEXT

  ; BEGIN
      LSrcFileName := FM3CLArgs . SrcFileName
    ; SkipDepth := 0 
    ; CompileUnit ( LSrcFileName ) 
    END Run

(*EXPORTED:*)
; PROCEDURE ModuleId
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdAtom : FM3Base . AtomTyp
    ; Position : FM3Base .tPosition 
    )

  = VAR LIdText : TEXT
  ; VAR LNameFromFilename : TEXT

  ; BEGIN (* ModuleId *) 
      IF UnitRef = NIL THEN RETURN END (*IF*)
    ; IF IdAtom = FM3Base . AtomNull THEN RETURN END (*IF*)
    ; IF UnitRef ^ . UntSrcFileName = NIL THEN RETURN END (*IF*) 
    ; LIdText := FM3Units . TextOfIdAtom ( IdAtom )
    ; UnitRef ^ . UntUnitIdentAtom := IdAtom 
    ; UnitRef ^ . UntUnitIdentPos := Position 
    ; LNameFromFilename
        := Pathname . ReplaceExt ( UnitRef ^ . UntSrcFileName , "" )  
    ; IF NOT Text . Equal ( LIdText , LNameFromFilename)
      THEN
        FM3Messages . InfoArr
          ( ARRAY OF REFANY
              { "Module name "
              , LIdText
              , "does not match file name "
              , UnitRef ^ . UntSrcFileName
              , "." 
              }
          ) 
      END (*IF*)
    END ModuleId 

(*EXPORTED:*)
; PROCEDURE CheckUnitFinalId
    ( UnitRef : FM3Units . UnitRefTyp
    ; EndIdAtom : FM3Base . AtomTyp 
    ; UnitKind : FM3Units . UnitKindTyp
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
              , " "
              , FM3Units . TextOfIdAtom ( UnitRef ^ . UntUnitIdentAtom )
              , ", at " 
              , PosImage ( UnitRef ^ . UntUnitIdentPos )  
              , " must repeat its name ("
              , FM3Units . UnitKindSectionNo ( UnitKind )
              , ")." 
              } 
          )
      END (*IF*) 
    END CheckUnitFinalId

(* ---------------------------- Unnest stack ------------------------ *) 

(*EXPORTED:*)
; PROCEDURE UnnestCoord ( ) : LONGINT
  (* Current coordinate of the current unit. *)
  
  = BEGIN
      RETURN RdBackFile . LengthL
               ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack ) 
    END UnnestCoord

(*EXPORTED:*)
; PROCEDURE PushUnnestStk ( READONLY ParsAttr : tParsAttribute )
   
  = BEGIN
(*FIXME: Don't push unnest if skipping.. *) 
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END PushUnnestStk

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
; PROCEDURE PushUnnest ( Value : INTEGER )

  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
        , VAL ( Value , LONGINT ) 
        )

    END PushUnnest

(*EXPORTED:*)
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  
  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
        , Value
        )
    END PushUnnestLong

(*EXPORTED:*)
; PROCEDURE Push_ListSepPatchPos
    ( ListTokLt : Itk . TokTyp
    ; C : LONGINT
    ; ElemNo : INTEGER
    ; READONLY Position : tPosition
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ElemNo , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd
          ( WRdBack , VAL ( ListTokLt + Itk . LtToListSepPatch , LONGINT ) )
      END (*WITH*) 
    END Push_ListSepPatchPos 

(*EXPORTED:*)
; PROCEDURE Push_L ( T : Itk . TokTyp )

  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
        , VAL ( T , LONGINT ) 
        )
    END Push_L

(*EXPORTED:*)
; PROCEDURE Push_LP
    ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_LP

(*EXPORTED:*)
; PROCEDURE Push_RP
    ( T : Itk . TokTyp ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + Itk . LtToRt , LONGINT ) ) 
      END (*WITH*) 
    END Push_RP

(*EXPORTED:*)
; PROCEDURE Push_LI ( T : Itk . TokTyp ; I : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_LI

(*EXPORTED:*)
; PROCEDURE Push_LIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_LIP

(*EXPORTED:*)
; PROCEDURE Push_LIP_rip
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LIP_rip

(*EXPORTED:*)
; PROCEDURE Push_EIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOne , LONGINT ) ) 
      END (*WITH*) 
    END Push_EIP

(*EXPORTED:*)
; PROCEDURE Push_ECIP
    ( T : Itk . TokTyp
    ; Coord : LONGINT
    ; I : INTEGER
    ; READONLY Position : tPosition
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , Coord ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToListSepPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_ECIP

(*EXPORTED:*)
; PROCEDURE Push_LCr ( T : Itk . TokTyp ; C : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_LCr

(*EXPORTED:*)
; PROCEDURE Push_LCP_rp
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY Position : tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_LCP_rp

(*EXPORTED:*)
; PROCEDURE Push_LCPI_rpi
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; READONLY Position : tPosition 
    ; I : INTEGER 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LCPI_rpi

(*EXPORTED:*)
; PROCEDURE Push_LCIP_rip
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I : INTEGER 
    ; READONLY Position : tPosition 
    )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LCIP_rip

(*EXPORTED:*)
; PROCEDURE Push_LCP_eCP_rP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionLt : tPosition
   ; CEins : LONGINT
   ; READONLY PositionEins : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LCP_eCP_rP

(*EXPORTED:*)
; PROCEDURE Push_LCP_eCP_zCP_rP
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
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LCP_eCP_zCP_rP 

(*EXPORTED:*)
; PROCEDURE Push_LCP_eCPB_zCP_rP
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
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LCP_eCPB_zCP_rP 

(*EXPORTED:*)
; PROCEDURE Push_LCPeCprp
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; CInfix : LONGINT 
   ; READONLY PositionInfix : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_LCPeCprp

(*EXPORTED:*)
; PROCEDURE Push_ECPrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionRt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionRt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( PositionOne . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionOne . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_ECPrP

(*EXPORTED:*)
; PROCEDURE Push_ECIP_riP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; I : INTEGER 
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
    END Push_ECIP_riP

(*EXPORTED:*)
; PROCEDURE Push_LCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( ORD ( B ) , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_LCBr

(*EXPORTED:*)
; PROCEDURE Push_LCIri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_LCIri

(*EXPORTED:*)
; PROCEDURE Push_LI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I0 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_LI3

(*EXPORTED:*)
; PROCEDURE Push_LI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I5 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I4 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I3 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I0 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_LI6

(*EXPORTED:*)
; PROCEDURE Push_LCeCr ( T : Itk . TokTyp ; Ct , Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) )
      END (*WITH*) 
    END Push_LCeCr

(*EXPORTED:*)
; PROCEDURE Push_LCIeCri
    ( T : Itk . TokTyp ; Ct : LONGINT ; I : INTEGER ; Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 

      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 

      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_LCIeCri

(*EXPORTED:*)
; PROCEDURE Pop4 ( )

  = BEGIN (*Pop4*)
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
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
      PushUnnest ( TokLt + Itk . LtToRt )
    (* Left token, to be moved leftward: *) 
    ; PushUnnestLong ( PatchCoord ) (*Patch*)
    ; PushUnnest ( TokLt + Itk . LtToPatch )
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
    ; PushUnnest ( ElemNo )
    ; PushUnnest ( TokLt + Itk . LtToRt )
    (* Left token, to be moved leftward: *) 
    ; PushUnnest ( ElemNo )
    ; PushUnnestLong ( PatchCoord ) (*Patch*)
    ; PushUnnest ( TokLt + Itk . LtToPatch )
    END MakeElem

(*EXPORTED:*)
; PROCEDURE MakeListPos
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY Position : tPosition
    ; READONLY ElemsAttr : tParsAttribute 
    )

  = BEGIN
      LHSAttr . PaInt := ElemsAttr . PaInt (* Valid Id count. *) 
    ; LHSAttr . PaUnnestCoord := ElemsAttr . PaUnnestCoord (* Redundant? used? *)
    ; IF TRUE OR ElemsAttr . PaInt > 0
(* REVIEW: bracket the list even if empty.  Do we really want this? *) 
      THEN 
        PushUnnest ( Position . Column ) 
      ; PushUnnest ( Position . Line ) 
      ; PushUnnest ( ElemsAttr . PaInt ) (* Elem Ct. *)
      ; PushUnnest ( TokLt + Itk . LtToRt )
      ; PushUnnest ( Position . Column ) 
      ; PushUnnest ( Position . Line ) 
      ; PushUnnest ( ElemsAttr . PaInt )
      ; PushUnnestLong ( ElemsAttr . PaUnnestCoord ) 
      ; PushUnnest ( TokLt + Itk . LtToPatch )
      END (*IF*)
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

; PROCEDURE CopyOperands
    ( OpndCt : [ 0 .. 6 ] 
    ; FromRdBack , ToRdBack : RdBackFile . T
    ; MaybeSkip : BOOLEAN (* ToRdBack is conditional on SkipNoStack. *) 
    )
  (* Copy operands, up to 6, without final reversing.  Pop/Push reverses
     them, but this procedure does its own reversal, resulting in
     net same order.
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*CopyOperands*)
<* ASSERT MaybeSkip = ( ToRdBack = FM3Globals . P2RdBack ) *> 
      IF MaybeSkip
         AND IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
             > 0 
      THEN (* Just read and discard OpndCt values. *)
        (* The obvious loop is unrolled. *) 
        IF OpndCt >= 1 
        THEN EVAL FM3Compress . GetBwd ( FromRdBack )
        ; IF OpndCt >= 2
          THEN EVAL FM3Compress . GetBwd ( FromRdBack )
          ; IF OpndCt >= 3
            THEN EVAL FM3Compress . GetBwd ( FromRdBack )
            ; IF OpndCt >= 4
              THEN EVAL FM3Compress . GetBwd ( FromRdBack )
              ; IF OpndCt >= 5
                THEN EVAL FM3Compress . GetBwd ( FromRdBack )
                ; IF OpndCt >= 6
                  THEN EVAL FM3Compress . GetBwd ( FromRdBack )
                  END (*IF*) 
                END (*IF*) 
              END (*IF*) 
            END (*IF*) 
          END (*IF*) 
        END (*IF*) 
      ELSE (* Actually copy, without net reversal. *) 
        (* The obvious loop is unrolled. *) 
        IF OpndCt >= 1 
        THEN
          LOpnd1 := FM3Compress . GetBwd ( FromRdBack )
        ; IF OpndCt >= 2
          THEN LOpnd2 := FM3Compress . GetBwd ( FromRdBack )
          ; IF OpndCt >= 3
            THEN LOpnd3 := FM3Compress . GetBwd ( FromRdBack )
            ; IF OpndCt >= 4
              THEN LOpnd4 := FM3Compress . GetBwd ( FromRdBack )
              ; IF OpndCt >= 5
                THEN LOpnd5 := FM3Compress . GetBwd ( FromRdBack )
                ; IF OpndCt >= 6
                  THEN LOpnd6 := FM3Compress . GetBwd ( FromRdBack )
                  ; FM3Compress . PutBwd ( ToRdBack , LOpnd6 ) 
                  END (*IF*) 
                ; FM3Compress . PutBwd ( ToRdBack , LOpnd5 ) 
                END (*IF*) 
              ; FM3Compress . PutBwd ( ToRdBack , LOpnd4 ) 
              END (*IF*) 
            ; FM3Compress . PutBwd ( ToRdBack , LOpnd3 ) 
            END (*IF*) 
          ; FM3Compress . PutBwd ( ToRdBack , LOpnd2 ) 
          END (*IF*)
        ; FM3Compress . PutBwd ( ToRdBack , LOpnd1 ) 
        END (*IF*)
      END (*IF*) 
    END CopyOperands

(* Insert tok *) 

(*EXPORTED*)
; PROCEDURE StartSkipping ( PairNo := FIRST (  INTEGER ) ) 

  = BEGIN

    END StartSkipping

(*EXPORTED*)
; PROCEDURE StopSkipping ( PairNo := FIRST (  INTEGER ) ) 

  = VAR LDepth : CARDINAL

  ; BEGIN
    END StopSkipping

; PROCEDURE SkipLt ( SkipNo : INTEGER )

  = BEGIN 
      WITH WSkipNoStack = FM3Globals . SkipNoStack
      , WSkipRange = IntIntVarArray . TouchedRange ( WSkipNoStack )
      DO
        <* ASSERT WSkipRange . Hi > 0 *>
        <* ASSERT 
             IntIntVarArray . Fetch ( WSkipNoStack , WSkipRange . Hi )
             = SkipNo
        *>
        IntIntVarArray . Project (* Pop SkipNoStack. *) 
          ( WSkipNoStack , RangeUtils . TrimHi ( WSkipRange ) )
     (* And throw this token away. *) 
      END (*WITH*)
    END SkipLt

; PROCEDURE Unnest ( LMUnnestDepth : LONGINT )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LUnnestRdBack : RdBackFile . T 
  ; VAR LPatchRdBack : RdBackFile . T 
  ; VAR LParsePassRdBack : RdBackFile . T 
  ; VAR LUnnestCoord : LONGINT
  ; VAR LPatchTokenL , LPatchedTokenL , LTokenL : LONGINT 
  ; VAR LPatchToken , LPatchedToken , LToken : Itk . TokTyp
  ; VAR LScopeNo : FM3Base . ScopeNoTyp
  ; VAR LAtom : FM3Base . AtomTyp
  ; VAR LPosition : tPosition
  ; VAR LDeclKind : FM3Decls . DeclKindTyp
  ; VAR LSkipNo : INTEGER

  ; BEGIN (* Unnest *) 
      LUnitRef := FM3Units . UnitStackTopRef
    (* For now, let's assume the skip mechanism is only used during pass 2.*)
    ; LUnnestRdBack := LUnitRef . UntUnnestStackRdBack 
    ; LPatchRdBack := LUnitRef . UntPatchStackRdBack 
    ; LParsePassRdBack := LUnitRef . UntParsePassRdBack
    ; LMUnnestDepth := MAX ( LMUnnestDepth , LUnitRef . UntUnnestStackEmptyCoord )
    ; EVAL GetBwd ( LUnitRef ^ . UntUnnestStackRdBack ) 
    ; EVAL GetBwd ( LUnitRef ^ . UntUnnestStackRdBack ) 

    ; LOOP
        LUnnestCoord := RdBackFile . LengthL ( LUnnestRdBack )
      ; IF LUnnestCoord <= LMUnnestDepth
           (* ^Nothing more to pop off Unnest strack. *) 
           AND RdBackFile . LengthL ( LPatchRdBack )
               <= LUnitRef . UntPatchStackTopCoord
           (* ^ Nothing more to pop off Patch stack. *) 
        THEN EXIT
        END (*IF*)
      (* Check first for a patch. *) 
      ; IF LUnnestCoord <= LUnitRef . UntPatchStackTopCoord
        THEN

        (* Modify and move token from the patch stack to the output. *) 
          <*ASSERT LUnnestCoord = LUnitRef . UntPatchStackTopCoord
                   (* Haven't missed a patch stack token. *)
          *> 
          LPatchTokenL := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchToken := VAL ( LPatchTokenL , Itk . TokTyp ) 
        ; <*ASSERT
              IntSets . IsElement
                ( LPatchToken , FM3SharedGlobals . GTokSetPatch )
          *>
          IF LPatchToken = Itk . ItkSkipLtPatch
          THEN (* Special handling, patching ItkSkipLtPatch. *)
            SkipLt ( GetBwdInt ( FM3Globals . PatchRdBack ) )  
          ELSE 
            LPatchedToken := LPatchToken - Itk . LtToPatch
  (* FIXME: The patch operation can apply to any non-Rt token.  I think
            the necessary bias is always the same as LtToPatch, but check
            this and then use a better name for it.
  *) 

         (* Copy the operands, reversing them to counteract the reversal
            accomplished by stack operations. *)
          ; CopyOperands
              ( FM3Utils . TokenOpndCt ( LPatchedToken ) 
              , LPatchRdBack
              , LParsePassRdBack
              , MaybeSkip := TRUE 
              ) 

          (* Put the patched token. *)
          ; LPatchedTokenL := VAL ( LPatchedToken , LONGINT ) 
          ; PutBwdP2 ( LParsePassRdBack , LPatchedTokenL )
          END (*IF*) 

        (* Conceptually finish popping the Patch stack by caching the
           next patch coordinate, which, unusually, is on top of its token.
        *)
        ; LUnitRef . UntPatchStackTopCoord 
            := FM3Compress . GetBwd ( LPatchRdBack )
          
        (* Now loop, possibly for more patches. *)
        ELSE (* Look at the top token on the Unnest stack. *) 
          LTokenL := FM3Compress . GetBwd ( LUnnestRdBack )
        ; LToken := VAL ( LTokenL , Itk . TokTyp ) 
        ; IF IntSets . IsElement ( LToken , FM3SharedGlobals . GTokSetPatch )
          THEN 

          (* Move this token from the unnest stack to the patch stack. *)
            PutBwd
              ( LPatchRdBack
              , LUnitRef . UntPatchStackTopCoord
              ) (* Uncache the existing patch coordinate by pushing it on top
                   of its token. *) 

          ; LUnitRef . UntPatchStackTopCoord
              := FM3Compress . GetBwd ( LUnnestRdBack )
                 (* New cached top patch coordinate. *)
          ; CopyOperands 
              ( FM3Utils . TokenOpndCt ( LToken ) 
              , LUnnestRdBack
              , LPatchRdBack
              , MaybeSkip := FALSE  
              ) 
          ; PutBwd ( LPatchRdBack , LTokenL )
            (* ^Push the token code deeper than its patch coordinate. *) 
          ELSE (* Move this token to the ParsePass stack. *) 
            CASE LToken OF (* Specially handled tokens. *)

            | Itk . ItkSkipLt
            => SkipLt ( GetBwdInt ( FM3Globals . P1RdBack ) ) 
 
            | Itk . ItkSkipRt
            => LSkipNo := GetBwdInt ( LUnnestRdBack )
              ; WITH WSkipNoStack = FM3Globals . SkipNoStack
                DO IntIntVarArray . Assign
                    ( WSkipNoStack
                    , IntIntVarArray . TouchedRange ( WSkipNoStack ) . Hi + 1  
                    , LSkipNo
                    )
                (* Discard this token. *)
                END (*WITH*) 
            
            | Itk . ItkDeclScopeRt 
            , Itk . ItkDeclScopeLt 
            , Itk . ItkLookupScopeRt 
            , Itk . ItkLookupScopeLt 
            => LScopeNo := GetBwdScopeNo ( LUnnestRdBack ) 
              ; <* ASSERT FALSE *>
                ScopeRtR2L ( LScopeNo )

(* CONSISTIFY: For some of these, get the operands inside the called proc. *) 
            | Itk . ItkDuplDeclId
            => LAtom := GetBwdAtom ( LUnnestRdBack )
              ; LPosition := GetBwdPos ( LUnnestRdBack )
              ; EVAL DuplDeclIdR2L ( LAtom , LPosition )

            | Itk . ItkDeclId
            => LDeclKind := GetBwdDeclKind ( LUnnestRdBack )
              ; LAtom := GetBwdAtom ( LUnnestRdBack )
              ; LPosition := GetBwdPos ( LUnnestRdBack )
              ; EVAL DeclIdR2L ( LDeclKind , LAtom , LPosition )

(* FIXME: We now use different tokens for different declkinds, eg.
          ItkVALUEFormalIdListElem.  But is that neecessary? *) 
              
            | Itk . ItkIdRefAtom 
            => LAtom := GetBwdAtom ( LUnnestRdBack )
              ; LPosition := GetBwdPos ( LUnnestRdBack )
              ; EVAL IdentRefR2L ( LAtom , LPosition )

            | Itk . ItkQualIdAtoms 
            => EVAL QualIdentR2L ( LUnnestRdBack )

            | Itk . ItkBlockRt
            => LScopeNo := GetBwdScopeNo ( LUnnestRdBack ) 
              ; ScopeRtR2L ( LScopeNo  )
              ; CopyOperands
                 ( 2 (*Position*)
                 , LUnnestRdBack
                 , LParsePassRdBack
                 , MaybeSkip := TRUE 
                 )
              ; PutBwdP2 ( LParsePassRdBack , VAL ( LScopeNo , LONGINT ) ) 
              ; PutBwdP2 ( LParsePassRdBack , VAL ( Itk . ItkBlockRt , LONGINT ) )

            | Itk . ItkBlockLt 
            => CopyOperands
                 ( FM3Utils . TokenOpndCt ( LToken ) 
                 , LUnnestRdBack
                 , LParsePassRdBack
                 , MaybeSkip := TRUE 
                 )
              ; PutBwdP2 ( LParsePassRdBack , LTokenL )
              ; EVAL FM3Scopes . PopDeclScope ( ) 

            ELSE (* Move directly, unnest to the output.*)
              CopyOperands
                ( FM3Utils . TokenOpndCt ( LToken )
                , LUnnestRdBack
                , LParsePassRdBack
                , MaybeSkip := TRUE 
                ) 
            ; PutBwdP2 ( LParsePassRdBack , LTokenL )
            END (*CASE*) 
          END (*IF*) 
        (* And loop *)           
        END (*IF*)

      END (*LOOP*)
    END Unnest

; PROCEDURE LookupId
    ( READONLY Scope : FM3Scopes . ScopeTyp 
    ; IdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ) 
  : FM3Base . DeclNoTyp 
  (* PRE: IdAtom is in Scope's dictionary. *) 

  = VAR LDeclNoInt : INTEGER
  ; VAR LMsg : TEXT
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupId*)
      TRY
        LFound 
          := FM3Dict_Int_Int . LookupFixed
               ( Scope . ScpDeclDict
               , IdAtom
               , FM3Base . HashNull
               , (*OUT*) LDeclNoInt
               )
      ; IF LFound
        THEN
          RETURN LDeclNoInt (* Implied NARROW. *) 
        ELSE LMsg := "not found"
        END (*IF*) 
      EXCEPT FM3Dict_Int_Int . Error ( EMsg )
        => LFound := FALSE
        ; LMsg := EMsg 
      END (*EXCEPT*)
    ; IF NOT LFound
      THEN 
        FatalArr
          ( ARRAY OF REFANY
              { "While looking up decl of \""
              , FM3Units . TextOfIdAtom ( IdAtom ) 
              , "\" in scope at "
              , PosImage ( Scope . ScpPosition )
              , ", "
              , LMsg 
              , "." 
              }
          , Position 
          ) 
      END (*IF*) 
    END LookupId

; PROCEDURE LookupBlockRef
    ( IdAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp
  (* POST: Error message written if not declared. *)

  = VAR LBlockScopeRef : FM3Scopes . ScopeRefTyp 
  ; VAR LDeclNoInt : INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupBlockRef*)
      LBlockScopeRef := FM3Scopes . DeclScopeStackTopRef
    ; LOOP
        IF LBlockScopeRef = NIL (* Undeclared? *) 
        THEN 
          FM3Messages . ErrorArr
             ( ARRAY OF REFANY
                 { "Undeclared identifier \""
                 , FM3Units . TextOfIdAtom ( IdAtom )
                 , "\", (2.1)." 
                 }
             , Position 
             )
        ; RETURN FM3Base . DeclNoNull
        ELSIF NOT LBlockScopeRef ^ . ScpKind IN FM3Scopes . ScopeKindSetBlock
           (* ^Can this happen? *) 
        THEN LBlockScopeRef := LBlockScopeRef ^ . ScpStackLink
        ELSIF NOT IntSets . IsElement
                    ( IdAtom , LBlockScopeRef ^ . ScpDeclIdSet )   
        THEN LBlockScopeRef := LBlockScopeRef ^ . ScpStackLink
        ELSE TRY 
            LFound := FM3Dict_Int_Int . LookupFixed
                        ( LBlockScopeRef . ScpDeclDict
                        , IdAtom
                        , FM3Base . HashNull
                        , (*OUT*) LDeclNoInt 
                        )
          EXCEPT FM3Dict_Int_Int . Error ( EMsg )
            => LFound := FALSE
(* CHECK: Do we want this to be a fatal compiler error? *) 
          END (*EXCEPT*)
        ; IF LFound
          THEN RETURN LDeclNoInt (* Implied NARROW. *)
          ELSE LBlockScopeRef := LBlockScopeRef ^ . ScpStackLink 
          END (*IF*) 
        END (*IF*) 
      END (*LOOP*) 
    END LookupBlockRef
    
(* ----------------------------- Scopes ---------------------------- *)

(* These are called by the parser: *) 

(*EXPORTED.*)
; PROCEDURE ScopeEmpty ( ScopeKind : FM3Scopes . ScopeKindTyp )

  = BEGIN (*ScopeEmpty*)
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
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack 
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
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack 
      DO IF WScan . SaIsReservedId 
        THEN LTokToPut := Itk . ItkReservedId 
        ELSE
          WITH WIdentRefSet = FM3Scopes . DeclScopeStackTopRef ^ . ScpRefIdSet
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
      WITH WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO
        IF CheckQualNotReserved ( StkLtIdAttribute )
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
      WITH WScope = FM3Scopes . DeclScopeStackTopRef ^
      DO (* Block*) 
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
      END (*WITH*) 
    END DeclScopeRtL2R

(* Right-to-left scope handling.  These are called during unnesting. *)
(* Call sites read the Itk and its args, and pass in the args. *) 

; PROCEDURE ScopeRtR2L ( ScopeNo : FM3Base . ScopeNoTyp )

  = VAR LScopeMap : FM3Scopes . ScopeMapTyp 
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      LScopeMap := FM3Units . UnitStackTopRef ^ . UntScopeMap 
    ; LScopeRef := VarArray_Int_Refany . Fetch ( LScopeMap , ScopeNo )
    ; FM3Scopes . PushDeclScopeRef ( LScopeRef ) 
    END ScopeRtR2L

; PROCEDURE DuplDeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp
  (* Append a temporary, pseudo-decl node to the linked list rooted at
     the decl number.  The position of the original declaration of
     the ident is not known right now.
  *) 

  = PROCEDURE Visit ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *)  
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
 
    = VAR LOldDeclRef : FM3Decls . DeclRefTyp  
    ; VAR LNewDeclRef : FM3Decls . DeclRefTyp  

    ; BEGIN
        LOldDeclRef := DeclRefany (* Implied NARROW. *) 
      ; LNewDeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
     (* ^This will have, as a side-effect, made DeclRefany = LNewDeclRef *)
      ; LNewDeclRef . DclLink := LOldDeclRef 
      ; LNewDeclRef . DclSelfScopeRef := FM3Scopes . DeclScopeStackTopRef (* Why not? *)
      ; LNewDeclRef . DclIdAtom := DeclIdAtom 
      ; LNewDeclRef . DclDeclNo := DeclNoI 
      ; LNewDeclRef . DclPos := Position 
      ; LNewDeclRef . DclKind := FM3Decls . DeclKindTyp . DkDuplDecl
      END Visit

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block. *)
        LDeclNo
          := LookupId ( FM3Scopes . DeclScopeStackTopRef ^ , DeclIdAtom , Position )
      ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdR2L
    
; PROCEDURE DeclIdR2L
    ( DeclKind : FM3Decls . DeclKindTyp
    ; DeclIdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    )
  : FM3Base . DeclNoTyp
  (* This will be the only decl of DeclIdAtom in the current scope. *) 

  = PROCEDURE VisitDecl ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *) 
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
    = VAR LDeclRef : FM3Decls . DeclRefTyp
    ; VAR LParentScopeRef : FM3Scopes . ScopeRefTyp 
    ; VAR LIdentText : TEXT

    ; BEGIN (* VisitDecl *)
        LDeclRef := DeclRefany (* Implied NARROW. *)
      ; LParentScopeRef := FM3Scopes . DeclScopeStackTopRef 
      ; IF LDeclRef # NIL (* Some duplicate decls of DeclNoI also exist? *) 
        THEN (* Dispense with them with error messages. *) 
          LIdentText := FM3Units . TextOfIdAtom ( DeclIdAtom ) 
        ; WHILE LDeclRef # NIL
          DO
            ErrorArr
              ( ARRAY OF REFANY
                  { PosImage ( LDeclRef . DclPos )
                  , " Duplicate declaration of \""
                  , LIdentText
                  , "\", ignored, original at "
                  , PosImage ( Position )
                  , "." 
                  } 
              )
          ; LDeclRef := LDeclRef ^ . DclLink
          END (*WHILE*) 
        END (*IF*)

      (* Now handle the original/only declaration. *) 
      ; LDeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . DeclScopeStackTopRef , DeclNoI )
      ; LDeclRef . DclLink := NIL 
      ; LDeclRef . DclSelfScopeRef := NIL
(* TODO: ^ Get this from parser.  Also set the reverse link ScpOwningDeclNo. *) 
      ; LDeclRef . DclIdAtom := DeclIdAtom 
      ; LDeclRef . DclDeclNo := DeclNoI
      ; LDeclRef . DclPos := Position 
      ; LDeclRef . DclKind := DeclKind 
      ; DeclRefany := LDeclRef 
      END VisitDecl

  ; BEGIN (*DeclIdR2L*) 
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    ; VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block *)
        WITH WppRdBack = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        DO 
          LDeclNo
            := LookupId
                 ( FM3Scopes . DeclScopeStackTopRef ^ , DeclIdAtom , Position ) 
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          VarArray_Int_Refany . CallbackWithElem 
            ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , VisitDecl )
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
(* FIXME: Put a token for the Id. *) 
        ; RETURN LDeclNo
        END (*WITH*) 
      END (* Block *) 
    END DeclIdR2L

; PROCEDURE IdentRefR2L
    ( IdentRefAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp 

  = VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*IdentRefR2L*)
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN FM3Base . DeclNoNull 
      END (*IF*) 
    ; WITH WScope = FM3Scopes . DeclScopeStackTopRef ^  
           , WppRdBack
             = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack 
      DO
        IF IntSets . IsElement ( IdentRefAtom , WScope . ScpDeclIdSet )
        THEN (* Decl'd in this scope.  Replace Id with DeclNo. *) 
          LDeclNo := LookupId ( WScope , IdentRefAtom , Position )
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          PutBwdP2 ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )  
        ; RETURN LDeclNo
        ELSE (* Leave as-is, for later resolution. *)
          PutBwdP2 ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( IdentRefAtom , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkIdRefAtom , LONGINT ) )  
        ; RETURN FM3Base . DeclNoNull 
        END (*IF*)
      END (*WITH*) 
    END IdentRefR2L

(* Old version:
; PROCEDURE QualIdentR2L
    ( UnnestRdBack : RdBackFile . T ) : FM3Base . DeclNoTyp 

  = VAR LAtomLt , LAtomRt : FM3Base . AtomTyp
  ; VAR LPosLt , LPosRt : tPosition
  ; VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*QualIdentL2R*)
      IF IntIntVarArray . TouchedRange ( FM3Globals . SkipNoStack ) . Hi > 0
      THEN RETURN
      END (*IF*) 
    ; WITH WScope = FM3Scopes . DeclScopeStackTopRef ^  
           , WppRdBack
             = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack 
      DO
        LAtomLt := GetBwdAtom ( UnnestRdBack ) 
      ; LAtomRt := GetBwdAtom ( UnnestRdBack ) 
      ; LPosLt := GetBwdPos ( UnnestRdBack ) 
      ; LPosRt := GetBwdPos ( UnnestRdBack ) 
      ; IF IntSets . IsElement ( LAtomLt , WScope . ScpDeclIdSet )
        THEN (* Left Id Atom Decl'd in this scope.  Replace it with DeclNo. *) 
          LookupId ( WScope , IdentRefAtom , (*OUT*) LDeclNo )
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          PutBwdP2 ( WppRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LAtomRt , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkQualIdDeclNoAtom , LONGINT ) )  
        ; RETURN LDeclNo
        ELSE (* Leave as-is. *)


        ; PutBwdP2 
            ( WppRdBack
            , VAL ( StkLtIdAttribute . Scan . Position . Column , LONGINT )
            )
        ; PutBwdP2 
            ( WppRdBack
            , VAL ( StkLtIdAttribute . Scan . Position . Line , LONGINT )
            )
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkInvalidRef , LONGINT ) )
        ; RETURN FM3Base . DeclNoNull 
        END (*IF*)
      END (*WITH*) 
    END QualIdentR2L
*)

; PROCEDURE QualIdentR2L
    ( UnnestRdBack : RdBackFile . T ) : FM3Base . DeclNoTyp 

  = VAR LAtomLt , LAtomRt : FM3Base . AtomTyp
  ; VAR LPosLt , LPosRt : tPosition
  ; VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*QualIdentL2R*)
      WITH WppRdBack = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack 
      DO
        LAtomLt := GetBwdAtom ( UnnestRdBack ) 
      ; LAtomRt := GetBwdAtom ( UnnestRdBack ) 
      ; LPosLt := GetBwdPos ( UnnestRdBack ) 
      ; LPosRt := GetBwdPos ( UnnestRdBack )
      
      ; LDeclNo := LookupBlockRef ( LAtomLt , LPosLt )
      ; IF LDeclNo = FM3Base . DeclNoNull  
        THEN (* Undeclared.  Convert to invalid ref. *) 
          PutBwdP2 ( WppRdBack , VAL ( LPosLt . Column , LONGINT ) )
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Line , LONGINT ) )
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkInvalidRef , LONGINT ) )
        ELSE (* Left Id Atom resolved in this scope.  Replace it with DeclNo. *)
          PutBwdP2 ( WppRdBack , VAL ( LPosRt . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosRt . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Column , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LPosLt . Line , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LAtomRt , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwdP2 ( WppRdBack , VAL ( Itk . ItkQualIdDeclNoAtom , LONGINT ) )  
        ; RETURN LDeclNo
        END (*IF*)
      ; RETURN FM3Base . DeclNoNull 
      END (*WITH*) 
    END QualIdentR2L

(* ----------------------- Procedure signatures --------------------- *)


; BEGIN (*FM3ParsePass*)
    InitVarInfo ( )
  END FM3ParsePass
.

