
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3ParsePass

(* The first pass.
   1. Scan and Parse
   2. Replace identifers by numeric atoms. 
   3. Convert to a fully-delimited intermediate token stream, written
      to a file ready to be read backwards.
   4. Pull constructs that have a scope of identifiers out to the left
      of their containing scope constructs.
   5. Build a global table of atom-accessed Units.UnitTyp records for
      compilation units,
   6. For each unit, build atom-accessed records for identifiers, scopes,
      and declarations.
   7. For each scope, build a compact dictionary mapping identifier
      to decl atoms.
   8. Resolve some identifier occurrences to decl atoms and insert these
      into the token stream.
*)

; IMPORT FileWr
; IMPORT Fmt 
; IMPORT FS 
; IMPORT OSError
; IMPORT Pathname
; IMPORT Stdio
; IMPORT Text 
; IMPORT Thread 
; IMPORT UniRd
; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3CLArgs
; IMPORT FM3Compress
; FROM FM3Compress IMPORT GetBwd
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int
; IMPORT FM3DisAsm 
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3IntToks AS Itk
; FROM FM3IntToks IMPORT LtToRt , LtToPatch , LtToOnePatch , LtToTwoPatch 
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks AS Stk
; FROM FM3StreamUtils
    IMPORT GetBwdAtom , GetBwdDeclKind , GetBwdPos , GetBwdScopeNo 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT FatalArr , ErrorArr , Log
; IMPORT FM3Parser
; IMPORT FM3Predefined
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SrcToks
; IMPORT FM3Units 
; IMPORT FM3Utils 
; IMPORT RdBackFile
; IMPORT VarArray_Int_Refany 

; IMPORT FM3SharedGlobals 

; FROM File IMPORT Byte  

; CONST PosImage = FM3Utils . PositionImage

; VAR FileTagVersion := VAL ( ORD ( '1' ) , Byte )  

; CONST LeftFileTagLen
    = BYTESIZE ( FM3SharedGlobals . FM3FileTagLt )
    + BYTESIZE ( FM3SharedGlobals . FM3FileKindRdBackLt )
    + BYTESIZE ( FileTagVersion )

; CONST ALOSE = FM3Messages . AtomListToOSError

(*EXPORTED*)
; PROCEDURE StartSkipping ( ) : CARDINAL (* depth after. *)

  = BEGIN
      INC ( SkipDepth )
    ; RETURN SkipDepth 
    END StartSkipping

(*EXPORTED*)
; PROCEDURE StopSkipping ( ) : CARDINAL (* depth before. *)
  (* Let's let callers check expected depth, so a failure will
     be detected at the call site.
  *)

  = VAR LDepth : CARDINAL

  ; BEGIN
      LDepth := SkipDepth
    ; IF LDepth > 0 THEN DEC ( SkipDepth ) END (*IF*)  
    ; RETURN LDepth 
    END StopSkipping

; PROCEDURE PutBwd ( File : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. Catch OSError.E.
     2. Conditionally do nothing when skipping. 
  *)

  = BEGIN
      IF SkipDepth <= 0
      THEN
        TRY
          FM3Compress . PutBwd ( File , ValueL ) 
        EXCEPT OSError . E ( EMsg )
        => FatalArr
             ( ARRAY OF REFANY
                 { "Unable to write to readbaci file: "
(*TODO: Give RdBackFile a "Filename" function,, then insert it here. *) 
                 , ALOSE ( EMsg ) , "."  
                 }
             ) 
        END (*EXCEPT*) 
      END (*IF*)
    END PutBwd

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

    (* Create build files for the unit. *) 
    ; LUnitRef ^ . UntBuildDirPath
        := LSrcFilePath & "/" & FM3Globals . BuildDirRelPath
(* TODO: Use Pathname to construct paths so works in Windows too. *) 
    ; LUnitRef ^ . UntPatchStackName
        := SrcFileName & FM3Globals . PatchStackSuffix   
    ; LUnitRef ^ . UntUnnestStackName
        := SrcFileName & FM3Globals . UnnestStackSuffix   
    ; LUnitRef ^ . UntParsePassName
        := SrcFileName & FM3Globals . ParsePassSuffix   
    ; TRY (*EXCEPT*)
        (* Heh, heh.  Only code the exception handler once for all files. *) 
        LFullFileName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntUnnestStackName
               , NIL
               )
      ; LUnitRef ^ . UntUnnestStackRdBack
          := RdBackFile . Create ( LFullFileName , Truncate := TRUE )
          
      ; LFullFileName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntPatchStackName
               , NIL
               ) 
      ; LUnitRef ^ . UntPatchStackRdBack
          := RdBackFile . Create ( LFullFileName , Truncate := TRUE )
      
      ; LFullFileName
          := Pathname . Join
               ( LUnitRef ^ . UntBuildDirPath 
               , LUnitRef ^ . UntParsePassName
               , NIL
               )
      ; LUnitRef ^ . UntParsePassRdBack
          := RdBackFile . Create ( LFullFileName , Truncate := TRUE )
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
      | RdBackFile . Preexists
      => FatalArr
           ( ARRAY OF REFANY
               { "Unable to open build file \""
               , LFullFileName
               , "\", already exists and is nonempty."
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

    ; LUnitRef ^ . UntCharsAtomDict
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

(* TODO: Consistify "Depth" in ideentifiers with "Length" ih RdBackFile. *)  
  ; VAR LUnnDepthL , LPpDepthL : LONGINT
  ; VAR LLengthL : LONGINT
  ; VAR LUnnestFullFileName , LUnnestFullCopyName : TEXT 
  ; VAR LParsePassFullFileName , LParsePassFullCopyName : TEXT 
  ; VAR LUnnestFailed , LParsePassFailed : BOOLEAN

  ; BEGIN
      IF SkipDepth > 0 THEN RETURN END (*IF*) 
    ; LUnitRef := OpenUnit ( SrcFileName ) 
    ; FM3Units . PushUnit ( LUnitRef ) 
    ; FM3Units . UnitStackTopRef := LUnitRef
(* TODO ^ Replace uses of this by FM3Units . UnitStackTopRef. *) 
    ; Log ( "Compiling " , SrcFileName , "..." )
    ; TRY 
        LUnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *)

      (* Unnest stack before building ParsePass file. *) 
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkRightEnd , LONGINT )
          )
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      ; LUnnDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
      ; LUnnestFailed := FALSE 
      ; FM3Parser . CloseFM3Parser ( )
(*TODO ^ Do this sometime later. *) 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      ELSE
(*TODO: get the exception name here and put into the messages later. *) 
        LUnnDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkRightEndIncomplete , LONGINT )
          )
      ; LUnnestFailed := TRUE 
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
    ; IF LUnnestFailed OR FM3CLArgs . DoDisAsmUnnest
      THEN (* Disassemble it now. *) 
        DisAsm ( LUnitRef , LUnnestFullFileName )
      ; TRY FS . DeleteFile ( LUnnestFullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF LUnnestFailed 
      THEN 
        FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure pushing unnest stack at depth "
               , Fmt . LongInt ( LUnnDepthL ) 
               }
           )
(*TODO: Get exception name, etc. and put in these FatalError messages. *) 
      ; RAISE FM3SharedUtils . FatalError ( "Failure pushing unnest stack." )
      END (*IF*) 

    (* Now build the ParsePass RdBack. *) 
    ; TRY 
        Unnest ( LUnitRef ^ . UntParsePassEmptyCoord ) 
      ; PutBwd
          ( LUnitRef ^ . UntParsePassRdBack
          , VAL ( Itk . ItkLeftEnd , LONGINT )
          )
      ; PutBwd
          ( LUnitRef ^ . UntParsePassRdBack
          , VAL ( Itk . ItkEOF , LONGINT )
          )
      ; LPpDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack )
      ; LParsePassFailed := FALSE 
      EXCEPT
      | FM3SharedUtils . Terminate ( Arg )
      => (*Re-*) RAISE FM3SharedUtils . Terminate ( Arg ) 
      ELSE 
(*TODO: get the exception name here and put into the messages later. *) 
        LPpDepthL := RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack )
      ; PutBwd
          ( LUnitRef ^ . UntParsePassRdBack
          , VAL ( Itk . ItkLeftEndIncomplete , LONGINT )
          )
      ; LParsePassFailed := TRUE 
      ; IF NOT LUnnestFailed AND NOT FM3CLArgs . DoDisAsmUnnest  
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

    ; IF LParsePassFailed OR FM3CLArgs . DoDisAsmParsePass
      THEN (* Disassemble it now. *) 
        DisAsm ( LUnitRef , LParsePassFullFileName )
      ; TRY FS . DeleteFile ( LParsePassFullCopyName )
        EXCEPT OSError . E => (* It didn't exist. *) 
        END (*EXCEPT*) 
      END (*IF*)
    ; IF LParsePassFailed 
      THEN 
        FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Failure writing parse pass at depth "
               , Fmt . LongInt ( LPpDepthL ) 
               }
           )
      ; RAISE FM3SharedUtils . FatalError ( "Failure writing parse pass." )
      END (*IF*)

    (* Close first pass. *) 
    ; LUnitRef . UntParsePassResult := 0 
    ; EVAL FM3Scanner . PopState ( )
(* TODO^ Maybe do this elsewhere. *) 

    (* Finish with patch stack. *) 
    ; LUnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( LUnitRef ^ . UntPatchStackRdBack ) 
    ; Log ( "Patch stack " , LUnitRef ^ . UntPatchStackName , " peak size = "
          , FM3Base . Int64Image  ( LUnitRef ^ . UntMaxPatchStackDepth ) , "."  
          ) 
    ; IF NOT RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack )
             <= LUnitRef ^ . UntPatchStackEmptyCoord 
      THEN
        LUnitRef . UntParsePassResult := 1 
      ; Log ( "Patch stack " , LUnitRef ^ . UntPatchStackName
            , " final size = "
            , FM3Base . Int64Image
                ( RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack ) )
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
      (* No point in keeping the patch stack. It has pogo-sticked and now is empty. *) 
    ; TRY FS . DeleteFile ( LUnitRef ^ . UntPatchStackName )
      EXCEPT OSError . E => (* It didn't exist. *) 
      END (*EXCEPT*) 

    (* Finish with unnest stack. *) 
    ; LUnitRef ^ . UntMaxUnnestStackDepth
        := RdBackFile . MaxLengthL ( LUnitRef ^ . UntUnnestStackRdBack )
    ; Log ( "Unnest stack " , LUnitRef ^ . UntUnnestStackName , " peak size = "
          , FM3Base . Int64Image  ( LUnitRef ^ . UntMaxUnnestStackDepth ) , "." 
          )
    ; LLengthL := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
    ; RdBackFile . Close (  LUnitRef ^ . UntUnnestStackRdBack , - 1L )
    ; IF LLengthL # LUnitRef ^ . UntUnnestStackEmptyCoord 
      THEN
        LUnitRef . UntParsePassResult := 2 
      ; Log ( "Unnest stack " , LUnitRef ^ . UntUnnestStackName
            , " final size = "
            , FM3Base . Int64Image ( LLengthL ) 
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
    ; Log ( "Parse pass output file "
          , LUnitRef ^ . UntParsePassName , " has " 
          , FM3Base . Int64Image 
              ( RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack ) ) 
          , " bytes."
          )
    ; PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkLeftEnd , LONGINT ) 
        )
    ; PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkEOF , LONGINT ) 
        )

    ; RdBackFile . Close ( LUnitRef ^ . UntParsePassRdBack , - 1L )
    (* ^Don't, later when next pass is implemented. *) 

(* TODO: display code point counts. *)

    ; Log ( "Finished compiling " , SrcFileName , "." )
    ; LPoppedUnitRef := FM3Units . PopUnit ( )
    ; <* ASSERT LPoppedUnitRef = LUnitRef *> 
      FM3Units . UnitStackTopRef := LUnitRef
    END CompileUnit
    
(*EXPORTED*)
; PROCEDURE Run ( )

  = VAR LSrcFileName : TEXT

  ; BEGIN
      LSrcFileName := FM3CLArgs . SrcFileName
    ; SkipDepth := 0 
    ; CompileUnit ( LSrcFileName ) 
    END Run


(* ---------------------------- Unnest stack ------------------------ *) 

(*EXPORTED:*)
; PROCEDURE CheckUnitEndId
    ( READONLY StartAttr : tParsAttribute 
    ; VAR EndAttr : tParsAttribute 
    ; UnitKind : FM3Units . UnitKindTyp
    )
  = BEGIN 
      IF StartAttr . Scan . SaAtom # FM3Base . AtomNull
         AND EndAttr . Scan . SaAtom # FM3Base . AtomNull
         AND EndAttr . Scan . SaAtom # StartAttr . Scan . SaAtom 
      THEN
        FM3Messages . ErrorArr
          ( ARRAY OF REFANY 
              { "Identifier at end of "
              , FM3Units . UnitKindImage ( UnitKind )
              , " "
              , FM3Units . TextOfIdAtom ( StartAttr . Scan . SaAtom )
              , ", at " 
              , PosImage ( StartAttr . Scan . Position ) 
              , " must repeat its name ("
              , FM3Units . UnitKindSectionNo ( UnitKind )
              , ")." 
              } 
          )
      (* Just in case it's used from the right end. *) 
      ; EndAttr . Scan . SaAtom := StartAttr . Scan . SaAtom 
      ; EndAttr . Scan . SaArgValue := StartAttr . Scan . SaArgValue 
      ; EndAttr . Scan . SaHash := StartAttr . Scan . SaHash 
      ; EndAttr . Scan . SaChars := StartAttr . Scan . SaChars 
      END (*IF*) 
    END CheckUnitEndId

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
    ; Position : FM3Scanner . tPosition
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
; PROCEDURE Push_LP ( T : Itk . TokTyp ; Position : FM3Scanner . tPosition )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_LP

(*EXPORTED:*)
; PROCEDURE Push_LIP
    ( T : Itk . TokTyp ; I : INTEGER ; Position : FM3Scanner . tPosition )

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
   ( T : Itk . TokTyp ; C : LONGINT ; Position : FM3Scanner . tPosition )

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
    ; Position : FM3Scanner . tPosition 
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
    ; Position : FM3Scanner . tPosition 
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
   ; PositionLt : FM3Scanner . tPosition
   ; CEins : LONGINT
   ; PositionEins : FM3Scanner . tPosition
   ; PositionRt : FM3Scanner . tPosition
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
   ; PL : FM3Scanner . tPosition
   ; Ce : LONGINT
   ; Pe : FM3Scanner . tPosition
   ; Cz : LONGINT
   ; Pz : FM3Scanner . tPosition
   ; Pr : FM3Scanner . tPosition
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
; PROCEDURE Push_LCPeCprp
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; CInfix : LONGINT 
   ; PositionInfix : FM3Scanner . tPosition
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
   ; PositionOne : FM3Scanner . tPosition
   ; PositionRt : FM3Scanner . tPosition
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
   ; PositionOne : FM3Scanner . tPosition
   ; PositionRt : FM3Scanner . tPosition
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
; PROCEDURE PushEXPORTSMain  ( READONLY Position : FM3Scanner . tPosition )

  = BEGIN (*PushEXPORTSMain *)
      PushUnnest ( Position . Column ) 
    ; PushUnnest ( Position . Line ) 
    ; PushUnnest ( 1 (* ElemCt *) ) 
    ; PushUnnest ( Itk . ItkExportIdListLt )
    
    ; PushUnnest ( Position . Column ) 
    ; PushUnnest ( Position . Line ) 
    ; PushUnnest ( FM3Predefined . AtomMain ) 
    ; PushUnnest ( Stk . StkIdent )
    
    ; PushUnnest ( Position . Column ) 
    ; PushUnnest ( Position . Line ) 
    ; PushUnnest ( 1 (* ElemCt *) )  
    ; PushUnnest ( Itk . ItkExportIdListRt )
    END PushEXPORTSMain

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
    ; VarLabel [ Dkt . DkValueFormal ] := "VALUE formal"
    ; VarLabel [ Dkt . DkVarFormal ] := "VAR formal"
    ; VarLabel [ Dkt . DkROFormal ] := "READONLY formal"
    ; VarLabel [ Dkt . DkRecField ] := "Record field"
    ; VarLabel [ Dkt . DkObjField ] := "Object field"

    ; VarSection [ Dkt . DkVar ] := "(2.4.3)"
    ; VarSection [ Dkt . DkValueFormal ] := "(2.2.8)"
    ; VarSection [ Dkt . DkVarFormal ] := "(2.2.8)"
    ; VarSection [ Dkt . DkROFormal ] := "(2.2.8)"
    ; VarSection [ Dkt . DkRecField ] := "(2.2.4)"
    ; VarSection [ Dkt . DkObjField ] := "(2.2.9)"
    END InitVarInfo 

(*EXPORTED.*)
; PROCEDURE RequireTypeAndOrValue 
    ( Position : FM3Scanner . tPosition
    ; HasType : BOOLEAN 
    ; HasValue : BOOLEAN
    )

  (* Anything that requires a type and/or value: 
     variable , formal, field.  Gets DeclKind from FM3Decls.TopDeclInfo. *) 

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
           );
       END (*WITH*) 
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
(* Rework or eliminate: *) 
  = BEGIN
      LHSAttr . PaInt := ElemNo 
    (* Right token: *) 
    ; PushUnnest ( ElemNo )
    ; PushUnnest ( TokLt + Itk . LtToRt )
    (* Left token, to bemoved leftward: *) 
    ; PushUnnest ( ElemNo )
    ; PushUnnestLong ( PatchCoord ) (*Patch*)
    ; PushUnnest ( TokLt + Itk . LtToPatch )
    END MakeElem

(*EXPORTED:*)
; PROCEDURE MakeList
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY ElemsAttr : tParsAttribute 
    )

  = BEGIN
      LHSAttr . PaInt := ElemsAttr . PaInt
    ; LHSAttr . PaUnnestCoord := ElemsAttr . PaUnnestCoord (* Ever used? *) 
    ; PushUnnest ( ElemsAttr . PaInt )
    ; PushUnnest ( TokLt + Itk . LtToRt )
    ; PushUnnest ( ElemsAttr . PaInt )
    ; PushUnnestLong ( ElemsAttr . PaUnnestCoord ) 
    ; PushUnnest ( TokLt + Itk . LtToPatch )
    END MakeList

(*EXPORTED:*)
; PROCEDURE MakeListPos
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; Position : FM3Scanner . tPosition
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
; PROCEDURE MakeListPatch
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; PatchCoord : LONGINT
    ; ElemCt : INTEGER 
    )

  = BEGIN
      LHSAttr . PaInt := ElemCt 
    ; LHSAttr . PaUnnestCoord := PatchCoord (* Ever used? *)
    ; PushUnnest ( ElemCt )
    ; PushUnnest ( TokLt + Itk . LtToRt )
    ; PushUnnest ( ElemCt )
    ; PushUnnestLong ( PatchCoord ) (*Patch*)
    ; PushUnnest ( TokLt )
    END MakeListPatch

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
    ( Atom : FM3Base . AtomTyp ; Pos : FM3Base . tPosition ) 

  = BEGIN (*Import*)
    END Import
      
(*EXPORTED:*)
; PROCEDURE FromImport
    ( IntfAtom : FM3Base . AtomTyp
    ; InftPos : FM3Base . tPosition
    ; DeclAtom : FM3Base . AtomTyp
    ; DeclPos : FM3Base . tPosition
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

; PROCEDURE RereverseOpnds
    ( OpndCt : INTEGER ; FromRdBack , ToRdBack : RdBackFile . T )
  (* Copy up to 6 operands, without final reversing.  Pop/Push reverses
     them, but this procedure does its own reversal, resulting in
     net same order.
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  
  ; BEGIN (*RereverseOpnds*)
      IF OpndCt >= 1 
      THEN
        LOpnd1 := FM3Compress . GetBwd ( FromRdBack )
      ; IF OpndCt >= 2
        THEN
          LOpnd2 := FM3Compress . GetBwd ( FromRdBack )
        ; IF OpndCt >= 3
          THEN
            LOpnd3 := FM3Compress . GetBwd ( FromRdBack )
          ; IF OpndCt >= 4
            THEN
              LOpnd4 := FM3Compress . GetBwd ( FromRdBack )
            ; IF OpndCt >= 5
              THEN
                LOpnd5 := FM3Compress . GetBwd ( FromRdBack )
              ; IF OpndCt >= 6
                THEN
                  LOpnd6 := FM3Compress . GetBwd ( FromRdBack )
                ; PutBwd ( ToRdBack , LOpnd6 ) 
                END (*IF*) 
              ; PutBwd ( ToRdBack , LOpnd5 ) 
              END (*IF*) 
            ; PutBwd ( ToRdBack , LOpnd4 ) 
            END (*IF*) 
          ; PutBwd ( ToRdBack , LOpnd3 ) 
          END (*IF*) 
        ; PutBwd ( ToRdBack , LOpnd2 ) 
        END (*IF*)
(* EXPANDME: For now, treat OpndCt < 0 as zero. *) 
      ; PutBwd ( ToRdBack , LOpnd1 ) 
      END (*IF*)

    END RereverseOpnds

; VAR GDoCopy := TRUE
(* TODO: ^Make this a command line option. *)
  
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
  ; VAR LPosition : FM3Base . tPosition
  ; VAR LDeclKind : FM3Decls . DeclKindTyp 

  ; BEGIN
      LUnitRef := FM3Units . UnitStackTopRef
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

        (* Move a modified token from the patch stack to the output. *) 
          <*ASSERT LUnnestCoord = LUnitRef . UntPatchStackTopCoord
                   (* Haven't missed a patch stack token. *)
          *> 
          LPatchTokenL
            := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchToken := VAL ( LPatchTokenL , Itk . TokTyp ) 
        ; <*ASSERT
              IntSets . IsElement
                ( LPatchToken , FM3SharedGlobals . GTokSetPatch )
          *>
          LPatchedToken := LPatchToken - Itk . LtToPatch
(* FIXME: The patch operation can apply to any non-Rt token.  I think
          the necessary bias is always the same as LtToPatch, but check
          this and then use a better name for it.
*) 
          
         (* Copy up to 6 operands, reversing them to coueract the reversal
            accomplished by stack operations. *)
        ; RereverseOpnds
            ( FM3Utils . TokenOpndCt ( LPatchedToken ) 
            , LPatchRdBack
            , LParsePassRdBack
            ) 

          (* Put the patched token. *)
        ; LPatchedTokenL := VAL ( LPatchedToken , LONGINT ) 
        ; PutBwd ( LParsePassRdBack , LPatchedTokenL )

        (* Conceptually finish popping the Patch stack by caching the
           next patch coordinate, which is on top of its token.
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
                 (* New cached top coordinate. *) 
          ; RereverseOpnds 
              ( FM3Utils . TokenOpndCt ( LToken ) 
              , LUnnestRdBack
              , LPatchRdBack
              ) 
          ; PutBwd ( LPatchRdBack , LTokenL )
            (* ^Push the token code deeper than its patch coordinate. *) 
          ELSE
            CASE LToken OF (* Specially handled tokens. *) 
            
            | Itk . ItkScopeRt 
            => LScopeNo := GetBwdScopeNo ( LUnnestRdBack ) 
              ; ScopeRtR2L ( LScopeNo )
              
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
          ItkVALUEFormalIdListElem. *) 
              
            | Itk . ItkIdRefAtom 
            => LAtom := GetBwdAtom ( LUnnestRdBack )
              ; LPosition := GetBwdPos ( LUnnestRdBack )
              ; EVAL IdentRefR2L ( LAtom , LPosition )

            | Itk . ItkBlockRt
            => LScopeNo := GetBwdScopeNo ( LUnnestRdBack ) 
              ; ScopeRtR2L ( LScopeNo  )
              ; RereverseOpnds
                 ( 2 (*Position*), LUnnestRdBack , LParsePassRdBack )
              ; PutBwd ( LParsePassRdBack , VAL ( LScopeNo , LONGINT ) ) 
              ; PutBwd ( LParsePassRdBack , VAL ( Itk . ItkBlockRt , LONGINT ) )

            | Itk . ItkBlockLt 
            , Itk . ItkScopeLt 
            => RereverseOpnds
                 ( FM3Utils . TokenOpndCt ( LToken ) 
                 , LUnnestRdBack
                 , LParsePassRdBack
                 )
              ; PutBwd ( LParsePassRdBack , LTokenL )
              ; EVAL FM3Scopes . PopScope ( ) 

            ELSE (* Move directly, unnest to the output.*)
              RereverseOpnds
                ( FM3Utils . TokenOpndCt ( LToken )
                , LUnnestRdBack
                , LParsePassRdBack
                ) 
            ; PutBwd ( LParsePassRdBack , LTokenL )
            END (*CASE*) 
          END (*IF*) 
        (* And loop *)           
        END (*IF*)

      END (*LOOP*)
    END Unnest

(*EXPORTED.*)
; PROCEDURE SnapshotUnnestStack (  )

  = BEGIN (*SnapshotUnnestStack*)
    END SnapshotUnnestStack

; PROCEDURE LookupId
    ( READONLY Scope : FM3Scopes . ScopeTyp
    ; IdAtom : FM3Base . AtomTyp
    ; VAR (*OUT*) DeclNo : FM3Base . DeclNoTyp 
    )
  (* PRE: IdAtom in in Scope's dictionary. *) 

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
          DeclNo := LDeclNoInt (* Implied NARROW. *) 
        ; RETURN
        ELSE LMsg := ", not found."
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
             ) 
      END (*IF*) 
    END LookupId

(* ----------------------------- Scopes ---------------------------- *)

(* These are called by the parser: *) 

(*EXPORTED.*)
; PROCEDURE ScopeEmpty ( ScopeKind : FM3Scopes . ScopeKindTyp )

  = BEGIN (*ScopeEmpty*)
    END ScopeEmpty

(* Left-to-right scope handling.  These are called by the parser. *)

(*EXPORTED.*)
; PROCEDURE ScopeLtL2R
    ( ScopeKind : FM3Scopes . ScopeKindTyp ; Position : FM3Base . tPosition )
  : FM3Base . ScopeNoTyp (* ScopeNo that was created. *) 

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LNewScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      LUnitRef := FM3Units . UnitStackTopRef 
    ; WITH WunRdBack = LUnitRef ^ . UntUnnestStackRdBack
      DO 
        LNewScopeRef := FM3Scopes . NewScopeRef ( LUnitRef ) 
      ; LNewScopeRef ^ . ScpOwningUnitNo := LUnitRef ^ . UntUnitNo 
      ; LNewScopeRef ^ . ScpKind := ScopeKind
      ; LNewScopeRef ^ . ScpPosition := Position
      ; FM3Scopes . PushScope ( LNewScopeRef ) 
      ; RETURN LNewScopeRef . ScpScopeNo
      END (*WITH*) 
    END ScopeLtL2R

(*EXPORTED.*)
; PROCEDURE DeclIdL2R 
    ( DeclIdTok : Itk . TokTyp 
    ; READONLY IdAttribute : tParsAttribute
    ; SepTok : Itk . TokTyp := Itk . ItkNull
                            (* ^Implies single decl id, not in a list. *)  
    ; READONLY SepPosition : FM3Scanner . tPosition := FM3Base . PositionNull 
    ; PriorIdCt : INTEGER := 0 (* Number of ids to left of this one. *)
    )
  : BOOLEAN (* Use this declared id.  (It's not predefined and not a duplicate
               in current scope.) *)
  (* PRE: IdAttribute is for an identifier in a declaration context. *) 

  = VAR LTokToPut : Itk . TokTyp
  ; VAR LResult : BOOLEAN

  ; BEGIN (*DeclIdL2R*)
      WITH WScope = FM3Scopes . ScopeStackTopRef ^
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
          ; LResult := TRUE (* Caller, Use this decl id. *)
          ELSE (* A Duplicate declaration of SaAtom in current scope. *)
            WScope . ScpDuplDeclIdSet
              := IntSets . Include
                   ( WScope . ScpDuplDeclIdSet , IdAttribute . Scan . SaAtom )
          (* Plan to push duplicate Ident token: *) 
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
        ; PutBwd ( WunRdBack , VAL ( LTokToPut , LONGINT ) )
        ; RETURN LResult
        END (*IF*)
      END (*WITH*) 
    END DeclIdL2R

(*EXPORTED.*)
; PROCEDURE IdentRefL2R ( READONLY IdAttribute : tParsAttribute ) 

  = VAR LTokToPut : Itk . TokTyp

  ; BEGIN (*IdentRefL2R*)
      WITH WScan = IdAttribute . Scan
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack 
      DO IF WScan . SaIsReservedId
        THEN LTokToPut := Itk . ItkIdReserved 
        ELSE
          WITH WIdentRefSet = FM3Scopes . ScopeStackTopRef ^ . ScpRefIdSet
          DO WIdentRefSet := IntSets . Include ( WIdentRefSet , Itk . ItkIdRefAtom )
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
; PROCEDURE ScopeRtL2R ( ScopeNo : FM3Scopes . ScopeNoTyp )
  (* Create an IdAtom-to-declNo, fixed-size dictionary for the scope, of
     exactly the needed size, and load it up with mappings of the idents
     declared in the scope, using a contiguously-numbered range of DeclNos.
  *) 

  = VAR SrtDeclNo : INTEGER 

  ; BEGIN (*ScopeRtL2R*)
      WITH WScope = FM3Scopes . ScopeStackTopRef ^
      DO (* Block*) 
        VAR LDeclCt : INTEGER
      ; VAR LExpectedToDeclNo : INTEGER 
    
      ; PROCEDURE SrtVisit ( DeclIdAtomI : INTEGER )
        (* PRE: DeclIdAtomI IN FM3Base . AtomTyp. *)
        = BEGIN
            FM3Dict_Int_Int . InsertFixed
              ( WScope . ScpDeclDict
              , DeclIdAtomI
              , FM3Base . HashNull
              , SrtDeclNo
              )
          ; INC ( SrtDeclNo ) 
          END SrtVisit
          
      ; BEGIN (* Block. *)
          <* ASSERT WScope . ScpScopeNo = ScopeNo *> 
          IF WScope . ScpKind = FM3Scopes . ScopeKindTyp . SkExports
          THEN
(*COMPLETEME: Handle getting exported interfaces here. *) 
          END (*IF*)
        ; LDeclCt := IntSets . Card ( WScope . ScpDeclIdSet )
        (* LDeclCt is exactly the needed dictionary size. *)
        ; WScope . ScpDeclDict 
            := FM3Dict_Int_Int . NewFixed 
                 ( LDeclCt , FM3SharedUtils . IntHash )
        ; SrtDeclNo := FM3Units . AllocateDeclNos ( LDeclCt )
        ; LExpectedToDeclNo := SrtDeclNo + LDeclCt 
        ; IntSets . ForAllDo ( WScope . ScpDeclIdSet , SrtVisit )
        ; <*ASSERT SrtDeclNo = LExpectedToDeclNo *> 
          TRY FM3Dict_Int_Int . FinalizeFixed ( WScope . ScpDeclDict )
          EXCEPT FM3Dict_Int_Int . Error ( EMsg )
          => FatalArr
               ( ARRAY OF REFANY
                 { "Finalizing Scope at "
                 , PosImage ( WScope . ScpPosition )
                 , EMsg
                 , "." 
                 }
               ) 
          END (*EXCEPT*)
        END (*Block*)
      END (*WITH*) 
    ; EVAL FM3Scopes . PopScope ( )   
    END ScopeRtL2R

(* Right-to-left scope handling.  These are called during unnesting. *)
(* Call sites read the Itk and its args, and pass in the args. *) 

; PROCEDURE ScopeRtR2L ( ScopeNo : FM3Base . ScopeNoTyp )

  = VAR LScopeMap : FM3Scopes . ScopeMapTyp 
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      LScopeMap := FM3Units . UnitStackTopRef ^ . UntScopeMap 
    ; LScopeRef := VarArray_Int_Refany . Fetch ( LScopeMap , ScopeNo )
    ; FM3Scopes . PushScope ( LScopeRef ) 
    END ScopeRtR2L

; PROCEDURE DuplDeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Base . DeclNoTyp
  (* Append a temporary, pseudo-decl node to the linked list rooted at
     the the decl number.  The position of the original declaration of
     the ident is not known right now.
  *) 

  = PROCEDURE Visit ( DeclNoI : INTEGER ; VAR DeclRefany : REFANY )
    (* PRE: DeclNoI IN FM3Base . DeclNoTyp *)  
    (* PRE: DeclRefany <: FM3Decls . DeclRefTyp. *) 
    = VAR LDeclRef : FM3Decls . DeclRefTyp 
    ; BEGIN
        LDeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . ScopeStackTopRef , DeclNoI )
      ; LDeclRef . DclLink := DeclRefany (* Implied NARROW. *) 
      ; LDeclRef . DclSelfScopeRef := FM3Scopes . ScopeStackTopRef (* Why not? *)
      ; LDeclRef . DclIdAtom := DeclIdAtom 
      ; LDeclRef . DclDeclNo := DeclNoI 
      ; LDeclRef . DclPos := Position 
      ; LDeclRef . DclKind := FM3Decls . DeclKindTyp . DkDuplDecl
      ; DeclRefany := LDeclRef 
      END Visit

  ; BEGIN (* DuplDeclIdR2L *) 
      VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block. *)
        LookupId
          ( FM3Scopes . ScopeStackTopRef ^ , DeclIdAtom , (*OUT*) LDeclNo ) 
      ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
        VarArray_Int_Refany . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , Visit )
      ; RETURN LDeclNo
      END (* Block. *) 
    END DuplDeclIdR2L
    
; PROCEDURE DeclIdR2L
    ( DeclKind : FM3Decls . DeclKindTyp
    ; DeclIdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition
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
      ; LParentScopeRef := FM3Scopes . ScopeStackTopRef 
      ; IF LDeclRef # NIL
        THEN  (* Some duplicate decls of DeclNoI were found. *) 
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

      ; LDeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . ScopeStackTopRef , DeclNoI )
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
      VAR LDeclNo : FM3Base . DeclNoTyp
    ; BEGIN (* Block *)
        WITH WppRdBack = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        DO 
          LookupId
            ( FM3Scopes . ScopeStackTopRef ^ , DeclIdAtom , (*OUT*) LDeclNo ) 
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          VarArray_Int_Refany . CallbackWithElem 
            ( FM3Units . UnitStackTopRef ^ . UntDeclMap , LDeclNo , VisitDecl )
        ; PutBwd ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Itk . ItkDeclNo , LONGINT ) )
(* FIXME: Put a token for the Id. *) 
        ; RETURN LDeclNo
        END (*WITH*) 
      END (* Block *) 
    END DeclIdR2L

; PROCEDURE IdentRefR2L
    ( IdentRefAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Base . DeclNoTyp 

  = VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*IdentRefR2L*)
      WITH WScope = FM3Scopes . ScopeStackTopRef ^  
           , WppRdBack
             = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack 
      DO
        IF IntSets . IsElement ( IdentRefAtom , WScope . ScpDeclIdSet )
        THEN (* Decl'd in this scope.  Replace Id with DeclNo. *) 
          LookupId ( WScope , IdentRefAtom , (*OUT*) LDeclNo )
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          PutBwd ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Itk . ItkIdRefDeclNo , LONGINT ) )  
        ; RETURN LDeclNo
        ELSE (* Leave as-is. *)
          PutBwd ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( IdentRefAtom , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Itk . ItkIdRefAtom , LONGINT ) )  
        ; RETURN FM3Base . DeclNoNull 
        END (*IF*)
      END (*WITH*) 
    END IdentRefR2L

(* ----------------------- Procedure signatures --------------------- *)


; BEGIN (*FM3ParsePass*)
    InitVarInfo ( )
  END FM3ParsePass
.

