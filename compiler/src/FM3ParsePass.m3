
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
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3IntToks AS Itk
; FROM FM3IntToks IMPORT LtToRt , LtToPatch , LtToOnePatch
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

; VAR GSkipDepth : CARDINAL := 0
  (* Zero means we are not skipping *) 

(*EXPORTED*)
; PROCEDURE StartSkipping ( ) : CARDINAL (* depth after. *)

  = BEGIN
      INC ( GSkipDepth )
    ; RETURN GSkipDepth 
    END StartSkipping

(*EXPORTED*)
; PROCEDURE StopSkipping ( ) : CARDINAL (* depth before. *)
  (* Let's let callers check expected depth, so a failure will
     be detected at the call site.
  *)

  = VAR LDepth : CARDINAL

  ; BEGIN
      LDepth := GSkipDepth
    ; IF LDepth > 0 THEN DEC ( GSkipDepth ) END (*IF*)  
    ; RETURN LDepth 
    END StopSkipping

; PROCEDURE PutBwd ( File : RdBackFile . T ; ValueL : LONGINT )
  (* Wrap FM3Compress . PutBwd
     1. catch OSError.E.
     2. conditionally do nothing when skipping. 
  *)

  = BEGIN
      IF GSkipDepth <= 0
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

; PROCEDURE OpenUnit ( FileName : TEXT ) : FM3Units . UnitRefTyp

  = VAR LFullFileName : TEXT 
  ; VAR LPathPrefix : TEXT 
  ; VAR LUniRd : UniRd . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN
    (* Open source file. *) 
      LPathPrefix := Pathname . Prefix ( FileName )
    ; LUniRd
        := FM3Files . OpenUniRd
             ( FileName , LPathPrefix , "source file " , NIL ) 
    ; LUnitRef := FM3Units . NewUnitRef ( )
    ; LUnitRef ^ . UntSrcFileName := FileName 
    ; LUnitRef ^ . UntSrcFilePrefix := LPathPrefix
    
    (* Create the log output file. A pure text file. *) 
    ; LUnitRef ^ . UntLogName := FileName & UnitLogSuffix
    ; LUnitRef ^ . UntWorkFilePrefix := FM3Globals . BuildDirName  

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

    (* Create the three readback files. *) 
    ; LUnitRef ^ . UntWorkFilePrefix := FM3Globals . BuildDirName  
(* TODO: Provide a path here. *) 
    ; LUnitRef ^ . UntPatchStackName
        := FileName & FM3Globals . PatchStackSuffix   
    ; LUnitRef ^ . UntUnnestStackName
        := FileName & FM3Globals . UnnestStackSuffix   
    ; LUnitRef ^ . UntParsePassName
        := FileName & FM3Globals . ParsePassSuffix   
    ; TRY (*EXCEPT*)
        (* Heh, heh.  Only code the exception handler once for all 3 files. *) 
        LFullFileName
          := Pathname . Join
               ( LUnitRef ^ . UntWorkFilePrefix 
               , LUnitRef ^ . UntUnnestStackName
               , NIL
               )
      ; LUnitRef ^ . UntUnnestStackRdBack
          := RdBackFile . Create ( LFullFileName , Truncate := TRUE )
          
      ; LFullFileName
          := Pathname . Join
               ( LUnitRef ^ . UntWorkFilePrefix 
               , LUnitRef ^ . UntPatchStackName
               , NIL
               ) 
      ; LUnitRef ^ . UntPatchStackRdBack
          := RdBackFile . Create ( LFullFileName , Truncate := TRUE )
      
      ; LFullFileName
          := Pathname . Join
               ( LUnitRef ^ . UntWorkFilePrefix 
               , LUnitRef ^ . UntParsePassName
               , NIL
               )
      ; LUnitRef ^ . UntParsePassRdBack
          := RdBackFile . Create ( LFullFileName , Truncate := TRUE )
      EXCEPT
      | OSError . E ( EMsg ) 
      => FatalArr
           ( ARRAY OF REFANY
               { "Unable to open source file \""
               , LFullFileName
               , "\": "
               , ALOSE ( EMsg)
               , "."
               } 
           ) 
      | RdBackFile . Preexists
      => FatalArr
           ( ARRAY OF REFANY
               { "Unable to open source file \""
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
    ; LUnitRef ^ . UntUnnestStackEmpty
        := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )
    ; LUnitRef ^ . UntMaxUnnestStackDepth
        := LUnitRef ^ . UntUnnestStackEmpty

    ; PutBwd
        ( LUnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkBOF , LONGINT ) )
    ; PutBwd
        ( LUnitRef ^ . UntPatchStackRdBack , VAL ( Itk . ItkLeftEnd , LONGINT ) )
    ; LUnitRef ^ . UntPatchStackEmpty
        := RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack )
    ; LUnitRef ^ . UntMaxPatchStackDepth
        := LUnitRef ^ . UntPatchStackEmpty
    ; LUnitRef . UntPatchStackTopCoord := LUnitRef . UntUnnestStackEmpty
    ; LUnitRef . UntPatchStackTopToken := VAL ( Itk . ItkNull , LONGINT )  

    ; PutBwd
        ( LUnitRef ^ . UntParsePassRdBack , VAL ( Itk . ItkBOF , LONGINT ) ) 
    ; PutBwd
        ( LUnitRef ^ . UntParsePassRdBack , VAL ( Itk . ItkRightEnd , LONGINT ) )
    ; LUnitRef ^ . UntParsePassEmpty
        := RdBackFile . LengthL ( LUnitRef ^ . UntParsePassRdBack )  

    (* Create unit data structures. *) 
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

; PROCEDURE DeleteFile ( PathPrefix , FileName : TEXT ) 

  = BEGIN 
      TRY  
        FS . DeleteFile ( Pathname . Join ( PathPrefix , FileName , NIL ) ) 
      EXCEPT OSError . E ( EMsg )
      => Log ( "Unable to remove " , FileName , ": " , ALOSE ( EMsg ) , "." 
             ) 
      END (*EXCEPT*) 
    END DeleteFile 

; PROCEDURE CloseUnit ( UnitRef : FM3Units . UnitRefTyp ) 
  = BEGIN
      UnitRef . UntParsePassResult := 0 
    ; EVAL FM3Scanner . PopState ( )
    ; UnitRef ^ . UntMaxPatchStackDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntPatchStackRdBack ) 
    ; Log ( "Patch stack " , UnitRef ^ . UntPatchStackName , " peak size = "
          , FM3Base . Int64Image  ( UnitRef ^ . UntMaxPatchStackDepth ) , "."  
          ) 
    ; TRY
        IF NOT RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack )
               <= UnitRef ^ . UntPatchStackEmpty 
        THEN
          UnitRef . UntParsePassResult := 1 
        ; Log ( "Patch stack " , UnitRef ^ . UntPatchStackName
              , " final size = "
              , FM3Base . Int64Image
                  ( RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack ) )
              ) 
        ; FatalArr
            ( ARRAY OF REFANY
                { "Patch stack is not sufficiently empty, should be "  
                , FM3Base . Int64Image ( UnitRef ^ . UntPatchStackEmpty )
                , "." 
                } 
            )
        END (*IF*)
      FINALLY 
        RdBackFile . Close (  UnitRef ^ . UntPatchStackRdBack )
      ; IF NOT FM3CLArgs . DoKeep
        THEN FS . DeleteFile ( UnitRef ^ . UntPatchStackName )
        END (*IF*)
      END (*FINALLY*)

    ; UnitRef ^ . UntMaxUnnestStackDepth
        := RdBackFile . MaxLengthL ( UnitRef ^ . UntUnnestStackRdBack )
    ; Log ( "Unnest stack " , UnitRef ^ . UntUnnestStackName , " peak size = "
          , FM3Base . Int64Image  ( UnitRef ^ . UntMaxUnnestStackDepth ) , "." 
          ) 
    ; TRY 
        IF NOT RdBackFile . LengthL ( UnitRef ^ . UntUnnestStackRdBack )
               <= UnitRef ^ . UntUnnestStackEmpty 
        THEN
          UnitRef . UntParsePassResult := 2 
        ; Log ( "Unnest stack " , UnitRef ^ . UntUnnestStackName
              , " final size = "
              , FM3Base . Int64Image
                  ( RdBackFile . LengthL ( UnitRef ^ . UntUnnestStackRdBack ) )
              ) 
        ; FatalArr
            ( ARRAY OF REFANY
                { "Unnest stack is not sufficiently empty, should be "
                , FM3Base . Int64Image ( UnitRef ^ . UntUnnestStackEmpty )
                , "."
                }
            )
        END (*IF*)
      FINALLY 
        RdBackFile . Close (  UnitRef ^ . UntUnnestStackRdBack )
      ; IF NOT FM3CLArgs . DoKeep
        THEN FS . DeleteFile ( UnitRef ^ . UntUnnestStackName )
        END (*IF*)
      END (*FINALLY*)

    ; Log ( "Parse pass output file "
          , UnitRef ^ . UntParsePassName , " has " 
          , FM3Base . Int64Image 
              ( RdBackFile . LengthL ( UnitRef ^ . UntParsePassRdBack) ) 
          , " bytes."
          ) 
    ; RdBackFile . Close (  UnitRef ^ . UntParsePassRdBack )
(* TODO: code point counts. *)
    END CloseUnit

; PROCEDURE CompileUnit ( SrcFileName : TEXT )

  = VAR LUnitRef , LPoppedUnitRef : FM3Units . UnitRefTyp

  ; BEGIN
      IF GSkipDepth > 0 THEN RETURN END (*IF*) 
    ; LUnitRef := OpenUnit ( SrcFileName ) 
    ; FM3Units . PushUnit ( LUnitRef ) 
    ; FM3Units . UnitStackTopRef := LUnitRef
(* TODO ^ Replace uses of this by FM3Units . UnitStackTopRef. *) 
    ; Log ( "Compiling " , SrcFileName , "..." ) 
    ; LUnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *) 
    ; FM3Parser . CloseFM3Parser  ( )

; Unnest ( LUnitRef ^ . UntUnnestStackEmpty ) 

    ; PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkLeftEnd , LONGINT ) 
        )
    ; PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkEOF , LONGINT ) 
        )

    ; CloseUnit ( LUnitRef ) 
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
    ; GSkipDepth := 0 
    ; CompileUnit ( LSrcFileName ) 
    END Run


(* ---------------------------- Unnest stack ------------------------ *)

(*EXPORTED:*)
; PROCEDURE UnnestCoord ( ) : LONGINT
  (* Of the current unit. *)
  
  = BEGIN
      RETURN RdBackFile . LengthL
               ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack ) 
    END UnnestCoord 

(*EXPORTED:*)
; PROCEDURE PushUnnestStk ( READONLY ParsAttr : tParsAttribute )
  (* Source token.  Some of these (in fact, probably the only ones that
     will be passed in) have arguments.
  *) 
  
  = BEGIN
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
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  
  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
        , Value
        )
    END PushUnnestLong

(*EXPORTED:*)
; PROCEDURE PushUnnest ( Value : INTEGER )

  = BEGIN
      PutBwd
        ( FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
        , VAL ( Value , LONGINT ) 
        )
    END PushUnnest

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
; PROCEDURE Push_LCPrp
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
    END Push_LCPrp

(*EXPORTED:*)
; PROCEDURE Push_LCPeCrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; PositionLt : FM3Scanner . tPosition
   ; COne : LONGINT
   ; PositionRt : FM3Scanner . tPosition
   )

  = BEGIN
      WITH WRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( PositionRt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionRt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , COne ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionLt . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( PositionLt . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , CLt ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_LCPeCrP

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
    (* Left token, to bemoved leftward: *) 
    ; PushUnnestLong ( PatchCoord ) (*Patch*)
    ; PushUnnest ( TokLt + Itk . LtToPatch )
    END MakeConstruct

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
      LHSAttr . PaInt := ElemsAttr . PaInt
    ; LHSAttr . PaUnnestCoord := ElemsAttr . PaUnnestCoord (* Ever used? *) 
    ; PushUnnest ( Position . Column ) 
    ; PushUnnest ( Position . Line ) 
    ; PushUnnest ( ElemsAttr . PaInt ) (* Elem Ct. *)
    ; PushUnnest ( TokLt + Itk . LtToRt )
    ; PushUnnest ( Position . Column ) 
    ; PushUnnest ( Position . Line ) 
    ; PushUnnest ( ElemsAttr . PaInt )
    ; PushUnnestLong ( ElemsAttr . PaUnnestCoord ) 
    ; PushUnnest ( TokLt + Itk . LtToPatch )
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
; PROCEDURE BeginBlock ( )

  = BEGIN (*BeginBlock*)
    END BeginBlock

; PROCEDURE RereverseOpnds
    ( Token : Itk . TokTyp ; FromRdBack , ToRdBack : RdBackFile . T )
  (* Copy up to 6 operands, without final reversing.  Pop/8ush reverses
     them, but this procedure does its own reversal, resulting in
     net same order.
  *)
 
  = VAR LOpnd1 , LOpnd2 , LOpnd3 , LOpnd4 , LOpnd5 , LOpnd6 : LONGINT
  ; VAR LOpndCt : INTEGER 
  
  ; BEGIN (*RereverseOpnds*)
      LOpndCt := FM3Utils . TokenOpndCt ( Token )
    ; IF LOpndCt >= 1 
      THEN
        LOpnd1 := FM3Compress . GetBwd ( FromRdBack )
      ; IF LOpndCt >= 2
        THEN
          LOpnd2 := FM3Compress . GetBwd ( FromRdBack )
        ; IF LOpndCt >= 3
          THEN
            LOpnd3 := FM3Compress . GetBwd ( FromRdBack )
          ; IF LOpndCt >= 4
            THEN
              LOpnd4 := FM3Compress . GetBwd ( FromRdBack )
            ; IF LOpndCt >= 5
              THEN
                LOpnd5 := FM3Compress . GetBwd ( FromRdBack )
              ; IF LOpndCt >= 6
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
(* EXPANDME: For now, treat LOpndCt < 0 as zero. *) 
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
  ; VAR LPatchTokenL , LNowPatchedTokenL , LTokenL : LONGINT 
  ; VAR LPatchToken , LNowPatchedToken , LToken : Itk . TokTyp
  ; VAR LScopeNo : FM3Base . ScopeNoTyp
  ; VAR LAtom : FM3Base . AtomTyp
  ; VAR LPosition : FM3Base . tPosition
  ; VAR LDeclKind : FM3Decls . DeclKindTyp 

  ; BEGIN
      LUnitRef := FM3Units . UnitStackTopRef
    ; LUnnestRdBack := LUnitRef . UntUnnestStackRdBack 
    ; LPatchRdBack := LUnitRef . UntPatchStackRdBack 
    ; LParsePassRdBack := LUnitRef . UntParsePassRdBack
    ; LMUnnestDepth := MAX ( LMUnnestDepth , LUnitRef . UntUnnestStackEmpty )

    ; IF GDoCopy THEN
        (* Temporarily make it look like the unnest stack is ended. *) 
        PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack
          , VAL ( Itk . ItkRightEnd , LONGINT )
          )
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack , VAL ( Itk . ItkEOF , LONGINT ) )
(* FIXME: But the copied portion will not have BOF and LeftEnd tokens. *) 
      ; RdBackFile . Copy
          ( LUnnestRdBack
          , FM3Globals . BuildDirName & "UnnestCopy"
          , LMUnnestDepth
          )
      ; EVAL GetBwd ( LUnitRef ^ . UntUnnestStackRdBack ) 
      ; EVAL GetBwd ( LUnitRef ^ . UntUnnestStackRdBack ) 
      END (*IF*)

    ; LOOP
        LUnnestCoord := RdBackFile . LengthL ( LUnnestRdBack )
      ; IF LUnnestCoord <= LMUnnestDepth
           (* ^Nothing more to pop off Unnest strack. *) 
           AND RdBackFile . LengthL ( LPatchRdBack )
               <= LUnitRef . UntPatchStackTopCoord
           (* ^ Nothing more to pop off Patch stack. *) 
        THEN EXIT
        END (*IF*)
      ; IF LUnnestCoord <= LUnitRef . UntPatchStackTopCoord
        THEN

        (* Move a modified token from the patch stack to the output. *) 
          <*ASSERT LUnnestCoord = LUnitRef . UntPatchStackTopCoord *>
          LPatchTokenL
            := FM3Compress . GetBwd ( LPatchRdBack )
        ; LPatchToken := VAL ( LPatchTokenL , Itk . TokTyp ) 
        ; <*ASSERT
              IntSets . IsElement
                ( LPatchToken , FM3SharedGlobals . GTokSetPatch )
          *>
          LNowPatchedToken := LPatchToken - Itk . LtToPatch
(* FIXME: The patch operation can apply to any non-Rt token.  I think
          the necessary bias is always the same as LtToPatch, but check
          this and then use a better name for it.
*) 
          
         (* Copy up to 6 operands, without reversing by stack operations. *)
        ; RereverseOpnds
            ( LNowPatchedToken
            , LPatchRdBack
            , LParsePassRdBack
            ) 

          (* Put the unpatched token. *)
        ; LNowPatchedTokenL := VAL ( LNowPatchedToken , LONGINT ) 
        ; PutBwd ( LParsePassRdBack , LNowPatchedTokenL )

        (* Conceptually finish popping the Patch stack by caching the
           next patch coordinate.
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
              ) (* Uncache the existing coordinate by pushing. *) 

          ; LUnitRef . UntPatchStackTopCoord
              := FM3Compress . GetBwd ( LUnnestRdBack )
                 (* New cached top coordinate. *) 
          ; RereverseOpnds
              ( LToken
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
              
            | Itk . ItkRefId
            => LAtom := GetBwdAtom ( LUnnestRdBack )
              ; LPosition := GetBwdPos ( LUnnestRdBack )
              ; EVAL RefIdR2L ( LAtom , LPosition )

            | Itk . ItkScopeLt 
            => RereverseOpnds
                 ( LToken , LUnnestRdBack , LParsePassRdBack )
              ; PutBwd ( LParsePassRdBack , LTokenL )
              ; EVAL FM3Scopes . PopScope ( ) 

            ELSE (* Move directly, unnest to the output.*)
              RereverseOpnds
                ( LToken , LUnnestRdBack , LParsePassRdBack ) 
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
      ; PutBwd ( WunRdBack , VAL ( LNewScopeRef . ScpScopeNo , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( Itk . ItkScopeLt , LONGINT ) )
      ; RETURN LNewScopeRef . ScpScopeNo
      END (*WITH*) 
    END ScopeLtL2R

(*EXPORTED.*)
; PROCEDURE DeclIdL2R ( DeclKind : FM3Decls . DeclKindTyp )
  : BOOLEAN (* Use this declared id.  It's not a duplicate. *) 

  = VAR LTokenL : LONGINT
  ; VAR LIdAtomL : LONGINT 
  ; VAR LIdAtom : FM3Base . AtomTyp

  ; BEGIN (*DeclIdL2R*)
      WITH WScope = FM3Scopes . ScopeStackTopRef ^
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack 
      DO
        (* An StkIdent, /w args, was pushed by parser shift, as it does for
           all variable terminals.  Convert it to ItkDeclId or ItkDuplDeclId.
        *) 
        LTokenL := GetBwd ( WunRdBack )
      ; <*ASSERT LTokenL = VAL ( FM3SrcToks . StkIdent , LONGINT ) *>
        LIdAtomL := GetBwd ( WunRdBack )
      ; LIdAtom := VAL ( LIdAtomL , FM3Base . AtomTyp ) 
      ; PutBwd ( WunRdBack , LIdAtomL )
      (* Each of the 3 tokens involved here has a Position argument,
         but no need to pop and repush it.
      *) 
      ; IF IntSets . IsElement ( LIdAtom , WScope . ScpDeclIdSet )
        THEN (* A duplicate declaration of LIdAtom. *)
             (* Except for this inserted Itk, skip the decl. *) 
          WScope . ScpDuplDeclIdSet
            := IntSets . Include ( WScope . ScpDuplDeclIdSet , LIdAtom )
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkDuplDeclId , LONGINT ) )
        ; RETURN FALSE 
        ELSE 
          WScope . ScpDeclIdSet
            := IntSets . Include ( WScope . ScpDeclIdSet , LIdAtom )
        ; PutBwd ( WunRdBack , VAL ( ORD ( DeclKind ) , LONGINT ) )  
        ; PutBwd ( WunRdBack , VAL ( Itk . ItkDeclId , LONGINT ) )  
        ; RETURN TRUE  
        END (*IF*)
      END (*WITH*) 
    END DeclIdL2R

(*EXPORTED.*)
; PROCEDURE RefIdL2R
    ( RefIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )

  = BEGIN (*RefIdL2R*)
      WITH WRefIdSet = FM3Scopes . ScopeStackTopRef ^ . ScpRefIdSet
           , WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack 
      DO
        WRefIdSet := IntSets . Include ( WRefIdSet , RefIdAtom )
      ; PutBwd ( WunRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( RefIdAtom , LONGINT ) ) 
      ; PutBwd ( WunRdBack , VAL ( Itk . ItkRefId , LONGINT ) ) 
      END (*WITH*) 
    END RefIdL2R

(*EXPORTED.*)
; PROCEDURE ScopeRtL2R ( )
  (* Create an IdAtom-to-declNo, fixed-size dictionary for the scope, of
     exactly the needed size, and load it up with mappings of the idents
     declared in the scope, and with a contiguously-numbered range of DeclNos.
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
          LDeclCt := IntSets . Card ( WScope . ScpDeclIdSet )
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
        ; WITH WunRdBack = FM3Units . UnitStackTopRef ^ . UntUnnestStackRdBack 
          DO 
            PutBwd ( WunRdBack , VAL ( WScope . ScpScopeNo , LONGINT ) ) 
          ; PutBwd ( WunRdBack , VAL ( Itk . ItkScopeRt , LONGINT ) )
          END (*WITH*) 
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
    ; WITH WppRdBack = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack 
      DO
        PutBwd ( WppRdBack , VAL ( ScopeNo , LONGINT ) ) 
      ; PutBwd ( WppRdBack , VAL ( Itk . ItkScopeRt , LONGINT ) )
      END (*WITH*) 
    END ScopeRtR2L

; PROCEDURE DuplDeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Base . DeclNoTyp 

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
  (* Tjhis will be the only decl of DeclIdAtom in the current scope. *) 

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
        ; RETURN LDeclNo
        END (*WITH*) 
      END (* Block *) 
    END DeclIdR2L

; PROCEDURE RefIdR2L
    ( RefIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Base . DeclNoTyp 

  = VAR LDeclNo : FM3Base . DeclNoTyp
  
  ; BEGIN (*RefIdR2L*)
      WITH WScope = FM3Scopes . ScopeStackTopRef ^  
           , WppRdBack
             = FM3Units . UnitStackTopRef ^ . UntParsePassRdBack 
      DO
        IF IntSets . IsElement ( RefIdAtom , WScope . ScpDeclIdSet )
        THEN (* Decl'd in this scope.  Replace Id with DeclNo. *) 
          LookupId ( WScope , RefIdAtom , (*OUT*) LDeclNo )
        ; <*ASSERT LDeclNo # FM3Base . DeclNoNull *>
          PutBwd ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( LDeclNo , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Itk . ItkRefNo , LONGINT ) )  
        ; RETURN LDeclNo
        ELSE (* Leave as-is. *)
          PutBwd ( WppRdBack , VAL ( Position . Column , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Position . Line , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( RefIdAtom , LONGINT ) ) 
        ; PutBwd ( WppRdBack , VAL ( Itk . ItkRefId , LONGINT ) )  
        ; RETURN FM3Base . DeclNoNull 
        END (*IF*)
      END (*WITH*) 
    END RefIdR2L

(* ----------------------- Procedure signatures --------------------- *)


; BEGIN (*FM3ParsePass*)
  END FM3ParsePass
.

