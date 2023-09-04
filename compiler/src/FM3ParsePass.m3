       
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
; FROM FM3Compress IMPORT PutBwd , GetBwd
; IMPORT FM3Decls 
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3IntToks AS Itk
; FROM FM3IntToks IMPORT LtToRt , LtToPatch , LtToOnePatch 
; IMPORT FM3SrcToks AS Stk 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT Info , Fatal , Log
; IMPORT FM3Parser
; IMPORT FM3Predefined
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SrcToks
; IMPORT FM3IntToks AS Itk 
; IMPORT FM3Units 
; IMPORT FM3Utils 
; IMPORT RdBackFile

; IMPORT FM3SharedGlobals 

; FROM File IMPORT Byte  

; TYPE Dkt = FM3Decls . DeclKindTyp

; CONST PosImage = FM3Utils . PositionImage

; VAR FileTagVersion := VAL ( ORD ( '1' ) , Byte )  

; CONST LeftFileTagLen
    = BYTESIZE ( FM3SharedGlobals . FM3FileTagLt )
    + BYTESIZE ( FM3SharedGlobals . FM3FileKindRdBackLt )
    + BYTESIZE ( FileTagVersion )

; CONST ALOSE = FM3Messages . AtomListToOSError 

(* 
; PROCEDURE FindFilePrefix ( FileName : TEXT ; Why : TEXT ) : TEXT

  = VAR LResult : TEXT
  
  ; BEGIN
      TRY
        LResult := Pathname . Prefix  ( FileName )
      EXCEPT OSError . E ( EMsg )
      => Fatal
           ( ARRAY OF TEXT
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
    ; LUnitRef := FM3Units . New ( )
    ; LUnitRef ^ . UntSrcFileName := FileName 
    ; LUnitRef ^ . UntSrcFilePrefix := LPathPrefix
    
    (* Create the log output file. A pure text file. *) 
    ; LUnitRef ^ . UntLogName := FileName & UnitLogSuffix
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
      => Fatal
           ( ARRAY OF TEXT
               { "Unable to open source file \""
               , LFullFileName
               , "\": "
               , ALOSE ( EMsg)
               , "."
               } 
           ) 
      | RdBackFile . Preexists
      => Fatal
           ( ARRAY OF TEXT
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
             ( FM3Globals . IdentInitAtomSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAChars
             )

    ; LUnitRef ^ . UntNumberAtomDict
        := FM3Atom_OAChars . New
             ( FM3Globals . NumberInitAtomSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAChars
             )

    ; LUnitRef ^ . UntCharsAtomDict
        := FM3Atom_OAChars . New
             ( FM3Globals . CharsInitAtomSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAChars
             )

    ; LUnitRef ^ . UntWCharsAtomDict
        := FM3Atom_OAWideChars . New
             ( FM3Globals . WideCharsInitAtomSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAWChars
             )

    ; LUnitRef ^ . UntScopes := FM3Scopes . NewMap ( )  
    ; LUnitRef ^ . UntDecls := FM3Decls . NewMap ( )

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
        ; Fatal
            ( ARRAY OF TEXT
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
        ; Fatal
            ( ARRAY OF TEXT
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

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; BEGIN
      LUnitRef := OpenUnit ( SrcFileName )
    ; LUnitRef . UntStackLink := FM3Globals . CurrentUnitRef
    ; FM3Globals . CurrentUnitRef := LUnitRef 
    ; Log ( "Compiling " , SrcFileName , "..." ) 
    ; LUnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *) 
    ; FM3Parser . CloseFM3Parser  ( )

; Unnest ( LUnitRef ^ . UntUnnestStackEmpty ) 

    ; PutBwd
        ( FM3Globals . CurrentUnitRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkLeftEnd , LONGINT ) 
        )
    ; PutBwd
        ( FM3Globals . CurrentUnitRef ^ . UntParsePassRdBack
        , VAL ( Itk . ItkEOF , LONGINT ) 
        )

    ; CloseUnit ( LUnitRef ) 
    ; Log ( "Finished compiling " , SrcFileName , "." )
    ; FM3Globals . CurrentUnitRef := LUnitRef . UntStackLink 
    END CompileUnit
    
(*EXPORTED*)
; PROCEDURE Run ( )

  = VAR LSrcFileName : TEXT

  ; BEGIN
      LSrcFileName := FM3CLArgs . SrcFileName
    ; CompileUnit ( LSrcFileName ) 
    END Run 

(* ---------------------------- Unnest stack ------------------------ *)

(*EXPORTED:*)
; PROCEDURE UnnestCoord ( ) : LONGINT
  (* Of the current unit. *)
  
  = BEGIN
      RETURN RdBackFile . LengthL
               ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack ) 
    END UnnestCoord 

(*EXPORTED:*)
; PROCEDURE PushUnnestStk ( READONLY ParsAttr : tParsAttribute )
  (* Source token.  Some of these (in fact, probably the only ones that
     will be passed in) have arguments.
  *) 
  
  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
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
        ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
        , Value
        )
    END PushUnnestLong

(*EXPORTED:*)
; PROCEDURE PushUnnest ( Value : INTEGER )

  = BEGIN
      PutBwd
        ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
        , VAL ( Value , LONGINT ) 
        )
    END PushUnnest

(*EXPORTED:*)
; PROCEDURE Push_T ( T : Itk . TokTyp )

  = BEGIN
      PutBwd
        ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
        , VAL ( T , LONGINT ) 
        )
    END Push_T

(*EXPORTED:*)
; PROCEDURE Push_TP ( T : Itk . TokTyp ; Position : FM3Scanner . tPosition )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_TP

(*EXPORTED:*)
; PROCEDURE Push_TCr ( T : Itk . TokTyp ; C : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_TCr

(*EXPORTED:*)
; PROCEDURE Push_TCPrp
   ( T : Itk . TokTyp ; C : LONGINT ; Position : FM3Scanner . tPosition )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_TCPrp

(*EXPORTED:*)
; PROCEDURE Push_TCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( ORD ( B ) , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_TCBr

(*EXPORTED:*)
; PROCEDURE Push_TCIri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , C ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_TCIri

(*EXPORTED:*)
; PROCEDURE Push_TI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I0 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_TI3

(*EXPORTED:*)
; PROCEDURE Push_TI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I5 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I4 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I3 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I2 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I1 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I0 , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T , LONGINT ) ) 
      END (*WITH*) 
    END Push_TI6

(*EXPORTED:*)
; PROCEDURE Push_TCoCr ( T : Itk . TokTyp ; Ct , Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) )
      END (*WITH*) 
    END Push_TCoCr

(*EXPORTED:*)
; PROCEDURE Push_TCIoCri
    ( T : Itk . TokTyp ; Ct : LONGINT ; I : INTEGER ; Co : LONGINT )

  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToRt , LONGINT ) ) 
      ; PutBwd ( WRdBack , Co ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToOnePatch , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( I , LONGINT ) ) 
      ; PutBwd ( WRdBack , Ct ) 
      ; PutBwd ( WRdBack , VAL ( T + LtToPatch , LONGINT ) ) 
      END (*WITH*) 
    END Push_TCIoCri

(*EXPORTED:*)
; PROCEDURE Pop4 ( )

  = BEGIN (*Pop4*)
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
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
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
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
  ; VAR LPatchTokenL , LNoPatchTokenL , LTokenL : LONGINT 
  ; VAR LPatchToken , LNoPatchToken , LToken : Itk . TokTyp 

  ; BEGIN
      LUnitRef := FM3Globals . CurrentUnitRef
    ; LUnnestRdBack := LUnitRef . UntUnnestStackRdBack 
    ; LPatchRdBack := LUnitRef . UntPatchStackRdBack 
    ; LParsePassRdBack := LUnitRef . UntParsePassRdBack
    ; LMUnnestDepth := MAX ( LMUnnestDepth , LUnitRef . UntUnnestStackEmpty )

    ; IF GDoCopy THEN
        (* Temporarily make it look like the unnest stack is ended. *) 
        PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack , VAL ( Itk . ItkRightEnd , LONGINT ) )
      ; PutBwd
          ( LUnitRef ^ . UntUnnestStackRdBack , VAL ( Itk . ItkEOF , LONGINT ) )
(* FIXME: But the copied portion will not have BOF and LeftEnd tokens. *) 
      ; RdBackFile . Copy ( LUnnestRdBack , "UnnestCopy" , LMUnnestDepth )
      ; EVAL GetBwd ( LUnitRef ^ . UntUnnestStackRdBack ) 
      ; EVAL GetBwd ( LUnitRef ^ . UntUnnestStackRdBack ) 
      END (*IF*)

    ; LOOP
        LUnnestCoord := RdBackFile . LengthL ( LUnnestRdBack )
      ; IF LUnnestCoord <= LMUnnestDepth
           AND RdBackFile . LengthL ( LPatchRdBack )
               <= LUnitRef . UntPatchStackTopCoord 
        THEN EXIT
        END (*IF*)
      ; IF LUnnestCoord <= LUnitRef . UntPatchStackTopCoord
        THEN

        (* Move a modified token from the patch stack to the output. *) 
          <*ASSERT LUnnestCoord = LUnitRef . UntPatchStackTopCoord *>
          LPatchTokenL
            := FM3Compress . GetBwd ( LUnitRef . UntPatchStackRdBack )
        ; LPatchToken := VAL ( LPatchTokenL , Itk . TokTyp ) 
        ; <*ASSERT
              IntSets . IsElement
                ( LPatchToken , FM3SharedGlobals . GTokSetPatch )
          *>
          LNoPatchToken := LPatchToken - Itk . LtToPatch
(* FIXME: The patch operation can apply to any non-Rt token.  I think
          the necessary bias is always the same as LtToPatch, but check
          this and then use a better name for it.
*) 
          
         (* Copy up to 6 operands, without reversing by stack operations. *)
        ; RereverseOpnds
            ( LNoPatchToken
            , LUnitRef . UntPatchStackRdBack
            , LUnitRef . UntParsePassRdBack
            ) 

          (* Put the unpatched token. *)
        ; LNoPatchTokenL := VAL ( LNoPatchToken , LONGINT ) 
        ; PutBwd ( LUnitRef . UntParsePassRdBack , LNoPatchTokenL )

        (* Conceptually finish popping the Patch stack by caching the
           next patch coordinate.
        *)
        ; LUnitRef . UntPatchStackTopCoord
            := FM3Compress . GetBwd ( LUnitRef . UntPatchStackRdBack )
          
        (* Now loop, possibly for more patches. *)
        ELSE (* Look at the top token on the Unnest stack. *) 
          LTokenL := FM3Compress . GetBwd ( LUnitRef . UntUnnestStackRdBack )
        ; LToken := VAL ( LTokenL , Itk . TokTyp ) 
        ; IF IntSets . IsElement ( LToken , FM3SharedGlobals . GTokSetPatch )
          THEN 

          (* Move this token from the unnest stack to the patch stack. *)
            PutBwd
              ( LUnitRef . UntPatchStackRdBack
              , LUnitRef . UntPatchStackTopCoord
              ) (* Uncache the existing coordinate by pushing. *) 

          ; LUnitRef . UntPatchStackTopCoord
              := FM3Compress . GetBwd ( LUnitRef . UntUnnestStackRdBack )
                 (* New cached top coordinate. *) 
          ; RereverseOpnds
              ( LToken
              , LUnitRef . UntUnnestStackRdBack
              , LUnitRef . UntPatchStackRdBack
              ) 
          ; PutBwd ( LUnitRef . UntPatchStackRdBack , LTokenL )
            (* ^Push the token code deeper than its patch coordinate. *)
          ELSE

          (* Move directly, unnest to the output. There is no patch coordinate. *)
            RereverseOpnds
              ( LToken
              , LUnitRef . UntUnnestStackRdBack
              , LUnitRef . UntParsePassRdBack
              ) 
          ; PutBwd ( LUnitRef . UntParsePassRdBack , LTokenL )
          END (*IF*) 
        (* And loop *)           
        END (*IF*)

      END (*LOOP*)
    END Unnest

(*EXPORTED.*)
; PROCEDURE SnapshotUnnestStack (  )

  = BEGIN (*SnapshotUnnestStack*)
    END SnapshotUnnestStack

; PROCEDURE LookupKnownId
    ( Scope : FM3Scopes . ScopeRefTyp
    ; DeclIdAtom : FM3Base . AtomTyp
    ; VAR DeclNo : FM3Decls . DeclNoTyp 
    )
  (* PREL DeclIdAtom in in Scope's dictionary. *) 

  = VAR LMsg : TEXT
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*LookupInownId*)
      TRY
        LFound 
          := FM3Dict_Int_Int . LookupFixed
               ( Scope . ScpDeclDict , DeclIdAtom , (*OUT*) DeclNo )
      ; IF LFound
        THEN RETURN
        ELSE LMsg := ", not found."
        END (*IF*) 
      EXCEPT FM3Dict_Int_Int . Error ( EMsg )
        => LFound := FALSE
        ; LMsg := EMsg 
      END (*EXCEPT*)
    ; IF NOT LFound
      THEN 
        Fatal
          ( ARRAY OF TEXT
              { "Looking up decl of \""
              , FM3Units . TextOfIdAtom ( DeclIdAtom ) 
              , "\" in scope at "
              , PosImage ( Scope . ScpPosition ) )
              , Msg
              , "." 
              }
             ) 
      END (*IF*) 
    END LookupKnownId

(*EXPORTED.*)
; PROCEDURE ScopeEmpty ( ScopeKind : ScopeKindTyp )

  = BEGIN (*ScopeEmpty*)
    END ScopeEmpty

(*EXPORTED.*)
; PROCEDURE ScopeLtL2R
    ( ScopeKind : ScopeKindTyp
    ; Position : FM3Base . tPosition
    ; InitSize : INTEGER 
    )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LNewScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LDepth : INTEGER 

  ; BEGIN 
      LUnitRef := FM3Units . UnitsStackTop 
    ; LNewScopeRef := FM3Scopes . NewScopeRef ( LUnitRef , InitSize ) 
    ; LNewScopeRef ^ . OwningUnitRef := LUnitRef 
    ; LNewScopeRef ^ . ScpKind := ScopeKind
    ; LNewScopeRef ^ . ScpPosition := Position 
    END ScopeLtL2R

(*EXPORTED.*)
; PROCEDURE DeclIdL2R
    ( DeclIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )

  = BEGIN (*DeclIdL2R*)
      WITH WScope = FM3Scopes . ScopeStackTop ^
           , WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack 
      DO
        PutBwd ( WRdBack , VAL ( Position . Column , LONGINT ) ) 
      ; PutBwd ( WRdBack , VAL ( Position . Line , LONGINT ) ) 
      ; PutBwd ( WRdBack , DeclIdAtom ) 
      ; IF IntSets . IsElement ( DeclIdAtom , WScope . ScpDeclIdSet )
        THEN (* A duplicate declaration of DeclIdAtom. *) 
          WScope . ScpDuplIdSet
            := IntSets . Include ( WScope . ScpDuplIdSet , DeclIdAtom )
(* CHECK ^ Is this needed? *) 
        ; PutBwd ( Itk . ItkDuplDeclLt )  
        ELSE 
          WScope . ScpDeclIdSet
            := IntSets . Include ( WScope . ScpDeclIdSet , DeclIdAtom )
        ; PutBwd ( Itk . ItkDeclLt )  
        END (*IF*)
      END (*WITH*) 
    END DeclIdL2R

(*EXPORTED.*)
; PROCEDURE DuplDeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )

  = VAR DdidDeclNo : FM3Decls . DeclNoTyp

  ; PROCEDURE DdidVisit
      ( VAR DeclRef : FM3Decls . DeclRefTyp )
    = VAR LDeclRef : FM3Decls . DeclRefTyp 
    ; BEGIN
        LDeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . ScopeStackTop , DdidDeclNo )
      ; LDeclRef . Link := DeclRef
      ; LDeclRef . SelfScopeRef := NIL 
      ; LDeclRef . IdAtom := DeclIdAtom 
      ; LDeclRef . DeclNo := DdidDeclNo 
      ; LDeclRef . DeclPos := Position 
      ; LDeclRef . DclKind := Dk . DkDuplDecl
      ; DeclRef := LDeclRef 
      END DdidVisit

    BEGIN (* DuplDeclIdR2L *) 
      WITH WScope = FM3Scopes . ScopeStackTop ^ 
      DO
        LookupKnownId ( WScope , DeclIdAtom , (*OUT*) DdidDeclNo ) ) 
      ; VarArray . CallbackWithElem
          ( WScope . ScpDeclMap , DdidDeclNo , DdidVisit )
      ; RETURN DidDeclNo 
      END (*WITH*) 
    END DuplDeclIdR2L 

(*EXPORTED.*)
; PROCEDURE RefIdL2R
    ( RefIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )

  = BEGIN (*RefIdL2R*)
      WITH WRefIdSet = FM3Scopes . ScopeStackTop ^ . ScpRefIdSet
      DO WRefIdSet := IntSets . Include ( WRefIdSet , RefIdAtom )
      END (*WITH*) 
    END RefIdL2R

(*EXPORTED.*)
; PROCEDURE ScopeRtL2R ( ScopeKind : ScopeKindTyp )
  (* Create an identifier-to-declNo dictionary for the scope, of
     exactly the needed size, and load it up with DeclIdAtom to
     DeclNo mappings, using the idents declared in the scope and
     a contiguously-numbered range of DeclNos.
  *) 

  = VAR LDeclCt : INTEGER
  ; VAR LDeclNo : INTEGER 
  ; VAR LExpectedToDeclNo : INTEGER 

  ; BEGIN (*ScopeRtL2R*)
      WITH WScope = FM3Scopes . ScopeStackTop ^
      DO (* Start Block*) 
      
        PROCEDURE SrVisit ( DeclIdAtom : FM3Base . AtomTyp )
        = BEGIN
            FM3Dict_Int_Int . InsertFixed
              ( WScope . ScpDeclDict , DeclIdAtom , LDeclNo )
          ; INC ( LDeclNo ) 
          END SrVisit
          
      ; BEGIN (* Block statements. *)
          LDeclCt := IntSets . Card ( WScope . ScpDeclIdSet )
        (* LDeclCt is exactly the needed dictionary size. *)
        ; WScope . ScpDeclDict 
            := FM3Dict_Int_Int . NewFixed 
                 ( LDeclCt , FM3SharedUtils . IntHash )
        ; LDeclNo := FM3Units . AllocateDeclNos ( LDeclCt )
        ; LExpectedToDeclNo := LDeclNo + LDeclCt 
        ; IntSets . ForAllDo ( WScope . ScpDeclIdSet , SrVisit )
          <*ASSERT LDeclNo = LExpectedToDeclNo *> 
        ; TRY FM3Dict_Int_Int . FinalizeFixed ( WScope . ScpDeclDict )
          EXCEPT FM3Dict_Int_Int . Error ( EMsg )
          => Fatal
               ( ARRAY OF TEXT
                 { "Finalizing Scope at "
                 , PosImage ( WScope . ScpPosition )
                 , EMsg
                 , "'" 
                 }
               ) 
          END (*EXCEPT*) 
        END (*Block*)
      END (*WITH*) 
    END ScopeRtL2R

(*EXPORTED.*)
; PROCEDURE DeclIdR2L
    ( DeclIdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition
    ; DeclKind : Dkt
    )
  : FM3Decls . DeclNoTyp 

  = VAR DidDeclNo : FM3Decls . DeclNoTyp

  ; PROCEDURE DidVisit
      ( VAR DeclRef : FM3Decls . DeclRefTyp )
    = VAR LCharsRef : FM3Atom_OAChars . KeyTyp
    ; VAR LIdentText : TEXT

    ; BEGIN (* DidVisit *) 
        IF DeclRef # NIL
        THEN  (* Some duplicate decls exist. *)
          LIdentText := FM3Units . TextOfIdAtom ( DeclIdAtom ) 
        ; WHILE DeclRef # NIL
          DO
            ErrorArr
              ( ARRAY OF TEXT
                  { PosImage ( DeclRef . DclPosition )
                  , "Duplicate declaration of \""
                  , LIdentText
                  , "\", ignored, original at "
                  , PosImage ( Position )
                  , " ( )." 
                  } 
              )
            ; DeclRef := DeclRef ^ . DclLink
          END (*WHILE*) 
        END (*IF*)
        
      ; DeclRef
          := FM3Decls . NewDeclRef ( FM3Scopes . ScopeStackTop , DidDeclNo )
      ; DeclRef . Link := NIL 
      ; DeclRef . SelfScopeRef := NIL 
      ; DeclRef . IdAtom := DeclIdAtom 
      ; DeclRef . DeclNo := DidDeclNo 
      ; DeclRef . DeclPos := Position 
      ; DeclRef . DclKind := DeclKind  
      END DidVisit

  ; BEGIN (*DeclIdR2R*)
      WITH WScope = FM3Scopes . ScopeStackTop ^ 
      DO 
        LookupKnownId ( WScope , DeclIdAtom , (*OUT*) DidDeclNo ) ) 
      ; LDeclRef := FM3Decls . New ( WScope , DidDeclNo )
      ; VarArray . CallbackWithElem
          ( WScope . ScpDeclMap , DidDeclNo , DidVisit )
      ; RETURN DidDeclNo 
      END (*WITH*) 
    END DeclIdR2L

(*EXPORTED.*)
; PROCEDURE RefIdR2L
    ( RefIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Decls . DeclNoTyp 

  = VAR LDeclNo : FM3Decls . DeclNoTyp
  ; VAR LFound : BOOLEAN
  
  ; BEGIN (*RefIdR2L*)
      WITH WScope = FM3Scopes . ScopeStackTop ^ 
      DO IF NOT IntSets . IsElement ( WScope . ScpRefIdSet , RefIdAtom )
        THEN RETURN FM3Decls . DeclNoNull 
        ELSE LookupKnownId ( WScope , DeclIdAtom , (*OUT*) LDeclNo )
        ; RETURN LDeclNo 
        END (*IF*)
      END (*WITH*) 
    END RefIdR2L

(* ----------------------- Procedure signatures --------------------- *)


; BEGIN (*FM3ParsePass*)
  END FM3ParsePass
.

