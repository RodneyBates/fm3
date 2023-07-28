        
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
; IMPORT UniRd
; IMPORT Wr 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3CLArgs
; IMPORT FM3Compress
; IMPORT FM3Decls 
; IMPORT FM3Files
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT Info , Fatal , Log
; IMPORT FM3Parser
; IMPORT FM3Scanner
; IMPORT FM3Scopes
; IMPORT FM3SrcToks
; IMPORT FM3Units 
; IMPORT FM3Utils 
; IMPORT RdBackFile

; FROM FM3SharedGlobals IMPORT
    FM3FileTagLt , FM3FileTagRtBwd , FM3FileTagRdBackLt , FM3FileTagRdBackRt

; FROM File IMPORT Byte  

; VAR FileTagVersion := VAL ( ORD ( '1' ) , Byte )  

; CONST LeftFileTagLen
    = BYTESIZE ( FM3FileTagLt )
    + BYTESIZE ( FM3FileTagRdBackLt )
    + BYTESIZE ( FileTagVersion )

; CONST ALOSE = FM3Messages . AtomListToOSError 

(* 
; PROCEDURE FindFilePrefix ( FileName : TEXT ; Why : TEXT ) : TEXT

  = VAR LResult : TEXT
  
  ; BEGIN
      TRY
        LResult := Pathname . Prefix  ( FileName )
      EXCEPT OSError . E ( EMsg )
      => Fatal ( "Unable to get absolute path for "
               , FileName , ", " , Why , ": " , ALOSE ( EMsg ) , "."  
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
      => Wr . PutText ( Stdio . stderr , "Unable to open unit log file " ) 
      ; Wr . PutText ( Stdio . stderr , LUnitRef ^ . UntLogName ) 
      ; Wr . PutText ( Stdio . stderr , ": " ) 
      ; Wr . PutText
          ( Stdio . stderr , FM3Messages . AtomListToOSError ( EAtoms ) ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      ; Wr . PutText ( Stdio . stderr , "Will proceed without it." ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      ; Wr . Flush ( Stdio . stderr )
      ; LUnitRef ^ . UntLogWrT := NIL 
      END (*EXCEPT*)

    (* Create the three readback files. *) 
    ; LUnitRef ^ . UntWorkFilePrefix := "." 
(* TODO: Provide a path here. *) 
    ; LUnitRef ^ . UntPatchStackName
        := FileName & FM3Globals . PatchStackSuffix   
    ; LUnitRef ^ . UntUnnestStackName
        := FileName & FM3Globals. UnnestStackSuffix   
    ; LUnitRef ^ . UntParsePassName
        := FileName & FM3Globals. ParsePassSuffix   
    ; TRY
        LFullFileName
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
               , LUnitRef ^ . UntUnnestStackName
               , NIL
               )
      ; LUnitRef ^ . UntUnnestStackRdBack
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
           ( "Unable to open source file \""
           , LFullFileName , "\": " , ALOSE ( EMsg) , "."
           ) 
      | RdBackFile . Preexists
      => Fatal
           ( "Unable to open source file \""
           , LFullFileName
           , "\", already exists and is nonempty."
           )
      END (*EXCEPT*)

    (* Write file tags to the readback files.*)
(* COMPLETEME *)
    ; LUnitRef ^ . UntPatchStackEmpty
        := RdBackFile . LengthL ( LUnitRef ^ . UntPatchStackRdBack )
    ; LUnitRef ^ . UntUnnestStackEmpty
        := RdBackFile . LengthL ( LUnitRef ^ . UntUnnestStackRdBack )

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
        IF NOT RdBackFile . IsEmpty ( UnitRef ^ . UntPatchStackRdBack )
        THEN
          UnitRef . UntParsePassResult := 1 
        ; Log ( "Patch stack " , UnitRef ^ . UntPatchStackName
              , " final size = "
              , FM3Base . Int64Image
                  ( RdBackFile . LengthL ( UnitRef ^ . UntPatchStackRdBack ) )
              ) 
        ; Fatal ( "Patch stack is not empty." ) 
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
        IF NOT RdBackFile . IsEmpty ( UnitRef ^ . UntUnnestStackRdBack )
        THEN
          UnitRef . UntParsePassResult := 2 
        ; Log ( "Unnest stack " , UnitRef ^ . UntUnnestStackName
              , " final size = "
              , FM3Base . Int64Image
                  ( RdBackFile . LengthL ( UnitRef ^ . UntUnnestStackRdBack ) )
              ) 
        ; Fatal ( "Unnest stack is not empty." )
        END (*IF*)
      FINALLY 
        RdBackFile . Close (  UnitRef ^ . UntUnnestStackRdBack )
      ; IF NOT FM3CLArgs . DoKeep
        THEN FS . DeleteFile ( UnitRef ^ . UntUnnestStackName )
        END (*IF*)
      END (*FINALLY*)

    ; Info ( "Parse pass output file "
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
    ; Info ( "Compiling " , SrcFileName , "..." ) 
    ; LUnitRef ^ . UntParseResult := FM3Parser . FM3Parser ( )
(* TODO:           ^Something with this? *) 
    ; FM3Parser . CloseFM3Parser  ( )
    ; CloseUnit ( LUnitRef ) 
    ; Info ( "Finished compiling " , SrcFileName , "." )
    ; FM3Globals . CurrentUnitRef := LUnitRef . UntStackLink 
    END CompileUnit
    
(*EXPORTED*)
; PROCEDURE Run ( )

  = VAR LSrcFileName : TEXT

  ; BEGIN
      LSrcFileName := FM3CLArgs . SrcFileName
    ; CompileUnit ( LSrcFileName ) 
    END Run 

(*EXPORTED:*)
; PROCEDURE UnnestStackLen ( ) : LONGINT
  (* Of the current unit. *)
  
  = BEGIN
      RETURN RdBackFile . LengthL
               ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack ) 
    END UnnestStackLen 

(*EXPORTED:*)
; PROCEDURE PushUnnestStk ( Token : INTEGER )
  (* Source token.  Some of these (in fact, probably the only ones that
     will be passed in) have arguments.
  *) 
  
  = BEGIN
      WITH WRdBack = FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
      DO 
        FM3Compress . PutBwd ( WRdBack , VAL ( Token , LONGINT ) )
      ; IF FM3SrcToks . StkIdent <= Token
           AND Token <= FM3SrcToks . StkWideCharLit
        THEN
          FM3Compress . PutBwd
            ( WRdBack
            , VAL ( FM3Scanner . Attribute . Position . Column , LONGINT )
            )
        ; FM3Compress . PutBwd
            ( WRdBack , VAL ( FM3Scanner . Attribute . SaAtom , LONGINT ) )
        END (*IF*) 
      ; CASE Token OF
      
(* Keep DumpWork.DumpNumericBwd consistent with this:*) 
     (* | FM3SrcToks . StkIdent => *) 
        | FM3SrcToks . StkIntLit 
        , FM3SrcToks . StkLongIntLit 
        , FM3SrcToks . StkBasedLit 
        , FM3SrcToks . StkLongBasedLit
        , FM3SrcToks . StkRealLit 
        , FM3SrcToks . StkLongRealLit 
        , FM3SrcToks . StkExtendedLit 
          => PushOACharsBwd ( WRdBack , FM3Scanner . Attribute . SaChars )
        | FM3SrcToks . StkTextLit 
          => PushOACharsBwd
               ( WRdBack , FM3Scanner . Attribute . SaChars )
        | FM3SrcToks . StkWideTextLit 
          => PushOAWideCharsBwd
               ( WRdBack , FM3Scanner . Attribute . SaWideChars )
        | FM3SrcToks . StkCharLit 
        , FM3SrcToks . StkWideCharLit 
          => FM3Compress . PutBwd
               ( WRdBack
               , VAL ( ORD ( FM3Scanner . Attribute . SaWCh ) , LONGINT ) 
               )
     (* | FM3SrcToks . StkLexErrChars => Throw these away, for now. *) 
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
      DO FM3Compress . PutBwd
           ( RdBack , VAL ( ORD ( Chars ^ [ RI ] ) , LONGINT ) )
      END (*FOR*)
    ; FM3Compress . PutBwd ( RdBack , VAL ( LNumber , LONGINT ) )
    END PushOACharsBwd 
    
; PROCEDURE PushOAWideCharsBwd
    ( RdBack : RdBackFile . T ; Chars : REF ARRAY OF WIDECHAR )

  = VAR LNumber : INTEGER

  ; BEGIN
      LNumber := NUMBER ( Chars ^ ) 
    ; FOR RI := LNumber - 1 TO 0 BY - 1 
      DO FM3Compress . PutBwd
           ( RdBack , VAL ( ORD ( Chars ^ [ RI ] ) , LONGINT ) ) 
      END (*FOR*)
    ; FM3Compress . PutBwd ( RdBack , VAL ( LNumber , LONGINT ) )
    END PushOAWideCharsBwd 
    
(*EXPORTED:*)
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  
  = BEGIN
      FM3Compress . PutBwd
        ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
        , Value
        )
    END PushUnnestLong

(*EXPORTED:*)
; PROCEDURE PushUnnest ( Value : INTEGER )

  = BEGIN
      FM3Compress . PutBwd
        ( FM3Globals . CurrentUnitRef ^ . UntUnnestStackRdBack
        , VAL ( Value , LONGINT ) 
        )
    END PushUnnest 

; BEGIN
  END FM3ParsePass
.

