        
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

; IMPORT FS 
; IMPORT OSError
; IMPORT Pathname 
; IMPORT UniRd 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3CLArgs 
; IMPORT FM3Decls 
; IMPORT FM3Files
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; FROM FM3Messages IMPORT Info , Fatal , Log
; IMPORT FM3Parser
; IMPORT FM3Scanner
; IMPORT FM3Scopes
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

; PROCEDURE OpenUnit ( FileName : TEXT ) : FM3Units . UnitRefTyp

  = VAR LFullFileName : TEXT 
  ; VAR LPathPrefix : TEXT 
  ; VAR LUniRd : UniRd . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN
      LPathPrefix := Pathname . Prefix ( FileName )
    ; LUniRd
        := FM3Files . OpenUniRd
             ( LPathPrefix , FileName , " source file " , NIL ) 
    ; LUnitRef := FM3Units . New ( )
    ; LUnitRef ^ . UntSrcFileName := FileName 
    ; LUnitRef ^ . UntSrcFilePrefix := LPathPrefix  
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
           ( "Unable to open " , LFullFileName , ": " , ALOSE ( EMsg) , "." ) 
      | RdBackFile . Preexists
      => Fatal
           ( "Unable to open "
           , LFullFileName
           , ", already exists and is nonempty."
           )
      END (*EXCEPT*)
 
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

    ; FM3Scanner . PushState ( LUniRd , LUnitRef ) 
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
      Info ( "Compiling " , SrcFileName , "..." ) 
    ; LUnitRef := OpenUnit ( SrcFileName )
    ; LUnitRef . UntStackLink := FM3Globals . CurrentUnitRef
    ; FM3Globals . CurrentUnitRef := LUnitRef 
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

; BEGIN
    Run ( ) 
  END FM3ParsePass
.

