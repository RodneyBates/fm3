        
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
   5. Build a compact dictionary of identifier atoms for each scope.
   6. Build a global table of atom-accessed records for compilation units,
   7. For each unit, build atom-accessed records for identifiers, scopes,
      and declarations.
   8. Resolve some identifier occurrences to decl atoms and insert these
      into the token stream.
*)

; IMPORT UniRd 

; IMPORT FM3CLArgs 
; IMPORT FM3Files 
; IMPORT FM3Globals 
; FROM Messages IMPORT FATAL
; IMPORT FM3Parser
; IMPORT FM3Scanner 
; IMPORT RdBackFile

; FROM FM3SharedGlobals
  IMPORT FM3TagLt , FM3TagRtBwd , FM3TagRdBackLt , FM3TagRdBackRt 

; CONST LeftTagLen
    = BYTESIZE ( FM3TagLeft )
    + BYTESIZE ( FM3TagRdBackBOF )
    + BYTESIZE ( TagVersion )

; VAR CurrentUnitRef : Units , UnitRefTyp

; PROCEDURE FindFilePath ( FileName : TEXT ; Why : TEXT ) : TEXT

  = BEGIN
      RETURN "./"
    END FindFilePath

; PROCEDURE OpenUnit ( FileName : T$XT ; Why : TEXT ) : Units . UnitRefTyp

  = VAR LPathName : TEXT 
  ; VAR LUniRd : UniRd . T 

  ; BEGIN
      LPathName := FindFilePath ( FileName , Why )
    ; LUniRd
        := FM3Files . OpenUniRd
             ( LPathName , FileName , " source file " , Why ) 
    ; FM3Scanner . Push ( LUniRd , LUnitRef ) 
    ; LUnitRef := FM3Units . New ( )
    ; LUnitRef ^ . UntSrcFileName := FileName 
    ; LUnitRef ^ . UntSrcFilePath := LPathName  
    ; LUnitRef ^ . UntWorkFilePath :=  
    ; LUnitRef ^ . UntPatchStackName
        := FileName & FM3Globals. PatchStackSuffix   
    ; LUnitRef ^ . UntUnnestStackName
        := FileName & FM3Globals. UntUnnestStackSuffix   
    ; LUnitRef ^ . UntParsePassName
        := FileName & FM3Globals. ParsePassSuffix   
    ; TRY
        LFileName
          := LUnitRef ^ . UntWorkFilePath & FM3Globals . PathSep
             & LUnitRef ^ . UntPatchStackName 
      ; LUnitRef ^ . UntPatchStackRdBack
          := RdBackFile . Create ( LFileName , Truncate := TRUE )
      ; LFileName
          := LUnitRef ^ . UntWorkFilePath & FM3Globals . PathSep
             & LUnitRef ^ . UntUnnestStackName 
      ; LUnitRef ^ . UntUnnestStackRdBack
          := RdBackFile . Create ( LFileName , Truncate := TRUE )
      ; LFileName
          := LUnitRef ^ . UntWorkFilePath & FM3Globals . PathSep
             & LUnitRef ^ . UntParsePassName 
      ; LUnitRef ^ . UntParsePassRdBack
          := RdBackFile . Create ( LFileName , Truncate := TRUE )
      EXCEPT
      | OSError . E ( EMsg ) 
      => FATAL ( "Unable to open " , LFileName , ": OSError.E(" , EMsg , ").") 
      | RdBackFile . Preexists
      => FATAL
           ( "Unable to open "
           , LFileName
           , ", already exists and is nonempty."
           )
      END (*EXCEPT*)
    ; LUnitRef ^ . UntMaxPatchStackDepth := 0
    ; LUnitRef ^ . UntMaxUnnestStackDepth := 0

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
        := FM3Atom_OAChars . New
             ( FM3Globals . WCharsInitAtomSize
             , StartAtom := FM3Globals . FirstRealAtom
             , HashFunc := FM3Utils . HashOfOAWChars
             )

    ; LUnitRef ^ . UntScopes := Scopes . NewMap ( )  
    ; LUnitRef ^ . UntDecls := Decls . NewMap

    ; FM3Scanner . PushState ( LUniRd , LUnitRef ) 
    END OpenUnit

; PROCEDURE CloseUnit ( UnitRef : FM3Units . UnitRefTyp )

  = BEGIN
      FM3Scanner . PopState ( )
    ; IF NOT RdBackFile . IsEmpty ( UnitRef ^ . UntPatchStackRdBack )
      THEN
        Info
          ( "Patch stack " , UnitRef ^ . UntPatchStackName , " is not empty." ) 
      END (*IF*) 
    ; RdBackFile . Close (  UnitRef ^ . UntPatchStackRdBack ) 
    ; IF NOT RdBackFile . IsEmpty ( UnitRef ^ . UntUnnestStackRdBack )
      THEN
        Info
          ( "Patch stack " , UnitRef ^ . UntUnnestStackName , " is not empty." ) 
      END (*IF*) 
    ; RdBackFile . Close (  UnitRef ^ . UntUnnestStackRdBack )
    ; Info ( "Parse pass output file "
           , UnitRef ^ . UntParsePassName
           , " has " 
           , FM3Base . Int64Image 
               ( RdBAckFule . LengthL ( UnitRef ^ . UntParsePassRdBack) ) 
           , " bytes."
           ) 
    ; RdBackFile . Close (  UnitRef ^ . UntParsePassRdBack )
(* TODO: More stats: max sizes of stacks, code point counts. *) 
    END CloseUnit 

; PROCEDURE CompileUnit ( SrcFileName : TEXT )

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; BEGIN
      Info ( "Compiling " , SrcFileName , "..." ) 
    ; LUnitRef := OpenUnit ( SrcFileName , " compiler temp " )
    ; FM3Parser . Parser ( )
    ; FM3Parser . CloseParser ( )
    ; CloseUnit ( LUnitRef ) 
    ; Info ( "Finished compiling " , SrcFileName , "..." ) 
    END CompileUnit

; PROCEDURE Run ( )

  = VAR LSrcFileName : TEXT

  ; BEGIN
      LSrcFileName := FM3CLArgs . SrcFileName
    ; CompileUnit ( LsrcFileName ) 
    END Run 

; BEGIN
    Run ( ) 
  END FM3ParsePass
.

