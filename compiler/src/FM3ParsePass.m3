        
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

; IMPORT FM3Globals 
; IMPORT FM3Parser
; IMPORT RdBackFile

; FROM FM3SharedGlobals
  IMPORT FM3TagLt , FM3TagRtBwd , FM3TagRdBackLt , FM3TagRdBackRt 

; CONST LeftTagLen
    = BYTESIZE ( FM3TagLeft )
    + BYTESIZE ( FM3TagRdBackBOF )
    + BYTESIZE ( TagVersion )

; VAR CurrentUnitRef : Units , UnitRefTyp

; PROCEDURE FindFilePath ( FileName : TEXT ; Why : TEXT ) : TEXT

; PROCEDURE OpenUnit ( FileName : T$XT ; Why : TEXT ) : Units . UnitRefTyp

  = VAR LPathName : TEXT 
  ; VAR LFullFileName : TEXT
  ; VAR LUniRd : UniRd . T 

  ; BEGIN
      LPathName := FindFileName ( FileName , Why )
    ; LFullFileName := LPathName & LFileName 
    ; LUniRd := UniRd . Open ( LFullFileName ) (* COMPLETEME *)
    ; LUnitRef := Units . New ( )
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
      => Fatal ( "Unable to open " , LFileName , ": OSError.E(" , EMsg , ").") 
      | RdBackFile . Preexists
      => Fatal
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
    END OpenUnit

(* Connect to a scanner. *) 

; BEGIN
  END FM3ParsePass
.

