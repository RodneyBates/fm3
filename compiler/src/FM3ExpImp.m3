
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3ExpImp

; IMPORT Pathname 
; IMPORT Text
; IMPORT TextWr
; IMPORT Wr

; IMPORT FM3ExpImpRef  
; IMPORT IntSets

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3Compile
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int 
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3Scanner
; IMPORT FM3Scopes (* For Revelation of ScopeRefTyp *) 
; IMPORT FM3SharedUtils 
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT VarArray_Int_ExpImpRef  
; IMPORT VarArray_Int_Refany  

; TYPE Us = FM3Units . UnitStateTyp 

; PROCEDURE ReportCyclic
    ( UnitRef : FM3Units . UnitRefTyp ; Position : FM3Base . tPosition )
  (* PRE: UnitRef is both the first-visited and last-visited in the cycle. *)

  = VAR LWrT : TextWr . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LNextUnitRef : FM3Units . UnitRefTyp

  ; BEGIN
      LUnitRef := UnitRef (* Start at bottom. *)  
    ; LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "Cyclic import of" )
    ; LNextUnitRef := NIL 
    ; LOOP 
        Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
      ; Wr . PutText
          ( LWrT
          , Pathname . Join
              ( LUnitRef ^ . UntSrcFilePath
              , LUnitRef ^ . UntSrcFileSimpleName
              )
          )
      ; IF LNextUnitRef # NIL AND LUnitRef = UnitRef 
        THEN (* At the the starting unit for the second time. *) 
          Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText
            ( LWrT
            , FM3Utils . PositionImage ( LUnitRef . UntPositionOfImport )
            )
        ; Wr . PutText ( LWrT , ", imports" )
        ; LNextUnitRef := LUnitRef . UntUnitRefImporting 
        ; LUnitRef . UntUnitRefImporting := NIL  
        ; LUnitRef ^ . UntInCycle := TRUE
        ; LUnitRef := LNextUnitRef 
        END (*IF*) 
      END (*LOOP*) 

    ; FM3Messages . ErrorArr
        ( ARRAY OF REFANY { TextWr . ToText ( LWrT ) }
        , Pos := Position 
        ) 
    END ReportCyclic

(*EXPORTED*) 
; PROCEDURE GetInterface
    ( IdentChars : FM3OpenArray_Char . T
    ; Position : FM3Base . tPosition
      (* ^Of the EXPORTS or IMPORT directive's identifier in the
          current unit.
      *) 
    ; IsExport : BOOLEAN
    )
  : FM3Units . UnitRefTyp
    (* ^The interface unit that was [ex/im]ported, possibly NIL *)
  (* If not already done, compile or load the interface named by IdentChars. *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp 
  ; VAR LSrcFileName : TEXT 
  ; VAR LAdjective : TEXT 

  ; BEGIN
      IF IdentChars = NIL THEN RETURN NIL END (*IF*)
    ; IF NUMBER ( IdentChars ^ ) = 0 THEN RETURN NIL END (*IF*)
    ; LSrcFileName
        := Pathname . Join
             ( NIL , Text . FromChars ( IdentChars ^ ) , "i3" ) 
    ; LIntfUnitRef := FM3Compile . UnitOfFileName ( LSrcFileName )  
    ; IF LIntfUnitRef . UntState = Us . UsNull 
      THEN (* Haven't seen this unit yet. *)
      (* Compile it. *)
(*TODO: Or load it. *)
        IF IsExport
        THEN LAdjective := "exported "
        ELSE LAdjective := "imported " 
        END (*IF*) 

      (* Compare this to similar code in FM3Compile.CompileOrLoadCLUnit. *) 
      ; IF FM3Compile . FindAndOpenUnitSrcFile
             ( LIntfUnitRef , Adjective := LAdjective )
           AND LIntfUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
        THEN
          FM3Units . UnitStackTopRef ^ . UntUnitRefImporting := LIntfUnitRef
          (*^ To detect future cyclic imports. *) 
        ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport := Position
          (* ^For possible cyclic-imports message. *) 
        ; LIntfUnitRef ^ . UntState := Us . UsImporting 
        ; FM3Units . PushUnit ( LIntfUnitRef )
        ; FM3Units . CacheTopUnitValues ( )
          (* SetUnitLog will have to wait until Pass1.InitPass1 has
             created the WrT. *) 
        ; FM3Compile . CompileUnitFromSrc ( LIntfUnitRef ) 
        ; FM3Units . UnitStackTopRef ^ . UntUnitRefImporting := NIL 
        ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport
            := FM3Base . PositionNull 
        ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitRef *>
          FM3Messages . SetUnitLog ( LIntfUnitRef ^ . UntLogWrT ) 
        ; FM3Units . CacheTopUnitValues ( ) 
     (* ELSE it wasn't found. *)
        (* Let LIntfUnitRef remain in UntState = Us . UsNull to suppress
           cascaded error messages.
        *)
        END (*IF*) 
      ELSE (* This unit already exists. *) 
        IF LIntfUnitRef . UntUnitRefImporting # NIL 
        THEN (* Cyclic imports/exports. *)
          LIntfUnitRef ^ . UntState := Us . UsNotUsable 
        ; ReportCyclic  ( LIntfUnitRef , Position )
        END (*IF*)
      END (*IF*)
    ; RETURN LIntfUnitRef 
    END GetInterface

; PROCEDURE CheckDuplicateImport
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; IdentChars : FM3Atom_OAChars . KeyTyp
    ; FromPosition : FM3Base . tPosition
    ; IntoUnitRef : FM3Units . UnitRefTyp
    ; IntoIdentAtom : FM3Base . AtomTyp 
    ; ImportPosition : FM3Base . tPosition
    )
  : BOOLEAN (* Check passed. *) 

  = VAR LPrevUnitRef : FM3Units . UnitRefTyp
  ; VAR LPrevImportExpImpRef : FM3ExpImpRef . T 

  ; BEGIN 
      IF IntSets . IsElement
           ( IntoIdentAtom , IntoUnitRef ^ . UntExpImpIdSet )
      THEN (* Importing a duplicate identifier. *)
        LPrevImportExpImpRef
          := VarArray_Int_ExpImpRef . Fetch
               ( IntoUnitRef . UntExpImpMap , IntoIdentAtom )
      ; IF LPrevImportExpImpRef . EirUnitNo # FM3Base . UnitNoNull
        THEN (* And it's useable, so it's a true duplicate. *)
          LPrevUnitRef (* Implicit NARROW. *) 
            := VarArray_Int_Refany . Fetch
                 ( FM3Units . UnitsMap
                 , LPrevImportExpImpRef . EirImportingUnitNo 
                 ) 
        ; FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Duplicate import of \""
                , IdentChars
                , "\", previously imported at "
                , LPrevUnitRef ^ . UntUnitIdent
                , ":" 
                , FM3Utils . PositionImage
                    ( LPrevImportExpImpRef . EirImportingUnitPosition )
                , ", declared at "
                , FromUnitRef ^ . UntUnitIdent
                , ":" 
                , FM3Utils . PositionImage ( FromPosition ) 
                , ", (2.5.1)"
                , Wr . EOL 
                } 
            , ImportPosition 
            )
        ; RETURN FALSE
        END (*IF*)
      END (*IF*)
      (* We handle declarations (not imports) only later, so there is no need
         to check for a same-named declaration.
      *)
    ; RETURN TRUE 
    END CheckDuplicateImport 

(*EXPORTED.*)
; PROCEDURE ImportDeclByNo
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; FromUnitDeclNo : FM3Base . DeclNoTyp
    ; Position : FM3Base . tPosition
      (* ^Of the EXPORTS or IMPORT directive's identifier. *) 
    ; <*UNUSED*> IsExport : BOOLEAN 
    )
  : BOOLEAN (* Success. *)
  (* PRE: FromUnitDeclNo leads to a DeclRef in FromUnitRef^. *)

  = VAR LFromUnitDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LRemoteDeclRef : FM3ExpImpRef . T 

  ; BEGIN
      LFromUnitDeclRef (* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FromUnitRef . UntDeclMap , FromUnitDeclNo )  
    ; <* ASSERT LFromUnitDeclRef # NIL *>
      LIntoUnitRef := FM3Units . UnitStackTopRef 
    ; <* ASSERT
           FM3Atom_OAChars . Key
             ( FromUnitRef ^ . UntIdentAtomDict
             , LFromUnitDeclRef ^ . DclIdAtom
             , (*OUT*) LIdentChars
             )
      *>
      LIntoIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( LIntoUnitRef ^ . UntIdentAtomDict
             , LIdentChars
             , FM3Utils . HashNull
             )

    ; IF CheckDuplicateImport
           ( FromUnitRef
           , LIdentChars
           , LFromUnitDeclRef ^ . DclPos 
           , LIntoUnitRef
           , LIntoIdentAtom
           , Position
           )
      THEN (* all is legal, so do the real import. *)
        LRemoteDeclRef . EirUnitNo := FromUnitRef ^ . UntUnitNo 
      ; LRemoteDeclRef . EirDeclNo  := FromUnitDeclNo
      ; LRemoteDeclRef . EirImportingUnitNo := LIntoUnitRef ^ . UntUnitNo 
      ; LRemoteDeclRef . EirImportingUnitPosition := Position
      ; LIntoUnitRef ^ . UntExpImpIdSet
          := IntSets . Include 
               ( LIntoUnitRef ^ . UntExpImpIdSet , LIntoIdentAtom )
      ; VarArray_Int_ExpImpRef . Assign
          ( LIntoUnitRef . UntExpImpMap , LIntoIdentAtom , LRemoteDeclRef )
      ; RETURN TRUE
      ELSE RETURN FALSE 
      END (*IF*) 
    END ImportDeclByNo 

(*EXPORTED.*)
; PROCEDURE ImportDeclByIdent
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; READONLY IdScanAttribute : FM3Scanner . tScanAttribute
      (* ^Containing info about the to-be-imported identifier. *) 
    )
  : BOOLEAN (* Success. *) 

  = VAR LFromDeclDict : FM3Dict_Int_Int . FixedTyp 
  ; VAR LFromDeclIdSet : IntSets . T
  ; VAR LNote : TEXT 
  ; VAR LFromAtom : FM3Base . AtomTyp
  ; VAR LFromDeclNoInt : INTEGER 
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LRemoteDeclRef : FM3ExpImpRef . T 
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      IF FromUnitRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF NOT FromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN FALSE 
      END (*IF*) 
    ; IF IdScanAttribute . SaChars = NIL THEN RETURN FALSE END (*IF*)
    ; LFromAtom (* Lookup in the remote unit. *) 
        := FM3Atom_OAChars . LookupKey  
             ( FromUnitRef ^ . UntIdentAtomDict
             , IdScanAttribute . SaChars 
             , IdScanAttribute . SaHash 
             )
    ; IF LFromAtom = FM3Base . AtomNull
      THEN (* The ident is not in the from-interface at all. *)
        LFound := FALSE
      ELSE
        LFromDeclIdSet := FromUnitRef ^ . UntDeclScopeRef ^ . ScpDeclIdSet 
      ; LFromDeclDict := FromUnitRef ^ . UntDeclScopeRef ^ . ScpDeclDict
      ; IF NOT IntSets . IsElement ( LFromAtom , LFromDeclIdSet )
        THEN (* It's not in the from-interface's top declaration scope. *)
          LFound := FALSE 
        ELSE 
          LFound
            := FM3Dict_Int_Int . LookupFixed
                 ( LFromDeclDict
                 , LFromAtom
                 , FM3Base . HashNull
                 , (*OUT*) LFromDeclNoInt
                 )
        END (*IF*) 
      END (*IF*)
    ; IF NOT LFound
      THEN
        IF IntSets . IsElement ( LFromAtom , FromUnitRef ^ . UntExpImpIdSet )
        THEN (* But it's imported. *) 
          LNote
            := FM3SharedUtils . CatArrT
                 ( ARRAY OF REFANY
                     { FM3Messages . NLIndent
                     , "(It's known via export/import,"
                     , " but those are not transitively importable (2.5.1).)"
                     }
                 ) 
        ELSE LNote := NIL 
        END (*IF*) 
      ; FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Interface "
              , FromUnitRef ^ . UntUnitIdent 
              , " has no declaration named \""
              , IdScanAttribute . SaChars 
              , "\"" 
              , LNote
              , Wr . EOL 
              }
          , IdScanAttribute . Position 
          )
          
      (* Insert an import that is not useable, but duplicatable. *)
      ; LIntoUnitRef := FM3Units . UnitStackTopRef
      (* If there's already a non-useable there, this will just overlay it,
         changing only the position, which will probably be unused anyway.
      *) 
      ; LIntoIdentAtom
          := FM3Atom_OAChars . MakeAtom
               ( LIntoUnitRef ^ . UntIdentAtomDict
               , IdScanAttribute . SaChars
               , IdScanAttribute . SaHash
               )
      ; LRemoteDeclRef . EirUnitNo := FM3Base . UnitNoNull
      (* ^Makes it present but not useable. *) 
      ; LRemoteDeclRef . EirImportingUnitNo := LIntoUnitRef ^ . UntUnitNo  
      ; LRemoteDeclRef . EirDeclNo := FM3Base . DeclNoNull 
      ; LRemoteDeclRef . EirImportingUnitPosition := IdScanAttribute . Position
      ; LIntoUnitRef ^ . UntExpImpIdSet
          := IntSets . Include 
               ( LIntoUnitRef ^ . UntExpImpIdSet , LIntoIdentAtom )
      ; VarArray_Int_ExpImpRef . Assign
          ( LIntoUnitRef . UntExpImpMap , LIntoIdentAtom , LRemoteDeclRef )
      ; RETURN FALSE 
      ELSE
        RETURN
          ImportDeclByNo
            ( FromUnitRef
            , LFromDeclNoInt
            , IdScanAttribute . Position
            , IsExport := FALSE
            ) 
      END (*IF*) 
    END ImportDeclByIdent

(*EXPORTED.*)
; PROCEDURE ImportIntfASIdent
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; READONLY ASScanAttribute : FM3Scanner . tScanAttribute
      (* ^Containing info about the to-be-imported AS identifier. *) 
    )

  = VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LRemoteDeclRef : FM3ExpImpRef . T 

  ; BEGIN
      IF FromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF NOT FromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN
      END (*IF*) 
    ; IF ASScanAttribute . SaChars = NIL THEN RETURN END (*IF*)
    ; LIntoUnitRef := FM3Units . UnitStackTopRef 
    ; LIntoIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( LIntoUnitRef ^ . UntIdentAtomDict
             , ASScanAttribute . SaChars
             , ASScanAttribute . SaHash
             )
    ; IF CheckDuplicateImport
           ( FromUnitRef
           , ASScanAttribute . SaChars 
           , FromUnitRef ^ .UntUnitIdentPos 
           , LIntoUnitRef
           , LIntoIdentAtom
           , ASScanAttribute . Position
           )
      THEN 
        LRemoteDeclRef . EirUnitNo := FromUnitRef . UntUnitNo  
      ; LRemoteDeclRef . EirDeclNo  := FM3Base . DeclNoNull 
      ; LRemoteDeclRef . EirImportingUnitNo := LIntoUnitRef ^ . UntUnitNo 
      ; LRemoteDeclRef . EirImportingUnitPosition := ASScanAttribute . Position
      ; LIntoUnitRef ^ . UntExpImpIdSet
          := IntSets . Include 
               ( LIntoUnitRef ^ . UntExpImpIdSet , LIntoIdentAtom )
      ; VarArray_Int_ExpImpRef . Assign
          ( LIntoUnitRef . UntExpImpMap , LIntoIdentAtom , LRemoteDeclRef )
      END (*IF*) 
    END ImportIntfASIdent

(*EXPORTED.*)
; PROCEDURE ImportAllDecls
    ( FromUnitRef :  FM3Units . UnitRefTyp
    ; READONLY Position : FM3Base . tPosition
    )

  = BEGIN
      IF FromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF NOT FromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN
      END (*IF*) 
    ; WITH WScopeRef = FromUnitRef ^ . UntDeclScopeRef
      DO IF WScopeRef = NIL THEN RETURN END (*IF*)
      ; FOR RDeclNo := WScopeRef ^ . ScpMinDeclNo
            TO WScopeRef ^ . ScpMinDeclNo + WScopeRef ^ . ScpDeclCt - 1
        DO EVAL ImportDeclByNo
             ( FromUnitRef 
             , RDeclNo 
             , Position (* Of the EXPORTS directive's identifier. *) 
             , IsExport := TRUE
             )
        END (*FOR*)
      END (*WITH*) 
    END ImportAllDecls 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
