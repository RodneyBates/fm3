
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
  (* PRE: ExpImportUnitRef is both the bottom and top unit in the cycle. *)

  = VAR LWrT : TextWr . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LNextUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN
      LUnitRef := UnitRef (* Start at bottom. *)  
    ; LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "Cyclic import of" )
    ; LOOP 
        Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
      ; Wr . PutText
          ( LWrT
          , Pathname . Join
              ( LUnitRef ^ . UntSrcFilePath
              , LUnitRef ^ . UntSrcFileSimpleName
              )
          )
      ; LNextUnitRef := LUnitRef . UntUnitRefImporting 
      ; LUnitRef . UntUnitRefImporting := NIL  
      ; LUnitRef ^ . UntInCycle := TRUE 
      ; IF LUnitRef = FM3Units . UnitStackTopRef 
        THEN (* The unit containing the cycle-closing [ex/im]port clause. *) 
          Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText
            ( LWrT
            , FM3Utils . PositionImage ( LUnitRef . UntPositionOfImport )
            )
        ; Wr . PutText ( LWrT , ", imports" )
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
      (* ^Of the EXPORTS or IMPORT directive's identifier. *) 
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
             ( NIL , Text . FromChars ( IdentChars ^ ) , ".i3" ) 
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
        THEN 
          FM3Units . UnitStackTopRef ^ . UntUnitRefImporting := LIntfUnitRef
          (*^ To detect cyclic imports. *) 
        ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport := Position
          (* ^For possible cyclic-imports message. *) 
        ; LIntfUnitRef ^ . UntState := Us . UsImporting 
        ; FM3Units . PushUnit ( LIntfUnitRef )
        ; FM3Units . CacheTopUnitValues ( ) 
        ; FM3Compile . CompileUnitFromSrc ( LIntfUnitRef ) 
        ; FM3Units . UncacheTopUnitValues ( ) 
        ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitRef *>
     (* ELSE it wasn't found. *) 
        END (*IF*) 
      ELSE
        IF LIntfUnitRef . UntUnitRefImporting # NIL 
        THEN (* Cyclic imports/exports. *)
          LIntfUnitRef ^ . UntState := Us . UsNotUsable 
        ; ReportCyclic  ( LIntfUnitRef , Position )
        END (*IF*)
      END (*IF*)
    ; RETURN LIntfUnitRef 
    END GetInterface

(*EXPORTED.*)
; PROCEDURE ImportDeclByNo
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; FromUnitDeclNo : FM3Base . DeclNoTyp
    ; Position : FM3Base . tPosition
      (* ^Of the EXPORTS or IMPORT directive's identifier. *) 
    ; IsExport : BOOLEAN 
    )
  : BOOLEAN (* Success. *)
  (* PRE: FromUnitDeclNo leads to a DeclRef in FromUnitRef^. *)

  = VAR LFromUnitDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LPrevImportExpImpRef : FM3ExpImpRef . T 
  ; VAR LRemoteDeclRef : FM3ExpImpRef . T 

  ; BEGIN
      LFromUnitDeclRef
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
    ; IF IntSets . IsElement
           ( LIntoIdentAtom , LIntoUnitRef ^ . UntExpImpIdSet )
      THEN (* Importing a duplicate identifier. *)
        LPrevImportExpImpRef
          := VarArray_Int_ExpImpRef . Fetch
               ( LIntoUnitRef . UntExpImpMap , LIntoIdentAtom )
      ; IF LPrevImportExpImpRef . EirUnitNo # FM3Base . UnitNoNull
        THEN (* And it's not a no-use, so it's a true duplicate. *) 
          FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Duplicate import of \""
                , LIdentChars
                , "\", previously imported at "
                , FM3Utils . PositionImage
                    ( LPrevImportExpImpRef . EirPosition ) 
                , ", declared at "
                , FromUnitRef ^ . UntUnitIdent
                , ":" 
                , FM3Utils . PositionImage ( LFromUnitDeclRef ^ . DclPos ) 
                , ", (2.5.1)"
                , Wr . EOL 
                } 
            , Position 
            )
        ; RETURN FALSE
        END (*IF*)
      END (*IF*)
      
    (* Otherwise, all is legal, so do the real import. *)
    ; LRemoteDeclRef . EirUnitNo := FromUnitRef ^ . UntUnitNo 
    ; LRemoteDeclRef . EirDeclNo  := FromUnitDeclNo
    ; LRemoteDeclRef . EirPosition := Position
    ; LIntoUnitRef ^ . UntExpImpIdSet
        := IntSets . Include 
             ( LIntoUnitRef ^ . UntExpImpIdSet , LIntoIdentAtom )
    ; VarArray_Int_ExpImpRef . Assign
        ( LIntoUnitRef . UntExpImpMap , LIntoIdentAtom , LRemoteDeclRef )
    ; RETURN TRUE 
    END ImportDeclByNo 

(*EXPORTED.*)
; PROCEDURE ImportDeclByIdent
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; IdScanAttribute : FM3Scanner . tScanAttribute
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
    ; IF IdScanAttribute . SaChars = NIL THEN RETURN FALSE END (*IF*)
    ; LFromAtom (* In the remote unit. *) 
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
        THEN
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
          
      (* Insert an import that is not checkable, but duplicatable. *)
      ; LIntoUnitRef := FM3Units . UnitStackTopRef
      (* If there's already a non-checkable there, this will just overlay it,
         changing only the position, which will probably be unused anyway.
      *) 
      ; LIntoIdentAtom
          := FM3Atom_OAChars . MakeAtom
               ( LIntoUnitRef ^ . UntIdentAtomDict
               , IdScanAttribute . SaChars
               , IdScanAttribute . SaHash
               )
      ; LRemoteDeclRef . EirUnitNo := FM3Base . UnitNoNull 
      ; LRemoteDeclRef . EirDeclNo  := FM3Base . DeclNoNull 
      ; LRemoteDeclRef . EirPosition := IdScanAttribute . Position
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
; PROCEDURE ImportIntfByIdent
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; IdScanAttribute : FM3Scanner . tScanAttribute
      (* ^Containing info about the to-be-imported identifier. *) 
    )

  = VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LRemoteDeclRef : FM3ExpImpRef . T 

  ; BEGIN
      IF FromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF IdScanAttribute . SaChars = NIL THEN RETURN END (*IF*)
    ; LIntoUnitRef := FM3Units . UnitStackTopRef 
    ; LIntoIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( LIntoUnitRef ^ . UntIdentAtomDict
             , IdScanAttribute . SaChars
             , IdScanAttribute . SaHash
             )
    ; LRemoteDeclRef . EirUnitNo := FromUnitRef . UntUnitNo  
    ; LRemoteDeclRef . EirDeclNo  := FM3Base . DeclNoNull 
    ; LRemoteDeclRef . EirPosition := IdScanAttribute . Position
    ; LIntoUnitRef ^ . UntExpImpIdSet
        := IntSets . Include 
             ( LIntoUnitRef ^ . UntExpImpIdSet , LIntoIdentAtom )
    ; VarArray_Int_ExpImpRef . Assign
        ( LIntoUnitRef . UntExpImpMap , LIntoIdentAtom , LRemoteDeclRef )
    END ImportIntfByIdent

(*EXPORTED.*)
; PROCEDURE ImportAllDecls
    ( FromUnitRef :  FM3Units . UnitRefTyp
    ; IdScanAttribute : FM3Scanner . tScanAttribute
      (* ^Containing info about the to-be-imported identifier. *) 
    )

  = BEGIN
      IF FromUnitRef = NIL THEN RETURN END (*IF*) 
    ; WITH WScopeRef = FromUnitRef ^ . UntDeclScopeRef
      DO IF WScopeRef = NIL THEN RETURN END (*IF*)
      ; FOR RDeclNo := WScopeRef ^ . ScpMinDeclNo
            TO WScopeRef ^ . ScpMinDeclNo + WScopeRef ^ . ScpDeclCt - 1
        DO EVAL ImportDeclByNo
             ( FromUnitRef 
             , RDeclNo 
             , IdScanAttribute . Position
               (* ^Of the EXPORTS directive's identifier. *) 
             , IsExport := TRUE
             )
        END (*FOR*)
      END (*WITH*) 
    END ImportAllDecls 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
