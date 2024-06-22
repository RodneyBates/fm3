
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

; IMPORT FM3ExpImpProxy  
; IMPORT IntSets

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3Compile
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int 
; IMPORT FM3IntToks AS Itk
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3Pass1 
; IMPORT FM3Scanner
; IMPORT FM3Scopes (* For Revelation of ScopeRefTyp *) 
; IMPORT FM3SharedUtils 
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT VarArray_Int_ExpImpProxy  
; IMPORT VarArray_Int_Refany  

; TYPE Us = FM3Units . UnitStateTyp 

; PROCEDURE ReportCyclic
    ( UnitRef : FM3Units . UnitRefTyp ; Position : FM3Base . tPosition )
  (* PRE: UnitRef is both the first-visited and last-visited in the cycle.
          We have to visit it twice.
  *)

  = VAR LWrT : TextWr . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LNextUnitRef : FM3Units . UnitRefTyp

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "Cyclic import of" )
    ; LUnitRef := UnitRef (* Start at bottom. *)  
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
          Wr . PutChar ( LWrT , '.' ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText
            ( LWrT
            , FM3Utils . PositionImage ( LUnitRef ^ . UntPositionOfImport )
            )
        ; Wr . PutText ( LWrT , ", imports" )
        ; LNextUnitRef := LUnitRef ^ . UntImportingUnitRef 
        ; LUnitRef ^ . UntImportingUnitRef := NIL  
        ; LUnitRef ^ . UntInCycle := TRUE
        ; LUnitRef . UntState := Us . UsNotUsable
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
      (* ^In the current unit of the to-be [ex/im]ported identifier. *) 
    ; IsExport : BOOLEAN
    )
  : FM3Units . UnitRefTyp
    (* ^The interface unit to be [ex/im]ported, # NIL *)
  (* If not already done, compile or load the interface named by IdentChars. *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp 
  ; VAR LSrcFileName : TEXT 
  ; VAR LAdjective : TEXT 

  ; BEGIN
      IF IdentChars = NIL THEN RETURN NIL END (*IF*)
    ; IF NUMBER ( IdentChars ^ ) = 0 THEN RETURN NIL END (*IF*)
    ; LSrcFileName
        := Pathname . Join
             ( NIL
             , Text . FromChars ( IdentChars ^ )
             , FM3Base . InterfaceFileNameSuffix
             ) 
    ; LIntfUnitRef := FM3Compile . GetUnitRefOfFileName ( LSrcFileName )
    ; IF LIntfUnitRef ^ . UntState = Us . UsNull 
      THEN (* Haven't seen this unit yet. *)
      (* Compile it. *)
(*TODO: Or load it. *)
        IF IsExport
        THEN LAdjective := "exported "
        ELSE LAdjective := "imported " 
        END (*IF*) 

      (* Compare this to similar code in FM3Compile.CompileOrLoadCLUnit. *) 
      ; IF FM3Compile . FindAndOpenUnitSrcFile
             ( LIntfUnitRef , LAdjective , Position )
           AND LIntfUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
        THEN
          FM3Units . UnitStackTopRef ^ . UntImportingUnitRef
            := LIntfUnitRef
          (*^ To detect future cyclic imports. *) 
        ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport := Position
          (* ^For possible cyclic-imports message. *) 
        ; LIntfUnitRef ^ . UntState := Us . UsImporting 
        ; FM3Units . PushUnit ( LIntfUnitRef )
        ; FM3Units . CacheTopUnitValues ( )
          (* SetUnitLog will have to wait until Pass1.InitPass1 has
             created its WrT.  
             FM3Messages . SetUnitLog ( LIntfUnitRef ^ . UntLogWrT )
          *) 
        ; FM3Compile . CompileUnitFromSrc ( LIntfUnitRef ) 
        ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitRef *>
          FM3Units . UnitStackTopRef ^ . UntImportingUnitRef := NIL 
        ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport
            := FM3Base . PositionNull 
        ; FM3Messages . SetUnitLog ( FM3Units . UnitStackTopRef ^ . UntLogWrT ) 
        ; FM3Units . CacheTopUnitValues ( ) 
        ELSE (* it wasn't found. *)
          LIntfUnitRef ^ . UntState := Us . UsNotUsable 
          (* ^Suppress cascaded error messages. *)
        END (*IF*) 
      ELSE (* This unit already exists. *) 
        FM3Units . UnitStackTopRef ^ . UntImportingUnitRef
          := LIntfUnitRef
        (*^ To detect future cyclic imports. *) 
      ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport := Position
        (* ^For possible cyclic-imports message. *) 
      ; IF LIntfUnitRef ^ . UntImportingUnitRef # NIL 
        THEN (* Cyclic imports/exports. *)
          ReportCyclic  ( LIntfUnitRef , Position )
        END (*IF*)
      END (*IF*)
    ; RETURN LIntfUnitRef 
    END GetInterface

; PROCEDURE CheckDuplicateImport
    ( IntoUnitRef : FM3Units . UnitRefTyp
    ; IntoIdentAtom : FM3Base . AtomTyp 
    ; ImportPosition : FM3Base . tPosition 
    ; IsExport : BOOLEAN 
    )
  : BOOLEAN (* Check passed. *) 

  = VAR LPrevExpImpUnitRef : FM3Units . UnitRefTyp
  ; VAR LPrevDeclUnitRef : FM3Units . UnitRefTyp
  ; VAR LPrevDeclRef : FM3Decls . DeclRefTyp
  ; VAR LDirectiveText : TEXT 
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp
  ; VAR LPrevExpImpProxy : FM3ExpImpProxy . T
  ; VAR LPrevDeclPosition : FM3Base . tPosition 

  ; BEGIN 
      IF IntSets . IsElement
           ( IntoIdentAtom , IntoUnitRef ^ . UntExpImpIdSet )
      THEN (* Importing a duplicate identifier. *)
        LPrevExpImpProxy
          := VarArray_Int_ExpImpProxy . Fetch
               ( IntoUnitRef ^ . UntExpImpMap , IntoIdentAtom )
      ; IF LPrevExpImpProxy . EipUnitNo # FM3Base . UnitNoNull
        THEN (* And it's useable, so this would be a true duplicate. *)
          LPrevExpImpUnitRef (* Implicit NARROW. *) 
            := VarArray_Int_Refany . Fetch
                 ( FM3Units . UnitsMap
                 , LPrevExpImpProxy . EipImportingUnitNo 
                 )
        ; IF NOT FM3Atom_OAChars . Key 
                   ( IntoUnitRef ^ . UntIdentAtomDict
                   , IntoIdentAtom
                   , (*OUT*) LIdentChars
                   )
          THEN LIdentChars := NIL
          END (*IF*)
        ; LPrevDeclUnitRef (* Implicit NARROW. *) 
            := VarArray_Int_Refany . Fetch
                 ( FM3Units . UnitsMap
                 , LPrevExpImpProxy . EipUnitNo 
                 )
        ; IF LPrevExpImpProxy . EipDeclNo = FM3Base . DeclNoNull
          THEN LPrevDeclPosition := LPrevDeclUnitRef ^ . UntUnitIdentPos 
          ELSE
            LPrevDeclRef (*Implied NARROW*) 
              := VarArray_Int_Refany . Fetch
                   ( LPrevDeclUnitRef ^ . UntDeclMap
                   , LPrevExpImpProxy . EipDeclNo
                   )
          ; IF LPrevDeclRef = NIL THEN LPrevDeclPosition := FM3Base . PositionNull 
            ELSE LPrevDeclPosition := LPrevDeclRef . DclPos
            END (*IF*) 
          END (*IF*)
        ; IF IsExport THEN LDirectiveText := "export" 
          ELSE LDirectiveText := "import"
          END (*IF*) 
        ; FM3Messages . ErrorArr
            ( ARRAY OF REFANY
                { "Duplicate "
                , LDirectiveText 
                , " of \""
                , LIdentChars
                , "\", previously from "
                , LPrevExpImpUnitRef ^ . UntSrcFileSimpleName 
                , ":" 
                , FM3Utils . PositionImage
                    ( LPrevExpImpProxy . EipImportingUnitPosition )
                ,"," 
                , FM3Messages . NLIndent
                , "declared at "
                , LPrevDeclUnitRef ^ . UntSrcFileSimpleName 
                , ":" 
                , FM3Utils . PositionImage ( LPrevDeclPosition ) 
                , ", (2.5.1)"
                } 
            , ImportPosition 
            )
        ; RETURN FALSE
        END (*IF*)
      END (*IF*)
      (* We handle true declarations (not imports) only later, so there is
         no need to check for a same-named declaration.
      *)
    ; RETURN TRUE 
    END CheckDuplicateImport

; PROCEDURE InsertExpImp
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdentAtom : FM3Base . AtomTyp
    ; READONLY Proxy : FM3ExpImpProxy . T 
    )

  = BEGIN 
      WITH WSet = UnitRef ^ . UntExpImpIdSet
      DO WSet := IntSets . Include ( WSet , IdentAtom )
      END (*WITH*) 
    ; WITH WSet = UnitRef ^ . UntScopeRef ^ . ScpDeclIdSet 
      DO WSet := IntSets . Include ( WSet , IdentAtom )
      END (*WITH*) 
    ; VarArray_Int_ExpImpProxy . Assign
        ( UnitRef ^ . UntExpImpMap , IdentAtom , Proxy )
    END InsertExpImp

(*EXPORTED.*)
; PROCEDURE ImportDeclByNo
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; FromUnitDeclNo : FM3Base . DeclNoTyp
    ; ExpImpPosition : FM3Base . tPosition
      (* ^Of the EXPORTS or IMPORT directive's interface identifier. *) 
    ; IsExport : BOOLEAN 
    )
  : BOOLEAN (* Success. *)
  (* PRE: FromUnitDeclNo leads to a DeclRef in FromUnitRef^. *)

  = VAR LFromUnitDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LProxy : FM3ExpImpProxy . T 

  ; BEGIN
      LFromUnitDeclRef (* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FromUnitRef ^ . UntDeclMap , FromUnitDeclNo )  
    ; <* ASSERT LFromUnitDeclRef # NIL *>
      LIntoUnitRef := FM3Units . UnitStackTopRef
    ; LIntoIdentAtom
        := FM3Compile . ConvertAndCreateIdentAtom
             ( LFromUnitDeclRef ^ . DclIdAtom , FromUnitRef , LIntoUnitRef )
(*CHECK: Can we get the hash of the chars? *) 
    ; <* ASSERT LIntoIdentAtom # FM3Base . AtomNull *>  
      IF CheckDuplicateImport
           ( LIntoUnitRef
           , LIntoIdentAtom
           , ExpImpPosition
           , IsExport 
           )
      THEN (* All is legal, so do the real import. *)
        LProxy . EipUnitNo := FromUnitRef ^ . UntUnitNo 
      ; LProxy . EipDeclNo  := FromUnitDeclNo
      ; LProxy . EipImportingUnitNo := LIntoUnitRef ^ . UntUnitNo 
      ; LProxy . EipImportingUnitPosition := ExpImpPosition
      ; InsertExpImp ( LIntoUnitRef , LIntoIdentAtom , LProxy ) 
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

  = VAR LNote : TEXT 
  ; VAR LFromAtom : FM3Base . AtomTyp
  ; VAR LFromDeclNoInt : INTEGER 
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LProxy : FM3ExpImpProxy . T 
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      IF FromUnitRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF NOT FromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN FALSE 
      END (*IF*) 
    ; IF IdScanAttribute . SaChars = NIL THEN RETURN FALSE END (*IF*)
    ; LNote := NIL 
    ; LFromAtom (* Lookup the ident among the remote unit's atoms. *) 
        := FM3Atom_OAChars . LookupKey  
             ( FromUnitRef ^ . UntIdentAtomDict
             , IdScanAttribute . SaChars 
             , IdScanAttribute . SaHash 
             )
    ; IF LFromAtom = FM3Base . AtomNull
      THEN (* The ident is nowhere in the from-interface at all. *)
        LFound := FALSE
      ELSIF NOT IntSets . IsElement
        ( LFromAtom , FromUnitRef ^ . UntScopeRef ^ . ScpDeclIdSet )
      THEN (* It's not in the from-interface's top declaration scope. *)
        LFound := FALSE 
      ELSIF IntSets . IsElement ( LFromAtom , FromUnitRef ^ . UntExpImpIdSet )
      THEN (* It's imported, thus not transitively importable. *) 
        LNote := NonTransitiveNote
      ; LFound := FALSE 
      ELSE
        LFound
          := FM3Dict_Int_Int . LookupFixed
               ( FromUnitRef ^ . UntScopeRef ^ . ScpDeclDict 
               , LFromAtom
               , FM3Base . HashNull
               , (*OUT*) LFromDeclNoInt
               )
      END (*IF*)
      
    ; IF NOT LFound
      THEN
        IF IntSets . IsElement ( LFromAtom , FromUnitRef ^ . UntExpImpIdSet )
        THEN (* But it's imported into the from-interface. *) 
          LNote := NonTransitiveNote 
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
              }
          , IdScanAttribute . Position 
          )
          
      (* Insert an import that is not useable, but is replaceable by a
         later one, useable or not, with the same ident. .
      *)
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
      ; LProxy . EipUnitNo := FM3Base . UnitNoNull
      (* ^Makes it present but not useable. *) 
      ; LProxy . EipImportingUnitNo := LIntoUnitRef ^ . UntUnitNo  
      ; LProxy . EipDeclNo := FM3Base . DeclNoNull 
      ; LProxy . EipImportingUnitPosition := IdScanAttribute . Position
      ; InsertExpImp ( LIntoUnitRef , LIntoIdentAtom , LProxy ) 
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

(*EXPORTED*) 
; PROCEDURE ImportASPass1
    ( READONLY IntfScanAttr : FM3Scanner . tScanAttribute
    ; READONLY ASScanAttr : FM3Scanner . tScanAttribute
    )

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LASIdentAtom : FM3Base . AtomTyp
  
  ; PROCEDURE AssignProxy
      ( Ss : INTEGER ; VAR (*READONLY*) Elem : FM3ExpImpProxy . T ) 

    = BEGIN
        Elem . EipUnitNo := LIntfUnitRef . UntUnitNo
      ; Elem . EipDeclNo := FM3Base . DeclNoNull
      ; Elem . EipImportingUnitNo := FM3Units . UnitStackTopRef ^ . UntUnitNo
      ; Elem . EipImportingUnitPosition := ASScanAttr . Position 
      END AssignProxy
 
  ; BEGIN
      LIntfUnitRef
        := GetInterface
             ( IntfScanAttr . SaChars
             , IntfScanAttr . Position
             , IsExport := FALSE
             )
    ; LASIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
             , ASScanAttr . SaChars
             , ASScanAttr . SaHash
             )
    ; IF CheckDuplicateImport
           ( FM3Units . UnitStackTopRef 
           , LASIdentAtom
           , ASScanAttr . Position
           , IsExport := FALSE  
           )
      THEN (* OK *) 
        FM3Pass1 . PutBwd_LIIP
          ( Itk . ItkImportAS
          , LIntfUnitRef ^ . UntUnitNo
          , LASIdentAtom 
          , ASScanAttr . Position 
          )
      ; INC ( FM3Units . UnitStackTopRef ^ . UntExpImpCt )
      ; WITH WUnitIdSet = FM3Units . UnitStackTopRef ^ . UntExpImpIdSet
        DO WUnitIdSet := IntSets . Include ( WUnitIdSet , LASIdentAtom )
        END (*WITH*) 
      ; WITH WScopeIdSet 
               = FM3Units . UnitStackTopRef ^ . UntScopeRef ^ . ScpDeclIdSet
        DO WScopeIdSet := IntSets . Include ( WScopeIdSet , LASIdentAtom ) 
        END (*WITH*)
      ; VarArray_Int_ExpImpProxy . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntExpImpMap
          , LASIdentAtom
          , AssignProxy
          ) 
      END (*IF*)
    END ImportASPass1 

(*EXPORTED.*)
; PROCEDURE ImportASPass2
    ( FromUnitNo : FM3Base . UnitNoTyp
    ; IntoIdentAtom : FM3Base . AtomTyp
    ; READONLY ImportPosition : FM3Base . tPosition 
    )

  = VAR LFromUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LProxy : FM3ExpImpProxy . T 

  ; BEGIN

RETURN ; 
      IF IntoIdentAtom = FM3Base . AtomNull THEN RETURN END (*IF*)
    ; LFromUnitRef (* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , FromUnitNo ) 
    ; IF LFromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF NOT LFromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN
      END (*IF*) 
    ; LIntoUnitRef := FM3Units . UnitStackTopRef 
    ; IF CheckDuplicateImport
           ( LIntoUnitRef , IntoIdentAtom , ImportPosition , IsExport := FALSE )
      THEN (* Not a duplicate. *)
        LProxy . EipUnitNo := LFromUnitRef ^ . UntUnitNo  
      ; LProxy . EipDeclNo  := FM3Base . DeclNoNull 
      ; LProxy . EipImportingUnitNo := LIntoUnitRef ^ . UntUnitNo 
      ; LProxy . EipImportingUnitPosition := ImportPosition
      ; InsertExpImp ( LIntoUnitRef , IntoIdentAtom , LProxy ) 
      END (*IF*)
    END ImportASPass2

(*EXPORTED.*)
; PROCEDURE CountDecls ( FromUnitRef :  FM3Units . UnitRefTyp )
    : INTEGER (* Number of decls in FromUnitRef^ *) 

  = BEGIN
      IF FromUnitRef = NIL THEN RETURN 0 END (*IF*)
    ; IF NOT FromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN 0 
      END (*IF*) 
    ; WITH WScopeRef = FromUnitRef ^ . UntScopeRef
      DO IF WScopeRef = NIL THEN RETURN 0
        ELSE RETURN WScopeRef ^ . ScpDeclCt 
        END (*IF*)
      END (*WITH*) 
    END CountDecls 

(*EXPORTED.*)
; PROCEDURE ImportAllDecls
    ( FromUnitRef :  FM3Units . UnitRefTyp
    ; READONLY ExportPosition : FM3Base . tPosition
      (* ^Of the EXPORTS directive's identifier. *)
    )

  = BEGIN
      IF FromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF NOT FromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN
      END (*IF*) 
    ; WITH WScopeRef = FromUnitRef ^ . UntScopeRef
      DO IF WScopeRef = NIL THEN RETURN END (*IF*)
      ; FOR RDeclNo := WScopeRef ^ . ScpMinDeclNo
            TO WScopeRef ^ . ScpMinDeclNo + WScopeRef ^ . ScpDeclCt - 1
        DO EVAL ImportDeclByNo
             ( FromUnitRef 
             , RDeclNo 
             , ExportPosition  
             , IsExport := TRUE
             )
        END (*FOR*)
      END (*WITH*) 
    END ImportAllDecls 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
