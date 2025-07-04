
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
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
; IMPORT FM3Globals 
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3Scanner
; IMPORT FM3Scopes (* For Revelation of ScopeRefTyp *) 
; IMPORT FM3Units 
; IMPORT FM3Utils
; IMPORT VarArray_Int_ExpImpProxy  
; IMPORT VarArray_Int_Refany  

; TYPE Ust = FM3Units . UnitStateTyp 

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
    ; LOOP 
        Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
      ; Wr . PutText
          ( LWrT
          , Pathname . Join
              ( LUnitRef ^ . UntSrcFilePath
              , LUnitRef ^ . UntSrcFileSimpleName
              )
          )
      ; LNextUnitRef := LUnitRef ^ . UntImportingUnitRef 
      ; IF LNextUnitRef = UnitRef 
        THEN (* Coming back to the the starting unit. *) 
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
        ; LUnitRef ^ . UntImportingUnitRef := NIL  
        ; LUnitRef ^ . UntInExpImpCycle := TRUE
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
      (* ^Interface unit name, without file name suffix. *)  
    ; Position : FM3Base . tPosition
      (* ^In the current unit of the to-be [ex/im]ported identifier. *) 
    ; IsExport : BOOLEAN
    )
  : FM3Units . UnitRefTyp
    (* ^The Desired interface. *)
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
    ; IF LIntfUnitRef ^ . UntState = Ust . UsNotUsable
      THEN RETURN NIL
      END (*IF*) 
    ; IF LIntfUnitRef ^ . UntState = Ust . UsNull 
      THEN (* Haven't previously seen this unit. *)
      (* Compile it. *)
(*TODO: Or load it. *)
        IF IsExport
        THEN LAdjective := "exported "
        ELSE LAdjective := "imported " 
        END (*IF*) 

      (* Compare this to similar code in FM3Compile.CompileOrLoadCLUnit. *) 
      ; IF NOT FM3Compile . FindAndOpenUnitSrcFile
                 ( LIntfUnitRef , LAdjective , Position )
        THEN
          LIntfUnitRef ^ . UntState := Ust . UsNotUsable 
          (* ^Suppress cascaded error messages. *)
        ; RETURN LIntfUnitRef 
        END (*IF*)

      (* Compile LIntfUnitRef^. *) 
      ; FM3Units . UnitStackTopRef ^ . UntImportingUnitRef
          := LIntfUnitRef
        (*^ To detect future cyclic imports. *) 
      ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport := Position
        (* ^For possible cyclic-imports message. *) 
      ; LIntfUnitRef ^ . UntState := Ust . UsImporting

      ; FM3Units . PushUnit ( LIntfUnitRef )
      ; FM3Units . CacheTopUnitValues ( )
        (* SetUnitLog will have to wait until Pass1.InitPass1 has
           created its WrT.  
           FM3Messages . SetUnitLog ( LIntfUnitRef ^ . UntLogWrT )
        *) 
      ; FM3Compile . CompileUnitFromSrc ( LIntfUnitRef ) 
      ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitRef *>
        FM3Messages . SetUnitLog ( FM3Units . UnitStackTopRef ^ . UntLogWrT ) 
      ; FM3Units . CacheTopUnitValues ( )
      ; FM3Units . UnitStackTopRef ^ . UntImportingUnitRef := NIL 
      ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport
          := FM3Base . PositionNull 
      ; LIntfUnitRef ^ . UntState := Ust . UsCompiled 
      ; RETURN LIntfUnitRef 

      ELSE (* This unit already exists and is usable. *)
(* TODO: This will need some thought and work for compiled but outdated units. *)
        FM3Units . UnitStackTopRef ^ . UntImportingUnitRef
          := LIntfUnitRef
        (*^ To detect future cyclic imports. *) 
      ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport := Position
        (* ^For possible cyclic-imports message. *)
      ; IF LIntfUnitRef ^ . UntImportingUnitRef # NIL 
        THEN (* Cyclic imports/exports. *)
          ReportCyclic  ( LIntfUnitRef , Position )
        ; RETURN NIL 
        END (*IF*)
      ; FM3Units . UnitStackTopRef ^ . UntImportingUnitRef := NIL 
      ; FM3Units . UnitStackTopRef ^ . UntPositionOfImport
          := FM3Base . PositionNull
      ; RETURN LIntfUnitRef 
      END (*IF*)
    END GetInterface

(*EXPORTED.*)
; PROCEDURE CheckDuplicateExpImp
    ( IntoUnitRef : FM3Units . UnitRefTyp
    ; NewIdentAtom : FM3Base . AtomTyp 
    ; ImportPosition : FM3Base . tPosition
      (* ^Of Ident that brought NewIdentAtom into IntoUnitRef. *)
    ; DuplicatorKind : TEXT 
    )
  : BOOLEAN (* Check passed. *)
  (* Check that NewImportAtom does not duplicate one already [ex|im]ported. *)
  (* Emit error return FALSE, if failure. *) 

  = VAR LPrevExpImpUnitRef : FM3Units . UnitRefTyp
  ; VAR LPrevDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp
  ; VAR LPrevExpImpProxy : FM3ExpImpProxy . T
  ; VAR LPrevDeclPosition : FM3Base . tPosition

  ; BEGIN 
      IF NOT IntSets . IsElement
               ( NewIdentAtom , IntoUnitRef ^ . UntExpImpIdSet )
      THEN RETURN TRUE
      END (*IF*) 
    ; LPrevExpImpProxy
        := VarArray_Int_ExpImpProxy . Fetch
             ( IntoUnitRef ^ . UntExpImpMap , NewIdentAtom )
    ; IF LPrevExpImpProxy . EipUnitNo = FM3Globals . UnitNoNull
      THEN RETURN TRUE
      END (*IF*) 
    ; LPrevExpImpUnitRef (* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FM3Units . UnitsMap
             , LPrevExpImpProxy . EipUnitNo 
             )
    ; IF LPrevExpImpUnitRef ^ . UntState = Ust . UsNotUsable 
      THEN RETURN TRUE
      END (*IF*)
      
    (* There is a previous same-named [ex|im]port and it's usable. *) 
    ; IF NOT FM3Atom_OAChars . Key 
               ( IntoUnitRef ^ . UntIdentAtomDict
               , NewIdentAtom
               , (*OUT*) LIdentChars
               )
      THEN LIdentChars := NIL
      END (*IF*)
    ; IF LPrevExpImpProxy . EipDeclNo = FM3Globals . DeclNoNull
      THEN LPrevDeclPosition := LPrevExpImpUnitRef ^ . UntUnitIdentPos 
      ELSE
        LPrevDeclRef (*Implied NARROW*) 
          := VarArray_Int_Refany . Fetch
               ( LPrevExpImpUnitRef ^ . UntDeclMap
               , LPrevExpImpProxy . EipDeclNo
               )
      ; IF LPrevDeclRef = NIL (* Shouldn't happen. *) 
        THEN LPrevDeclPosition := FM3Base . tPosition { 0 , 0 }  
        ELSE LPrevDeclPosition := LPrevDeclRef . DclPos
        END (*IF*)
      END (*IF*)
    ; FM3Messages . ErrorArr
        ( ARRAY OF REFANY
            { "Duplicate "
            , DuplicatorKind 
            , " of \""
            , LIdentChars
            , "\", previously introduced at "
            , FM3Utils . PositionImage
                ( LPrevExpImpProxy . EipImportingUnitPosition )
            ,"," 
            , FM3Messages . NLIndent
            , "    original declaration at "
            , LPrevExpImpUnitRef ^ . UntSrcFileSimpleName 
            , ":" 
            , FM3Utils . PositionImage ( LPrevDeclPosition ) 
            , ", (2.5.1)"
            } 
        , ImportPosition 
        )
    ; RETURN FALSE
    END CheckDuplicateExpImp

; PROCEDURE InsertExpImp
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdentAtom : FM3Base . AtomTyp
    ; READONLY Proxy : FM3ExpImpProxy . T 
    )

  = BEGIN 
      WITH WSet = UnitRef ^ . UntExpImpIdSet
      DO WSet := IntSets . Include ( WSet , IdentAtom )
      END (*WITH*) 
    ; VarArray_Int_ExpImpProxy . Assign
        ( UnitRef ^ . UntExpImpMap , IdentAtom , Proxy )
    END InsertExpImp

(*EXPORTED.*)
; PROCEDURE ImportDeclByNo
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; FromUnitDeclNo : FM3Globals . DeclNoTyp
    ; ExpImpPosition : FM3Base . tPosition
      (* ^Of the EXPORTS or IMPORT directive's interface identifier. *) 
    ; DuplicatorKind : TEXT 
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
      IF CheckDuplicateExpImp
           ( LIntoUnitRef
           , LIntoIdentAtom
           , ExpImpPosition
           , DuplicatorKind 
           )
      THEN (* All is legal, so do the real import. *)
        LProxy . EipUnitNo := FromUnitRef ^ . UntSelfUnitNo 
      ; LProxy . EipDeclNo  := FromUnitDeclNo
      ; LProxy . EipImportingUnitNo := LIntoUnitRef ^ . UntSelfUnitNo 
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

  ; BEGIN (*ImportDeclByIdent*)
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
        FM3Messages . ErrorArr
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
         later one, useable or not, with the same ident. 
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
      ; LProxy . EipUnitNo := FM3Globals . UnitNoNull
      (* ^Makes it present but not useable. *) 
      ; LProxy . EipImportingUnitNo := LIntoUnitRef ^ . UntSelfUnitNo  
      ; LProxy . EipDeclNo := FM3Globals . DeclNoNull 
      ; LProxy . EipImportingUnitPosition := IdScanAttribute . Position
      ; InsertExpImp ( LIntoUnitRef , LIntoIdentAtom , LProxy ) 
      ; RETURN FALSE 
      ELSE (* Valid imported ident. *) 
        RETURN
          ImportDeclByNo
            ( FromUnitRef
            , LFromDeclNoInt
            , IdScanAttribute . Position
            , "import"
            ) 
      END (*IF*) 
    END ImportDeclByIdent

(*EXPORTED*) 
; PROCEDURE ImportAS
    ( READONLY IntfScanAttr : FM3Scanner . tScanAttribute
    ; READONLY ASScanAttr : FM3Scanner . tScanAttribute
    )

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp
  ; VAR LASIdentAtom : FM3Base . AtomTyp
  
  ; PROCEDURE AssignProxy
      ( <*UNUSED*> Ss : INTEGER ; VAR (*READONLY*) Elem : FM3ExpImpProxy . T ) 

    = BEGIN
        Elem . EipUnitNo := LIntfUnitRef . UntSelfUnitNo
      ; Elem . EipDeclNo := FM3Globals . DeclNoNull
      ; Elem . EipImportingUnitNo
          := FM3Units . UnitStackTopRef ^ . UntSelfUnitNo
      ; Elem . EipImportingUnitPosition := ASScanAttr . Position 
      END AssignProxy
 
  ; BEGIN (* ImportAS *)
      LIntfUnitRef
        := GetInterface
             ( IntfScanAttr . SaChars
             , IntfScanAttr . Position
             , IsExport := FALSE
             )
    ; IF LIntfUnitRef = NIL THEN RETURN END (*IF*) 
    ; LASIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( FM3Units . UnitStackTopRef ^ . UntIdentAtomDict
             , ASScanAttr . SaChars
             , ASScanAttr . SaHash
             )
    ; IF CheckDuplicateExpImp
           ( FM3Units . UnitStackTopRef 
           , LASIdentAtom
           , ASScanAttr . Position
           , "import"  
           )
      THEN (* OK, not a duplicate. *)
        INC ( FM3Units . UnitStackTopRef ^ . UntExpImpCt )
      ; WITH WUnitIdSet = FM3Units . UnitStackTopRef ^ . UntExpImpIdSet
        DO WUnitIdSet := IntSets . Include ( WUnitIdSet , LASIdentAtom )
        END (*WITH*) 
      ; VarArray_Int_ExpImpProxy . CallbackWithElem
          ( FM3Units . UnitStackTopRef ^ . UntExpImpMap
          , LASIdentAtom
          , AssignProxy
          ) 
      END (*IF*)
    END ImportAS

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
      DO IF WScopeRef = NIL OR WScopeRef ^ . ScpDeclCt = 0 THEN RETURN END (*IF*)
      ; <* ASSERT WScopeRef ^ . ScpMinDeclNo > 0 *>
        FOR RDeclNo := WScopeRef ^ . ScpMinDeclNo
            TO WScopeRef ^ . ScpMinDeclNo + WScopeRef ^ . ScpDeclCt - 1
        DO EVAL ImportDeclByNo
             ( FromUnitRef 
             , RDeclNo 
             , ExportPosition  
             , "export"
             )
        END (*FOR*)
      END (*WITH*) 
    END ImportAllDecls 

(*EXPORTED.*)
; PROCEDURE Done ( ) 

  = BEGIN
      VarArray_Int_ExpImpProxy . Compact
        ( FM3Units . UnitStackTopRef ^ . UntExpImpMap )
    ; FM3Units . UnitStackTopRef ^ . UntNextDeclNo
        := VarArray_Int_ExpImpProxy . TouchedRange
             ( FM3Units . UnitStackTopRef ^ . UntExpImpMap  ) . Hi
           + 1 
    END Done 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
