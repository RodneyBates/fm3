
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2026  Rodney M. Bates.                                    *)
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
    ( UnitTRef : FM3Units . UnitTRefTyp ; Position : FM3Base . tPosition )
  (* PRE: UnitTRef is both the first-visited and last-visited in the cycle.
          We have to visit it twice.
  *)

  = VAR LWrT : TextWr . T
  ; VAR LUnitTRef : FM3Units . UnitTRefTyp 
  ; VAR LNextUnitTRef : FM3Units . UnitTRefTyp

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "Cyclic import of" )
    ; LUnitTRef := UnitTRef (* Start at bottom. *)  
    ; LOOP 
        Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
      ; Wr . PutText
          ( LWrT
          , Pathname . Join
              ( LUnitTRef ^ . UttUnitRef ^ . UntSrcFilePath
              , LUnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
              )
          )
      ; LNextUnitTRef := LUnitTRef ^ . UttImportingUnitTRef 
      ; IF LNextUnitTRef = UnitTRef 
        THEN (* Coming back to the the starting unit. *) 
          Wr . PutChar ( LWrT , '.' ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText
            ( LWrT
            , FM3Utils . PositionImage ( LUnitTRef ^ . UttRequestPosition )
            )
        ; Wr . PutText ( LWrT , ", imports" )
        ; LUnitTRef ^ . UttImportingUnitTRef := NIL  
        ; LUnitTRef ^ . UttUnitRef ^ . UntInExpImpCycle := TRUE
        ; LUnitTRef := LNextUnitTRef 
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
  : FM3Units . UnitTRefTyp
    (* ^The Desired interface. *)
  (* If not already done, compile or load the interface named by IdentChars. *) 

  = VAR LIntfUnitTRef : FM3Units . UnitTRefTyp 
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
    ; LIntfUnitTRef
        := FM3Compile . GetUnitTRefOfFileName ( LSrcFileName , Position )
    ; IF LIntfUnitTRef ^ . UttUnitRef ^ . UntState = Ust . UsNotUsable
      THEN RETURN NIL
      END (*IF*) 
    ; IF LIntfUnitTRef ^ . UttUnitRef ^ . UntState = Ust . UsNull 
      THEN (* Haven't previously seen this unit. *)
      (* Compile it. *)
(*TODO: Or load it. *)
        IF IsExport
        THEN LAdjective := "exported "
        ELSE LAdjective := "imported " 
        END (*IF*) 

      (* Compare this to similar code in FM3Compile.CompileOrLoadCLUnit. *) 
      ; IF NOT FM3Compile . FindAndOpenUnitSrcFile
                 ( LIntfUnitTRef , LAdjective , Position )
        THEN
          LIntfUnitTRef ^ . UttUnitRef ^ . UntState := Ust . UsNotUsable 
          (* ^Suppress cascaded error messages. *)
        ; RETURN LIntfUnitTRef 
        END (*IF*)

      (* Compile LIntfUnitTRef^. *) 
      ; FM3Units . UnitTStackTopRef ^ . UttImportingUnitTRef
          := LIntfUnitTRef
        (*^ To detect future cyclic imports. *) 
      ; FM3Units . UnitTStackTopRef ^ . UttRequestPosition := Position
        (* ^For possible cyclic-imports message. *) 
      ; LIntfUnitTRef ^ . UttUnitRef ^ . UntState := Ust . UsImporting

      ; FM3Units . PushUnit ( LIntfUnitTRef )
      ; FM3Units . CacheTopUnitValues ( )
        (* SetUnitLog will have to wait until Pass1.InitPass1 has
           created its WrT.  
           FM3Messages . SetUnitLog ( LIntfUnitTRef ^ . UttLogWrT )
        *) 
      ; FM3Compile . CompileUnitFromSrc ( LIntfUnitTRef ) 
      ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitTRef *>
        FM3Messages . SetUnitLog ( FM3Units . UnitTStackTopRef ^ . UttLogWrT ) 
      ; FM3Units . CacheTopUnitValues ( )
      ; FM3Units . UnitTStackTopRef ^ . UttImportingUnitTRef := NIL 
      ; FM3Units . UnitTStackTopRef ^ . UttRequestPosition
          := FM3Base . PositionNull 
      ; LIntfUnitTRef ^ . UttUnitRef ^ . UntState := Ust . UsCompiled 
      ; RETURN LIntfUnitTRef 

      ELSE (* This unit already exists and is usable. *)
(* TODO: This will need some thought and work for compiled but outdated units. *)
        FM3Units . UnitTStackTopRef ^ . UttImportingUnitTRef
          := LIntfUnitTRef
        (*^ To detect future cyclic imports. *) 
      ; FM3Units . UnitTStackTopRef ^ . UttRequestPosition := Position
        (* ^For possible cyclic-imports message. *)
      ; IF LIntfUnitTRef ^ . UttImportingUnitTRef # NIL 
        THEN (* Cyclic imports/exports. *)
          ReportCyclic  ( LIntfUnitTRef , Position )
        ; RETURN NIL 
        END (*IF*)
      ; FM3Units . UnitTStackTopRef ^ . UttImportingUnitTRef := NIL 
      ; FM3Units . UnitTStackTopRef ^ . UttRequestPosition
          := FM3Base . PositionNull
      ; RETURN LIntfUnitTRef 
      END (*IF*)
    END GetInterface

(*EXPORTED.*)
; PROCEDURE CheckDuplicateExpImp
    ( IntoUnitRef : FM3Units . UnitRefTyp
    ; NewIdentAtom : FM3Base . AtomTyp 
    ; ImportPosition : FM3Base . tPosition
      (* ^Of Ident that brought NewIdentAtom into IntoUnitRef. *)
    ; DuplicatorKindText : TEXT   
    )
  : BOOLEAN (* Check passed. *)
  (* Check that NewIdentAtom does not duplicate one already [ex|im]ported. *)
  (* Emit error return FALSE, if failure. *) 

  = VAR LPrevExpImpUnitTRef : FM3Units . UnitTRefTyp
  ; VAR LPrevExpImpUnitRef : FM3Units . UnitRefTyp
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
    ; LPrevExpImpUnitTRef (* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FM3Units . UnitsTMap
             , LPrevExpImpProxy . EipUnitNo 
             )
    ; LPrevExpImpUnitRef := LPrevExpImpUnitTRef ^ . UttUnitRef 
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
            , DuplicatorKindText 
            , " of \""
            , LIdentChars
            , "\", previously introduced at "
            , FM3Utils . PositionImage
                ( LPrevExpImpProxy . EipImportingUnitPosition )
            ,"," 
            , FM3Messages . NLIndent
            , "  original declaration at "
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
    ( FromUnitTRef : FM3Units . UnitTRefTyp
    ; FromUnitDeclNo : FM3Globals . DeclNoTyp
    ; ExpImpPosition : FM3Base . tPosition
      (* ^Of the EXPORTS or IMPORT directive's interface identifier. *) 
    ; DuplicatorKindText : TEXT 
    )
  : BOOLEAN (* Success. *)
  (* PRE: FromUnitDeclNo leads to a DeclRef in FromUnitRef^. *)

  = VAR LFromUnitDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LProxy : FM3ExpImpProxy . T 

  ; BEGIN (* ImportDeclByNo *) 
      LFromUnitDeclRef (* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FromUnitTRef ^ . UttUnitRef ^ . UntDeclMap , FromUnitDeclNo )  
    ; <* ASSERT LFromUnitDeclRef # NIL *>
      LIntoUnitRef := FM3Units . UnitTStackTopRef ^ . UttUnitRef 
    ; LIntoIdentAtom
        := FM3Compile . ConvertAndCreateIdentAtom
             ( LFromUnitDeclRef ^ . DclIdAtom
             , FromUnitTRef ^ . UttUnitRef
             , LIntoUnitRef
             )
(*CHECK: Can we get the hash of the chars? *) 
    ; <* ASSERT LIntoIdentAtom # FM3Base . AtomNull *>  
      IF CheckDuplicateExpImp
           ( LIntoUnitRef
           , LIntoIdentAtom
           , ExpImpPosition
           , DuplicatorKindText 
           )
      THEN (* All is legal, so do the real import. *)
        LProxy . EipUnitNo := FromUnitTRef ^ . UttSelfUnitNo 
      ; LProxy . EipDeclNo  := FromUnitDeclNo
      ; LProxy . EipImportingUnitNo
          := FM3Units . UnitTStackTopRef ^ . UttSelfUnitNo 
      ; LProxy . EipImportingUnitPosition := ExpImpPosition
      ; InsertExpImp ( LIntoUnitRef , LIntoIdentAtom , LProxy ) 
      ; RETURN TRUE
      ELSE RETURN FALSE 
      END (*IF*) 
    END ImportDeclByNo 

(*EXPORTED.*)
; PROCEDURE ImportDeclByIdent
    ( FromUnitTRef : FM3Units . UnitTRefTyp
    ; READONLY IdScanAttribute : FM3Scanner . tScanAttribute
      (* ^Containing info about the to-be-imported identifier. *) 
    )
  : BOOLEAN (* Success. *) 

  = VAR LFromUnitRef : FM3Units . UnitRefTyp
  ; VAR LNote : TEXT 
  ; VAR LFromAtom : FM3Base . AtomTyp
  ; VAR LFromDeclNoInt : INTEGER 
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LProxy : FM3ExpImpProxy . T 
  ; VAR LFound : BOOLEAN 

  ; BEGIN (*ImportDeclByIdent*)
      IF FromUnitTRef = NIL THEN RETURN FALSE END (*IF*)
    ; LFromUnitRef := FromUnitTRef ^ . UttUnitRef 
    ; IF NOT LFromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN FALSE 
      END (*IF*) 
    ; IF IdScanAttribute . SaChars = NIL THEN RETURN FALSE END (*IF*)
    ; LNote := NIL 
    ; LFromAtom (* Lookup the ident among the remote unit's atoms. *) 
        := FM3Atom_OAChars . LookupKey  
             ( LFromUnitRef ^ . UntIdentAtomDict
             , IdScanAttribute . SaChars 
             , IdScanAttribute . SaHash 
             )
    ; IF LFromAtom = FM3Base . AtomNull
      THEN (* The ident is nowhere in the from-interface at all. *)
        LFound := FALSE
      ELSIF IntSets . IsElement ( LFromAtom , LFromUnitRef ^ . UntExpImpIdSet )
      THEN (* It's imported, thus not transitively importable. *) 
        LNote := NonTransitiveNote
      ; LFound := FALSE 
      ELSE
        LFound
          := FM3Dict_Int_Int . LookupFixed
               ( LFromUnitRef ^ . UntScopeRef ^ . ScpDeclDict 
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
              , LFromUnitRef ^ . UntUnitIdent 
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
      ; LIntoUnitRef := FM3Units . UnitTStackTopRef ^ . UttUnitRef  
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
      ; LProxy . EipImportingUnitNo
          := FM3Units . UnitTStackTopRef ^ . UttSelfUnitNo  
      ; LProxy . EipDeclNo := FM3Globals . DeclNoNull 
      ; LProxy . EipImportingUnitPosition := IdScanAttribute . Position
      ; InsertExpImp ( LIntoUnitRef , LIntoIdentAtom , LProxy ) 
      ; RETURN FALSE 
      ELSE (* Valid imported ident. *) 
        RETURN
          ImportDeclByNo
            ( FromUnitTRef
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

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntfUnitTRef : FM3Units . UnitTRefTyp
  ; VAR LASIdentAtom : FM3Base . AtomTyp
  
  ; PROCEDURE AssignProxy
      ( <*UNUSED*> Ss : INTEGER ; VAR (*READONLY*) Elem : FM3ExpImpProxy . T ) 

    = BEGIN
        Elem . EipUnitNo := LIntfUnitTRef . UttSelfUnitNo
      ; Elem . EipDeclNo := FM3Globals . DeclNoNull
      ; Elem . EipImportingUnitNo
          := FM3Units . UnitTStackTopRef ^ . UttSelfUnitNo
      ; Elem . EipImportingUnitPosition := ASScanAttr . Position 
      END AssignProxy
 
  ; BEGIN (* ImportAS *)
      LUnitRef := FM3Units . UnitTStackTopRef ^ . UttUnitRef 
    ; LIntfUnitTRef
        := GetInterface
             ( IntfScanAttr . SaChars
             , IntfScanAttr . Position
             , IsExport := FALSE
             )
    ; IF LIntfUnitTRef = NIL THEN RETURN END (*IF*) 
    ; LASIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( LUnitRef ^ . UntIdentAtomDict
             , ASScanAttr . SaChars
             , ASScanAttr . SaHash
             )
    ; IF CheckDuplicateExpImp
           ( FM3Units . UnitTStackTopRef ^ . UttUnitRef  
           , LASIdentAtom
           , ASScanAttr . Position
           , "import"  
           )
      THEN (* OK, not a duplicate. *)
        INC ( LUnitRef ^ . UntExpImpCt )
      ; WITH WUnitIdSet = LUnitRef ^ . UntExpImpIdSet
        DO WUnitIdSet := IntSets . Include ( WUnitIdSet , LASIdentAtom )
        END (*WITH*) 
      ; VarArray_Int_ExpImpProxy . CallbackWithElem
          ( LUnitRef ^ . UntExpImpMap
          , LASIdentAtom
          , AssignProxy
          ) 
      END (*IF*)
    END ImportAS

(*EXPORTED.*)
; PROCEDURE CountDecls ( LFromUnitRef :  FM3Units . UnitRefTyp )
    : INTEGER (* Number of decls in LFromUnitRef^ *) 

  = BEGIN
      IF LFromUnitRef = NIL THEN RETURN 0 END (*IF*)
    ; IF NOT LFromUnitRef ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN 0 
      END (*IF*) 
    ; WITH WScopeRef = LFromUnitRef ^ . UntScopeRef
      DO IF WScopeRef = NIL THEN RETURN 0
        ELSE RETURN IntSets . Card ( WScopeRef ^ . ScpDeclIdSet )  
        END (*IF*)
      END (*WITH*) 
    END CountDecls 

(*EXPORTED.*)
; PROCEDURE ImportAllDecls
    ( FromUnitTRef :  FM3Units . UnitTRefTyp
    ; READONLY ExportPosition : FM3Base . tPosition
      (* ^Of the EXPORTS directive's identifier. *)
    )

  = VAR LFromUnitRef : FM3Units . UnitRefTyp
  ; VAR LDeclCt : INTEGER

  ; BEGIN
      IF FromUnitTRef = NIL THEN RETURN END (*IF*)
    ; LFromUnitRef := FromUnitTRef ^ . UttUnitRef 
    ; IF NOT LFromUnitRef  ^ . UntState IN FM3Units . UnitStateSetUsable
      THEN RETURN
      END (*IF*) 
    ; WITH WScopeRef = LFromUnitRef ^ . UntScopeRef
      DO IF WScopeRef = NIL THEN RETURN END (*IF*)
      ; LDeclCt := IntSets . Card ( WScopeRef ^ . ScpDeclIdSet )  
      ; IF LDeclCt = 0 THEN RETURN END (*IF*)
      ; <* ASSERT WScopeRef ^ . ScpMinDeclNo > 0 *>
        FOR RDeclNo := WScopeRef ^ . ScpMinDeclNo
            TO WScopeRef ^ . ScpMinDeclNo + LDeclCt - 1
        DO EVAL ImportDeclByNo
             ( FromUnitTRef 
             , RDeclNo 
             , ExportPosition  
             , "export"
             )
        END (*FOR*)
      END (*WITH*) 
    END ImportAllDecls 

(*EXPORTED.*)
; PROCEDURE Done ( ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; BEGIN
      LUnitRef := FM3Units . UnitTStackTopRef  ^ . UttUnitRef 
    ; VarArray_Int_ExpImpProxy . Compact ( LUnitRef ^ . UntExpImpMap )
    ; LUnitRef ^ . UntNextDeclNo
        := VarArray_Int_ExpImpProxy . TouchedRange ( LUnitRef ^ . UntExpImpMap )
           . Hi
           + 1
    ; FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Compiling " , LUnitRef ^ . UntSrcFileSimpleName , " ..." }

        )
    END Done 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
