
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
; IMPORT Thread 
; IMPORT Wr

; IMPORT VarArray_Int_ExpImpProxy  
; IMPORT VarArray_Int_Refany  
; IMPORT IntSets

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3Compile
; IMPORT FM3Decls
; IMPORT FM3Dict_Int_Int 
; IMPORT FM3ExpImpProxy  
; IMPORT FM3Globals 
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3Scanner
; IMPORT FM3Scopes (* For Revelation of ScopeRefTyp *) 
; IMPORT FM3Units 
; IMPORT FM3Utils

; TYPE Ust = FM3Units . UnitStateTyp
; TYPE Urkt = FM3Base . UnitReqKindTyp 

; PROCEDURE ReportCyclic
    ( UnitTRef : FM3Units . UnitTRefTyp
    ; ReqPosition : FM3Base . tPosition
    ; <*UNUSED*> ReqKind : FM3Base . UnitReqKindTyp
    )
  (* PRE: UnitTRef is both the first-visited and last-visited in the cycle.
          We have to visit it twice.
  *)

  = VAR LWrT : TextWr . T
  ; VAR LUnitTRef : FM3Units . UnitTRefTyp 
  ; VAR LNextUnitTRef : FM3Units . UnitTRefTyp

  ; <*FATAL Thread . Alerted , Wr . Failure *>
    BEGIN (*ReportCyclic*) 
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "Cyclic import of" )
    ; LUnitTRef := UnitTRef (* Start at bottom. *)  
    ; LOOP (* Work forward. *) 
        Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
      ; Wr . PutText
          ( LWrT
          , Pathname . Join
              ( LUnitTRef ^ . UttUnitRef ^ . UntSrcFilePath
              , LUnitTRef ^ . UttUnitRef ^ . UntSrcFileSimpleName
              )
          )
      ; LNextUnitTRef := LUnitTRef ^ . UttImportsUnitTRef 
      ; IF LNextUnitTRef = UnitTRef 
        THEN (* Have circled back to the the starting unit. *) 
          Wr . PutChar ( LWrT , '.' ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE
(*TODO: Put ReqPosition and ReqKind back into Utt and use them here. *) 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText ( LWrT , FM3Utils . PositionImage ( ReqPosition ) ) 
        ; Wr . PutText ( LWrT , ", imports" )
        ; LUnitTRef ^ . UttInExpImpCycle := TRUE
(* TODO ^ Use this to avoid repeat work and messages.
          And Maybe just set not usable.
*) 
        ; LUnitTRef := LNextUnitTRef 
        END (*IF*) 
      END (*LOOP*) 

    ; FM3Messages . ErrorArr
        ( ARRAY OF REFANY { TextWr . ToText ( LWrT ) }
        , Pos := ReqPosition 
        ) 
    END ReportCyclic

(*EXPORTED*) 
; PROCEDURE GetInterface
    ( IdentChars : FM3OpenArray_Char . T
      (* ^Interface unit name, without file name suffix. *)  
    ; READONLY ReqPosition : FM3Base . tPosition
      (* ^v In the current unit of the to-be [ex/im]ported interace ident. *) 
    ; ReqKind : FM3Base . UnitReqKindTyp 
    )
  : FM3Units . UnitTRefTyp
    (* ^The Desired interface. *)
  (* Compile or load the interface named by IdentChars, as appropriate. *) 

  = VAR LIntfUnitTRef : FM3Units . UnitTRefTyp 
  ; VAR LImportingUnitTRef : FM3Units . UnitTRefTyp 
  ; VAR LSrcFileName : TEXT 

  ; BEGIN (*GetInterface*)
    (* Convert interface unit name to source file name. *) 
      IF IdentChars = NIL THEN RETURN NIL END (*IF*)
    ; IF NUMBER ( IdentChars ^ ) = 0 THEN RETURN NIL END (*IF*)
    ; LSrcFileName
        := Pathname . Join
             ( NIL
             , Text . FromChars ( IdentChars ^ )
             , FM3Base . InterfaceFileNameSuffix
             ) 
    ; LIntfUnitTRef := FM3Compile . GetUnitTRefOfFileName ( LSrcFileName )
    ; IF LIntfUnitTRef ^ . UttImportsUnitTRef # NIL 
      THEN (* Was set by a previous visit. Cyclic imports/exports. *)
        ReportCyclic  ( LIntfUnitTRef , ReqPosition , ReqKind )
      ; RETURN LIntfUnitTRef 
      END (*IF*)

    ; LImportingUnitTRef := FM3Units . UnitTStackTopRef 
    ; LImportingUnitTRef ^ . UttImportsUnitTRef := LIntfUnitTRef 
      (* ^To detect cyclic imports. *)
    ; LIntfUnitTRef ^ . Utt1stReqPosition := ReqPosition 
    ; LIntfUnitTRef ^ . Utt1stReqKind := ReqKind 
    ; LImportingUnitTRef ^ . UttState := Ust . UsImporting
    ; FM3Compile . AcquireUnit ( LIntfUnitTRef , ReqPosition , ReqKind )  

    ; LImportingUnitTRef ^ . UttImportsUnitTRef := NIL
    ; RETURN LIntfUnitTRef 
    END GetInterface

(*EXPORTED.*)
; PROCEDURE VerifyNotDuplicateExpImp
    ( IntoUnitRef : FM3Units . UnitRefTyp
    ; NewIdentAtom : FM3Base . AtomTyp 
    ; ImportPosition : FM3Base . tPosition
      (* ^Of Ident that brought NewIdentAtom into IntoUnitRef. *)
    ; DuplicatorKindText : TEXT   
    )
  : BOOLEAN (* Check passed. *)
  (* Check that NewIdentAtom does not duplicate one previously [ex|im]ported. *)
  (* Emit error return FALSE, if failure. *) 

  = VAR LPrevExpImpUnitTRef : FM3Units . UnitTRefTyp
  ; VAR LPrevExpImpUnitRef : FM3Units . UnitRefTyp
  ; VAR LPrevDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIdentChars : FM3Atom_OAChars . KeyTyp
  ; VAR LPrevExpImpProxy : FM3ExpImpProxy . T
  ; VAR LPrevDeclPosition : FM3Base . tPosition

  ; <*FATAL Thread . Alerted *>
    BEGIN (*VerifyNotDuplicateExpImp*)
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
             ( FM3Units . UnitTMap
             , LPrevExpImpProxy . EipUnitNo 
             )
    ; LPrevExpImpUnitRef := LPrevExpImpUnitTRef ^ . UttUnitRef 
    ; IF NOT ( LPrevExpImpUnitTRef ^ . UttState IN FM3Units . UnitStateSetUsable ) 
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
    END VerifyNotDuplicateExpImp

; PROCEDURE InsertExpImp
    ( UnitRef : FM3Units . UnitRefTyp
    ; IdentAtom : FM3Base . AtomTyp
    ; READONLY Proxy : FM3ExpImpProxy . T 
    )

  = <* FATAL VarArray_Int_ExpImpProxy . AllocationFailure *>
    BEGIN (*InsertExpImp*) 
      WITH WSet = UnitRef ^ . UntExpImpIdSet
      DO WSet := IntSets . Include ( WSet , IdentAtom )
      END (*WITH*) 
    ; VarArray_Int_ExpImpProxy . Assign
        ( UnitRef ^ . UntExpImpMap , IdentAtom , Proxy )
    END InsertExpImp

; PROCEDURE ImportDeclByNo
    ( FromUnitTRef : FM3Units . UnitTRefTyp
    ; FromUnitDeclNo : FM3Globals . DeclNoTyp
    ; ExpImpPosition : FM3Base . tPosition
    ; ReqKind : FM3Base . UnitReqKindTyp  
    )
  : BOOLEAN (* Success. *)
  (* PRE: FromUnitDeclNo leads to a DeclRef in FromUnitRef^. *)

  = VAR LFromUnitDeclRef : FM3Decls . DeclRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LProxy : FM3ExpImpProxy . T 

  ; BEGIN (*ImportDeclByNo*) 
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
      IF VerifyNotDuplicateExpImp
           ( LIntoUnitRef
           , LIntoIdentAtom
           , ExpImpPosition
           , FM3Units . UnitReqKindTag ( ReqKind ) 
           )
      THEN (* All is legal, so do the real import. *)
        LProxy . EipUnitFileName
          := Pathname . Last ( FromUnitTRef ^ . UttSrcFilePath ) 
      ; LProxy . EipUnitNo := FromUnitTRef ^ . UttSelfUnitNo 
      ; LProxy . EipDeclNo  := FromUnitDeclNo
      ; LProxy . EipImportingUnitNo
          := FM3Units . UnitTStackTopRef ^ . UttSelfUnitNo 
      ; LProxy . EipImportingUnitPosition := ExpImpPosition
      ; LProxy . EipReqKind := ReqKind 
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
  ; VAR LIntoUnitTRef : FM3Units . UnitTRefTyp
  ; VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntoIdentAtom : FM3Base . AtomTyp  
  ; VAR LProxy : FM3ExpImpProxy . T 
  ; VAR LFound : BOOLEAN 

  ; <*FATAL Thread . Alerted *>
    BEGIN (*ImportDeclByIdent*)
      IF FromUnitTRef = NIL THEN RETURN FALSE END (*IF*)
    ; LFromUnitRef := FromUnitTRef ^ . UttUnitRef 
    ; IF LFromUnitRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF NOT FromUnitTRef ^ . UttState IN FM3Units . UnitStateSetUsable
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
        <* FATAL FM3Dict_Int_Int . Error *>
        BEGIN 
          LFound
            := FM3Dict_Int_Int . LookupFixed
                 ( LFromUnitRef ^ . UntScopeRef ^ . ScpDeclDict 
                 , LFromAtom
                 , FM3Base . HashNull
                 , (*OUT*) LFromDeclNoInt
                 )
        END (*Block*) 
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
      ; LIntoUnitTRef := FM3Units . UnitTStackTopRef 
      ; LIntoUnitRef := LIntoUnitTRef ^ . UttUnitRef  
      (* If there's already a non-useable there, this will just overlay it,
         changing only the position, which will probably be unused anyway.
      *) 
      ; LIntoIdentAtom
          := FM3Atom_OAChars . MakeAtom
               ( LIntoUnitRef ^ . UntIdentAtomDict
               , IdScanAttribute . SaChars
               , IdScanAttribute . SaHash
               )
      ; LProxy . EipUnitFileName
          := Pathname . Last ( FromUnitTRef ^ . UttSrcFilePath ) 
      ; LProxy . EipUnitNo := FM3Globals . UnitNoNull
      (* ^Makes it present but not useable. *) 
      ; LProxy . EipImportingUnitNo := LIntoUnitTRef ^ . UttSelfUnitNo  
      ; LProxy . EipDeclNo := FM3Globals . DeclNoNull 
      ; LProxy . EipImportingUnitPosition := IdScanAttribute . Position
      ; LProxy . EipReqKind := Urkt . UrkImport  
      ; InsertExpImp ( LIntoUnitRef , LIntoIdentAtom , LProxy ) 
      ; RETURN FALSE 
      ELSE (* Valid imported ident. *) 
        RETURN
          ImportDeclByNo
            ( FromUnitTRef
            , LFromDeclNoInt
            , IdScanAttribute . Position
            , Urkt . UrkImport 
            ) 
      END (*IF*) 
    END ImportDeclByIdent

(*EXPORTED*) 
; PROCEDURE ImportAS
    ( READONLY IntfScanAttr : FM3Scanner . tScanAttribute
    ; READONLY ASScanAttr : FM3Scanner . tScanAttribute
      (*       ^These 2 will have equal SaChars, for "IMPORT I;" *) 
    )

  = VAR LIntoUnitRef : FM3Units . UnitRefTyp
  ; VAR LIntfUnitTRef : FM3Units . UnitTRefTyp
  ; VAR LASIdentAtom : FM3Base . AtomTyp
  
  ; PROCEDURE AssignProxy
      ( <*UNUSED*> Ss : INTEGER ; VAR (*READONLY*) Elem : FM3ExpImpProxy . T ) 

    = BEGIN
        Elem . EipUnitFileName
          := Pathname . Last ( LIntfUnitTRef . UttSrcFilePath ) 
      ; Elem . EipUnitNo := LIntfUnitTRef . UttSelfUnitNo
      ; Elem . EipDeclNo := FM3Globals . DeclNoNull
      ; Elem . EipImportingUnitNo
          := FM3Units . UnitTStackTopRef ^ . UttSelfUnitNo
      ; Elem . EipImportingUnitPosition := ASScanAttr . Position
      ; Elem . EipReqKind := Urkt . UrkImport 
      END AssignProxy
 
  ; BEGIN (*ImportAS*)
      LIntoUnitRef := FM3Units . UnitTStackTopRef ^ . UttUnitRef 
    ; LIntfUnitTRef
        := GetInterface
             ( IntfScanAttr . SaChars
             , IntfScanAttr . Position
             , Urkt . UrkExport 
             )
    ; IF LIntfUnitTRef = NIL THEN RETURN END (*IF*) 
    ; LASIdentAtom
        := FM3Atom_OAChars . MakeAtom
             ( LIntoUnitRef ^ . UntIdentAtomDict
             , ASScanAttr . SaChars
             , ASScanAttr . SaHash
             )
    ; IF VerifyNotDuplicateExpImp
           ( FM3Units . UnitTStackTopRef ^ . UttUnitRef  
           , LASIdentAtom
           , ASScanAttr . Position
           , "import"  
           )
      THEN (* OK, not a duplicate. *)
        WITH WUnitIdSet = LIntoUnitRef ^ . UntExpImpIdSet
        DO WUnitIdSet := IntSets . Include ( WUnitIdSet , LASIdentAtom )
        END (*WITH*) 
      ; <* FATAL ANY *>
        BEGIN VarArray_Int_ExpImpProxy . CallbackWithElem
                ( LIntoUnitRef ^ . UntExpImpMap
                , LASIdentAtom
                , AssignProxy
                )
        END (*Block*) 
      END (*IF*)
    END ImportAS

(*EXPORTED.*)
; PROCEDURE ImportAllDecls
    ( FromUnitTRef :  FM3Units . UnitTRefTyp
    ; READONLY ExportPosition : FM3Base . tPosition
      (* ^Of the EXPORTS directive's identifier. *)
    )

  = VAR LFromUnitRef : FM3Units . UnitRefTyp
  ; VAR LDeclCt : INTEGER

  ; BEGIN (*ImportAllDecls*)
      IF FromUnitTRef = NIL THEN RETURN END (*IF*)
    ; LFromUnitRef := FromUnitTRef ^ . UttUnitRef 
    ; IF LFromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF NOT FromUnitTRef  ^ . UttState IN FM3Units . UnitStateSetUsable
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
             , Urkt . UrkExport
             )
        END (*FOR*)
      END (*WITH*) 
    END ImportAllDecls 

(*EXPORTED.*)
; PROCEDURE Done ( ) 

  = VAR LUnitRef : FM3Units . UnitRefTyp

  ; <* FATAL VarArray_Int_ExpImpProxy . AllocationFailure *>
    BEGIN (*Done*) 
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
