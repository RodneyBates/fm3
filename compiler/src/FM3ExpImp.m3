
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

; IMPORT FM3Dict_Int_Int 
; IMPORT IntIntVarArray 
; IMPORT IntSets

; IMPORT FM3Atom_OAChars
; IMPORT FM3Base
; IMPORT FM3Compile
; IMPORT FM3Decls 
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3Scanner
; IMPORT FM3Scopes (* For Revelation of ScopeRefTyp *) 
; IMPORT FM3Units 
; IMPORT FM3Utils

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
      ; LNextUnitRef := LUnitRef . UntImportingUnitRef 
      ; LUnitRef . UntImportingUnitRef := NIL  
      ; LUnitRef ^ . UntInCycle := TRUE 
      ; IF LUnitRef = FM3Units . UnitStackTopRef 
        THEN (* The unit containing the cycle-closing [ex/im]port clause. *) 
          Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText
            ( LWrT
            , FM3Utils . PositionImage ( LUnitRef . UntImportingPosition )
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
    ; IsExport : BOOLEAN
    )
  : FM3Units . UnitRefTyp
    (* ^The interface unit that was [ex/im]ported, possibly NIL *) 

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
          FM3Units . UnitStackTopRef ^ . UntImportingUnitRef := LIntfUnitRef
          (*^ To detect cyclic imports. *) 
        ; FM3Units . UnitStackTopRef ^ . UntImportingPosition
            := Position
        ; LIntfUnitRef ^ . UntState := Us . UsImporting 
        ; FM3Units . PushUnit ( LIntfUnitRef )
        ; FM3Units . CacheTopUnitValues ( ) 
        ; FM3Compile . CompileUnitFromSrc ( LIntfUnitRef ) 
        ; FM3Units . UncacheTopUnitValues ( ) 
        ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitRef *>
          RETURN NIL  
        ELSE RETURN LIntfUnitRef 
        END (*IF*) 
      ELSE
        IF LIntfUnitRef . UntImportingUnitRef # NIL 
        THEN (* Cyclic imports/exports. *)
          ReportCyclic  ( LIntfUnitRef , Position )
        ; RETURN NIL
        ELSE RETURN LIntfUnitRef 
        END (*IF*)
      END (*IF*)
    ; (* Do the actual [ex/im]port. *) 
    END GetInterface

(*
; PROCEDURE ImportDeclByIdent
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; IdScanAttribute : FM3Scanner . tScanAttribute
    )

  = VAR LFromDeclDict : FM3Dict_Int_Int . GrowableTyp 
  ; VAR LFromDeclIdSet : IntSets . T
  ; VAR LFromExpImpIdSet : IntSets . T
  ; VAR LNote : REF ARRAY OF REFANY 
  ; VAR LFromAtom : FM3Base . AtomTyp
  ; VAR LFromDeclNo : FM3Base . DeclNoTyp
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      IF FromUnitRef = NIL THEN RETURN END (*IF*)
    ; IF IdScanAttribute . SaChars = NIL THEN RETURN END (*IF*)
    ; LFromAtom
        := FM3Atom_OAChars . MakeAtom 
             ( FromUnitRef ^ . UntIdentAtomDict
             , IdScanAttribute . SaChars 
             , IdScanAttribute . SaHash 
             )
    ; IF LFromAtom = FM3Base . AtomNull
      THEN LFound := FALSE
      ELSE
        LFromDeclDict := FromUnitRef ^ . UntDeclScopeRef ^ . ScpDeclDict
      ; IF NOT IntSets . IsElement ( LFromAtom , LFromDeclIdSet )
        THEN LFound := FALSE 
        ELSE 
          LFromDeclNo := FM3Dict . LookupGrowable ( LFromDeclDict , LFromAtom )
        ; LFound := LFromDeclNo # FM3Base . DeclNoNull
        END (*IF*) 
      END (*IF*)
    ; IF NOT LFound
      THEN
        LFromExpImpIdSet := FromUnitRef ^ . UntExpImpScopeRef ^ . ScpDeclIdSet
      ; IF IntSets . IsElement ( LFromAtom , LFromExpImpIdSet )
        THEN
          LNote := NIL  
        ELSE LNote := NIL 
        END (*IF*) 
      ; FM3Messages . ErrorArr
          ( ARRAY OF REFANY
              { "Interface "
              , FromUnitRef ^ . UntUnitIdent 
              , " has no declaration named "
              , Text . FromChars ( IdScanAttribute . SaChars ^ )
              , LNote
              , Wr . EOL 
              }
          , IdScanAttribute . Position 
          ) 
      ELSE 
      
      END (*IF*) 
    END ImportDeclByIdent
*) 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
