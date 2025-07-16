
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Scopes

; IMPORT Fmt 

; IMPORT IntRanges
; IMPORT IntSets
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Globals
; IMPORT FM3Messages
; IMPORT FM3SharedUtils 
; IMPORT FM3Units
; IMPORT FM3Utils 
; IMPORT Ranges_Int

(*EXPORTED*) 
; PROCEDURE NewScopeMap ( ScopeCt : FM3Globals . ScopeNoTyp ) : ScopeMapTyp
  (* One of these per unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , ScopeCt - 1 } ) 
    END NewScopeMap

(*EXPORTED.*)
; PROCEDURE ScopeRefImage ( ScopeRef : ScopeRefTyp ) : TEXT 
  (* DeclNo, REF, and Position. *) 
  
  = VAR LResult : TEXT

  ; BEGIN (*ScopeRefImage*)
      IF ScopeRef = NIL THEN RETURN "NIL" END (*IF*)
    ; LResult := FM3SharedUtils . CatArrT
        ( ARRAY OF REFANY
            { "ScopeNo " 
            , Fmt . Int ( ScopeRef ^ . ScpSelfScopeNo ) 
            , " at " 
            , FM3Utils . RefanyImage ( ScopeRef )
            , " "
            , FM3Utils . PositionImage ( ScopeRef ^ . ScpPosition )
            }
        ) 
    ; RETURN LResult 
    END ScopeRefImage

(*EXPORTED*) 
; PROCEDURE NewScopeRef
    ( OwningUnitRef : FM3Units . UnitRefTyp
    ; ScopeKind : ScopeKindTyp
    ; READONLY Position : FM3Base . tPosition
    )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef Owned by OwningUnitRef. *) 

  = VAR LUnitScopeMap : FM3Base . MapTyp
  ; VAR LScopeRef : ScopeRefTyp
  ; VAR LScopeNo : FM3Globals . ScopeNoTyp
  ; VAR LRange : Ranges_Int . RangeTyp  

  ; BEGIN
      LUnitScopeMap := OwningUnitRef ^ . UntScopeMap 
    ; LRange := VarArray_Int_Refany . TouchedRange ( LUnitScopeMap )
    ; IF Ranges_Int . RangeIsEmpty ( LRange ) 
      THEN LScopeNo := FM3Globals . ScopeNoFirstReal
      ELSE LScopeNo := LRange . Hi + 1
      END (* IF *) 
    ; LScopeRef := NEW ( ScopeRefTyp )
    ; LScopeRef ^ . ScpSelfScopeNo := LScopeNo
    ; LScopeRef ^ . ScpKind := ScopeKind
    ; LScopeRef ^ . ScpPosition := Position
    ; LScopeRef ^ . ScpDeclStackHt := - 1 
    ; LScopeRef ^ . ScpOpenStackHt := - 1 
 
    ; LScopeRef ^ . ScpDeclIdSet := IntSets . Empty ( )
    ; LScopeRef ^ . ScpDeclDict := NIL 
    ; LScopeRef ^ . ScpFormalIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpDeclCt := - 27 (* FM3Globals . DeclNoNull*)  
    ; LScopeRef ^ . ScpOwningUnitRef := OwningUnitRef 
    ; LScopeRef ^ . ScpOwningDeclNo := FM3Globals . DeclNoNull
    ; VarArray_Int_Refany . Assign ( LUnitScopeMap , LScopeNo , LScopeRef )
    ; RETURN LScopeRef 
    END NewScopeRef

(*EXPORTED.*)
; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Globals . ScopeNoTyp ) : ScopeRefTyp
  (* In the current unit. *) 

  = VAR LScopeMap : ScopeMapTyp 
  ; VAR LScopeRef : ScopeRefTyp

  ; BEGIN
      LScopeMap := FM3Units . UnitStackTopRef ^ . UntScopeMap
    ; IF LScopeMap = NIL THEN RETURN NIL END 
    ; LScopeRef := VarArray_Int_Refany . Fetch ( LScopeMap , ScopeNo )
    ; RETURN LScopeRef  
    END ScopeRefOfScopeNo 

(*EXPORTED.*)
; PROCEDURE PushDeclScopeRef ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushDeclScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; <* ASSERT ScopeRef ^ . ScpDeclStackHt = - 1 *>
      IF DeclScopeStackTopRef = NIL
      THEN ScopeRef ^ . ScpDeclStackHt := 1 
      ELSE ScopeRef ^ . ScpDeclStackHt
             := DeclScopeStackTopRef ^ . ScpDeclStackHt + 1
      END (*IF*) 

;  IF ScopeRef ^ . ScpDeclStackHt = 2
  THEN VAR Debug : INTEGER
  ; BEGIN
      Debug := 13
    END
  END

    ; ScopeRef ^ . ScpDeclStackLink := DeclScopeStackTopRef
    ; DeclScopeStackTopRef := ScopeRef
    ; INC ( DeclScopeStackCt ) 
    END PushDeclScopeRef

(*EXPORTED.*)
; PROCEDURE PopDeclScopeRef ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopDeclScope*)
      LPoppedScopeRef := DeclScopeStackTopRef 
    ; DeclScopeStackTopRef := LPoppedScopeRef ^ . ScpDeclStackLink
    ; DEC ( DeclScopeStackCt ) 
    ; <* ASSERT ( DeclScopeStackTopRef = NIL ) = ( DeclScopeStackCt = 0 ) *>
      LPoppedScopeRef ^ . ScpDeclStackHt := - 1 
    ; RETURN LPoppedScopeRef
    END PopDeclScopeRef 

(*EXPORTED.*)
; PROCEDURE PruneDeclScopeStack ( ToDepth : INTEGER := 0 )

  = BEGIN (*PruneDeclScopeStack*)
      IF DeclScopeStackCt > ToDepth 
      THEN 
        FM3Messages . FM3LogArrUnit
          ( ARRAY OF REFANY
              { "Scope number "
              , Fmt . Int ( DeclScopeStackTopRef ^ . ScpSelfScopeNo )
              , " remains on decl scope stack at depth "
              , Fmt . Int ( DeclScopeStackCt )
              , " when expected down to depth "
              , Fmt . Int ( ToDepth )
              , "." 
              } 
          , DeclScopeStackTopRef ^ . ScpPosition
          ) 
      ; REPEAT EVAL PopDeclScopeRef ( ) 
      ; UNTIL DeclScopeStackCt <= ToDepth  
      END (*IF*) 
   END PruneDeclScopeStack

(*EXPORTED.*)
; PROCEDURE PushOpenScopeRef ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushOpenScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; <* ASSERT ScopeRef ^ . ScpOpenStackHt = - 1 *>
      IF OpenScopeStackTopRef = NIL
      THEN ScopeRef ^ . ScpOpenStackHt := 1 
      ELSE ScopeRef ^ . ScpOpenStackHt
             := OpenScopeStackTopRef ^ . ScpOpenStackHt + 1
      END (*IF*) 
    ; ScopeRef ^ . ScpOpenScopeStackLink := OpenScopeStackTopRef
    ; OpenScopeStackTopRef := ScopeRef
    ; INC ( OpenScopeStackCt ) 
    END PushOpenScopeRef

(*EXPORTED.*)
; PROCEDURE PopOpenScopeRef ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopOpenScope*)
      LPoppedScopeRef := OpenScopeStackTopRef 
    ; OpenScopeStackTopRef := LPoppedScopeRef ^ . ScpOpenScopeStackLink
    ; DEC ( OpenScopeStackCt ) 
    ; <* ASSERT ( OpenScopeStackTopRef = NIL ) = ( OpenScopeStackCt = 0 ) *>
      LPoppedScopeRef ^ . ScpOpenStackHt := - 1 
    ; RETURN LPoppedScopeRef
    END PopOpenScopeRef (*EXPORTED.*)

(*EXPORTED.*)
; PROCEDURE PruneOpenScopeStack ( ToDepth : INTEGER := 0 )

  = BEGIN (*PruneOpenScopeStack*)
      IF OpenScopeStackCt > ToDepth 
      THEN 
        FM3Messages . FM3LogArrUnit
          ( ARRAY OF REFANY
              { "Scope number "
              , Fmt . Int ( OpenScopeStackTopRef ^ . ScpSelfScopeNo )
              , " remains on open scope stack at depth "
              , Fmt . Int ( OpenScopeStackCt )
              , " when expected down to depth "
              , Fmt . Int ( ToDepth )
              , "." 
              } 
          , OpenScopeStackTopRef ^ . ScpPosition
          ) 
      ; REPEAT EVAL PopOpenScopeRef ( ) 
      ; UNTIL OpenScopeStackCt <= ToDepth  
      END (*IF*) 
   END PruneOpenScopeStack

; BEGIN
    DeclScopeStackTopRef := NIL
  ; DeclScopeStackCt := 0 
  ; OpenScopeStackTopRef := NIL
  ; OpenScopeStackCt := 0 
  END FM3Scopes
.

