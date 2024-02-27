
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Scopes

; IMPORT IntRanges
; IMPORT IntSets
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Decls 
; IMPORT FM3Dict_Int_Int
; IMPORT FM3Globals 
; IMPORT FM3SharedUtils
; IMPORT FM3Units
; IMPORT Ranges_Int

(*EXPORTED*) 
; PROCEDURE NewScopeMap ( ScopeCt : FM3Base . ScopeNoTyp ) : ScopeMapTyp
  (* One of these per unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , ScopeCt - 1 } ) 
    END NewScopeMap

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
  ; VAR LScopeNo : FM3Base . ScopeNoTyp
  ; VAR LRange : Ranges_Int . RangeTyp  

  ; BEGIN
      LUnitScopeMap := OwningUnitRef ^ . UntScopeMap 
    ; LRange := VarArray_Int_Refany . TouchedRange ( LUnitScopeMap )
    ; IF Ranges_Int . RangeIsEmpty ( LRange ) 
      THEN LScopeNo := FM3Base . ScopeNoFirstReal 
      ELSE LScopeNo := LRange . Hi + 1
      END (* IF *) 
    ; LScopeRef := NEW ( ScopeRefTyp )
    ; LScopeRef ^ . ScpScopeNo := LScopeNo
    ; LScopeRef ^ . ScpKind := ScopeKind
    ; LScopeRef ^ . ScpPosition := Position
    ; LScopeRef ^ . ScpOnStackCt := 0

    ; LScopeRef ^ . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpDuplDeclIdSet := IntSets . Empty ( )
    ; LScopeRef ^ . ScpMinDeclNo := FM3Base . DeclNoMax 
    ; LScopeRef ^ . ScpDeclCt := FM3Base . DeclNoNull  
    ; LScopeRef ^ . ScpOwningUnitRef := OwningUnitRef 
    ; LScopeRef ^ . ScpOwningDeclNo := FM3Base . DeclNoNull
    ; VarArray_Int_Refany . Assign ( LUnitScopeMap , LScopeNo , LScopeRef )
    ; RETURN LScopeRef 
    END NewScopeRef

(*EXPORTED.*)
; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Base . ScopeNoTyp )
  : FM3Scopes . ScopeRefTyp
  (* In the current unit. *) 

  = VAR LScopeMap : FM3Scopes . ScopeMapTyp 
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp

  ; BEGIN
      LScopeMap := FM3Units . UnitStackTopRef ^ . UntScopeMap
    ; IF LScopeMap = NIL TEN RETURN NIL END 
    ; LScopeRef := VarArray_Int_Refany . Fetch ( LScopeMap , ScopeNo )
    ; RETURN LScopeRef  
    END ScopeRefOfScopeNo 

(*EXPORTED.*)
; PROCEDURE PushDeclScopeRef ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushDeclScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; ScopeRef ^ . ScpStackLink := DeclScopeStackTopRef
    ; DeclScopeStackTopRef := ScopeRef
    ; INC ( ScopeRef ^ . ScpOnStackCt ) 
    ; INC ( DeclScopeStackCt ) 
    END PushDeclScopeRef

(*EXPORTED.*)
; PROCEDURE PushLookupScopeRef ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushLookupScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; ScopeRef ^ . ScpStackLink := LookupScopeStackTopRef
    ; LookupScopeStackTopRef := ScopeRef
    ; INC ( DeclScopeStackCt ) 
    ; INC ( LookupScopeStackCt ) 
    END PushLookupScopeRef

(*EXPORTED.*)
; PROCEDURE PopDeclScope ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopDeclScope*)
      LPoppedScopeRef := DeclScopeStackTopRef 
    ; DeclScopeStackTopRef := LPoppedScopeRef . ScpStackLink
    ; DEC ( DeclScopeStackCt ) 
    ; DEC ( LPoppedScopeRef ^ . ScpOnStackCt )
    ; <* ASSERT ( DeclScopeStackTopRef = NIL ) = ( DeclScopeStackCt = 0 ) *> 
      <* ASSERT LPoppedScopeRef ^ . ScpOnStackCt <= DeclScopeStackCt *> 
     RETURN LPoppedScopeRef
    END PopDeclScope 

(*EXPORTED.*)
; PROCEDURE PopLookupScope ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopLookupScope*)
      LPoppedScopeRef := LookupScopeStackTopRef 
    ; LookupScopeStackTopRef := LPoppedScopeRef . ScpStackLink
    ; DEC ( LookupScopeStackCt ) 
    ; DEC ( LPoppedScopeRef ^ . ScpOnStackCt )
    ; <* ASSERT ( LookupScopeStackTopRef = NIL ) = ( LookupScopeStackCt = 0 ) *>
      <* ASSERT LPoppedScopeRef ^ . ScpOnStackCt <= LookupScopeStackCt *> 
     RETURN LPoppedScopeRef
    END PopLookupScope 

; BEGIN
    DeclScopeStackTopRef := NIL
  ; DeclScopeStackCt := 0 
  ; LookupScopeStackTopRef := NIL
  ; LookupScopeStackCt := 0 
  END FM3Scopes
.

