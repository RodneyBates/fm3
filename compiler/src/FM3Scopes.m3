
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
; PROCEDURE NewScopeMap ( ScopeCt : FM3Globals . ScopeNoTyp ) : ScopeMapTyp
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
    ; LScopeRef ^ . ScpOnDeclStackCt := 0
    ; LScopeRef ^ . ScpOnOpenScopeStackCt := 0
 
    ; LScopeRef ^ . ScpDeclIdSet := IntSets . Empty ( )
                    (* ^For a unit, includes [ex|im]ported idents. *) 
                    (* ^For a procedure w/ body, includes formals. *)
    ; LScopeRef ^ . ScpDeclDict := NIL 
    ; LScopeRef ^ . ScpFormalIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpDuplDeclIdSet := IntSets . Empty ( )
    ; LScopeRef ^ . ScpDeclCt := FM3Globals . DeclNoNull  
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
    ; ScopeRef ^ . ScpDeclStackLink := DeclScopeStackTopRef
    ; DeclScopeStackTopRef := ScopeRef
    ; INC ( ScopeRef ^ . ScpOnDeclStackCt ) 
    ; INC ( DeclScopeStackCt ) 
    END PushDeclScopeRef

(*EXPORTED.*)
; PROCEDURE PopDeclScopeRef ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopDeclScope*)
      LPoppedScopeRef := DeclScopeStackTopRef 
    ; DeclScopeStackTopRef := LPoppedScopeRef . ScpDeclStackLink
    ; DEC ( DeclScopeStackCt ) 
    ; DEC ( LPoppedScopeRef ^ . ScpOnDeclStackCt )
    ; <* ASSERT ( DeclScopeStackTopRef = NIL ) = ( DeclScopeStackCt = 0 ) *> 
      RETURN LPoppedScopeRef
    END PopDeclScopeRef 

(*EXPORTED.*)
; PROCEDURE PushOpenScopeRef ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushOpenScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; ScopeRef ^ . ScpOpenScopeStackLink := OpenScopeStackTopRef
    ; OpenScopeStackTopRef := ScopeRef
    ; INC ( ScopeRef ^ . ScpOnOpenScopeStackCt ) 
    ; INC ( OpenScopeStackCt ) 
    END PushOpenScopeRef

(*EXPORTED.*)
; PROCEDURE PopOpenScopeRef ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopOpenScope*)
      LPoppedScopeRef := OpenScopeStackTopRef 
    ; OpenScopeStackTopRef := LPoppedScopeRef . ScpOpenScopeStackLink
    ; DEC ( OpenScopeStackCt ) 
    ; DEC ( LPoppedScopeRef ^ . ScpOnOpenScopeStackCt )
    ; <* ASSERT ( OpenScopeStackTopRef = NIL ) = ( OpenScopeStackCt = 0 ) *>
      RETURN LPoppedScopeRef
    END PopOpenScopeRef 

; BEGIN
    DeclScopeStackTopRef := NIL
  ; DeclScopeStackCt := 0 
  ; OpenScopeStackTopRef := NIL
  ; OpenScopeStackCt := 0 
  END FM3Scopes
.

