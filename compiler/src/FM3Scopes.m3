
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
; PROCEDURE NewCompScopeRef ( ) : ScopeRefTyp
  (* A single, unique scope pertaining to the entire run of the compiler
     and containing only names of source files.
  *) 

  = VAR LScopeRef : ScopeRefTyp

  ; BEGIN
      LScopeRef := NEW ( ScopeRefTyp )
    ; LScopeRef ^ . ScpScopeNo := FM3Base . ScopeNoFirstReal  
    ; LScopeRef ^ . ScpKind := ScopeKindTyp . SkComp 
    ; LScopeRef ^ . ScpPosition := FM3Base . PositionNull 
    ; LScopeRef ^ . ScpOnDeclStackCt := 0
    ; LScopeRef ^ . ScpOnLookupStackCt := 0
    ; LScopeRef ^ . ScpDeclIdSet := IntSets . Empty ( )
    ; LScopeRef ^ . ScpFormalIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef ^ . ScpDuplDeclIdSet := IntSets . Empty ( )
    ; LScopeRef ^ . ScpMinDeclNo := FM3Base . DeclNoMax 
    ; LScopeRef ^ . ScpDeclCt := FM3Base . DeclNoNull  
    ; LScopeRef ^ . ScpOwningUnitRef := NIL 
    ; LScopeRef ^ . ScpOwningDeclNo := FM3Base . DeclNoNull
    ; RETURN LScopeRef 
    END NewCompScopeRef

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
    ; LScopeRef ^ . ScpOnDeclStackCt := 0
    ; LScopeRef ^ . ScpOnLookupStackCt := 0
 
    ; LScopeRef ^ . ScpDeclIdSet := IntSets . Empty ( )
                    (* ^For a procedure w/ body, includes formals. *) 
    ; LScopeRef ^ . ScpFormalIdSet := IntSets . Empty ( )  
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
; PROCEDURE ScopeRefOfScopeNo ( ScopeNo : FM3Base . ScopeNoTyp ) : ScopeRefTyp
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
; PROCEDURE PushLookupScopeRef ( ScopeRef : ScopeRefTyp ) 

  = BEGIN (*PushLookupScopeRef*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; ScopeRef ^ . ScpLookupStackLink := LookupScopeStackTopRef
    ; LookupScopeStackTopRef := ScopeRef
    ; INC ( ScopeRef ^ . ScpOnLookupStackCt ) 
    ; INC ( LookupScopeStackCt ) 
    END PushLookupScopeRef

(*EXPORTED.*)
; PROCEDURE PopLookupScopeRef ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*PopLookupScope*)
      LPoppedScopeRef := LookupScopeStackTopRef 
    ; LookupScopeStackTopRef := LPoppedScopeRef . ScpLookupStackLink
    ; DEC ( LookupScopeStackCt ) 
    ; DEC ( LPoppedScopeRef ^ . ScpOnLookupStackCt )
    ; <* ASSERT ( LookupScopeStackTopRef = NIL ) = ( LookupScopeStackCt = 0 ) *>
      RETURN LPoppedScopeRef
    END PopLookupScopeRef 

; BEGIN
    DeclScopeStackTopRef := NIL
  ; DeclScopeStackCt := 0 
  ; LookupScopeStackTopRef := NIL
  ; LookupScopeStackCt := 0 
  END FM3Scopes
.

