
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Scopes

; IMPORT IntRanges
; IMPORT IntSets
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Dict_Int_Int
; IMPORT FM3SharedUtils 

(*EXPORTED*) 
; PROCEDURE NewMap ( ScopeCt := InitScopeCt ) : ScopeMapTyp
  (* A single global map of scopes. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , ScopeCt - 1 } ) 
    END NewMap

(*EXPORTED*) 
; PROCEDURE NewScope
    ( OwningUnitRef : FM3Units . UnitRefTyp ; InitDictCt := DefaultInitDictCt )
  : ScopeRefTyp
  (* Allocate and connect a ScopeNo and ScopeRef Owned by OwningUnitRef. *) 

  = VAR LScopeMap : FM3Scopes . ScopeMapTyp
  ; VAR LScopeRef : ScopeRefTyp
  ; VAR LScopeNo : ScopeNoTyp
  ; VAR LRange : Rages_Int . T 

  ; BEGIN
      LScopesMap := OwningUnitRef ^ . UntScopesMap 
    ; LRange := VarArray_Int_Refany . TouchedRange ( LScopesMap )
    ; IF Ranges_Int . RangeIsEmpty ( LRange ) 
      THEN LScopeNo := 0
      ELSE LScopeNo := LRange . Hi + 1
      END (* IF *) 
    ; LScopeRef := NEW ( ScopeRefTyp )
    ; LScopeRef . ScpNumber := LScopeNo  
    ; LScopeRef . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef . ScpDuplIdSet := IntSets . Empty ( )  
    ; LScopeRef . ScpDuplIdSet := IntSets . Empty ( )  
    ; LScopeRef . ScpDeclCt := 0
    ; LScopeRef . OwningDeclNo := FM3Base . MapNoNull
    ; LScopeRef . ScpDeclMap := FMeDecls . NewMap ( InitDictCt ) 
    ; VarArray_Int_Refany . Assign ( LScopesMap , LScopeNo , LScopeRef )
    ; RETURN LScopeRef 
    END NewScope 

(*EXPORTED.*)
; PROCEDURE Push ( ScopeRef : ScopeRefTyp  ) : INTEGER (* Depth after push. *) 

  = BEGIN (*Push*)
      IF ScopeRef = NIL THEN RETURN END (*IF*)
    ; WITH WUnit = FM3Globals . CurrentUnitRef
      DO 
        ScopeRef ^ . ScpStackLink := WUnit . UntScopeStackTop 
      ; WUnit . UntScopeStackTop := ScopeRef
      ; INC ( WUnit . UntStackDepth ) (* Overflow? *)  
      ; RETURN WUnit . UntStackDepth
      END (*WITH*) 
    END Push
(* TODO: Put scope depth into the scope object (and probably not
        in the Unit object.
*)




(*EXPORTED.*)
; PROCEDURE Pop ( ) : INTEGER (* Depth before pop. *) 

  = VAR LPoppedScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LDepthBefore : INTEGER

  ; BEGIN (*Pop*)
      WITH WUnit = FM3Globals . CurrentUnitRef
      DO 
        LDepthBefore := WUnit . UntScopeStackDepth
      ; <* ASSERT LDepthBefore > 0 *>
        DEC ( WUnit . UntScopeStackDepth )
      ; LPoppedScope := WUnit . UntScopeStackTop 
      ; <* ASSERT LPoppedScope # NIL *>
        WUnit . UntScopeStackTop := LPoppedScope . ScpStackLink 
      ; RETURN LDepthBefore  
      END (*WITH*) 
    END Pop

; BEGIN
    ScopeStackTop := NIL 
  END FM3Scopes
.

