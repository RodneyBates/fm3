
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
; PROCEDURE NewScopeRef ( OwningUnitRef : FM3Units . UnitRefTyp ) : ScopeRefTyp
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
    ; LScopeRef . ScpScopeNo := LScopeNo  
    ; LScopeRef . ScpRefIdSet := IntSets . Empty ( )  
    ; LScopeRef . ScpDuplDeclIdSet := IntSets . Empty ( )
    ; LScopeRef . ScpMinDeclNo := FM3Base . DeclNoMax 
    ; LScopeRef . ScpDeclCt := 0
    ; LScopeRef . ScpOwningUnitNo := FM3Base . UnitNoNull
    ; LScopeRef . ScpOwningDeclNo := FM3Base . DeclNoNull
    ; LScopeRef . ScpStackDepth := 0
    ; VarArray_Int_Refany . Assign ( LUnitScopeMap , LScopeNo , LScopeRef )
    ; RETURN LScopeRef 
    END NewScopeRef 

(*EXPORTED.*)
; PROCEDURE PushScope ( ScopeRef : ScopeRefTyp ) 

  = VAR LBeneathScopeRef : ScopeRefTyp

  ; BEGIN (*Push*)
      IF ScopeRef = NIL THEN RETURN END (*IF*) 
    ; <* ASSERT ScopeRef . ScpStackDepth = 0 *> (* Not already on stack. *)
      LBeneathScopeRef := ScopeStackTopRef  
    ; IF LBeneathScopeRef = NIL
      THEN ScopeRef . ScpStackDepth := 1
      ELSE ScopeRef . ScpStackDepth := LBeneathScopeRef . ScpStackDepth + 1
      END (*IF*) 
    ; ScopeRef ^ . ScpStackLink := LBeneathScopeRef  
    ; ScopeStackTopRef := ScopeRef
    END PushScope

(*EXPORTED.*)
; PROCEDURE PopScope ( ) : ScopeRefTyp  

  = VAR LPoppedScopeRef : ScopeRefTyp

  ; BEGIN (*Pop*)
      LPoppedScopeRef := ScopeStackTopRef 
    ; <* ASSERT LPoppedScopeRef # NIL *>
      ScopeStackTopRef := LPoppedScopeRef . ScpStackLink
    ; IF ScopeStackTopRef = NIL
      THEN <* ASSERT LPoppedScopeRef . ScpStackDepth = 1 *>
      ELSE
        <* ASSERT
             ScopeStackTopRef . ScpStackDepth
             = LPoppedScopeRef . ScpStackDepth - 1
        *>
      END (*IF*)
    ; LPoppedScopeRef . ScpStackDepth := 0  (* Note not on stack. *)
    ; RETURN LPoppedScopeRef
    END PopScope 

; BEGIN
    ScopeStackTopRef := NIL 
  END FM3Scopes
.

