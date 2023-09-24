
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Units

; IMPORT Text 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_OAWideChars 
; IMPORT FM3Base 
; IMPORT FM3Decls
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3Scopes
; IMPORT Ranges_Int
; IMPORT VarArray_Int_Refany 

; VAR NextUnitNo : INTEGER := 1 

(*EXPORTED*) 
; PROCEDURE NewUnitMap ( InitUnitCt : FM3Base . UnitNoTyp ) : UnitMapTyp
  (* One UnnitMap in a compile. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , Ranges_Int . RangeTyp {  0 , InitUnitCt - 1 } ) 
    END NewUnitMap

(*EXPORTED.*)
; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitMap. *)

  = VAR LUnitRef : UnitRefTyp
  ; VAR LUnitNo : FM3Base . UnitNoTyp 

  ; BEGIN
      LUnitRef := NEW ( UnitRefTyp )
    ; IF LUnitRef = NIL
      THEN
        FM3Messages  . FatalArr
          ( ARRAT OF REFANY { "Allocation of a FM3Units.UnitRefTyp failed." } )
      END 

    ; LUnitNo := NextUnitNo
    ; INC ( NextUnitNo )

    ; LUnitRef . UntStackLink := NIL 
    ; LUnitRef . UntUnitNo := LUnitNo
    ; LUnitRef . UntSrcFileName := NIL 
    ; LUnitRef . UntSrcFilePrefix := NIL(* Without simple name. *) 
    ; LUnitRef . UntWorkFilePrefix := NIL
    ; LUnitRef . UntPatchStackName := NIL
    ; LUnitRef . UntPatchStackRdBack := NIL
    ; LUnitRef . UntMaxPatchStackDepth := 0L 
    ; LUnitRef . UntUnnestStackName := NIL
    ; LUnitRef . UntUnnestStackRdBack := NIL
    ; LUnitRef . UntMaxUnnestStackDepth := 0L 
    ; LUnitRef . UntParsePassName := NIL
    ; LUnitRef . UntParsePassRdBack := NIL
    ; LUnitRef . UntIdentAtomDict
        := FM3Atom_OAChars . New
             ( FM3Globals . IdentAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef . UntNumberAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . NumberAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef . UntCharsAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . CharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef . UntWCharsAtomDict 
        := FM3Atom_OAWideChars . New
             ( FM3Globals . WideCharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef . UntScopeMap
        := FM3Scopes . NewScopeMap ( FM3Globals . InitScopeCtPerUnit ) 
    ; LUnitRef . UntDeclMap 
        := FM3Decls . NewDeclMap ( FM3Globals . InitDeclCtPerUnit ) 
    ; LUnitRef . UntNextDeclNo := 1

    ; VarArray_Int_Refany . Assign ( UnitMap , LUnitNo , LUnitRef )
    ; RETURN LUnitRef 
    END NewUnitRef
    
(*EXPORTED.*)
; PROCEDURE AllocateDeclNos ( Ct : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Ct Decl numbers, unique
     within the current scope, and return the lowest number.
  *) 

  = VAR LResult : INTEGER 

  ; BEGIN (*AllocateDeclNos*)
      IF UnitStackTopRef = NIL THEN RETURN FM3Base . DeclNoNull END (*IF*)
    ; LResult := UnitStackTopRef ^ . UntNextDeclNo
    ; INC ( UnitStackTopRef ^ . UntNextDeclNo , Ct )
    ; RETURN LResult 
    END AllocateDeclNos
    
(*EXPORTED.*)
; PROCEDURE TextOfIdAtom ( IdAtom : FM3Base . AtomTyp ) : TEXT 
  (* In the current unit. *) 

  = VAR LCharsRef : FM3Atom_OAChars . KeyTyp
  ; VAR LIdentText : TEXT 

  ; BEGIN (*TextOfIdAtom*)
      IF NOT FM3Atom_OAChars . Key 
               ( UnitStackTopRef ^ . UntIdentAtomDict
               , IdAtom
               , (*OUT*) LCharsRef
               )
      THEN LIdentText := "<NotFound>"
      ELSIF LCharsRef = NIL
      THEN LIdentText := "<NIL>"
      ELSE LIdentText := Text . FromChars ( LCharsRef ^ )
      END (*IF*)
    END TextOfIdAtom

(*EXPORTED.*)
; PROCEDURE PushUnit ( UnitRef : UnitRefTyp ) 

  = VAR LBeneathUnitRef : UnitRefTyp

  ; BEGIN (*Push*)
      IF UnitRef = NIL THEN RETURN END (*IF*) 
    ; <* ASSERT UnitRef . UntStackDepth = 0 *> (* Not already on stack. *)
      LBeneathUnitRef := UnitStackTopRef 
    ; IF LBeneathUnitRef = NIL
      THEN UnitRef . UntStackDepth := 1
      ELSE UnitRef . UntStackDepth := LBeneathUnitRef . UntStackDepth + 1
      END (*IF*) 
    ; UnitRef ^ . UntStackLink := LBeneathUnitRef  
    ; UnitStackTopRef := UnitRef
    END PushUnit

(*EXPORTED.*)
; PROCEDURE PopUnit ( ) : UnitRefTyp  

  = VAR LPoppedUnitRef : UnitRefTyp

  ; BEGIN (*Pop*)
      LPoppedUnitRef := UnitStackTopRef  
    ; <* ASSERT LPoppedUnitRef # NIL *>
      UnitStackTopRef := LPoppedUnitRef ^ . UntStackLink
    ; IF UnitStackTopRef = NIL
      THEN <* ASSERT LPoppedUnitRef ^ . UntStackDepth = 1 *> 
      ELSE 
        <* ASSERT
             UnitStackTopRef ^ . UntStackDepth
             = LPoppedUnitRef ^ . UntStackDepth - 1
        *>
      END (*IF*)
    ; LPoppedUnitRef . UntStackDepth := 0 (* Note not on stack> *)  
    ; RETURN LPoppedUnitRef
    END PopUnit

; BEGIN
    UnitMap := NewUnitMap ( FM3Globals . InitUnitCt - 1 ) 

  ; NextUnitNo := 1
  ; UnitStackTopRef := NIL 
  END FM3Units
.

