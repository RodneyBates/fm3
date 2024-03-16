
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
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

; TYPE Ukt = UnitKindTyp 

(*EXPORTED*) 
; PROCEDURE UnitKindImage ( Kind : Ukt ) : TEXT

  = BEGIN
      CASE Kind OF 
      | Ukt . UkNull => RETURN "<NullUnitKind>"
      | Ukt . UkInterface => RETURN "INTERFACE"
      | Ukt . UkGenInterface => RETURN "GENERIC INTERFACE"
      | Ukt . UkInstInterface => RETURN "iNTERFACE"
      | Ukt . UkModule => RETURN "MODULE"
      | Ukt . UkGenModule => RETURN "GENERIC MODULE"
      | Ukt . UkInstModule => RETURN "MODULE"
      END (*CASE*) 
    END UnitKindImage 

(*EXPORTED*) 
; PROCEDURE UnitKindSectionNo ( Kind : Ukt ) : TEXT

  = BEGIN
      CASE Kind OF 
      | Ukt . UkNull => RETURN ""
      | Ukt . UkInterface => RETURN "2.5.2"
      | Ukt . UkGenInterface => RETURN "2.5.5"
      | Ukt . UkInstInterface => RETURN "2.5.5"
      | Ukt . UkModule => RETURN "2.5.3"
      | Ukt . UkGenModule => RETURN "2.5.5"
      | Ukt . UkInstModule => RETURN "2.5.5"
      END (*CASE*) 
    END UnitKindSectionNo  


(*EXPORTED*) 
; PROCEDURE NewUnitMap ( InitUnitCt : FM3Base . UnitNoTyp ) : UnitMapTyp
  (* One UnitMap in a compile. *) 

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
          ( ARRAY OF REFANY { "Allocation of a FM3Units.UnitRefTyp failed." } )
      END 

    ; LUnitNo := NextUnitNo
    ; INC ( NextUnitNo )

    ; LUnitRef ^ . UntStackLink := NIL 
    ; LUnitRef ^ . UntUnitNo := LUnitNo
    ; LUnitRef ^ . UntSrcFileSimpleName := NIL 
    ; LUnitRef ^ . UntSrcFilePath := NIL
    ; LUnitRef ^ . UntBuildDirPath := NIL
    ; LUnitRef ^ . UntPatchStackName := NIL
    ; LUnitRef ^ . UntPatchStackRdBack := NIL
    ; LUnitRef ^ . UntMaxPatchStackDepth := 0L 
    ; LUnitRef ^ . UntPass1OutSimpleName := NIL
    ; LUnitRef ^ . UntPass1OutRdBack := NIL
    ; LUnitRef ^ . UntMaxPass1OutDepth := 0L 
    ; LUnitRef ^ . UntPass2OutSimpleName := NIL
    ; LUnitRef ^ . UntPass2OutRdBack := NIL
    ; LUnitRef ^ . UntUnitIdentAtom := FM3Base . AtomNull
    ; LUnitRef ^ . UntUnitIdentPos := FM3Base . PositionNull 
    ; LUnitRef ^ . UntUnsafe := FALSE 
    ; LUnitRef ^ . UntIdentAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . IdentAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntNumberAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . NumberAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntCharsAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . CharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntWCharsAtomDict 
        := FM3Atom_OAWideChars . New
             ( FM3Globals . WideCharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := NIL
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntScopeMap
        := FM3Scopes . NewScopeMap ( FM3Globals . InitScopeCtPerUnit ) 
    ; LUnitRef ^ . UntDeclMap 
        := FM3Decls . NewDeclMap ( FM3Globals . InitDeclCtPerUnit ) 
    ; LUnitRef ^ . UntNextDeclNo := 1

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
    ; RETURN LIdentText 
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
    ; FM3Globals . P1RdBack := UnitRef . UntPass1OutRdBack 
    ; FM3Globals . PatchRdBack := UnitRef . UntPatchStackRdBack 
    ; FM3Globals . P2RdBack := UnitRef . UntPass2OutRdBack 
    ; UnitRef ^ . UntStackLink := LBeneathUnitRef  
    ; UnitStackTopRef := UnitRef
    END PushUnit

(*EXPORTED.*)
; PROCEDURE PopUnit ( ) : UnitRefTyp  

  = VAR LPoppedUnitRef : UnitRefTyp

  ; BEGIN (*Pop*)
      FM3Globals . P1RdBack := NIL 
    ; FM3Globals . PatchRdBack := NIL 
    ; FM3Globals . P2RdBack := NIL 
    ; LPoppedUnitRef := UnitStackTopRef  
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

(*EXPORTED.*)
; PROCEDURE CurrentlyInModule ( ) : BOOLEAN

  = BEGIN 
      RETURN UnitStackTopRef ^ . UntKind IN UnitKindSetModule  
    END CurrentlyInModule

; BEGIN
    UnitMap := NewUnitMap ( FM3Globals . InitUnitCt - 1 ) 

  ; NextUnitNo := 1
  ; UnitStackTopRef := NIL 
  END FM3Units
.

