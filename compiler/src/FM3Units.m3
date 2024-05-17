
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Units

; IMPORT Text

; IMPORT IntSets 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *)

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_OAWideChars 
; IMPORT FM3Atom_Text  
; IMPORT FM3Base 
; IMPORT FM3Decls
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3Scopes
; IMPORT FM3Utils 
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


; PROCEDURE NewUnitsMap
    ( InitUnitCt : FM3Base . UnitNoTyp ) : VarArray_Int_Refany . T
  (* One UnitsMap in a compile. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , Ranges_Int . RangeTyp {  0 , InitUnitCt - 1 } ) 
    END NewUnitsMap

(*EXPORTED*) 
; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitsMap. *)

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
(* TODO: Either complete the list of constant-initialized fields, or bite
         nails and rely on the declaration.
*) 
    ; LUnitRef ^ . UntStackLink := NIL 
    ; LUnitRef ^ . UntUnitNo := LUnitNo
    ; LUnitRef ^ . UntSrcFileSimpleName := NIL 
    ; LUnitRef ^ . UntSrcFilePath := NIL
    ; LUnitRef ^ . UntBuildDirPath := NIL
    ; LUnitRef ^ . UntPatchStackSimpleName := NIL
    ; LUnitRef ^ . UntPatchStackRdBack := NIL
    ; LUnitRef ^ . UntMaxPatchStackDepth := 0L 
    ; LUnitRef ^ . UntUnitRefImporting := NIL 
    ; LUnitRef ^ . UntPositionOfImport := FM3Base . PositionNull  
    ; LUnitRef ^ . UntPass1OutSimpleName := NIL
    ; LUnitRef ^ . UntPass1OutRdBack := NIL
    ; LUnitRef ^ . UntMaxPass1OutLength := 0L 
    ; LUnitRef ^ . UntPass2OutSimpleName := NIL
    ; LUnitRef ^ . UntPass2OutRdBack := NIL
    ; LUnitRef ^ . UntDeclScopeRef := NIL
    ; LUnitRef ^ . UntSkipStackBase := 0 
    ; LUnitRef ^ . UntUnitIdent := NIL 
    ; LUnitRef ^ . UntUnitIdentPos := FM3Base . PositionNull
    ; LUnitRef ^ . UntState := UnitStateTyp . UsNull
    ; LUnitRef ^ . UntUnsafe := FALSE 
    ; LUnitRef ^ . UntInCycle := FALSE
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
             , HashFunc := FM3Utils . HashOfOAChars 
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
    ; LUnitRef ^ . UntExpImpIdSet := IntSets . Empty ( ) 
    ; LUnitRef ^ . UntDeclMap 
        := FM3Decls . NewDeclMap ( FM3Globals . InitDeclCtPerUnit ) 
    ; LUnitRef ^ . UntNextDeclNo := 1
    ; LUnitRef ^ . UntSkipStackBase
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi 
    ; VarArray_Int_Refany . Assign ( UnitsMap , LUnitNo , LUnitRef )
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

  ; BEGIN (*PushUnit*)
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
; PROCEDURE CacheTopUnitValues ( )

  = BEGIN
      IF UnitStackTopRef = NIL 
      THEN
        FM3Globals . P1RdBack := NIL 
      ; FM3Globals . PatchRdBack := NIL 
      ; FM3Globals . P2RdBack := NIL 
      ELSE 
        FM3Globals . P1RdBack := UnitStackTopRef . UntPass1OutRdBack 
      ; FM3Globals . PatchRdBack := UnitStackTopRef . UntPatchStackRdBack 
      ; FM3Globals . P2RdBack := UnitStackTopRef . UntPass2OutRdBack
      END (*IF*)    
    END CacheTopUnitValues 

(*EXPORTED.*)
; PROCEDURE PopUnit ( ) : UnitRefTyp  

  = VAR LPoppedUnitRef : UnitRefTyp

  ; BEGIN (*PopUnit*)
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

(*EXPORTED.*)
; PROCEDURE CurrentUnitIsModule ( ) : BOOLEAN

  = BEGIN 
      RETURN UnitStackTopRef ^ . UntKind IN UnitKindSetModule  
    END CurrentUnitIsModule

; BEGIN
    UnitsAtomDict  
      := FM3Atom_Text . New
           ( FM3Globals . InitUnitsCt + FM3Globals . InitUnitsCt DIV 3
           , FM3Base . AtomFirstReal
           , HashFunc := FM3Utils . HashOfText 
           , DoReverseMap := TRUE (* Needed? *) 
           )
           
  ; UnitsMap := NewUnitsMap ( FM3Globals . InitUnitsCt - 1 )
  ; NextUnitNo := 1
  ; UnitStackTopRef := NIL 
  END FM3Units
.

