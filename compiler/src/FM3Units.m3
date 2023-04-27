
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Units

; FROM Messages IMPORT FATAL
; IMPORT VarArray_Int_Refany 

; VAR NextUnitNo : INTEGER := 1 

; PROCEDURE New ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitMap. *)

  = VAR LUnitRef : UnitRefTyp
  ; VAR LUnitNo : UnitNoTyp 

  ; BEGIN
      LUnitRef := NEW ( UnitRefTyp )
    ; IF LUnitRef = NIL
      THEN FATAL ( "Allocation of a FM3Units.UnitRefTyp failed." )
      END 

    ; LUnitNo := NextUnitNo
    ; INC ( NextUnitNo )
    
    ; LUnitRef . UntUnitNo := LUnitNo
    ; LUnitRef . UntSrcFileName := NIL 
    ; LUnitRef . UntSrcFilePath := NIL(* Without simple name. *) 
    ; LUnitRef . UntWorkFilePath := NIL
    ; LUnitRef . UntPatchStackName := NIL
    ; LUnitRef . UntPatchStackRdBack := NIL
    ; LUnitRef . UntMaxPatchStackDepth := 0 
    ; LUnitRef . UntUnnestStackName := NIL
    ; LUnitRef . UntUnnestStackRdBack := NIL
    ; LUnitRef . UntMaxUnnestStackDepth := 0 
    ; LUnitRef . UntParsePassName := NIL
    ; LUnitRef . UntParsePassRdBack := NIL
    ; LUnitRef . UntIdentAtomDict := NIL (* Identifiers. *)   
    ; LUnitRef . UntNumberAtomDict := NIL (* Numeric literals. *)  
    ; LUnitRef . UntCharsAtomDict := NIL(* TEXT literals. *) 
    ; LUnitRef . UntWCharsAtomDict := NIL (* Wide TEXT literals. *)
    ; LUnitRef . UntScopes := NIL 
    ; LUnitRef . UntDecls := NIL

    ; VarArray_Int_Refany . Assign ( UnitMap , LUnitNo , LUnitRef ) 
    END New
    

; BEGIN
    NextUnitNo := 1
  ; UnitMap := VarArray_Int_Refany . New ( UnitNoNull , InitialAlloc := 10 )  
  END FM3Units
.

