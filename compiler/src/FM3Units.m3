
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Units

; IMPORT Text 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Base 
; IMPORT FM3Decls 
; FROM FM3Messages IMPORT Fatal
; IMPORT VarArray_Int_Refany 

; VAR NextUnitNo : INTEGER := 1 

(*EXPORTED.*)
; PROCEDURE New ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitMap. *)

  = VAR LUnitRef : UnitRefTyp
  ; VAR LUnitNo : UnitNoTyp 

  ; BEGIN
      LUnitRef := NEW ( UnitRefTyp )
    ; IF LUnitRef = NIL
      THEN Fatal ( "Allocation of a FM3Units.UnitRefTyp failed." )
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
    ; LUnitRef . UntIdentAtomDict := NIL (* Identifiers. *)   
    ; LUnitRef . UntNumberAtomDict := NIL (* Numeric literals. *)  
    ; LUnitRef . UntCharsAtomDict := NIL(* TEXT literals. *) 
    ; LUnitRef . UntWCharsAtomDict := NIL (* Wide TEXT literals. *)
    ; LUnitRef . UntScopesMap := NIL
    ; LUnitRef . UntScopeStackTop := NIL 
    ; LUnitRef . UntScopeStackDepth := 0 
    ; LUnitRef . UntDeclsMap := FM3Decls . NewMap )( ) 
    ; LUnitRef , UntNextDeclNo := 0 

    ; VarArray_Int_Refany . Assign ( UnitMap , LUnitNo , LUnitRef )
    ; RETURN LUnitRef 
    END New
    
(*EXPORTED.*)
; PROCEDURE AllocateDeclNos ( Ct : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Ct Decl numbers, unique
     within the current scope, and return the lowest number.
  *) 

  = VAR LResult : INTEGER 

  ; BEGIN (*AllocateDeclNos*)
      IF UnitStackTop = NIL THEN RETURN FM3Decls . DeclNoNull END (*IF*)
    ; LResult := UnitStackTop ^ . UntNextDeclNo
    ; INC UnitStackTop ^ . UntNextDeclNo ( Ct )
    ; RETURN LResult 
    END AllocateDeclNos
    
(*EXPORTED.*)
; PROCEDURE TextOfIdAtom ( IdAtom : FM3Base . AtomTyp ) : TEXT 
  (* In the current unit. *) 

  = VAR LCharsRef : FM3Atom_OAChars . KeyTyp
  ; VAR LIdentText : TEXT 

  ; BEGIN (*TextOfIdAtom*)
      IF NOT FM3Atom_OAChars . Key 
               ( FM3Globals . CurrentUnitRef ^ . UntIdentAtomDict
               , IdAtom
               , (*OUT*) LCharsRef
               )
      THEN LIdentText := <NotFound>
      ELSIF LCharsRef = NIL
      THEN LIdentText := <NIL>
      ELSE LIdentText := Text . FromChars ( LCharsRef ^ ) 
    END TextOfIdAtom

; BEGIN
    NextUnitNo := 1
  ; UnitMap := VarArray_Int_Refany . New ( NIL )  
  ; UnitStackTop := NIL 
  END FM3Units
.

