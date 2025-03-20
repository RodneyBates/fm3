
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Things to help looking at iternal operations.
   Suitable for calling within a debigger.
*)

MODULE FM3Introspection

; IMPORT VarArray_Int_Refany 

; IMPORT FM3Base
; IMPORT FM3Decls
; IMPORT FM3Globals 
; IMPORT FM3Units 

(*EXPORTED.*)
; PROCEDURE CurrentSrcFileName ( ) : TEXT 

  = VAR LUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN (*CurrentSrcFileName*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; IF LUnitRef = NIL
      THEN RETURN "No current unit." 
      ELSE RETURN LUnitRef ^ . UntSrcFileSimpleName
      END (*IF*) 
    END CurrentSrcFileName 

(*EXPORTED.*)
; PROCEDURE DeclNoImage ( DeclNo : FM3Globals . DeclNoTyp ) : TEXT 

  = VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LDeclMap : FM3Base . MapTyp
  ; VAR LDeclRef : FM3Decls . DeclRefTyp 

  ; BEGIN (*DeclNoImage*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; IF LUnitRef = NIL
      THEN RETURN "No current unit." 
      END (*IF*)
    ; LDeclMap := LUnitRef ^ . UntDeclMap 
    ; IF LDeclMap = NIL
      THEN RETURN "No declaration map." 
      END (*IF*)
    ; LDeclRef(* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FM3Units . UnitStackTopRef ^ . UntDeclMap , DeclNo )
    ; IF LDeclRef = NIL
      THEN RETURN "DeclNo not mapped." 
      END (*IF*)
    ; RETURN FM3Decls . DeclRefImage ( LDeclRef ) 
    END DeclNoImage
      
; BEGIN (*FM3Introspection*)
  END FM3Introspection
  .


  