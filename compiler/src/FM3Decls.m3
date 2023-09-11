
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT IntRanges 

; IMPORT FM3Base
; IMPORT FM3Globals 
; IMPORT FM3Scopes
; IMPORT VarArray_Int_Refany

(*EXPORTED*) 
; PROCEDURE NewDeclMap ( InitDeclCt : FM3Base . DeclNoTyp ) : DeclMapTyp
  (* One of these per Unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitDeclCt - 1 } ) 
    END NewDeclMap

(*EXPORTED*) 
; PROCEDURE NewDeclRef
    ( ParentScopeRef : FM3Scopes . ScopeRefTyp ; DeclNo : FM3Base . DeclNoTyp )
  : DeclRefTyp
  (* Allocate a DeclRef and connect it into ParentScopeRef ^. *)

  = VAR LDeclRef : DeclRefTyp

  ; BEGIN
      LDeclRef := NEW ( DeclRefTyp )
    ; LDeclRef . DclDeclNo := DeclNo  
    ; LDeclRef . DclParentScopeRef := ParentScopeRef  
    ; VarArray_Int_Refany . Assign
        ( FM3Globals . CurrentUnitRef ^ . UntDeclMap , DeclNo , LDeclRef )
    ; RETURN LDeclRef 
    END NewDeclRef 

; BEGIN
  END FM3Decls
.

