
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Decls

; IMPORT IntRanges 

; IMPORT FM3Scopes
; IMPORT VarArray_Int_Refany

(*EXPORTED*) 
; PROCEDURE NewMap ( InitDeclCt := DefaultInitDeclCt ) : DeclMapTyp

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitDeclCt - 1 } ) 
    END NewMap

(*EXPORTED*) 
; PROCEDURE NewDeclRef
    ( ParentScopeRef : FM3Scopes . ScopeRefTyp ; DeclNo : DeclNoTyp )
  : DeclRefTyp
  (* Allocate a DeclRef and connect it into ParentScopeRef ^. *)

  = VAR LDeclRef : DeclRefTyp

  ; BEGIN
      LDeclRef := NEW ( DeclRefTyp )
    ; LDeclRef . DclNumber := DeclNo  
    ; LDeclRef . DclParentScopeRef := ParentScopeRef  
    ; VarArray_Int_Refany . Assign
        ( ParentScopeRef ^ . ScpDeclMap , DeclNo , LDeclRef )
    ; RETURN LDeclRef 
    END NewDeclRef 

; BEGIN
  END FM3Decls
.

