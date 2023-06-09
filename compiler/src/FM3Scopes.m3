
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

; VAR NextScopeNo : ScopeNoTyp := 0 

(*EXPORTED*) 
; PROCEDURE NewMap ( ScopeCt := InitScopeCt ) : ScopeMapTyp

  = BEGIN
      NextScopeNo := 0 
    ; RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , ScopeCt - 1 } ) 
    END NewMap

(*TODO: Move this somewhere central & unshared (See also FM3Decls) *) 
; PROCEDURE IntHash ( Val : INTEGER ) : FM3Base . HashTyp

  = BEGIN
      RETURN VAL ( Val , FM3Base . HashTyp ) 
    END IntHash 

(*EXPORTED*) 
; PROCEDURE NewScope
    ( Map : ScopeMapTyp ; InitDictCt := DefaultInitDictCt ) : ScopeNoTyp
  (* Allocate and connect a ScopeNo and scopeRef. *)

  = VAR LScopeNo : ScopeNoTyp  
  ; VAR LScopeRef : ScopeRefTyp

  ; BEGIN
      LScopeNo := NextScopeNo
    ; INC ( NextScopeNo )
    ; LScopeRef := NEW ( ScopeRefTyp )
    ; LScopeRef . ScpNumber := LScopeNo  
    ; LScopeRef . ScpDecldIdSet := IntSets . Empty ( )  
    ; LScopeRef . ScpDeclDict 
        := FM3Dict_Int_Int . NewGrowable 
             ( InitDictCt , (*FM3SharedUtils .*) IntHash ) 
    ; LScopeRef . ScpDeclCt := 0 
    ; VarArray_Int_Refany . Assign ( Map , LScopeNo , LScopeRef ) 
    END NewScope 

; BEGIN
  END FM3Scopes
.

