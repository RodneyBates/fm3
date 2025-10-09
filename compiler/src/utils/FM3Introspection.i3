
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Things to help looking at iternal operations.
   Suitable for calling within a debigger.
*)

INTERFACE FM3Introspection

; IMPORT IntSets 

; IMPORT FM3Decls
; IMPORT FM3Globals
; IMPORT FM3Graph 

; PROCEDURE CurrentSrcFileName ( ) : TEXT 

; PROCEDURE DeclNoRef
    ( DeclNo : FM3Globals . DeclNoTyp 
    ; UnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull 
      (* ^UnitNoNull implies current unit. *) 
    )
  : FM3Decls . DeclRefTyp

; PROCEDURE DeclNoTypImage ( DeclNo : FM3Globals . DeclNoTyp ) : TEXT 

; PROCEDURE DeclNoRefImage ( DeclNo : FM3Globals . DeclNoTyp ) : TEXT

; PROCEDURE ScopeDeclsInfo ( ) : TEXT
  (* For the current Decl scope. *) 

; PROCEDURE GraphArc ( Pred , Succ : INTEGER ) : INTEGER
  (* In the graph of the current decl scope. *) 
  (* Scope-relative Pred & Succ. *) 
  (* < 0 => something was invalid. *) 

; PROCEDURE GraphArcImage ( Arc : FM3Graph . ArcTyp ) : TEXT 
  (* In the current open scope. *)

; PROCEDURE GraphImage ( ) : TEXT 
  (* Of the current open scope. *)

; PROCEDURE IntSetsImage ( Set : IntSets . T  ) : TEXT

; END FM3Introspection

.
