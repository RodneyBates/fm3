
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Resolve connections among expressions, which so far, are each islands. *) 

INTERFACE FM3Resolve

; IMPORT FM3Exprs

; PROCEDURE ResolveExpr
    ( ExprRef : FM3Exprs . ExprRefTyp ; ExprKind : FM3Exprs . ExprKindTyp )
  : FM3Exprs . ExprStateTyp

; END FM3Resolve
.
