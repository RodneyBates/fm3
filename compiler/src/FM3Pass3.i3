
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Pass3

; IMPORT FM3Base
; IMPORT FM3Exprs 
; IMPORT FM3Scopes 
; IMPORT FM3Units 

; PROCEDURE RunPass3 ( )

; PROCEDURE ResolveOperand
    ( Expr1Opnd : FM3Exprs . Expr1OpndTyp
    ; ExprKind : FM3Exprs . ExprKindTyp
    )
  : FM3Exprs . ExprStateTyp

; END FM3Pass3
.
