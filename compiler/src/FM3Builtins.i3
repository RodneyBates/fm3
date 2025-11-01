
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Exprs for constants, types, and operations  builtin to Modula-3. *)  

INTERFACE FM3Builtins

; IMPORT FM3Base 
; IMPORT FM3Exprs 
; IMPORT FM3SrcToks

; PROCEDURE BuiltinExpr
    ( Opcode : FM3SrcToks . TokTyp ; Position := FM3Base . PositionNull )
  : FM3Exprs . ExprTyp (* NIL if not an Id denoting an ExprTyp *) 

; PROCEDURE IsOperationTok ( Tok : FM3SrcToks . TokTyp ) : BOOLEAN 

; PROCEDURE OpExprKind ( BuiltinOpcode : FM3SrcToks . TokTyp )
  : FM3Exprs . ExprKindTyp
  (* Meaningful only for reserved ident (starting with StkRid) or something
     that can occcur in a standard interface (starting with StkPd).
  *)  

; CONST ActualsCtAtLeastOne = - 2 

; END FM3Builtins
.

