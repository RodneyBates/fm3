
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
(* ^This builds FM3Expr.ExprTyp objects for builtin things.
   This happens in Pass2, when references to declared entities
   can not yet be followed, so types are not known in general.
   Builtin types and builtin constants (whose types are always
   builtin) have types set, but other builtins not.

   An ExprTyp node for a builtin type or constant is always a leaf
   of an expression tree.  Since it has no descendents, a single ExprTyp
   object is created at initialization for each builtin and returned
   possibly multiple times.

   In contrast, each occurrence of a function or procedure needs a
   distinct ExprTyp object with its own descendents, types, etc. So
   each call returns a newly allocated object with only properties
   that are the same in every instance set.  Caller must set any
   relevant other fields.
*) 


; PROCEDURE IsOperationTok ( Tok : FM3SrcToks . TokTyp ) : BOOLEAN 
  (* Reserved or standard ids that denote functions or operations. *) 

; PROCEDURE OpExprKind ( BuiltinOpcode : FM3SrcToks . TokTyp )
  : FM3Exprs . ExprKindTyp
  (* Meaningful only for reserved ident (starting with StkRid) or something
     that can occcur in a standard interface (starting with StkPd).
  *)  

; CONST ActualsCtAtLeastOne = - 2 

; END FM3Builtins
.

