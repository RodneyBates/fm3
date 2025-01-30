
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Properties of operations builtin to Modula-3. *)  

INTERFACE FM3BuiltinOps

; IMPORT FM3Exprs 
; IMPORT FM3SrcToks

; TYPE OpPropertiesTyp
    = RECORD
        OpResultType : FM3Exprs . ExprTyp := NIL
      ; OpExprKind := FM3Exprs . ExprKindTyp . EkNull 
      ; OpOpndCt : INTEGER 
      ; OpLtExprKindsAllowed : FM3Exprs . ExprKindSetTyp
      ; OpRtExprKindsAllowed : FM3Exprs . ExprKindSetTyp
      END

; PROCEDURE Properties ( Tok : FM3SrcToks . TokTyp ) : OpPropertiesTyp

; END FM3BuiltinOps
.

