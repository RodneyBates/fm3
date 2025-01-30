
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Exprs for types builtin to Modula-3. *)  

INTERFACE FM3BuiltinTypes

; IMPORT FM3Exprs 
; IMPORT FM3SrcToks

; PROCEDURE TypeExpr ( Tok : FM3SrcToks . TokTyp )
  : FM3Exprs . ExprTyp (* NIL if not a reserved id denoting a type > *) 

; END FM3BuiltinTypes
.

