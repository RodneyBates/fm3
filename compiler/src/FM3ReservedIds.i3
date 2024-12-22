
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Expressions for things named by Modula-3 reserved identifiers. *) 

INTERFACE FM3ReservedIds

; IMPORT FM3Exprs 
; IMPORT FM3SrcToks

; TYPE ReservedIdTyp = [ FM3SrcToks . StkMinRid .. FM3SrcToks . StkMaxRid ]

; TYPE ExprRefsTyp = ARRAY ReservedIdTyp OF FM3Exprs . ExprTyp

; VAR ExprRefs : ExprRefsTyp 

; END FM3ReservedIds
.
