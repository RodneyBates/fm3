
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Target

(* A mix of declared constants ( In FM3LoTypes), final variables (here),
   and dynamically allocated values ( InFM3Pass3).  Initially declared here
   to avoid cyclic imports. 
*)
; TYPE LoTypeNoTyp = INTEGER 

(* Initialized in the body to target-dependent values and copied into
   FM3LoTypes:
*) 
; VAR LoTypeNoLong      : LoTypeNoTyp 
; VAR LoTypeNoInt       : LoTypeNoTyp
; VAR LoTypeNoWideChar  : LoTypeNoTyp
; VAR LoTypeNoAddr      : LoTypeNoTyp

; END FM3Target
.
