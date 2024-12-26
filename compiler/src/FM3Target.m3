
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Target

; IMPORT FM3LoTypes

; BEGIN
  (* A typical modern 64-bit target computer. *) 
    LoTypeNoLong := FM3LoTypes . LoTypeNoI64
  ; LoTypeNoInt := FM3LoTypes . LoTypeNoI64
  ; LoTypeNoWideChar := FM3LoTypes . LoTypeNoU32
  ; LoTypeNoAddr := FM3LoTypes . LoTypeNoAddr64
  
  (*** A typical not-so-modern 32-bit target computer. ** 
    LoTypeNoLong := FM3LoTypes . LoTypeNoI64
  ; LoTypeNoInt := FM3LoTypes . LoTypeNoI32
  ; LoTypeNoWideChar := FM3LoTypes . LoTypeNoU32
  ; LoTypeNoAddr := FM3LoTypes . LoTypeNoAddr32
  ****)
  END FM3Target
.
