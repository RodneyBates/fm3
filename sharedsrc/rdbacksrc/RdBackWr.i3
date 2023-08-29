
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE RdBackWr

(* A Wr.T subtype that forwards its output to a RdBackFile . T. *) 

; IMPORT Wr

; IMPORT RdBackFile
      
; TYPE T <: Wr . T 

; PROCEDURE New ( RbT : RdBackFile . T ) : T

; END RdBackWr
.


