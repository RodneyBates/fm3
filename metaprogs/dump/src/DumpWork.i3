 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE DumpWork

; IMPORT Wr 

; IMPORT RdBackFile 

; PROCEDURE DumpNumericBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

; PROCEDURE DumpInterpretBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

; END DumpWork
.

