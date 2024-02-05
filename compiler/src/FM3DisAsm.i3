 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3DisAsm 

; IMPORT Wr 

; IMPORT RdBackFile

; VAR Verbose : BOOLEAN := FALSE 

; PROCEDURE DumpNumericBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

; PROCEDURE DumpInterpretBwd ( RBT : RdBackFile . T ; WrT : Wr . T )

; PROCEDURE DisAsmWOperandsBwd ( RBT : RdBackFile . T ; WrT : Wr . T )
  (* PRE: RBT is open. *) 

; END FM3DisAsm
.

