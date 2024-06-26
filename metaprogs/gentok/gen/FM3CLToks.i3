
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3CLToks.gentok", with command line 
     "./gentok -S -L -i -n FM3CLToks.gentok". *)

INTERFACE FM3CLToks

; TYPE TokTyp = INTEGER 

; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

(* ABS 0: *)

; CONST TkMinTok                                     =     0

; CONST CltNull                                      =     0 (*16_00 *)
; CONST CltVersion                                   =     1 (*16_01 *)
; CONST CltHelp                                      =     2 (*16_02 *)
; CONST CltSrcFile                                   =     3 (*16_03 *)
; CONST CltSrcDir                                    =     4 (*16_04 *)
; CONST CltImportDir                                 =     5 (*16_05 *)
; CONST CltResourceDir                               =     6 (*16_06 *)
; CONST CltBuildDir                                  =     7 (*16_07 *)
; CONST CltDisAsmPasses                              =     8 (*16_08 *)
; CONST CltDisAsm                                    =     9 (*16_09 *)
; CONST CltKeepPasses                                =    10 (*16_0a *)
; CONST CltKeep                                      =    11 (*16_0b *)
(* Keep intermediate files. *)
; CONST CltRemoveUnusedDecls                         =    12 (*16_0c *)
; CONST CltStdErr                                    =    13 (*16_0d *)
(* Write compilation process messages to stderr. *)
; CONST CltFM3Log                                    =    14 (*16_0e *)
(* Write compilation process messages to compiler log file. *)
; CONST CltStdOut                                    =    15 (*16_0f *)
(* Write compiled code messages to stdout. *)
; CONST CltUnitLog                                   =    16 (*16_10 *)
(* Write compiled code messages to unit-specific log file. *)
(* End of file FM3CLToks.gentok *)
; CONST TkMaxTok                                     =    16

; END FM3CLToks
.

