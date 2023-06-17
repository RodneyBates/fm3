
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3SrcToks.gentok", with command line "
    ./gentok -S -L -n FM3SrcToks.gentok". *)

INTERFACE FM3SrcToks

; TYPE TokTyp = INTEGER 

; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

(* Not used in FM3Parser: *)
(* ABS 1: *)

; CONST TkMinTok                                     =     1

; CONST StkUnknown                                   =     1 (*16_01 *)
; CONST StkBOF                                       =     2 (*16_02 *)
; CONST StkEOF                                       =     3 (*16_03 *)
(* Near the top of lalr's max range to 2500. *)
(* Modula-3 Reserved words: *)
(* ABS 5: *)

; CONST StkRwAND                                     =     5 (*16_05 *)
; CONST StkRwANY                                     =     6 (*16_06 *)
; CONST StkRwARRAY                                   =     7 (*16_07 *)
; CONST StkRwAS                                      =     8 (*16_08 *)
; CONST StkRwBEGIN                                   =     9 (*16_09 *)
; CONST StkRwBITS                                    =    10 (*16_0a *)
; CONST StkRwBRANDED                                 =    11 (*16_0b *)
; CONST StkRwBY                                      =    12 (*16_0c *)
; CONST StkRwCASE                                    =    13 (*16_0d *)
; CONST StkRwCONST                                   =    14 (*16_0e *)
; CONST StkRwDIV                                     =    15 (*16_0f *)
; CONST StkRwDO                                      =    16 (*16_10 *)
; CONST StkRwELSE                                    =    17 (*16_11 *)
; CONST StkRwELSIF                                   =    18 (*16_12 *)
; CONST StkRwEND                                     =    19 (*16_13 *)
; CONST StkRwEVAL                                    =    20 (*16_14 *)
; CONST StkRwEXCEPT                                  =    21 (*16_15 *)
; CONST StkRwEXCEPTION                               =    22 (*16_16 *)
; CONST StkRwEXIT                                    =    23 (*16_17 *)
; CONST StkRwEXPORTS                                 =    24 (*16_18 *)
; CONST StkRwFINALLY                                 =    25 (*16_19 *)
; CONST StkRwFOR                                     =    26 (*16_1a *)
; CONST StkRwFROM                                    =    27 (*16_1b *)
; CONST StkRwGENERIC                                 =    28 (*16_1c *)
; CONST StkRwIF                                      =    29 (*16_1d *)
; CONST StkRwIMPORT                                  =    30 (*16_1e *)
; CONST StkRwIN                                      =    31 (*16_1f *)
; CONST StkRwINTERFACE                               =    32 (*16_20 *)
; CONST StkRwLOCK                                    =    33 (*16_21 *)
; CONST StkRwLOOP                                    =    34 (*16_22 *)
; CONST StkRwMETHODS                                 =    35 (*16_23 *)
; CONST StkRwMOD                                     =    36 (*16_24 *)
; CONST StkRwMODULE                                  =    37 (*16_25 *)
; CONST StkRwNOT                                     =    38 (*16_26 *)
; CONST StkRwOBJECT                                  =    39 (*16_27 *)
; CONST StkRwOF                                      =    40 (*16_28 *)
; CONST StkRwOR                                      =    41 (*16_29 *)
; CONST StkRwOVERRIDES                               =    42 (*16_2a *)
; CONST StkRwPROCEDURE                               =    43 (*16_2b *)
; CONST StkRwRAISE                                   =    44 (*16_2c *)
; CONST StkRwRAISES                                  =    45 (*16_2d *)
; CONST StkRwREADONLY                                =    46 (*16_2e *)
; CONST StkRwRECORD                                  =    47 (*16_2f *)
; CONST StkRwREF                                     =    48 (*16_30 *)
; CONST StkRwREPEAT                                  =    49 (*16_31 *)
; CONST StkRwRETURN                                  =    50 (*16_32 *)
; CONST StkRwREVEAL                                  =    51 (*16_33 *)
; CONST StkRwROOT                                    =    52 (*16_34 *)
; CONST StkRwSET                                     =    53 (*16_35 *)
; CONST StkRwTHEN                                    =    54 (*16_36 *)
; CONST StkRwTO                                      =    55 (*16_37 *)
; CONST StkRwTRY                                     =    56 (*16_38 *)
; CONST StkRwTYPE                                    =    57 (*16_39 *)
; CONST StkRwTYPECASE                                =    58 (*16_3a *)
; CONST StkRwUNSAFE                                  =    59 (*16_3b *)
; CONST StkRwUNTIL                                   =    60 (*16_3c *)
; CONST StkRwUNTRACED                                =    61 (*16_3d *)
; CONST StkRwVALUE                                   =    62 (*16_3e *)
; CONST StkRwVAR                                     =    63 (*16_3f *)
; CONST StkRwWHILE                                   =    64 (*16_c0 00 *)
; CONST StkRwWITH                                    =    65 (*16_c1 00 *)
(* Special character tokens: *)
; CONST StkSemicolon                                 =    66 (*16_c2 00 *)
; CONST StkDot                                       =    67 (*16_c3 00 *)
; CONST StkEqual                                     =    68 (*16_c4 00 *)
; CONST StkOpenParen                                 =    69 (*16_c5 00 *)
; CONST StkCloseParen                                =    70 (*16_c6 00 *)
; CONST StkComma                                     =    71 (*16_c7 00 *)
; CONST StkColon                                     =    72 (*16_c8 00 *)
; CONST StkSubtype                                   =    73 (*16_c9 00 *)
; CONST StkBecomes                                   =    74 (*16_ca 00 *)
; CONST StkOpenBrace                                 =    75 (*16_cb 00 *)
; CONST StkCloseBrace                                =    76 (*16_cc 00 *)
; CONST StkStroke                                    =    77 (*16_cd 00 *)
; CONST StkArrow                                     =    78 (*16_ce 00 *)
; CONST StkEllipsis                                  =    79 (*16_cf 00 *)
; CONST StkOpenBracket                               =    80 (*16_d0 00 *)
; CONST StkCloseBracket                              =    81 (*16_d1 00 *)
; CONST StkUnequal                                   =    82 (*16_d2 00 *)
; CONST StkLess                                      =    83 (*16_d3 00 *)
; CONST StkGreater                                   =    84 (*16_d4 00 *)
; CONST StkLessEqual                                 =    85 (*16_d5 00 *)
; CONST StkGreaterEqual                              =    86 (*16_d6 00 *)
; CONST StkPlus                                      =    87 (*16_d7 00 *)
; CONST StkMinus                                     =    88 (*16_d8 00 *)
; CONST StkAmpersand                                 =    89 (*16_d9 00 *)
; CONST StkStar                                      =    90 (*16_da 00 *)
; CONST StkSlash                                     =    91 (*16_db 00 *)
; CONST StkDeref                                     =    92 (*16_dc 00 *)
; CONST StkOpenPragma                                =    93 (*16_dd 00 *)
; CONST StkClosePragma                               =    94 (*16_de 00 *)
(* Variable tokens: *)
; CONST StkIdent                                     =    95 (*16_df 00 *)
; CONST StkIntLit                                    =    96 (*16_e0 00 *)
; CONST StkLongIntLit                                =    97 (*16_e1 00 *)
; CONST StkBasedLit                                  =    98 (*16_e2 00 *)
; CONST StkLongBasedLit                              =    99 (*16_e3 00 *)
; CONST StkRealLit                                   =   100 (*16_e4 00 *)
; CONST StkLongRealLit                               =   101 (*16_e5 00 *)
; CONST StkExtendedLit                               =   102 (*16_e6 00 *)
; CONST StkTextLit                                   =   103 (*16_e7 00 *)
; CONST StkWideTextLit                               =   104 (*16_e8 00 *)
; CONST StkCharLit                                   =   105 (*16_e9 00 *)
; CONST StkWideCharLit                               =   106 (*16_ea 00 *)
; CONST StkLexErrChars                               =   107 (*16_eb 00 *)
(* End FM3SrcToks.gentok *)
; CONST TkMaxTok                                     =   107

; END FM3SrcToks
.

