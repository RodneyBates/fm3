
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3SrcToks.gentok", with command line 
     "./gentok -S -L -i -n FM3SrcToks.gentok". *)

INTERFACE FM3SrcToks

; TYPE TokTyp = INTEGER 

; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

(* ABS 0: *)

; CONST TkMinTok                                     =     0

; CONST StkEOF                                       =     0 (*16_00 *)
(* From lalr, Gen.m3, FindFirstToken:
               "EndOfToken wird immer mit 0 codiert"
            *)
(* Not used in FM3Scanner or FM3Parser: *)
; CONST StkUnknown                                   =     1 (*16_01 *)
; CONST StkBOF                                       =     2 (*16_02 *)
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
; CONST StkReservedId                                =   108 (*16_ec 00 *)
(* Any reserved ident, but with the Rid as its atom. *)
; CONST StkPragmaId                                  =   109 (*16_ed 00 *)
(* Not a known pragma ident, but found where one is expected. *)
(* Reserved identifiers: *)
(* We want to scan these with the same lex machine as reserved words,
      but then treat each of them as source token StkReservedId, using the
      code the lex table gives as an atom id.  This may rebiasing them
      to agree with FM3PreDecl, which the scanner will do.
   *)
; CONST RidNull                                      =   110 (*16_ee 00 *)
(* Scanner will never present this. *)
; CONST RidABS                                       =   111 (*16_ef 00 *)
; CONST RidADDRESS                                   =   112 (*16_f0 00 *)
; CONST RidADR                                       =   113 (*16_f1 00 *)
; CONST RidADRSIZE                                   =   114 (*16_f2 00 *)
; CONST RidBITSIZE                                   =   115 (*16_f3 00 *)
; CONST RidBOOLEAN                                   =   116 (*16_f4 00 *)
; CONST RidBYTESIZE                                  =   117 (*16_f5 00 *)
; CONST RidCARDINAL                                  =   118 (*16_f6 00 *)
; CONST RidCEILING                                   =   119 (*16_f7 00 *)
; CONST RidCHAR                                      =   120 (*16_f8 00 *)
; CONST RidDEC                                       =   121 (*16_f9 00 *)
; CONST RidDISPOSE                                   =   122 (*16_fa 00 *)
; CONST RidEXTENDED                                  =   123 (*16_fb 00 *)
; CONST RidFALSE                                     =   124 (*16_fc 00 *)
; CONST RidFIRST                                     =   125 (*16_fd 00 *)
; CONST RidFLOAT                                     =   126 (*16_fe 00 *)
; CONST RidFLOOR                                     =   127 (*16_ff 00 *)
; CONST RidINC                                       =   128 (*16_80 01 *)
; CONST RidINTEGER                                   =   129 (*16_81 01 *)
; CONST RidISTYPE                                    =   130 (*16_82 01 *)
; CONST RidLAST                                      =   131 (*16_83 01 *)
; CONST RidLONGREAL                                  =   132 (*16_84 01 *)
; CONST RidLOOHOLE                                   =   133 (*16_85 01 *)
; CONST RidMAX                                       =   134 (*16_86 01 *)
; CONST RidMIN                                       =   135 (*16_87 01 *)
; CONST RidMUTEX                                     =   136 (*16_88 01 *)
; CONST RidNARROW                                    =   137 (*16_89 01 *)
; CONST RidNEW                                       =   138 (*16_8a 01 *)
; CONST RidNIL                                       =   139 (*16_8b 01 *)
; CONST RidNULL                                      =   140 (*16_8c 01 *)
; CONST RidNUMBER                                    =   141 (*16_8d 01 *)
; CONST RidORD                                       =   142 (*16_8e 01 *)
; CONST RidREAL                                      =   143 (*16_8f 01 *)
; CONST RidREFANY                                    =   144 (*16_90 01 *)
; CONST RidROUND                                     =   145 (*16_91 01 *)
; CONST RidSUBARRAY                                  =   146 (*16_92 01 *)
; CONST RidTEXT                                      =   147 (*16_93 01 *)
; CONST RidTRUE                                      =   148 (*16_94 01 *)
; CONST RidTRUNC                                     =   149 (*16_95 01 *)
; CONST RidTYPECODE                                  =   150 (*16_96 01 *)
; CONST RidVAL                                       =   151 (*16_97 01 *)
; CONST TkMaxTok                                     =   151

; END FM3SrcToks
.

