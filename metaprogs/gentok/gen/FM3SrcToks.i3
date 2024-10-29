
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
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
(* ABS 5: *)

(* Reserved identifiers: *)
(* We want to scan these with the same lex machine as reserved words,
      but then treat each of them as source token StkReservedId, using the
      code the lex table gives as an atom id.  This may rebiasing them
      to agree with FM3PreDecl, which the scanner will do.
   *)
; CONST TokMinPredef                                 =     5 (*16_05 *)
; CONST RidNull                                      =     6 (*16_06 *)
(* Scanner will never present this. *)
; CONST RidABS                                       =     7 (*16_07 *)
; CONST RidADDRESS                                   =     8 (*16_08 *)
; CONST RidADR                                       =     9 (*16_09 *)
; CONST RidADRSIZE                                   =    10 (*16_0a *)
; CONST RidBITSIZE                                   =    11 (*16_0b *)
; CONST RidBOOLEAN                                   =    12 (*16_0c *)
; CONST RidBYTESIZE                                  =    13 (*16_0d *)
; CONST RidCARDINAL                                  =    14 (*16_0e *)
; CONST RidCEILING                                   =    15 (*16_0f *)
; CONST RidCHAR                                      =    16 (*16_10 *)
; CONST RidDEC                                       =    17 (*16_11 *)
; CONST RidDISPOSE                                   =    18 (*16_12 *)
; CONST RidEXTENDED                                  =    19 (*16_13 *)
; CONST RidFALSE                                     =    20 (*16_14 *)
; CONST RidFIRST                                     =    21 (*16_15 *)
; CONST RidFLOAT                                     =    22 (*16_16 *)
; CONST RidFLOOR                                     =    23 (*16_17 *)
; CONST RidINC                                       =    24 (*16_18 *)
; CONST RidINTEGER                                   =    25 (*16_19 *)
; CONST RidISTYPE                                    =    26 (*16_1a *)
; CONST RidLAST                                      =    27 (*16_1b *)
; CONST RidLONGCARD                                  =    28 (*16_1c *)
; CONST RidLONGINT                                   =    29 (*16_1d *)
; CONST RidLONGREAL                                  =    30 (*16_1e *)
; CONST RidLOOPHOLE                                  =    31 (*16_1f *)
; CONST RidMAX                                       =    32 (*16_20 *)
; CONST RidMIN                                       =    33 (*16_21 *)
; CONST RidMUTEX                                     =    34 (*16_22 *)
; CONST RidNARROW                                    =    35 (*16_23 *)
; CONST RidNEW                                       =    36 (*16_24 *)
; CONST RidNIL                                       =    37 (*16_25 *)
; CONST RidNULL                                      =    38 (*16_26 *)
; CONST RidNUMBER                                    =    39 (*16_27 *)
; CONST RidORD                                       =    40 (*16_28 *)
; CONST RidREAL                                      =    41 (*16_29 *)
; CONST RidREFANY                                    =    42 (*16_2a *)
; CONST RidROUND                                     =    43 (*16_2b *)
; CONST RidSUBARRAY                                  =    44 (*16_2c *)
; CONST RidTEXT                                      =    45 (*16_2d *)
; CONST RidTRUE                                      =    46 (*16_2e *)
; CONST RidTRUNC                                     =    47 (*16_2f *)
; CONST RidTYPECODE                                  =    48 (*16_30 *)
; CONST RidVAL                                       =    49 (*16_31 *)
; CONST RidWIDECHAR                                  =    50 (*16_32 *)
(* These behave semantically like reserved identifiers, but each occurs in
      a different set of syntactic contexts from identifiers, so they are
      source-code reserved words.  We convert those after parsing into
      reserved idents with the following Rid atoms.
   *)
; CONST RidROOT                                      =    51 (*16_33 *)
; CONST RidUNTRACEDROOT                              =    52 (*16_34 *)
(* Idents in Word and Long. *)
; CONST Word_T                                       =    53 (*16_35 *)
; CONST Word_Size                                    =    54 (*16_36 *)
; CONST Word_Plus                                    =    55 (*16_37 *)
; CONST Word_Times                                   =    56 (*16_38 *)
; CONST Word_Minus                                   =    57 (*16_39 *)
; CONST Word_Divide                                  =    58 (*16_3a *)
; CONST Word_Mod                                     =    59 (*16_3b *)
; CONST Word_LT                                      =    60 (*16_3c *)
; CONST Word_LE                                      =    61 (*16_3d *)
; CONST Word_GT                                      =    62 (*16_3e *)
; CONST Word_GE                                      =    63 (*16_3f *)
; CONST Word_And                                     =    64 (*16_c0 00 *)
; CONST Word_Or                                      =    65 (*16_c1 00 *)
; CONST Word_Xor                                     =    66 (*16_c2 00 *)
; CONST Word_Not                                     =    67 (*16_c3 00 *)
; CONST Word_Shift                                   =    68 (*16_c4 00 *)
; CONST Word_LeftShift                               =    69 (*16_c5 00 *)
; CONST Word_RightShift                              =    70 (*16_c6 00 *)
; CONST Word_Rotate                                  =    71 (*16_c7 00 *)
; CONST Word_LeftRotate                              =    72 (*16_c8 00 *)
; CONST Word_RightRotate                             =    73 (*16_c9 00 *)
; CONST Word_Extract                                 =    74 (*16_ca 00 *)
; CONST Word_Insert                                  =    75 (*16_cb 00 *)
; CONST Long_T                                       =    76 (*16_cc 00 *)
; CONST Long_Size                                    =    77 (*16_cd 00 *)
; CONST Long_Plus                                    =    78 (*16_ce 00 *)
; CONST Long_Times                                   =    79 (*16_cf 00 *)
; CONST Long_Minus                                   =    80 (*16_d0 00 *)
; CONST Long_Divide                                  =    81 (*16_d1 00 *)
; CONST Long_Mod                                     =    82 (*16_d2 00 *)
; CONST Long_LT                                      =    83 (*16_d3 00 *)
; CONST Long_LE                                      =    84 (*16_d4 00 *)
; CONST Long_GT                                      =    85 (*16_d5 00 *)
; CONST Long_GE                                      =    86 (*16_d6 00 *)
; CONST Long_And                                     =    87 (*16_d7 00 *)
; CONST Long_Or                                      =    88 (*16_d8 00 *)
; CONST Long_Xor                                     =    89 (*16_d9 00 *)
; CONST Long_Not                                     =    90 (*16_da 00 *)
; CONST Long_Shift                                   =    91 (*16_db 00 *)
; CONST Long_LeftShift                               =    92 (*16_dc 00 *)
; CONST Long_RightShift                              =    93 (*16_dd 00 *)
; CONST Long_Rotate                                  =    94 (*16_de 00 *)
; CONST Long_LeftRotate                              =    95 (*16_df 00 *)
; CONST Long_RightRotate                             =    96 (*16_e0 00 *)
; CONST Long_Extract                                 =    97 (*16_e1 00 *)
; CONST Long_Insert                                  =    98 (*16_e2 00 *)
; CONST TokMaxPredef                                 =    99 (*16_e3 00 *)
(* Modula-3 Reserved words: *)
; CONST StkRwAND                                     =   100 (*16_e4 00 *)
; CONST StkRwANY                                     =   101 (*16_e5 00 *)
; CONST StkRwARRAY                                   =   102 (*16_e6 00 *)
; CONST StkRwAS                                      =   103 (*16_e7 00 *)
; CONST StkRwBEGIN                                   =   104 (*16_e8 00 *)
; CONST StkRwBITS                                    =   105 (*16_e9 00 *)
; CONST StkRwBRANDED                                 =   106 (*16_ea 00 *)
; CONST StkRwBY                                      =   107 (*16_eb 00 *)
; CONST StkRwCASE                                    =   108 (*16_ec 00 *)
; CONST StkRwCONST                                   =   109 (*16_ed 00 *)
; CONST StkRwDIV                                     =   110 (*16_ee 00 *)
; CONST StkRwDO                                      =   111 (*16_ef 00 *)
; CONST StkRwELSE                                    =   112 (*16_f0 00 *)
; CONST StkRwELSIF                                   =   113 (*16_f1 00 *)
; CONST StkRwEND                                     =   114 (*16_f2 00 *)
; CONST StkRwEVAL                                    =   115 (*16_f3 00 *)
; CONST StkRwEXCEPT                                  =   116 (*16_f4 00 *)
; CONST StkRwEXCEPTION                               =   117 (*16_f5 00 *)
; CONST StkRwEXIT                                    =   118 (*16_f6 00 *)
; CONST StkRwEXPORTS                                 =   119 (*16_f7 00 *)
; CONST StkRwFINALLY                                 =   120 (*16_f8 00 *)
; CONST StkRwFOR                                     =   121 (*16_f9 00 *)
; CONST StkRwFROM                                    =   122 (*16_fa 00 *)
; CONST StkRwGENERIC                                 =   123 (*16_fb 00 *)
; CONST StkRwIF                                      =   124 (*16_fc 00 *)
; CONST StkRwIMPORT                                  =   125 (*16_fd 00 *)
; CONST StkRwIN                                      =   126 (*16_fe 00 *)
; CONST StkRwINTERFACE                               =   127 (*16_ff 00 *)
; CONST StkRwLOCK                                    =   128 (*16_80 01 *)
; CONST StkRwLOOP                                    =   129 (*16_81 01 *)
; CONST StkRwMETHODS                                 =   130 (*16_82 01 *)
; CONST StkRwMOD                                     =   131 (*16_83 01 *)
; CONST StkRwMODULE                                  =   132 (*16_84 01 *)
; CONST StkRwNOT                                     =   133 (*16_85 01 *)
; CONST StkRwOBJECT                                  =   134 (*16_86 01 *)
; CONST StkRwOF                                      =   135 (*16_87 01 *)
; CONST StkRwOR                                      =   136 (*16_88 01 *)
; CONST StkRwOVERRIDES                               =   137 (*16_89 01 *)
; CONST StkRwPROCEDURE                               =   138 (*16_8a 01 *)
; CONST StkRwRAISE                                   =   139 (*16_8b 01 *)
; CONST StkRwRAISES                                  =   140 (*16_8c 01 *)
; CONST StkRwREADONLY                                =   141 (*16_8d 01 *)
; CONST StkRwRECORD                                  =   142 (*16_8e 01 *)
; CONST StkRwREF                                     =   143 (*16_8f 01 *)
; CONST StkRwREPEAT                                  =   144 (*16_90 01 *)
; CONST StkRwRETURN                                  =   145 (*16_91 01 *)
; CONST StkRwREVEAL                                  =   146 (*16_92 01 *)
; CONST StkRwROOT                                    =   147 (*16_93 01 *)
; CONST StkRwSET                                     =   148 (*16_94 01 *)
; CONST StkRwTHEN                                    =   149 (*16_95 01 *)
; CONST StkRwTO                                      =   150 (*16_96 01 *)
; CONST StkRwTRY                                     =   151 (*16_97 01 *)
; CONST StkRwTYPE                                    =   152 (*16_98 01 *)
; CONST StkRwTYPECASE                                =   153 (*16_99 01 *)
; CONST StkRwUNSAFE                                  =   154 (*16_9a 01 *)
; CONST StkRwUNTIL                                   =   155 (*16_9b 01 *)
; CONST StkRwUNTRACED                                =   156 (*16_9c 01 *)
; CONST StkRwVALUE                                   =   157 (*16_9d 01 *)
; CONST StkRwVAR                                     =   158 (*16_9e 01 *)
; CONST StkRwWHILE                                   =   159 (*16_9f 01 *)
; CONST StkRwWITH                                    =   160 (*16_a0 01 *)
(* Special character tokens: *)
; CONST StkSemicolon                                 =   161 (*16_a1 01 *)
; CONST StkDot                                       =   162 (*16_a2 01 *)
; CONST StkEqual                                     =   163 (*16_a3 01 *)
; CONST StkOpenParen                                 =   164 (*16_a4 01 *)
; CONST StkCloseParen                                =   165 (*16_a5 01 *)
; CONST StkComma                                     =   166 (*16_a6 01 *)
; CONST StkColon                                     =   167 (*16_a7 01 *)
; CONST StkSubtype                                   =   168 (*16_a8 01 *)
; CONST StkBecomes                                   =   169 (*16_a9 01 *)
; CONST StkOpenBrace                                 =   170 (*16_aa 01 *)
; CONST StkCloseBrace                                =   171 (*16_ab 01 *)
; CONST StkStroke                                    =   172 (*16_ac 01 *)
; CONST StkArrow                                     =   173 (*16_ad 01 *)
; CONST StkEllipsis                                  =   174 (*16_ae 01 *)
; CONST StkOpenBracket                               =   175 (*16_af 01 *)
; CONST StkCloseBracket                              =   176 (*16_b0 01 *)
; CONST StkUnequal                                   =   177 (*16_b1 01 *)
; CONST StkLess                                      =   178 (*16_b2 01 *)
; CONST StkGreater                                   =   179 (*16_b3 01 *)
; CONST StkLessEqual                                 =   180 (*16_b4 01 *)
; CONST StkGreaterEqual                              =   181 (*16_b5 01 *)
; CONST StkPlus                                      =   182 (*16_b6 01 *)
; CONST StkMinus                                     =   183 (*16_b7 01 *)
; CONST StkAmpersand                                 =   184 (*16_b8 01 *)
; CONST StkStar                                      =   185 (*16_b9 01 *)
; CONST StkSlash                                     =   186 (*16_ba 01 *)
; CONST StkDeref                                     =   187 (*16_bb 01 *)
; CONST StkOpenPragma                                =   188 (*16_bc 01 *)
; CONST StkClosePragma                               =   189 (*16_bd 01 *)
(* Variable tokens: *)
; CONST StkIdent                                     =   190 (*16_be 01 *)
; CONST StkIntLit                                    =   191 (*16_bf 01 *)
; CONST StkLongIntLit                                =   192 (*16_c0 01 *)
; CONST StkBasedLit                                  =   193 (*16_c1 01 *)
; CONST StkLongBasedLit                              =   194 (*16_c2 01 *)
; CONST StkRealLit                                   =   195 (*16_c3 01 *)
; CONST StkLongRealLit                               =   196 (*16_c4 01 *)
; CONST StkExtendedLit                               =   197 (*16_c5 01 *)
; CONST StkTextLit                                   =   198 (*16_c6 01 *)
; CONST StkWideTextLit                               =   199 (*16_c7 01 *)
; CONST StkCharLit                                   =   200 (*16_c8 01 *)
; CONST StkWideCharLit                               =   201 (*16_c9 01 *)
; CONST StkLexErrChars                               =   202 (*16_ca 01 *)
; CONST StkPragmaId                                  =   203 (*16_cb 01 *)
(* Not a known pragma ident, but found where one is expected. *)
(* End of FM3SrcToks.gentok. *)
; CONST TkMaxTok                                     =   203

; END FM3SrcToks
.

