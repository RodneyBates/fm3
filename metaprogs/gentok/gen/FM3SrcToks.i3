
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

; CONST StkEOF                                       =     0 (*16_00 *)
(* From lalr, Gen.m3, FindFirstToken:
               "EndOfToken wird immer mit 0 codiert"
            *)
(* Not used in FM3Scanner or FM3Parser: *)
; CONST StkUnknown                                   =     1 (*16_01 *)
; CONST StkBOF                                       =     2 (*16_02 *)
(* These source token codes' declarations have copies in FM3Parser.lalr,
      in lalr's own syntax, which these must remain consistent with.
   *)
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
; CONST StkPragmaId                                  =   108 (*16_ec 00 *)
(* Not a known pragma ident, but found where one is expected. *)
(* The scanner will recognize predefined ids in interfaces that have special
      treatment by the compiler and reserved ids using the same lex machine as
      reserved words, but treat each as an identifier whose atom id is actually
      the negated source token code. 
   *)
(* Reserved identifiers: *)
; CONST StkMinRid                                    =   109 (*16_ed 00 *)
; CONST RidNull                                      =   109 (*16_ed 00 *)
(* Scanner will never present this. *)
; CONST RidABS                                       =   110 (*16_ee 00 *)
; CONST RidADDRESS                                   =   111 (*16_ef 00 *)
; CONST RidADR                                       =   112 (*16_f0 00 *)
; CONST RidADRSIZE                                   =   113 (*16_f1 00 *)
; CONST RidBITSIZE                                   =   114 (*16_f2 00 *)
; CONST RidBOOLEAN                                   =   115 (*16_f3 00 *)
; CONST RidBYTESIZE                                  =   116 (*16_f4 00 *)
; CONST RidCARDINAL                                  =   117 (*16_f5 00 *)
; CONST RidCEILING                                   =   118 (*16_f6 00 *)
; CONST RidCHAR                                      =   119 (*16_f7 00 *)
; CONST RidDEC                                       =   120 (*16_f8 00 *)
; CONST RidDISPOSE                                   =   121 (*16_f9 00 *)
; CONST RidEXTENDED                                  =   122 (*16_fa 00 *)
; CONST RidFALSE                                     =   123 (*16_fb 00 *)
; CONST RidFIRST                                     =   124 (*16_fc 00 *)
; CONST RidFLOAT                                     =   125 (*16_fd 00 *)
; CONST RidFLOOR                                     =   126 (*16_fe 00 *)
; CONST RidINC                                       =   127 (*16_ff 00 *)
; CONST RidINTEGER                                   =   128 (*16_80 01 *)
; CONST RidISTYPE                                    =   129 (*16_81 01 *)
; CONST RidLAST                                      =   130 (*16_82 01 *)
; CONST RidLONGCARD                                  =   131 (*16_83 01 *)
; CONST RidLONGINT                                   =   132 (*16_84 01 *)
; CONST RidLONGREAL                                  =   133 (*16_85 01 *)
; CONST RidLOOPHOLE                                  =   134 (*16_86 01 *)
; CONST RidMAX                                       =   135 (*16_87 01 *)
; CONST RidMIN                                       =   136 (*16_88 01 *)
; CONST RidMUTEX                                     =   137 (*16_89 01 *)
; CONST RidNARROW                                    =   138 (*16_8a 01 *)
; CONST RidNEW                                       =   139 (*16_8b 01 *)
; CONST RidNIL                                       =   140 (*16_8c 01 *)
; CONST RidNULL                                      =   141 (*16_8d 01 *)
; CONST RidNUMBER                                    =   142 (*16_8e 01 *)
; CONST RidORD                                       =   143 (*16_8f 01 *)
; CONST RidREAL                                      =   144 (*16_90 01 *)
; CONST RidREFANY                                    =   145 (*16_91 01 *)
; CONST RidROUND                                     =   146 (*16_92 01 *)
; CONST RidSUBARRAY                                  =   147 (*16_93 01 *)
; CONST RidTEXT                                      =   148 (*16_94 01 *)
; CONST RidTRUE                                      =   149 (*16_95 01 *)
; CONST RidTRUNC                                     =   150 (*16_96 01 *)
; CONST RidTYPECODE                                  =   151 (*16_97 01 *)
; CONST RidVAL                                       =   152 (*16_98 01 *)
; CONST StkMaxRid                                    =   153 (*16_99 01 *)
; CONST RidWIDECHAR                                  =   153 (*16_99 01 *)
(* These behave semantically like reserved identifiers, but each occurs in
      a different set of syntactic contexts from identifiers, so they are
      source-code reserved words.  We convert those after parsing into
      reserved idents with the following Rid atoms.
   *)
; CONST RidROOT                                      =   154 (*16_9a 01 *)
; CONST RidUNTRACEDROOT                              =   155 (*16_9b 01 *)
(* Idents in Word and Long. *)
(* These, when negated, serve as pseudo-identifier atoms and
      pseudo-declaration numbers in both Word and Long interfaces.
      They are also context-independent operation codes in Word, but Long
      has a separate but parallel operation code numbering, generated
      by FM3LongToks. 
      These must be equivalent names in the same order as corresponding names
      in FM3SrcToks.gentok.
   *)
; CONST StkMinPredef                                 =   156 (*16_9c 01 *)
; CONST Word_T                                       =   156 (*16_9c 01 *)
; CONST Word_Size                                    =   157 (*16_9d 01 *)
; CONST Word_Plus                                    =   158 (*16_9e 01 *)
; CONST Word_Times                                   =   159 (*16_9f 01 *)
; CONST Word_Minus                                   =   160 (*16_a0 01 *)
; CONST Word_Divide                                  =   161 (*16_a1 01 *)
; CONST Word_Mod                                     =   162 (*16_a2 01 *)
; CONST Word_LT                                      =   163 (*16_a3 01 *)
; CONST Word_LE                                      =   164 (*16_a4 01 *)
; CONST Word_GT                                      =   165 (*16_a5 01 *)
; CONST Word_GE                                      =   166 (*16_a6 01 *)
; CONST Word_And                                     =   167 (*16_a7 01 *)
; CONST Word_Or                                      =   168 (*16_a8 01 *)
; CONST Word_Xor                                     =   169 (*16_a9 01 *)
; CONST Word_Not                                     =   170 (*16_aa 01 *)
; CONST Word_Shift                                   =   171 (*16_ab 01 *)
; CONST Word_LeftShift                               =   172 (*16_ac 01 *)
; CONST Word_RightShift                              =   173 (*16_ad 01 *)
; CONST Word_Rotate                                  =   174 (*16_ae 01 *)
; CONST Word_LeftRotate                              =   175 (*16_af 01 *)
; CONST Word_RightRotate                             =   176 (*16_b0 01 *)
; CONST Word_Extract                                 =   177 (*16_b1 01 *)
; CONST Word_Insert                                  =   178 (*16_b2 01 *)
(* Decl numbers and operation numbers in Long only. *)
(* REL 5: *)

; CONST Long_T                                       =   184 (*16_b8 01 *)
; CONST Long_Size                                    =   185 (*16_b9 01 *)
; CONST Long_Plus                                    =   186 (*16_ba 01 *)
; CONST Long_Times                                   =   187 (*16_bb 01 *)
; CONST Long_Minus                                   =   188 (*16_bc 01 *)
; CONST Long_Divide                                  =   189 (*16_bd 01 *)
; CONST Long_Mod                                     =   190 (*16_be 01 *)
; CONST Long_LT                                      =   191 (*16_bf 01 *)
; CONST Long_LE                                      =   192 (*16_c0 01 *)
; CONST Long_GT                                      =   193 (*16_c1 01 *)
; CONST Long_GE                                      =   194 (*16_c2 01 *)
; CONST Long_And                                     =   195 (*16_c3 01 *)
; CONST Long_Or                                      =   196 (*16_c4 01 *)
; CONST Long_Xor                                     =   197 (*16_c5 01 *)
; CONST Long_Not                                     =   198 (*16_c6 01 *)
; CONST Long_Shift                                   =   199 (*16_c7 01 *)
; CONST Long_LeftShift                               =   200 (*16_c8 01 *)
; CONST Long_RightShift                              =   201 (*16_c9 01 *)
; CONST Long_Rotate                                  =   202 (*16_ca 01 *)
; CONST Long_LeftRotate                              =   203 (*16_cb 01 *)
; CONST Long_RightRotate                             =   204 (*16_cc 01 *)
; CONST Long_Extract                                 =   205 (*16_cd 01 *)
; CONST StkMaxPredef                                 =   206 (*16_ce 01 *)
; CONST Long_Insert                                  =   206 (*16_ce 01 *)
(* These are only used as operator codes.  Scanner won't deliver them
      and parser wouldn't parse them if it did. *)
; CONST StkUnaryPlus                                 =   207 (*16_cf 01 *)
(* End of FM3SrcToks.gentok. *)
; CONST StkUnaryMinus                                =   208 (*16_d0 01 *)
; CONST TkMinTok                                     =     0

; CONST TkMaxTok                                     =   208

; END FM3SrcToks
.

