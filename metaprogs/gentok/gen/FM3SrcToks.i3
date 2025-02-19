
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
(* Use as a Null Opcode. *)
; CONST StkUnknown                                   =     1 (*16_01 *)
; CONST StkBOF                                       =     2 (*16_02 *)
(* Leave a little space. *)
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
(* The scanner will recognize standard ids declared in interfaces that
      have special treatment by the compiler and reserved ids, all using the
      same lex machine as reserved words. 
   *)
(* The parser will convert '+' and '-' into StkUnary* when syntactic
      context shows they really are unary.  They will be used later as
      operation codes.  The scanner will not produce them.
   *)
; CONST StkUnaryPlus                                 =   109 (*16_ed 00 *)
; CONST StkUnaryMinus                                =   110 (*16_ee 00 *)
(* Builtin Identifiers: *)
; CONST StkMinBuiltin                                =   111 (*16_ef 00 *)
(* Reserved identifiers: *)
; CONST StkMinStatic                                 =   111 (*16_ef 00 *)
; CONST StkMinRid                                    =   111 (*16_ef 00 *)
; CONST RidNull                                      =   111 (*16_ef 00 *)
(* ^Scanner will never present this, but parsing may convert to it in a
       context where a reserved ident is illegal.
   *)
(* Things that will have a single, shared ExprTyp node. *)
(* Constants: *)
; CONST RidFALSE                                     =   112 (*16_f0 00 *)
; CONST RidNIL                                       =   113 (*16_f1 00 *)
; CONST RidTRUE                                      =   114 (*16_f2 00 *)
(* Types: *)
; CONST StkMinType                                   =   115 (*16_f3 00 *)
; CONST RidADDRESS                                   =   115 (*16_f3 00 *)
; CONST RidBOOLEAN                                   =   116 (*16_f4 00 *)
; CONST RidCARDINAL                                  =   117 (*16_f5 00 *)
; CONST RidCHAR                                      =   118 (*16_f6 00 *)
; CONST RidEXTENDED                                  =   119 (*16_f7 00 *)
; CONST RidINTEGER                                   =   120 (*16_f8 00 *)
; CONST RidLONGCARD                                  =   121 (*16_f9 00 *)
; CONST RidLONGINT                                   =   122 (*16_fa 00 *)
; CONST RidLONGREAL                                  =   123 (*16_fb 00 *)
; CONST RidMUTEX                                     =   124 (*16_fc 00 *)
; CONST RidNULL                                      =   125 (*16_fd 00 *)
; CONST RidREAL                                      =   126 (*16_fe 00 *)
; CONST RidREFANY                                    =   127 (*16_ff 00 *)
; CONST RidTEXT                                      =   128 (*16_80 01 *)
; CONST StkMaxType                                   =   129 (*16_81 01 *)
; CONST RidWIDECHAR                                  =   129 (*16_81 01 *)
(* ROOT and UNTRACED behave semantically like reserved identifiers, but
      each occurs in a different set of syntactic contexts from identifiers,
      so they are source-code reserved words.  We convert those during parsing
      into reserved idents with the following Rid atoms.  FM3Scanner
      will not produce them.
   *)
; CONST RidROOT                                      =   130 (*16_82 01 *)
; CONST StkMaxStatic                                 =   131 (*16_83 01 *)
; CONST RidUNTRACEDROOT                              =   131 (*16_83 01 *)
(* These are operations.  Each instance requires its own ExprTyp
      node, with differing children and other fields.
   *)
; CONST StkMinOperation                              =   132 (*16_84 01 *)
(* Functions: *)
; CONST RidABS                                       =   132 (*16_84 01 *)
; CONST RidADR                                       =   133 (*16_85 01 *)
; CONST RidADRSIZE                                   =   134 (*16_86 01 *)
; CONST RidBITSIZE                                   =   135 (*16_87 01 *)
; CONST RidBYTESIZE                                  =   136 (*16_88 01 *)
; CONST RidCEILING                                   =   137 (*16_89 01 *)
; CONST RidFIRST                                     =   138 (*16_8a 01 *)
; CONST RidFLOAT                                     =   139 (*16_8b 01 *)
; CONST RidFLOOR                                     =   140 (*16_8c 01 *)
; CONST RidISTYPE                                    =   141 (*16_8d 01 *)
; CONST RidLAST                                      =   142 (*16_8e 01 *)
; CONST RidLOOPHOLE                                  =   143 (*16_8f 01 *)
; CONST RidMAX                                       =   144 (*16_90 01 *)
; CONST RidMIN                                       =   145 (*16_91 01 *)
; CONST RidNARROW                                    =   146 (*16_92 01 *)
; CONST RidNEW                                       =   147 (*16_93 01 *)
; CONST RidNUMBER                                    =   148 (*16_94 01 *)
; CONST RidORD                                       =   149 (*16_95 01 *)
; CONST RidROUND                                     =   150 (*16_96 01 *)
; CONST RidSUBARRAY                                  =   151 (*16_97 01 *)
; CONST RidTRUNC                                     =   152 (*16_98 01 *)
; CONST RidTYPECODE                                  =   153 (*16_99 01 *)
; CONST RidVAL                                       =   154 (*16_9a 01 *)
(* Proper procedures: *)
; CONST RidDISPOSE                                   =   155 (*16_9b 01 *)
; CONST RidDEC                                       =   156 (*16_9c 01 *)
; CONST StkMaxRid                                    =   157 (*16_9d 01 *)
; CONST RidINC                                       =   157 (*16_9d 01 *)
(* Standard identifiers.  Scanner will treat these as ordinary
      identifiers, except it adds their codes as a separate field.
      Later semantic processing will decide whether the standard
      meaning should  be used. 
   *)
; CONST StkMinStd                                    =   158 (*16_9e 01 *)
(* In Word and Long: *)
; CONST StkMinWordLong                               =   158 (*16_9e 01 *)
(* Also in many standard interfaces: *)
; CONST StkPdT                                       =   158 (*16_9e 01 *)
; CONST StkPdSize                                    =   159 (*16_9f 01 *)
; CONST StkPdPlus                                    =   160 (*16_a0 01 *)
; CONST StkPdTimes                                   =   161 (*16_a1 01 *)
; CONST StkPdMinus                                   =   162 (*16_a2 01 *)
; CONST StkPdDivide                                  =   163 (*16_a3 01 *)
; CONST StkPdMod                                     =   164 (*16_a4 01 *)
; CONST StkPdLT                                      =   165 (*16_a5 01 *)
; CONST StkPdLE                                      =   166 (*16_a6 01 *)
; CONST StkPdGT                                      =   167 (*16_a7 01 *)
; CONST StkPdGE                                      =   168 (*16_a8 01 *)
(* Bitwise *)
; CONST StkPdAnd                                     =   169 (*16_a9 01 *)
(* Bitwise *)
; CONST StkPdOr                                      =   170 (*16_aa 01 *)
(* Bitwise *)
; CONST StkPdXor                                     =   171 (*16_ab 01 *)
(* Bitwise *)
; CONST StkPdNot                                     =   172 (*16_ac 01 *)
; CONST StkPdShift                                   =   173 (*16_ad 01 *)
; CONST StkPdLeftShift                               =   174 (*16_ae 01 *)
; CONST StkPdRightShift                              =   175 (*16_af 01 *)
; CONST StkPdRotate                                  =   176 (*16_b0 01 *)
; CONST StkPdLeftRotate                              =   177 (*16_b1 01 *)
; CONST StkPdRightRotate                             =   178 (*16_b2 01 *)
; CONST StkPdExtract                                 =   179 (*16_b3 01 *)
; CONST StkMaxWordLong                               =   180 (*16_b4 01 *)
; CONST StkPdInsert                                  =   180 (*16_b4 01 *)
(* Translated to unique builtin decl codes in Long: *)
(* These must remain in the same order as their Word counterparts.
   See FM3Pass2 . FixDeclTok.
*)
; CONST StkMinLong                                   =   181 (*16_b5 01 *)
; CONST StkPd_Long_T                                 =   181 (*16_b5 01 *)
; CONST StkPd_Long_Size                              =   182 (*16_b6 01 *)
; CONST StkPd_Long_Plus                              =   183 (*16_b7 01 *)
; CONST StkPd_Long_Times                             =   184 (*16_b8 01 *)
; CONST StkPd_Long_Minus                             =   185 (*16_b9 01 *)
; CONST StkPd_Long_Divide                            =   186 (*16_ba 01 *)
; CONST StkPd_Long_Mod                               =   187 (*16_bb 01 *)
; CONST StkPd_Long_LT                                =   188 (*16_bc 01 *)
; CONST StkPd_Long_LE                                =   189 (*16_bd 01 *)
; CONST StkPd_Long_GT                                =   190 (*16_be 01 *)
; CONST StkPd_Long_GE                                =   191 (*16_bf 01 *)
(* Bitwise *)
; CONST StkPd_Long_And                               =   192 (*16_c0 01 *)
(* Bitwise *)
; CONST StkPd_Long_Or                                =   193 (*16_c1 01 *)
(* Bitwise *)
; CONST StkPd_Long_Xor                               =   194 (*16_c2 01 *)
(* Bitwise *)
; CONST StkPd_Long_Not                               =   195 (*16_c3 01 *)
; CONST StkPd_Long_Shift                             =   196 (*16_c4 01 *)
; CONST StkPd_Long_LeftShift                         =   197 (*16_c5 01 *)
; CONST StkPd_Long_RightShift                        =   198 (*16_c6 01 *)
; CONST StkPd_Long_Rotate                            =   199 (*16_c7 01 *)
; CONST StkPd_Long_LeftRotate                        =   200 (*16_c8 01 *)
; CONST StkPd_Long_RightRotate                       =   201 (*16_c9 01 *)
; CONST StkPd_Long_Extract                           =   202 (*16_ca 01 *)
; CONST StkMaxLong                                   =   203 (*16_cb 01 *)
; CONST StkPd_Long_Insert                            =   203 (*16_cb 01 *)
; CONST StkMaxOperation                              =   204 (*16_cc 01 *)
; CONST StkRTUniqueBrand                             =   204 (*16_cc 01 *)
(* COMPLETEME: Idents declared in other standard interfaces. *)
(* Standard interface names: *)
; CONST StkMinStdIntf                                =   205 (*16_cd 01 *)
; CONST StkPdMain                                    =   205 (*16_cd 01 *)
; CONST StkPdText                                    =   206 (*16_ce 01 *)
; CONST StkThread                                    =   207 (*16_cf 01 *)
; CONST StkPdWord                                    =   208 (*16_d0 01 *)
; CONST StkPdLong                                    =   209 (*16_d1 01 *)
(* For LONGINT. *)
; CONST StkPdReal                                    =   210 (*16_d2 01 *)
; CONST StkPdLongReal                                =   211 (*16_d3 01 *)
; CONST StkPdExtended                                =   212 (*16_d4 01 *)
; CONST StkPdRealFloat                               =   213 (*16_d5 01 *)
; CONST StkPdLongRealFloat                           =   214 (*16_d6 01 *)
; CONST StkPdExtendedFloat                           =   215 (*16_d7 01 *)
; CONST StkPdFloatMode                               =   216 (*16_d8 01 *)
; CONST StkPdLex                                     =   217 (*16_d9 01 *)
; CONST StkMaxStdIntf                                =   218 (*16_da 01 *)
; CONST StkMaxStd                                    =   218 (*16_da 01 *)
; CONST StkMaxBuiltin                                =   218 (*16_da 01 *)
; CONST StkPdFmt                                     =   218 (*16_da 01 *)
(* Runtime operations: *)
(* End of FM3SrcToks.gentok. *)
; CONST TkMinTok                                     =     0

; CONST TkMaxTok                                     =   218

; END FM3SrcToks
.

