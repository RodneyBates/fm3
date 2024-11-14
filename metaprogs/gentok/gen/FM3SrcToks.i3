
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
; CONST StkUnknown                                   =    -1 (*16_7f *)
; CONST StkBOF                                       =    -2 (*16_7e *)
(* These source token codes' declarations have copies in FM3Parser.lalr,
      in lalr's own syntax, which these must remain consistent with.
   *)
(* Modula-3 Reserved words: *)
(* ABS -5: *)

; CONST StkRwAND                                     =    -5 (*16_7b *)
; CONST StkRwANY                                     =    -6 (*16_7a *)
; CONST StkRwARRAY                                   =    -7 (*16_79 *)
; CONST StkRwAS                                      =    -8 (*16_78 *)
; CONST StkRwBEGIN                                   =    -9 (*16_77 *)
; CONST StkRwBITS                                    =   -10 (*16_76 *)
; CONST StkRwBRANDED                                 =   -11 (*16_75 *)
; CONST StkRwBY                                      =   -12 (*16_74 *)
; CONST StkRwCASE                                    =   -13 (*16_73 *)
; CONST StkRwCONST                                   =   -14 (*16_72 *)
; CONST StkRwDIV                                     =   -15 (*16_71 *)
; CONST StkRwDO                                      =   -16 (*16_70 *)
; CONST StkRwELSE                                    =   -17 (*16_6f *)
; CONST StkRwELSIF                                   =   -18 (*16_6e *)
; CONST StkRwEND                                     =   -19 (*16_6d *)
; CONST StkRwEVAL                                    =   -20 (*16_6c *)
; CONST StkRwEXCEPT                                  =   -21 (*16_6b *)
; CONST StkRwEXCEPTION                               =   -22 (*16_6a *)
; CONST StkRwEXIT                                    =   -23 (*16_69 *)
; CONST StkRwEXPORTS                                 =   -24 (*16_68 *)
; CONST StkRwFINALLY                                 =   -25 (*16_67 *)
; CONST StkRwFOR                                     =   -26 (*16_66 *)
; CONST StkRwFROM                                    =   -27 (*16_65 *)
; CONST StkRwGENERIC                                 =   -28 (*16_64 *)
; CONST StkRwIF                                      =   -29 (*16_63 *)
; CONST StkRwIMPORT                                  =   -30 (*16_62 *)
; CONST StkRwIN                                      =   -31 (*16_61 *)
; CONST StkRwINTERFACE                               =   -32 (*16_60 *)
; CONST StkRwLOCK                                    =   -33 (*16_5f *)
; CONST StkRwLOOP                                    =   -34 (*16_5e *)
; CONST StkRwMETHODS                                 =   -35 (*16_5d *)
; CONST StkRwMOD                                     =   -36 (*16_5c *)
; CONST StkRwMODULE                                  =   -37 (*16_5b *)
; CONST StkRwNOT                                     =   -38 (*16_5a *)
; CONST StkRwOBJECT                                  =   -39 (*16_59 *)
; CONST StkRwOF                                      =   -40 (*16_58 *)
; CONST StkRwOR                                      =   -41 (*16_57 *)
; CONST StkRwOVERRIDES                               =   -42 (*16_56 *)
; CONST StkRwPROCEDURE                               =   -43 (*16_55 *)
; CONST StkRwRAISE                                   =   -44 (*16_54 *)
; CONST StkRwRAISES                                  =   -45 (*16_53 *)
; CONST StkRwREADONLY                                =   -46 (*16_52 *)
; CONST StkRwRECORD                                  =   -47 (*16_51 *)
; CONST StkRwREF                                     =   -48 (*16_50 *)
; CONST StkRwREPEAT                                  =   -49 (*16_4f *)
; CONST StkRwRETURN                                  =   -50 (*16_4e *)
; CONST StkRwREVEAL                                  =   -51 (*16_4d *)
; CONST StkRwROOT                                    =   -52 (*16_4c *)
; CONST StkRwSET                                     =   -53 (*16_4b *)
; CONST StkRwTHEN                                    =   -54 (*16_4a *)
; CONST StkRwTO                                      =   -55 (*16_49 *)
; CONST StkRwTRY                                     =   -56 (*16_48 *)
; CONST StkRwTYPE                                    =   -57 (*16_47 *)
; CONST StkRwTYPECASE                                =   -58 (*16_46 *)
; CONST StkRwUNSAFE                                  =   -59 (*16_45 *)
; CONST StkRwUNTIL                                   =   -60 (*16_44 *)
; CONST StkRwUNTRACED                                =   -61 (*16_43 *)
; CONST StkRwVALUE                                   =   -62 (*16_42 *)
; CONST StkRwVAR                                     =   -63 (*16_41 *)
; CONST StkRwWHILE                                   =   -64 (*16_40 *)
; CONST StkRwWITH                                    =   -65 (*16_bf 7f *)
(* Special character tokens: *)
; CONST StkSemicolon                                 =   -66 (*16_be 7f *)
; CONST StkDot                                       =   -67 (*16_bd 7f *)
; CONST StkEqual                                     =   -68 (*16_bc 7f *)
; CONST StkOpenParen                                 =   -69 (*16_bb 7f *)
; CONST StkCloseParen                                =   -70 (*16_ba 7f *)
; CONST StkComma                                     =   -71 (*16_b9 7f *)
; CONST StkColon                                     =   -72 (*16_b8 7f *)
; CONST StkSubtype                                   =   -73 (*16_b7 7f *)
; CONST StkBecomes                                   =   -74 (*16_b6 7f *)
; CONST StkOpenBrace                                 =   -75 (*16_b5 7f *)
; CONST StkCloseBrace                                =   -76 (*16_b4 7f *)
; CONST StkStroke                                    =   -77 (*16_b3 7f *)
; CONST StkArrow                                     =   -78 (*16_b2 7f *)
; CONST StkEllipsis                                  =   -79 (*16_b1 7f *)
; CONST StkOpenBracket                               =   -80 (*16_b0 7f *)
; CONST StkCloseBracket                              =   -81 (*16_af 7f *)
; CONST StkUnequal                                   =   -82 (*16_ae 7f *)
; CONST StkLess                                      =   -83 (*16_ad 7f *)
; CONST StkGreater                                   =   -84 (*16_ac 7f *)
; CONST StkLessEqual                                 =   -85 (*16_ab 7f *)
; CONST StkGreaterEqual                              =   -86 (*16_aa 7f *)
; CONST StkPlus                                      =   -87 (*16_a9 7f *)
; CONST StkMinus                                     =   -88 (*16_a8 7f *)
; CONST StkAmpersand                                 =   -89 (*16_a7 7f *)
; CONST StkStar                                      =   -90 (*16_a6 7f *)
; CONST StkSlash                                     =   -91 (*16_a5 7f *)
; CONST StkDeref                                     =   -92 (*16_a4 7f *)
; CONST StkOpenPragma                                =   -93 (*16_a3 7f *)
; CONST StkClosePragma                               =   -94 (*16_a2 7f *)
(* Variable tokens: *)
; CONST StkIdent                                     =   -95 (*16_a1 7f *)
; CONST StkIntLit                                    =   -96 (*16_a0 7f *)
; CONST StkLongIntLit                                =   -97 (*16_9f 7f *)
; CONST StkBasedLit                                  =   -98 (*16_9e 7f *)
; CONST StkLongBasedLit                              =   -99 (*16_9d 7f *)
; CONST StkRealLit                                   =  -100 (*16_9c 7f *)
; CONST StkLongRealLit                               =  -101 (*16_9b 7f *)
; CONST StkExtendedLit                               =  -102 (*16_9a 7f *)
; CONST StkTextLit                                   =  -103 (*16_99 7f *)
; CONST StkWideTextLit                               =  -104 (*16_98 7f *)
; CONST StkCharLit                                   =  -105 (*16_97 7f *)
; CONST StkWideCharLit                               =  -106 (*16_96 7f *)
; CONST StkLexErrChars                               =  -107 (*16_95 7f *)
; CONST StkPragmaId                                  =  -108 (*16_94 7f *)
(* Not a known pragma ident, but found where one is expected. *)
(* Don't forget, values are negative and decreasing. *)
(* We want to scan reserved ids and predefined ids in interfaces that
      have to have special treatment by the compiler using the same lex
      machine as reserved words, but treat each as an identifier
      whose atom id is actually the source token code. 
   *)
(* Reserved identifiers: *)
; CONST TokMaxPredef                                 =  -109 (*16_93 7f *)
; CONST RidNull                                      =  -110 (*16_92 7f *)
(* Scanner will never present this. *)
; CONST RidABS                                       =  -111 (*16_91 7f *)
; CONST RidADDRESS                                   =  -112 (*16_90 7f *)
; CONST RidADR                                       =  -113 (*16_8f 7f *)
; CONST RidADRSIZE                                   =  -114 (*16_8e 7f *)
; CONST RidBITSIZE                                   =  -115 (*16_8d 7f *)
; CONST RidBOOLEAN                                   =  -116 (*16_8c 7f *)
; CONST RidBYTESIZE                                  =  -117 (*16_8b 7f *)
; CONST RidCARDINAL                                  =  -118 (*16_8a 7f *)
; CONST RidCEILING                                   =  -119 (*16_89 7f *)
; CONST RidCHAR                                      =  -120 (*16_88 7f *)
; CONST RidDEC                                       =  -121 (*16_87 7f *)
; CONST RidDISPOSE                                   =  -122 (*16_86 7f *)
; CONST RidEXTENDED                                  =  -123 (*16_85 7f *)
; CONST RidFALSE                                     =  -124 (*16_84 7f *)
; CONST RidFIRST                                     =  -125 (*16_83 7f *)
; CONST RidFLOAT                                     =  -126 (*16_82 7f *)
; CONST RidFLOOR                                     =  -127 (*16_81 7f *)
; CONST RidINC                                       =  -128 (*16_80 7f *)
; CONST RidINTEGER                                   =  -129 (*16_ff 7e *)
; CONST RidISTYPE                                    =  -130 (*16_fe 7e *)
; CONST RidLAST                                      =  -131 (*16_fd 7e *)
; CONST RidLONGCARD                                  =  -132 (*16_fc 7e *)
; CONST RidLONGINT                                   =  -133 (*16_fb 7e *)
; CONST RidLONGREAL                                  =  -134 (*16_fa 7e *)
; CONST RidLOOPHOLE                                  =  -135 (*16_f9 7e *)
; CONST RidMAX                                       =  -136 (*16_f8 7e *)
; CONST RidMIN                                       =  -137 (*16_f7 7e *)
; CONST RidMUTEX                                     =  -138 (*16_f6 7e *)
; CONST RidNARROW                                    =  -139 (*16_f5 7e *)
; CONST RidNEW                                       =  -140 (*16_f4 7e *)
; CONST RidNIL                                       =  -141 (*16_f3 7e *)
; CONST RidNULL                                      =  -142 (*16_f2 7e *)
; CONST RidNUMBER                                    =  -143 (*16_f1 7e *)
; CONST RidORD                                       =  -144 (*16_f0 7e *)
; CONST RidREAL                                      =  -145 (*16_ef 7e *)
; CONST RidREFANY                                    =  -146 (*16_ee 7e *)
; CONST RidROUND                                     =  -147 (*16_ed 7e *)
; CONST RidSUBARRAY                                  =  -148 (*16_ec 7e *)
; CONST RidTEXT                                      =  -149 (*16_eb 7e *)
; CONST RidTRUE                                      =  -150 (*16_ea 7e *)
; CONST RidTRUNC                                     =  -151 (*16_e9 7e *)
; CONST RidTYPECODE                                  =  -152 (*16_e8 7e *)
; CONST RidVAL                                       =  -153 (*16_e7 7e *)
; CONST RidWIDECHAR                                  =  -154 (*16_e6 7e *)
(* These behave semantically like reserved identifiers, but each occurs in
      a different set of syntactic contexts from identifiers, so they are
      source-code reserved words.  We convert those after parsing into
      reserved idents with the following Rid atoms.
   *)
; CONST RidROOT                                      =  -155 (*16_e5 7e *)
; CONST RidUNTRACEDROOT                              =  -156 (*16_e4 7e *)
(* Idents in Word and Long. *)
; CONST Word_T                                       =  -157 (*16_e3 7e *)
; CONST Word_Size                                    =  -158 (*16_e2 7e *)
; CONST Word_Plus                                    =  -159 (*16_e1 7e *)
; CONST Word_Times                                   =  -160 (*16_e0 7e *)
; CONST Word_Minus                                   =  -161 (*16_df 7e *)
; CONST Word_Divide                                  =  -162 (*16_de 7e *)
; CONST Word_Mod                                     =  -163 (*16_dd 7e *)
; CONST Word_LT                                      =  -164 (*16_dc 7e *)
; CONST Word_LE                                      =  -165 (*16_db 7e *)
; CONST Word_GT                                      =  -166 (*16_da 7e *)
; CONST Word_GE                                      =  -167 (*16_d9 7e *)
; CONST Word_And                                     =  -168 (*16_d8 7e *)
; CONST Word_Or                                      =  -169 (*16_d7 7e *)
; CONST Word_Xor                                     =  -170 (*16_d6 7e *)
; CONST Word_Not                                     =  -171 (*16_d5 7e *)
; CONST Word_Shift                                   =  -172 (*16_d4 7e *)
; CONST Word_LeftShift                               =  -173 (*16_d3 7e *)
; CONST Word_RightShift                              =  -174 (*16_d2 7e *)
; CONST Word_Rotate                                  =  -175 (*16_d1 7e *)
; CONST Word_LeftRotate                              =  -176 (*16_d0 7e *)
; CONST Word_RightRotate                             =  -177 (*16_cf 7e *)
; CONST Word_Extract                                 =  -178 (*16_ce 7e *)
; CONST Word_Insert                                  =  -179 (*16_cd 7e *)
; CONST Long_T                                       =  -180 (*16_cc 7e *)
; CONST Long_Size                                    =  -181 (*16_cb 7e *)
; CONST Long_Plus                                    =  -182 (*16_ca 7e *)
; CONST Long_Times                                   =  -183 (*16_c9 7e *)
; CONST Long_Minus                                   =  -184 (*16_c8 7e *)
; CONST Long_Divide                                  =  -185 (*16_c7 7e *)
; CONST Long_Mod                                     =  -186 (*16_c6 7e *)
; CONST Long_LT                                      =  -187 (*16_c5 7e *)
; CONST Long_LE                                      =  -188 (*16_c4 7e *)
; CONST Long_GT                                      =  -189 (*16_c3 7e *)
; CONST Long_GE                                      =  -190 (*16_c2 7e *)
; CONST Long_And                                     =  -191 (*16_c1 7e *)
; CONST Long_Or                                      =  -192 (*16_c0 7e *)
; CONST Long_Xor                                     =  -193 (*16_bf 7e *)
; CONST Long_Not                                     =  -194 (*16_be 7e *)
; CONST Long_Shift                                   =  -195 (*16_bd 7e *)
; CONST Long_LeftShift                               =  -196 (*16_bc 7e *)
; CONST Long_RightShift                              =  -197 (*16_bb 7e *)
; CONST Long_Rotate                                  =  -198 (*16_ba 7e *)
; CONST Long_LeftRotate                              =  -199 (*16_b9 7e *)
; CONST Long_RightRotate                             =  -200 (*16_b8 7e *)
; CONST Long_Extract                                 =  -201 (*16_b7 7e *)
; CONST Long_Insert                                  =  -202 (*16_b6 7e *)
; CONST TokMinPredef                                 =  -203 (*16_b5 7e *)
(* End of FM3SrcToks.gentok. *)
; CONST TkMinTok                                     =  -203

; CONST TkMaxTok                                     =     0

; END FM3SrcToks
.

