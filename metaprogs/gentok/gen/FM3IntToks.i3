
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3IntToks.gentok", with command line 
     "./gentok -T -t -c -n FM3IntToks.gentok". *)

INTERFACE FM3IntToks

; IMPORT IntSets

; TYPE TokTyp = INTEGER 

; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

; VAR TokSetTemp : IntSets . T

; VAR TokSetPatch : IntSets . T

; VAR TokSetW1Arg : IntSets . T
   (* ^At least one argument. *)

; VAR TokSetArgs : IntSets . T
   (* ^At least two arguments. *)

; CONST LtToTemp = 1
        (* ^Add this to Lt tokcode to get corresponding LtTemp tokcode. *)
; CONST LtToPatch = 2 
        (* ^Add this to Lt tokcode to get corresponding LtPatch tokcode. *)
; CONST LtToRt = 3    
        (* ^Add this to Lt tokcode to get corresponding Rt tokcode. *)
; CONST LtToOne = 4    
        (* ^Add this to Lt tokcode to get LM Infix tokcode. *)
; CONST LtToOnePatch = 6    
        (* ^Add this to Lt tokcode to get LM Infix patch tokcode. *)
; CONST LtToTwo = 7    
        (* ^Add this to Lt tokcode to get 2nd Infix tokcode. *)
; CONST LtToTwoPatch = 9    
        (* ^Add this to Lt tokcode to get 2nd Infix patch tokcode. *)
; CONST RtToLt = - 3    
        (* ^Add this to Rt tokcode to get corresponding Lt tokcode. *)
; CONST RtToTemp = - 2    
        (* ^Add this to Rt tokcode to get corresponding LtTemp tokcode. *)
; CONST RtToPatch = - 1    
        (* ^Add this to Rt tokcode to get corresponding LtPatch tokcode. *)

(* Being low-numbred, these will read and write either forward
   or backward, without value change.
*)
(* ABS 128: *)

; CONST TkMinTok                                     =   128

(* LONE ItkNull: *)
; CONST ItkNull                                      =   128 (*16_80 01 *)
(* LONE ItkBOF: *)
; CONST ItkBOF                                       =   129 (*16_81 01 *)
(* LONE ItkEOF: *)
; CONST ItkEOF                                       =   130 (*16_82 01 *)
(* LONE ItkLeftEnd: *)
; CONST ItkLeftEnd                                   =   131 (*16_83 01 *)
(* LONE ItkRightEnd: *)
; CONST ItkRightEnd                                  =   132 (*16_84 01 *)
(* Atom, Line, Column. *)
(* LONE ItkImport: *)
; CONST ItkImport                       (*ArgCt: 3*) =   133 (*16_85 01 *)
(* (Atom, Line, Column) for interface, decl. *)
(* LONE ItkFromImport: *)
; CONST ItkFromImport                   (*ArgCt: 6*) =   134 (*16_86 01 *)
(* (Atom, Line, Column) for interface, as. *)
(* LONE ItkImportAs: *)
; CONST ItkImportAs                     (*ArgCt: 6*) =   135 (*16_87 01 *)
(* LONE ItkFormalsListEmpty: *)
; CONST ItkFormalsListEmpty                          =   136 (*16_88 01 *)
(* Position. *)
(* LONE ItkFormalTypeAbsent: *)
; CONST ItkFormalTypeAbsent             (*ArgCt: 2*) =   137 (*16_89 01 *)
(* Position. *)
(* LONE ItkFormalExprAbsent: *)
; CONST ItkFormalExprAbsent             (*ArgCt: 2*) =   138 (*16_8a 01 *)
(* Position. *)
(* LONE ItkRaisesSetAbsent: *)
; CONST ItkRaisesSetAbsent              (*ArgCt: 2*) =   139 (*16_8b 01 *)
(* Position. *)
(* LONE ItkRaisesANY: *)
; CONST ItkRaisesANY                    (*ArgCt: 2*) =   140 (*16_8c 01 *)
(* Position. *)
(* LONE ItkResultTypeAbsent: *)
; CONST ItkResultTypeAbsent             (*ArgCt: 2*) =   141 (*16_8d 01 *)
(* Position. *)
(* LONE ItkProcBodyAbsent: *)
; CONST ItkProcBodyAbsent               (*ArgCt: 2*) =   142 (*16_8e 01 *)
(* Scopes and declarations within. *)
(* ScopeNo. *)
(* ItkScopePatch not used. *)
(* LONE ItkScopeLt: *)
; CONST ItkScopeLt                      (*ArgCt: 1*) =   143 (*16_8f 01 *)
(* ScopeNo. *)
(* LONE ItkScopeRt: *)
; CONST ItkScopeRt                      (*ArgCt: 1*) =   144 (*16_90 01 *)
(* IdAtom, position. Occurs before unnest. *)
(* LONE ItkRefId: *)
; CONST ItkRefId                        (*ArgCt: 3*) =   145 (*16_91 01 *)
(* DeclNo, position. Occurs after unnest. *)
(* LONE ItkRefNo: *)
; CONST ItkRefNo                        (*ArgCt: 3*) =   146 (*16_92 01 *)
(* IdAtom, position *)
(* LONE ItkDuplDeclId: *)
; CONST ItkDuplDeclId                   (*ArgCt: 3*) =   147 (*16_93 01 *)
(* DeclKind, IdAtom, position *)
(* LONE ItkDeclId: *)
; CONST ItkDeclId                       (*ArgCt: 4*) =   148 (*16_94 01 *)
(* DeclNo, position *)
(* LONE ItkDeclNo: *)
; CONST ItkDeclNo                       (*ArgCt: 3*) =   149 (*16_95 01 *)
(* FIXED ItkQualId: *)
; CONST ItkQualIdLt                                  =   150 (*16_96 01 *)
; CONST ItkQualIdLtTemp                              =   151 (*16_97 01 *)
; CONST ItkQualIdLtPatch                             =   152 (*16_98 01 *)
; CONST ItkQualIdRt                                  =   153 (*16_99 01 *)

(* Position. *)
(* FIXED ItkFuncSignature: *)
; CONST ItkFuncSignatureLt              (*ArgCt: 2*) =   154 (*16_9a 01 *)
; CONST ItkFuncSignatureLtTemp          (*ArgCt: 2*) =   155 (*16_9b 01 *)
; CONST ItkFuncSignatureLtPatch         (*ArgCt: 2*) =   156 (*16_9c 01 *)
; CONST ItkFuncSignatureRt              (*ArgCt: 2*) =   157 (*16_9d 01 *)

(* Position. *)
(* FIXED ItkProcSignature: *)
; CONST ItkProcSignatureLt              (*ArgCt: 2*) =   158 (*16_9e 01 *)
; CONST ItkProcSignatureLtTemp          (*ArgCt: 2*) =   159 (*16_9f 01 *)
; CONST ItkProcSignatureLtPatch         (*ArgCt: 2*) =   160 (*16_a0 01 *)
; CONST ItkProcSignatureRt              (*ArgCt: 2*) =   161 (*16_a1 01 *)

(* Position of colon. *)
(* FIXED ItkFormalType: *)
; CONST ItkFormalTypeLt                 (*ArgCt: 2*) =   162 (*16_a2 01 *)
; CONST ItkFormalTypeLtTemp             (*ArgCt: 2*) =   163 (*16_a3 01 *)
; CONST ItkFormalTypeLtPatch            (*ArgCt: 2*) =   164 (*16_a4 01 *)
; CONST ItkFormalTypeRt                 (*ArgCt: 2*) =   165 (*16_a5 01 *)

(* Position of ":=" *)
(* FIXED ItkFormalExpr: *)
; CONST ItkFormalExprLt                 (*ArgCt: 2*) =   166 (*16_a6 01 *)
; CONST ItkFormalExprLtTemp             (*ArgCt: 2*) =   167 (*16_a7 01 *)
; CONST ItkFormalExprLtPatch            (*ArgCt: 2*) =   168 (*16_a8 01 *)
; CONST ItkFormalExprRt                 (*ArgCt: 2*) =   169 (*16_a9 01 *)

(* Position of colon. *)
(* FIXED ItkResultType: *)
; CONST ItkResultTypeLt                 (*ArgCt: 2*) =   170 (*16_aa 01 *)
; CONST ItkResultTypeLtTemp             (*ArgCt: 2*) =   171 (*16_ab 01 *)
; CONST ItkResultTypeLtPatch            (*ArgCt: 2*) =   172 (*16_ac 01 *)
; CONST ItkResultTypeRt                 (*ArgCt: 2*) =   173 (*16_ad 01 *)

(* Position of RAISES. *)
(* FIXED ItkRaisesSet: *)
; CONST ItkRaisesSetLt                  (*ArgCt: 2*) =   174 (*16_ae 01 *)
; CONST ItkRaisesSetLtTemp              (*ArgCt: 2*) =   175 (*16_af 01 *)
; CONST ItkRaisesSetLtPatch             (*ArgCt: 2*) =   176 (*16_b0 01 *)
; CONST ItkRaisesSetRt                  (*ArgCt: 2*) =   177 (*16_b1 01 *)

(* Atom of signature. *)
(* FIXED ItkProcNoBody: *)
; CONST ItkProcNoBodyLt                 (*ArgCt: 1*) =   178 (*16_b2 01 *)
; CONST ItkProcNoBodyLtTemp             (*ArgCt: 1*) =   179 (*16_b3 01 *)
; CONST ItkProcNoBodyLtPatch            (*ArgCt: 1*) =   180 (*16_b4 01 *)
; CONST ItkProcNoBodyRt                 (*ArgCt: 1*) =   181 (*16_b5 01 *)

(* Atom of signature. *)
(* FIXED ItkProcWBody: *)
; CONST ItkProcWBodyLt                  (*ArgCt: 1*) =   182 (*16_b6 01 *)
; CONST ItkProcWBodyLtTemp              (*ArgCt: 1*) =   183 (*16_b7 01 *)
; CONST ItkProcWBodyLtPatch             (*ArgCt: 1*) =   184 (*16_b8 01 *)
; CONST ItkProcWBodyRt                  (*ArgCt: 1*) =   185 (*16_b9 01 *)

(* Position of equal sign. *)
(* FIXED ItkProcBody: *)
; CONST ItkProcBodyLt                   (*ArgCt: 2*) =   186 (*16_ba 01 *)
; CONST ItkProcBodyLtTemp               (*ArgCt: 2*) =   187 (*16_bb 01 *)
; CONST ItkProcBodyLtPatch              (*ArgCt: 2*) =   188 (*16_bc 01 *)
; CONST ItkProcBodyRt                   (*ArgCt: 2*) =   189 (*16_bd 01 *)

(* Position of "(". *)
(* FIXED ItkProcType: *)
; CONST ItkProcTypeLt                   (*ArgCt: 2*) =   190 (*16_be 01 *)
; CONST ItkProcTypeLtTemp               (*ArgCt: 2*) =   191 (*16_bf 01 *)
; CONST ItkProcTypeLtPatch              (*ArgCt: 2*) =   192 (*16_c0 01 *)
; CONST ItkProcTypeRt                   (*ArgCt: 2*) =   193 (*16_c1 01 *)

(* Position of LM declaration. *)
(* FIXED ItkBlock: *)
; CONST ItkBlockLt                      (*ArgCt: 2*) =   194 (*16_c2 01 *)
; CONST ItkBlockLtTemp                  (*ArgCt: 2*) =   195 (*16_c3 01 *)
; CONST ItkBlockLtPatch                 (*ArgCt: 2*) =   196 (*16_c4 01 *)
; CONST ItkBlockRt                      (*ArgCt: 2*) =   197 (*16_c5 01 *)

(* Position of BEGIN. *)
; CONST ItkBlockBeg                     (*ArgCt: 2*) =   198 (*16_c6 01 *)
; CONST ItkBlockBegTemp                 (*ArgCt: 2*) =   199 (*16_c7 01 *)
; CONST ItkBlockBegPatch                (*ArgCt: 2*) =   200 (*16_c8 01 *)

(* Type decls. *)
(* Position of ident (Lt) or position of semicolon (Rt). *)
(* FIXED ItkTypeDecl: *)
; CONST ItkTypeDeclLt                   (*ArgCt: 2*) =   201 (*16_c9 01 *)
; CONST ItkTypeDeclLtTemp               (*ArgCt: 2*) =   202 (*16_ca 01 *)
; CONST ItkTypeDeclLtPatch              (*ArgCt: 2*) =   203 (*16_cb 01 *)
; CONST ItkTypeDeclRt                   (*ArgCt: 2*) =   204 (*16_cc 01 *)

(* I don't think we need a position for this. *)
; CONST ItkTypeDeclEq                                =   205 (*16_cd 01 *)
; CONST ItkTypeDeclEqTemp                            =   206 (*16_ce 01 *)
; CONST ItkTypeDeclEqPatch                           =   207 (*16_cf 01 *)

(* Variable decls. *)
(* Position of LM ident (Lt) or position of semicolon (Rt). *)
(* FIXED ItkVarDecl: *)
; CONST ItkVarDeclLt                    (*ArgCt: 2*) =   208 (*16_d0 01 *)
; CONST ItkVarDeclLtTemp                (*ArgCt: 2*) =   209 (*16_d1 01 *)
; CONST ItkVarDeclLtPatch               (*ArgCt: 2*) =   210 (*16_d2 01 *)
; CONST ItkVarDeclRt                    (*ArgCt: 2*) =   211 (*16_d3 01 *)

(* Position of colon. *)
; CONST ItkVarDeclType                  (*ArgCt: 2*) =   212 (*16_d4 01 *)
; CONST ItkVarDeclTypeTemp              (*ArgCt: 2*) =   213 (*16_d5 01 *)
; CONST ItkVarDeclTypePatch             (*ArgCt: 2*) =   214 (*16_d6 01 *)

(* Position of becomes/ *)
; CONST ItkVarDeclVal                   (*ArgCt: 2*) =   215 (*16_d7 01 *)
; CONST ItkVarDeclValTemp               (*ArgCt: 2*) =   216 (*16_d8 01 *)
; CONST ItkVarDeclValPatch              (*ArgCt: 2*) =   217 (*16_d9 01 *)

(* GenTok implicitly gives list tokens one operand for element count,
   plus any count given explicitly here.
*)
(* Position of EXPORTS. *)
(* LIST ItkExportIdList: *)
; CONST ItkExportIdListLt               (*ArgCt: 3*) =   218 (*16_da 01 *)
; CONST ItkExportIdListLtTemp           (*ArgCt: 3*) =   219 (*16_db 01 *)
; CONST ItkExportIdListLtPatch          (*ArgCt: 3*) =   220 (*16_dc 01 *)
; CONST ItkExportIdListRt               (*ArgCt: 3*) =   221 (*16_dd 01 *)

(* Position of "(" *)
(* LIST ItkGenFormalIdList: *)
; CONST ItkGenFormalIdListLt            (*ArgCt: 3*) =   222 (*16_de 01 *)
; CONST ItkGenFormalIdListLtTemp        (*ArgCt: 3*) =   223 (*16_df 01 *)
; CONST ItkGenFormalIdListLtPatch       (*ArgCt: 3*) =   224 (*16_e0 01 *)
; CONST ItkGenFormalIdListRt            (*ArgCt: 3*) =   225 (*16_e1 01 *)

(* Position of "(" *)
(* LIST ItkGenActualIdList: *)
; CONST ItkGenActualIdListLt            (*ArgCt: 3*) =   226 (*16_e2 01 *)
; CONST ItkGenActualIdListLtTemp        (*ArgCt: 3*) =   227 (*16_e3 01 *)
; CONST ItkGenActualIdListLtPatch       (*ArgCt: 3*) =   228 (*16_e4 01 *)
; CONST ItkGenActualIdListRt            (*ArgCt: 3*) =   229 (*16_e5 01 *)

(* Position of "(" *)
(* LIST ItkFormalsList: *)
; CONST ItkFormalsListLt                (*ArgCt: 3*) =   230 (*16_e6 01 *)
; CONST ItkFormalsListLtTemp            (*ArgCt: 3*) =   231 (*16_e7 01 *)
; CONST ItkFormalsListLtPatch           (*ArgCt: 3*) =   232 (*16_e8 01 *)
; CONST ItkFormalsListRt                (*ArgCt: 3*) =   233 (*16_e9 01 *)

(* Position of VALUE or 1st ident, if no mode. *)
(* LIST ItkFormalVALUEIdList: *)
; CONST ItkFormalVALUEIdListLt          (*ArgCt: 3*) =   234 (*16_ea 01 *)
; CONST ItkFormalVALUEIdListLtTemp      (*ArgCt: 3*) =   235 (*16_eb 01 *)
; CONST ItkFormalVALUEIdListLtPatch     (*ArgCt: 3*) =   236 (*16_ec 01 *)
; CONST ItkFormalVALUEIdListRt          (*ArgCt: 3*) =   237 (*16_ed 01 *)

(* Position of VAR. *)
(* LIST ItkFormalVARIdList: *)
; CONST ItkFormalVARIdListLt            (*ArgCt: 3*) =   238 (*16_ee 01 *)
; CONST ItkFormalVARIdListLtTemp        (*ArgCt: 3*) =   239 (*16_ef 01 *)
; CONST ItkFormalVARIdListLtPatch       (*ArgCt: 3*) =   240 (*16_f0 01 *)
; CONST ItkFormalVARIdListRt            (*ArgCt: 3*) =   241 (*16_f1 01 *)

(* Position of VAR. *)
(* LIST ItkFormalREADONLYIdList: *)
; CONST ItkFormalREADONLYIdListLt       (*ArgCt: 3*) =   242 (*16_f2 01 *)
; CONST ItkFormalREADONLYIdListLtTemp   (*ArgCt: 3*) =   243 (*16_f3 01 *)
; CONST ItkFormalREADONLYIdListLtPatch  (*ArgCt: 3*) =   244 (*16_f4 01 *)
; CONST ItkFormalREADONLYIdListRt       (*ArgCt: 3*) =   245 (*16_f5 01 *)

(* LIST ItkDeclList: *)
; CONST ItkDeclListLt                   (*ArgCt: 1*) =   246 (*16_f6 01 *)
; CONST ItkDeclListLtTemp               (*ArgCt: 1*) =   247 (*16_f7 01 *)
; CONST ItkDeclListLtPatch              (*ArgCt: 1*) =   248 (*16_f8 01 *)
; CONST ItkDeclListRt                   (*ArgCt: 1*) =   249 (*16_f9 01 *)

(* Statements *)
(* FIXED ItkBecomes: *)
; CONST ItkBecomesLt                    (*ArgCt: 2*) =   250 (*16_fa 01 *)
; CONST ItkBecomesLtTemp                (*ArgCt: 2*) =   251 (*16_fb 01 *)
; CONST ItkBecomesLtPatch               (*ArgCt: 2*) =   252 (*16_fc 01 *)
; CONST ItkBecomesRt                    (*ArgCt: 2*) =   253 (*16_fd 01 *)

(* Position of infix token, x3. *)
; CONST ItkBecomesInfix                 (*ArgCt: 2*) =   254 (*16_fe 01 *)
; CONST ItkBecomesInfixTemp             (*ArgCt: 2*) =   255 (*16_ff 01 *)
; CONST ItkBecomesInfixPatch            (*ArgCt: 2*) =   256 (*16_80 02 *)

(*

FIXED ItkModule 2 (* Name, UnitNo. *)
        Begin
. 

LIST ItkExportsList
       1 1 (* Element Count, Element No. *) 
.

LIST ItkIdPlusList
       1 1 (* Element Count, Element No. *) 
.

FIXED ItkExport 2 (* Ident, Unit No. *) . 
*)
(* End of FM3IntToks. *)
; CONST TkMaxTok                                     =   256

; END FM3IntToks
.

