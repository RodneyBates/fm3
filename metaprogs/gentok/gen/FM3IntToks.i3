
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3IntToks.gentok", with command line 
     "./gentok -T -t -c -n -o FM3IntToks.gentok". *)

INTERFACE FM3IntToks

; IMPORT IntSets

; TYPE TokTyp = INTEGER 

; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

; PROCEDURE Operands ( TokNo : TokTyp ) : TEXT 

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
; CONST LtToListSep = 4    
        (* ^Add this to list Lt tokcode to get Sep tokcode. *)
; CONST LtToListSepPatch = 6    
        (* ^Add this to list Lt tokcode to get SepPatch tokcode. *)
; CONST LtToListElem = 7    
        (* ^Add this to list Lt tokcode to get ListElem tokcode. *)
; CONST RtToLt = - 3    
        (* ^Add this to Rt tokcode to get corresponding Lt tokcode. *)
; CONST RtToTemp = - 2    
        (* ^Add this to Rt tokcode to get corresponding LtTemp tokcode. *)
; CONST RtToPatch = - 1    
        (* ^Add this to Rt tokcode to get corresponding LtPatch tokcode. *)

(* ^Be sure not to overlap the source tokens! *)
(* Being low-numbered, these will read and write either forward
   or backward, without value change.
*)
(* ABS 160: *)

; CONST TkMinTok                                     =   160

(* LONE ItkNull: *)
; CONST ItkNull                         (*ArgCt: 0*) =   160 (*16_a0 01 *)
(* LONE ItkBOF: *)
; CONST ItkBOF                          (*ArgCt: 0*) =   161 (*16_a1 01 *)
(* LONE ItkEOF: *)
; CONST ItkEOF                          (*ArgCt: 0*) =   162 (*16_a2 01 *)
(* LONE ItkLeftEnd: *)
; CONST ItkLeftEnd                      (*ArgCt: 0*) =   163 (*16_a3 01 *)
(* LONE ItkRightEnd: *)
; CONST ItkRightEnd                     (*ArgCt: 0*) =   164 (*16_a4 01 *)
(* LONE ItkLeftEndIncomplete: *)
; CONST ItkLeftEndIncomplete            (*ArgCt: 0*) =   165 (*16_a5 01 *)
(* LONE ItkRightEndIncomplete: *)
; CONST ItkRightEndIncomplete           (*ArgCt: 0*) =   166 (*16_a6 01 *)
(* IdAtom, Line, Column. *)
(* LONE ItkUnitId: *)
; CONST ItkUnitId                       (*ArgCt: 3*) =   167 (*16_a7 01 *)
(* IdAtom, Line, Column. *)
(* LONE ItkImport: *)
; CONST ItkImport                       (*ArgCt: 3*) =   168 (*16_a8 01 *)
(* (IdAtom, Line, Column) for interface, decl. *)
(* LONE ItkFromImport: *)
; CONST ItkFromImport                   (*ArgCt: 6*) =   169 (*16_a9 01 *)
(* (IdAtom, Line, Column) for interface, as. *)
(* LONE ItkImportAs: *)
; CONST ItkImportAs                     (*ArgCt: 6*) =   170 (*16_aa 01 *)
(* LONE ItkFormalsListEmpty: *)
; CONST ItkFormalsListEmpty             (*ArgCt: 0*) =   171 (*16_ab 01 *)
(* Position. *)
(* LONE ItkFormalTypeAbsent: *)
; CONST ItkFormalTypeAbsent             (*ArgCt: 2*) =   172 (*16_ac 01 *)
(* Position. *)
(* LONE ItkFormalExprAbsent: *)
; CONST ItkFormalExprAbsent             (*ArgCt: 2*) =   173 (*16_ad 01 *)
(* Position. *)
(* LONE ItkRaisesSetAbsent: *)
; CONST ItkRaisesSetAbsent              (*ArgCt: 2*) =   174 (*16_ae 01 *)
(* Position. *)
(* LONE ItkRaisesANY: *)
; CONST ItkRaisesANY                    (*ArgCt: 2*) =   175 (*16_af 01 *)
(* Position. *)
(* LONE ItkResultTypeAbsent: *)
; CONST ItkResultTypeAbsent             (*ArgCt: 2*) =   176 (*16_b0 01 *)
(* Position. *)
(* LONE ItkProcBodyAbsent: *)
; CONST ItkProcBodyAbsent               (*ArgCt: 2*) =   177 (*16_b1 01 *)
(* Scopes and declarations within. *)
(* ScopeNo. *)
(* LONE ItkScopeLt: *)
; CONST ItkScopeLt                      (*ArgCt: 1*) =   178 (*16_b2 01 *)
(* ScopeNo. *)
(* LONE ItkScopeRt: *)
; CONST ItkScopeRt                      (*ArgCt: 1*) =   179 (*16_b3 01 *)
(* Rid*, Position. *)
(* LONE ItkIdReserved: *)
; CONST ItkIdReserved                   (*ArgCt: 3*) =   180 (*16_b4 01 *)
(* IdAtom, position. Occurs before unnest. *)
(* LONE ItkIdRefAtom: *)
; CONST ItkIdRefAtom                    (*ArgCt: 3*) =   181 (*16_b5 01 *)
(* DeclNo, position. Occurs after unnest. *)
(* LONE ItkIdRefDeclNo: *)
; CONST ItkIdRefDeclNo                  (*ArgCt: 3*) =   182 (*16_b6 01 *)
(* IdAtom, position *)
(* LONE ItkDuplDeclId: *)
; CONST ItkDuplDeclId                   (*ArgCt: 3*) =   183 (*16_b7 01 *)
(* DeclKind, IdAtom, position *)
(* LONE ItkDeclId: *)
; CONST ItkDeclId                       (*ArgCt: 4*) =   184 (*16_b8 01 *)
(* DeclNo, position *)
(* LONE ItkDeclNo: *)
; CONST ItkDeclNo                       (*ArgCt: 3*) =   185 (*16_b9 01 *)
(* FIXED ItkQualId: *)
; CONST ItkQualIdLt                     (*ArgCt: 0*) =   186 (*16_ba 01 *)
; CONST ItkQualIdLtTemp                 (*ArgCt: 0*) =   187 (*16_bb 01 *)
; CONST ItkQualIdLtPatch                (*ArgCt: 0*) =   188 (*16_bc 01 *)
; CONST ItkQualIdRt                     (*ArgCt: 0*) =   189 (*16_bd 01 *)

(* Position. *)
(* FIXED ItkFuncSignature: *)
; CONST ItkFuncSignatureLt              (*ArgCt: 2*) =   190 (*16_be 01 *)
; CONST ItkFuncSignatureLtTemp          (*ArgCt: 2*) =   191 (*16_bf 01 *)
; CONST ItkFuncSignatureLtPatch         (*ArgCt: 2*) =   192 (*16_c0 01 *)
; CONST ItkFuncSignatureRt              (*ArgCt: 2*) =   193 (*16_c1 01 *)

(* Position. *)
(* FIXED ItkProcSignature: *)
; CONST ItkProcSignatureLt              (*ArgCt: 2*) =   194 (*16_c2 01 *)
; CONST ItkProcSignatureLtTemp          (*ArgCt: 2*) =   195 (*16_c3 01 *)
; CONST ItkProcSignatureLtPatch         (*ArgCt: 2*) =   196 (*16_c4 01 *)
; CONST ItkProcSignatureRt              (*ArgCt: 2*) =   197 (*16_c5 01 *)

(* Position of colon. *)
(* FIXED ItkFormalType: *)
; CONST ItkFormalTypeLt                 (*ArgCt: 2*) =   198 (*16_c6 01 *)
; CONST ItkFormalTypeLtTemp             (*ArgCt: 2*) =   199 (*16_c7 01 *)
; CONST ItkFormalTypeLtPatch            (*ArgCt: 2*) =   200 (*16_c8 01 *)
; CONST ItkFormalTypeRt                 (*ArgCt: 2*) =   201 (*16_c9 01 *)

(* Position of ":=" *)
(* FIXED ItkFormalExpr: *)
; CONST ItkFormalExprLt                 (*ArgCt: 2*) =   202 (*16_ca 01 *)
; CONST ItkFormalExprLtTemp             (*ArgCt: 2*) =   203 (*16_cb 01 *)
; CONST ItkFormalExprLtPatch            (*ArgCt: 2*) =   204 (*16_cc 01 *)
; CONST ItkFormalExprRt                 (*ArgCt: 2*) =   205 (*16_cd 01 *)

(* Position of colon. *)
(* FIXED ItkResultType: *)
; CONST ItkResultTypeLt                 (*ArgCt: 2*) =   206 (*16_ce 01 *)
; CONST ItkResultTypeLtTemp             (*ArgCt: 2*) =   207 (*16_cf 01 *)
; CONST ItkResultTypeLtPatch            (*ArgCt: 2*) =   208 (*16_d0 01 *)
; CONST ItkResultTypeRt                 (*ArgCt: 2*) =   209 (*16_d1 01 *)

(* Position of RAISES. *)
(* FIXED ItkRaisesSet: *)
; CONST ItkRaisesSetLt                  (*ArgCt: 2*) =   210 (*16_d2 01 *)
; CONST ItkRaisesSetLtTemp              (*ArgCt: 2*) =   211 (*16_d3 01 *)
; CONST ItkRaisesSetLtPatch             (*ArgCt: 2*) =   212 (*16_d4 01 *)
; CONST ItkRaisesSetRt                  (*ArgCt: 2*) =   213 (*16_d5 01 *)

(* Atom of signature. *)
(* FIXED ItkProcNoBody: *)
; CONST ItkProcNoBodyLt                 (*ArgCt: 1*) =   214 (*16_d6 01 *)
; CONST ItkProcNoBodyLtTemp             (*ArgCt: 1*) =   215 (*16_d7 01 *)
; CONST ItkProcNoBodyLtPatch            (*ArgCt: 1*) =   216 (*16_d8 01 *)
; CONST ItkProcNoBodyRt                 (*ArgCt: 1*) =   217 (*16_d9 01 *)

(* Atom of signature. *)
(* FIXED ItkProcWBody: *)
; CONST ItkProcWBodyLt                  (*ArgCt: 1*) =   218 (*16_da 01 *)
; CONST ItkProcWBodyLtTemp              (*ArgCt: 1*) =   219 (*16_db 01 *)
; CONST ItkProcWBodyLtPatch             (*ArgCt: 1*) =   220 (*16_dc 01 *)
; CONST ItkProcWBodyRt                  (*ArgCt: 1*) =   221 (*16_dd 01 *)

(* Position of equal sign. *)
(* FIXED ItkProcBody: *)
; CONST ItkProcBodyLt                   (*ArgCt: 2*) =   222 (*16_de 01 *)
; CONST ItkProcBodyLtTemp               (*ArgCt: 2*) =   223 (*16_df 01 *)
; CONST ItkProcBodyLtPatch              (*ArgCt: 2*) =   224 (*16_e0 01 *)
; CONST ItkProcBodyRt                   (*ArgCt: 2*) =   225 (*16_e1 01 *)

(* Position of "(". *)
(* FIXED ItkProcType: *)
; CONST ItkProcTypeLt                   (*ArgCt: 2*) =   226 (*16_e2 01 *)
; CONST ItkProcTypeLtTemp               (*ArgCt: 2*) =   227 (*16_e3 01 *)
; CONST ItkProcTypeLtPatch              (*ArgCt: 2*) =   228 (*16_e4 01 *)
; CONST ItkProcTypeRt                   (*ArgCt: 2*) =   229 (*16_e5 01 *)

(* Construct no., Position of LM declaration. *)
(* FIXED ItkBlock: *)
; CONST ItkBlockLt                      (*ArgCt: 3*) =   230 (*16_e6 01 *)
; CONST ItkBlockLtTemp                  (*ArgCt: 3*) =   231 (*16_e7 01 *)
; CONST ItkBlockLtPatch                 (*ArgCt: 3*) =   232 (*16_e8 01 *)
; CONST ItkBlockRt                      (*ArgCt: 3*) =   233 (*16_e9 01 *)

(* Construct no.,Position of BEGIN. *)
; CONST ItkBlockBEGIN                   (*ArgCt: 3*) =   234 (*16_ea 01 *)
; CONST ItkBlockBEGINTemp               (*ArgCt: 3*) =   235 (*16_eb 01 *)
; CONST ItkBlockBEGINPatch              (*ArgCt: 3*) =   236 (*16_ec 01 *)

(* Type decls. *)
(* LONE ItkTypeDeclId: *)
; CONST ItkTypeDeclId                   (*ArgCt: 3*) =   237 (*16_ed 01 *)
(* Position of ident (Lt) or position of semicolon (Rt). *)
(* FIXED ItkTypeDecl: *)
; CONST ItkTypeDeclLt                   (*ArgCt: 2*) =   238 (*16_ee 01 *)
; CONST ItkTypeDeclLtTemp               (*ArgCt: 2*) =   239 (*16_ef 01 *)
; CONST ItkTypeDeclLtPatch              (*ArgCt: 2*) =   240 (*16_f0 01 *)
; CONST ItkTypeDeclRt                   (*ArgCt: 2*) =   241 (*16_f1 01 *)

; CONST ItkTypeDeclEquals               (*ArgCt: 2*) =   242 (*16_f2 01 *)
; CONST ItkTypeDeclEqualsTemp           (*ArgCt: 2*) =   243 (*16_f3 01 *)
; CONST ItkTypeDeclEqualsPatch          (*ArgCt: 2*) =   244 (*16_f4 01 *)

(*
LONE
ItkVarDeclId     IDENT POS . 
ItkRecFieldId    IDENT POS . 
ItkVALUEFormalId IDENT POS . 
ItkVARFormalId   IDENT POS . 
ItkROFormalId    IDENT POS . 
*)
(* FIXED ItkREFDef: *)
; CONST ItkREFDefLt                     (*ArgCt: 2*) =   245 (*16_f5 01 *)
; CONST ItkREFDefLtTemp                 (*ArgCt: 2*) =   246 (*16_f6 01 *)
; CONST ItkREFDefLtPatch                (*ArgCt: 2*) =   247 (*16_f7 01 *)
; CONST ItkREFDefRt                     (*ArgCt: 2*) =   248 (*16_f8 01 *)

(* Record type definitions. *)
(* field count, Position of RECORD, field count. *)
(* FIXED ItkRecDef: *)
; CONST ItkRecDefLt                     (*ArgCt: 3*) =   249 (*16_f9 01 *)
; CONST ItkRecDefLtTemp                 (*ArgCt: 3*) =   250 (*16_fa 01 *)
; CONST ItkRecDefLtPatch                (*ArgCt: 3*) =   251 (*16_fb 01 *)
; CONST ItkRecDefRt                     (*ArgCt: 3*) =   252 (*16_fc 01 *)

(* Position of LM ident (Lt) or position of semicolon (Rt). *)
(* FIXED ItkRecField: *)
; CONST ItkRecFieldLt                   (*ArgCt: 2*) =   253 (*16_fd 01 *)
; CONST ItkRecFieldLtTemp               (*ArgCt: 2*) =   254 (*16_fe 01 *)
; CONST ItkRecFieldLtPatch              (*ArgCt: 2*) =   255 (*16_ff 01 *)
; CONST ItkRecFieldRt                   (*ArgCt: 2*) =   256 (*16_80 02 *)

(* Position of colon. *)
; CONST ItkRecFieldType                 (*ArgCt: 2*) =   257 (*16_81 02 *)
; CONST ItkRecFieldTypeTemp             (*ArgCt: 2*) =   258 (*16_82 02 *)
; CONST ItkRecFieldTypePatch            (*ArgCt: 2*) =   259 (*16_83 02 *)

(* Position of becomes. *)
; CONST ItkRecFieldVal                  (*ArgCt: 2*) =   260 (*16_84 02 *)
; CONST ItkRecFieldValTemp              (*ArgCt: 2*) =   261 (*16_85 02 *)
; CONST ItkRecFieldValPatch             (*ArgCt: 2*) =   262 (*16_86 02 *)

(* LIST ItkFieldDeclList: *)
; CONST ItkFieldDeclListLt              (*ArgCt: 3*) =   263 (*16_87 02 *)
; CONST ItkFieldDeclListLtTemp          (*ArgCt: 3*) =   264 (*16_88 02 *)
; CONST ItkFieldDeclListLtPatch         (*ArgCt: 3*) =   265 (*16_89 02 *)
; CONST ItkFieldDeclListRt              (*ArgCt: 3*) =   266 (*16_8a 02 *)

; CONST ItkFieldDeclListSep             (*ArgCt: 3*) =   267 (*16_8b 02 *)
; CONST ItkFieldDeclListSepTemp         (*ArgCt: 3*) =   268 (*16_8c 02 *)
; CONST ItkFieldDeclListSepPatch        (*ArgCt: 3*) =   269 (*16_8d 02 *)

; CONST ItkFieldDeclListElem            (*ArgCt: 3*) =   270 (*16_8e 02 *)

(* Variable decls. *)
(* LIST ItkVarDeclList: *)
; CONST ItkVarDeclListLt                (*ArgCt: 3*) =   271 (*16_8f 02 *)
; CONST ItkVarDeclListLtTemp            (*ArgCt: 3*) =   272 (*16_90 02 *)
; CONST ItkVarDeclListLtPatch           (*ArgCt: 3*) =   273 (*16_91 02 *)
; CONST ItkVarDeclListRt                (*ArgCt: 3*) =   274 (*16_92 02 *)

; CONST ItkVarDeclListSep               (*ArgCt: 3*) =   275 (*16_93 02 *)
; CONST ItkVarDeclListSepTemp           (*ArgCt: 3*) =   276 (*16_94 02 *)
; CONST ItkVarDeclListSepPatch          (*ArgCt: 3*) =   277 (*16_95 02 *)

; CONST ItkVarDeclListElem              (*ArgCt: 3*) =   278 (*16_96 02 *)

(* One decl, with possibly multiple idents. *)
(* Position of LM ident (Lt) or position of semicolon (Rt). *)
(* FIXED ItkFieldDecl: *)
; CONST ItkFieldDeclLt                  (*ArgCt: 2*) =   279 (*16_97 02 *)
; CONST ItkFieldDeclLtTemp              (*ArgCt: 2*) =   280 (*16_98 02 *)
; CONST ItkFieldDeclLtPatch             (*ArgCt: 2*) =   281 (*16_99 02 *)
; CONST ItkFieldDeclRt                  (*ArgCt: 2*) =   282 (*16_9a 02 *)

(* Position of colon. *)
; CONST ItkFieldDeclType                (*ArgCt: 2*) =   283 (*16_9b 02 *)
; CONST ItkFieldDeclTypeTemp            (*ArgCt: 2*) =   284 (*16_9c 02 *)
; CONST ItkFieldDeclTypePatch           (*ArgCt: 2*) =   285 (*16_9d 02 *)

(* Position of becomes. *)
; CONST ItkFieldDeclVal                 (*ArgCt: 2*) =   286 (*16_9e 02 *)
; CONST ItkFieldDeclValTemp             (*ArgCt: 2*) =   287 (*16_9f 02 *)
; CONST ItkFieldDeclValPatch            (*ArgCt: 2*) =   288 (*16_a0 02 *)

(* One decl, with possibly multiple idents. *)
(* Position of LM ident (Lt) or position of semicolon (Rt). *)
(* FIXED ItkVarDecl: *)
; CONST ItkVarDeclLt                    (*ArgCt: 2*) =   289 (*16_a1 02 *)
; CONST ItkVarDeclLtTemp                (*ArgCt: 2*) =   290 (*16_a2 02 *)
; CONST ItkVarDeclLtPatch               (*ArgCt: 2*) =   291 (*16_a3 02 *)
; CONST ItkVarDeclRt                    (*ArgCt: 2*) =   292 (*16_a4 02 *)

(* Position of colon. *)
; CONST ItkVarDeclType                  (*ArgCt: 2*) =   293 (*16_a5 02 *)
; CONST ItkVarDeclTypeTemp              (*ArgCt: 2*) =   294 (*16_a6 02 *)
; CONST ItkVarDeclTypePatch             (*ArgCt: 2*) =   295 (*16_a7 02 *)

(* Position of becomes. *)
; CONST ItkVarDeclVal                   (*ArgCt: 2*) =   296 (*16_a8 02 *)
; CONST ItkVarDeclValTemp               (*ArgCt: 2*) =   297 (*16_a9 02 *)
; CONST ItkVarDeclValPatch              (*ArgCt: 2*) =   298 (*16_aa 02 *)

(* One formal, with possibly multiple idents. *)
(* Position of LM ident (Lt) or of semicolon (Rt). *)
(* FIXED ItkVALUEFormal: *)
; CONST ItkVALUEFormalLt                (*ArgCt: 2*) =   299 (*16_ab 02 *)
; CONST ItkVALUEFormalLtTemp            (*ArgCt: 2*) =   300 (*16_ac 02 *)
; CONST ItkVALUEFormalLtPatch           (*ArgCt: 2*) =   301 (*16_ad 02 *)
; CONST ItkVALUEFormalRt                (*ArgCt: 2*) =   302 (*16_ae 02 *)

(* Position of colon. *)
; CONST ItkVALUEFormalType              (*ArgCt: 2*) =   303 (*16_af 02 *)
; CONST ItkVALUEFormalTypeTemp          (*ArgCt: 2*) =   304 (*16_b0 02 *)
; CONST ItkVALUEFormalTypePatch         (*ArgCt: 2*) =   305 (*16_b1 02 *)

(* Position of becomes. *)
; CONST ItkVALUEFormalVal               (*ArgCt: 2*) =   306 (*16_b2 02 *)
; CONST ItkVALUEFormalValTemp           (*ArgCt: 2*) =   307 (*16_b3 02 *)
; CONST ItkVALUEFormalValPatch          (*ArgCt: 2*) =   308 (*16_b4 02 *)

(* One formal, with possibly multiple idents. *)
(* Position of LM ident (Lt) or of semicolon (Rt). *)
(* FIXED ItkVARFormal: *)
; CONST ItkVARFormalLt                  (*ArgCt: 2*) =   309 (*16_b5 02 *)
; CONST ItkVARFormalLtTemp              (*ArgCt: 2*) =   310 (*16_b6 02 *)
; CONST ItkVARFormalLtPatch             (*ArgCt: 2*) =   311 (*16_b7 02 *)
; CONST ItkVARFormalRt                  (*ArgCt: 2*) =   312 (*16_b8 02 *)

(* Position of colon. *)
; CONST ItkVARFormalType                (*ArgCt: 2*) =   313 (*16_b9 02 *)
; CONST ItkVARFormalTypeTemp            (*ArgCt: 2*) =   314 (*16_ba 02 *)
; CONST ItkVARFormalTypePatch           (*ArgCt: 2*) =   315 (*16_bb 02 *)

(* Position of becomes. *)
; CONST ItkVARFormalVal                 (*ArgCt: 2*) =   316 (*16_bc 02 *)
; CONST ItkVARFormalValTemp             (*ArgCt: 2*) =   317 (*16_bd 02 *)
; CONST ItkVARFormalValPatch            (*ArgCt: 2*) =   318 (*16_be 02 *)

(* One formal, with possibly multiple idents. *)
(* Position of LM ident (Lt) or of semicolon (Rt). *)
(* FIXED ItkROFormal: *)
; CONST ItkROFormalLt                   (*ArgCt: 2*) =   319 (*16_bf 02 *)
; CONST ItkROFormalLtTemp               (*ArgCt: 2*) =   320 (*16_c0 02 *)
; CONST ItkROFormalLtPatch              (*ArgCt: 2*) =   321 (*16_c1 02 *)
; CONST ItkROFormalRt                   (*ArgCt: 2*) =   322 (*16_c2 02 *)

(* Position of colon. *)
; CONST ItkROFormalType                 (*ArgCt: 2*) =   323 (*16_c3 02 *)
; CONST ItkROFormalTypeTemp             (*ArgCt: 2*) =   324 (*16_c4 02 *)
; CONST ItkROFormalTypePatch            (*ArgCt: 2*) =   325 (*16_c5 02 *)

(* Position of becomes, *)
; CONST ItkROFormalVal                  (*ArgCt: 2*) =   326 (*16_c6 02 *)
; CONST ItkROFormalValTemp              (*ArgCt: 2*) =   327 (*16_c7 02 *)
; CONST ItkROFormalValPatch             (*ArgCt: 2*) =   328 (*16_c8 02 *)

(* GenTok implicitly gives list tokens:
     The usual patch coordinate, if it's a patch token.
     One integer operand for element count (Lt & Rt tokens)
       or, for separator tokens, number of elements to left.  
     Position.  For Lt & Rt, of what source token  depends on the list.
       For separators, of the source separator, or if none, beginning
       of element to right.
*)
(* Position is EXPORTS. *)
(* LIST ItkExportIdList: *)
; CONST ItkExportIdListLt               (*ArgCt: 3*) =   329 (*16_c9 02 *)
; CONST ItkExportIdListLtTemp           (*ArgCt: 3*) =   330 (*16_ca 02 *)
; CONST ItkExportIdListLtPatch          (*ArgCt: 3*) =   331 (*16_cb 02 *)
; CONST ItkExportIdListRt               (*ArgCt: 3*) =   332 (*16_cc 02 *)

; CONST ItkExportIdListSep              (*ArgCt: 3*) =   333 (*16_cd 02 *)
; CONST ItkExportIdListSepTemp          (*ArgCt: 3*) =   334 (*16_ce 02 *)
; CONST ItkExportIdListSepPatch         (*ArgCt: 3*) =   335 (*16_cf 02 *)

; CONST ItkExportIdListElem             (*ArgCt: 3*) =   336 (*16_d0 02 *)

(* Position is "(" *)
(* LIST ItkGenFormalIdList: *)
; CONST ItkGenFormalIdListLt            (*ArgCt: 3*) =   337 (*16_d1 02 *)
; CONST ItkGenFormalIdListLtTemp        (*ArgCt: 3*) =   338 (*16_d2 02 *)
; CONST ItkGenFormalIdListLtPatch       (*ArgCt: 3*) =   339 (*16_d3 02 *)
; CONST ItkGenFormalIdListRt            (*ArgCt: 3*) =   340 (*16_d4 02 *)

; CONST ItkGenFormalIdListSep           (*ArgCt: 3*) =   341 (*16_d5 02 *)
; CONST ItkGenFormalIdListSepTemp       (*ArgCt: 3*) =   342 (*16_d6 02 *)
; CONST ItkGenFormalIdListSepPatch      (*ArgCt: 3*) =   343 (*16_d7 02 *)

; CONST ItkGenFormalIdListElem          (*ArgCt: 3*) =   344 (*16_d8 02 *)

(* Position is "(" *)
(* LIST ItkGenActualIdList: *)
; CONST ItkGenActualIdListLt            (*ArgCt: 3*) =   345 (*16_d9 02 *)
; CONST ItkGenActualIdListLtTemp        (*ArgCt: 3*) =   346 (*16_da 02 *)
; CONST ItkGenActualIdListLtPatch       (*ArgCt: 3*) =   347 (*16_db 02 *)
; CONST ItkGenActualIdListRt            (*ArgCt: 3*) =   348 (*16_dc 02 *)

; CONST ItkGenActualIdListSep           (*ArgCt: 3*) =   349 (*16_dd 02 *)
; CONST ItkGenActualIdListSepTemp       (*ArgCt: 3*) =   350 (*16_de 02 *)
; CONST ItkGenActualIdListSepPatch      (*ArgCt: 3*) =   351 (*16_df 02 *)

; CONST ItkGenActualIdListElem          (*ArgCt: 3*) =   352 (*16_e0 02 *)

(* Position is "(" *)
(* LIST ItkVarDeclIdList: *)
; CONST ItkVarDeclIdListLt              (*ArgCt: 3*) =   353 (*16_e1 02 *)
; CONST ItkVarDeclIdListLtTemp          (*ArgCt: 3*) =   354 (*16_e2 02 *)
; CONST ItkVarDeclIdListLtPatch         (*ArgCt: 3*) =   355 (*16_e3 02 *)
; CONST ItkVarDeclIdListRt              (*ArgCt: 3*) =   356 (*16_e4 02 *)

; CONST ItkVarDeclIdListSep             (*ArgCt: 3*) =   357 (*16_e5 02 *)
; CONST ItkVarDeclIdListSepTemp         (*ArgCt: 3*) =   358 (*16_e6 02 *)
; CONST ItkVarDeclIdListSepPatch        (*ArgCt: 3*) =   359 (*16_e7 02 *)

; CONST ItkVarDeclIdListElem            (*ArgCt: 3*) =   360 (*16_e8 02 *)

(* Position is "RECORD" *)
(* LIST ItkFieldDeclIdList: *)
; CONST ItkFieldDeclIdListLt            (*ArgCt: 3*) =   361 (*16_e9 02 *)
; CONST ItkFieldDeclIdListLtTemp        (*ArgCt: 3*) =   362 (*16_ea 02 *)
; CONST ItkFieldDeclIdListLtPatch       (*ArgCt: 3*) =   363 (*16_eb 02 *)
; CONST ItkFieldDeclIdListRt            (*ArgCt: 3*) =   364 (*16_ec 02 *)

; CONST ItkFieldDeclIdListSep           (*ArgCt: 3*) =   365 (*16_ed 02 *)
; CONST ItkFieldDeclIdListSepTemp       (*ArgCt: 3*) =   366 (*16_ee 02 *)
; CONST ItkFieldDeclIdListSepPatch      (*ArgCt: 3*) =   367 (*16_ef 02 *)

; CONST ItkFieldDeclIdListElem          (*ArgCt: 3*) =   368 (*16_f0 02 *)

(* Position is "(" *)
(* LIST ItkFormalsList: *)
; CONST ItkFormalsListLt                (*ArgCt: 3*) =   369 (*16_f1 02 *)
; CONST ItkFormalsListLtTemp            (*ArgCt: 3*) =   370 (*16_f2 02 *)
; CONST ItkFormalsListLtPatch           (*ArgCt: 3*) =   371 (*16_f3 02 *)
; CONST ItkFormalsListRt                (*ArgCt: 3*) =   372 (*16_f4 02 *)

; CONST ItkFormalsListSep               (*ArgCt: 3*) =   373 (*16_f5 02 *)
; CONST ItkFormalsListSepTemp           (*ArgCt: 3*) =   374 (*16_f6 02 *)
; CONST ItkFormalsListSepPatch          (*ArgCt: 3*) =   375 (*16_f7 02 *)

; CONST ItkFormalsListElem              (*ArgCt: 3*) =   376 (*16_f8 02 *)

(* Position is VALUE or 1st ident, if no mode. *)
(* LIST ItkVALUEFormalIdList: *)
; CONST ItkVALUEFormalIdListLt          (*ArgCt: 3*) =   377 (*16_f9 02 *)
; CONST ItkVALUEFormalIdListLtTemp      (*ArgCt: 3*) =   378 (*16_fa 02 *)
; CONST ItkVALUEFormalIdListLtPatch     (*ArgCt: 3*) =   379 (*16_fb 02 *)
; CONST ItkVALUEFormalIdListRt          (*ArgCt: 3*) =   380 (*16_fc 02 *)

; CONST ItkVALUEFormalIdListSep         (*ArgCt: 3*) =   381 (*16_fd 02 *)
; CONST ItkVALUEFormalIdListSepTemp     (*ArgCt: 3*) =   382 (*16_fe 02 *)
; CONST ItkVALUEFormalIdListSepPatch    (*ArgCt: 3*) =   383 (*16_ff 02 *)

; CONST ItkVALUEFormalIdListElem        (*ArgCt: 3*) =   384 (*16_80 03 *)

(* Position is VAR. *)
(* LIST ItkVARFormalIdList: *)
; CONST ItkVARFormalIdListLt            (*ArgCt: 3*) =   385 (*16_81 03 *)
; CONST ItkVARFormalIdListLtTemp        (*ArgCt: 3*) =   386 (*16_82 03 *)
; CONST ItkVARFormalIdListLtPatch       (*ArgCt: 3*) =   387 (*16_83 03 *)
; CONST ItkVARFormalIdListRt            (*ArgCt: 3*) =   388 (*16_84 03 *)

; CONST ItkVARFormalIdListSep           (*ArgCt: 3*) =   389 (*16_85 03 *)
; CONST ItkVARFormalIdListSepTemp       (*ArgCt: 3*) =   390 (*16_86 03 *)
; CONST ItkVARFormalIdListSepPatch      (*ArgCt: 3*) =   391 (*16_87 03 *)

; CONST ItkVARFormalIdListElem          (*ArgCt: 3*) =   392 (*16_88 03 *)

(* Position is READONLY. *)
(* LIST ItkROFormalIdList: *)
; CONST ItkROFormalIdListLt             (*ArgCt: 3*) =   393 (*16_89 03 *)
; CONST ItkROFormalIdListLtTemp         (*ArgCt: 3*) =   394 (*16_8a 03 *)
; CONST ItkROFormalIdListLtPatch        (*ArgCt: 3*) =   395 (*16_8b 03 *)
; CONST ItkROFormalIdListRt             (*ArgCt: 3*) =   396 (*16_8c 03 *)

; CONST ItkROFormalIdListSep            (*ArgCt: 3*) =   397 (*16_8d 03 *)
; CONST ItkROFormalIdListSepTemp        (*ArgCt: 3*) =   398 (*16_8e 03 *)
; CONST ItkROFormalIdListSepPatch       (*ArgCt: 3*) =   399 (*16_8f 03 *)

; CONST ItkROFormalIdListElem           (*ArgCt: 3*) =   400 (*16_90 03 *)

(* LIST ItkDeclList: *)
; CONST ItkDeclListLt                   (*ArgCt: 3*) =   401 (*16_91 03 *)
; CONST ItkDeclListLtTemp               (*ArgCt: 3*) =   402 (*16_92 03 *)
; CONST ItkDeclListLtPatch              (*ArgCt: 3*) =   403 (*16_93 03 *)
; CONST ItkDeclListRt                   (*ArgCt: 3*) =   404 (*16_94 03 *)

; CONST ItkDeclListSep                  (*ArgCt: 3*) =   405 (*16_95 03 *)
; CONST ItkDeclListSepTemp              (*ArgCt: 3*) =   406 (*16_96 03 *)
; CONST ItkDeclListSepPatch             (*ArgCt: 3*) =   407 (*16_97 03 *)

; CONST ItkDeclListElem                 (*ArgCt: 3*) =   408 (*16_98 03 *)

(* Statements *)
(* FIXED ItkBecomes: *)
; CONST ItkBecomesLt                    (*ArgCt: 2*) =   409 (*16_99 03 *)
; CONST ItkBecomesLtTemp                (*ArgCt: 2*) =   410 (*16_9a 03 *)
; CONST ItkBecomesLtPatch               (*ArgCt: 2*) =   411 (*16_9b 03 *)
; CONST ItkBecomesRt                    (*ArgCt: 2*) =   412 (*16_9c 03 *)

(* Position of infix token, x3. *)
; CONST ItkBecomesInfix                 (*ArgCt: 2*) =   413 (*16_9d 03 *)
; CONST ItkBecomesInfixTemp             (*ArgCt: 2*) =   414 (*16_9e 03 *)
; CONST ItkBecomesInfixPatch            (*ArgCt: 2*) =   415 (*16_9f 03 *)

(*

FIXED ItkModule IDENT INT 2 (* IdAtom , UnitNo. *)
        Begin
. 

LIST ItkExportsList
       (* Element Count, Element No. *) 
.

LIST ItkIdPlusList
       (* Element Count, Element No. *) 
.

FIXED ItkExport (* Ident, Unit No. *) . 
*)
(* End of FM3IntToks. *)
; CONST TkMaxTok                                     =   415

; END FM3IntToks
.

