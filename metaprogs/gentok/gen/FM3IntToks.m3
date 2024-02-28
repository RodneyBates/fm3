
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3IntToks.gentok", with command line 
     "./gentok -T -t -c -n -o FM3IntToks.gentok". *)

MODULE FM3IntToks

; IMPORT IntSets

(*EXPORTED*)
; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 160 => RETURN "ItkNull"
      | 161 => RETURN "ItkBOF"
      | 162 => RETURN "ItkEOF"
      | 163 => RETURN "ItkLeftEnd"
      | 164 => RETURN "ItkRightEnd"
      | 165 => RETURN "ItkLeftEndIncomplete"
      | 166 => RETURN "ItkRightEndIncomplete"
      | 167 => RETURN "ItkSkipLt"
      | 168 => RETURN "ItkSkipLtTemp"
      | 169 => RETURN "ItkSkipLtPatch"
      | 170 => RETURN "ItkSkipRt"
      | 171 => RETURN "ItkInterfaceLt"
      | 172 => RETURN "ItkInterfaceLtTemp"
      | 173 => RETURN "ItkInterfaceLtPatch"
      | 174 => RETURN "ItkInterfaceRt"
      | 175 => RETURN "ItkModuleLt"
      | 176 => RETURN "ItkModuleLtTemp"
      | 177 => RETURN "ItkModuleLtPatch"
      | 178 => RETURN "ItkModuleRt"
      | 179 => RETURN "ItkModuleBegin"
      | 180 => RETURN "ItkModuleBeginTemp"
      | 181 => RETURN "ItkModuleBeginPatch"
      | 182 => RETURN "ItkUnitId"
      | 183 => RETURN "ItkImport"
      | 184 => RETURN "ItkFromImport"
      | 185 => RETURN "ItkImportAs"
      | 186 => RETURN "ItkFormalsListEmpty"
      | 187 => RETURN "ItkFormalTypeAbsent"
      | 188 => RETURN "ItkFormalExprAbsent"
      | 189 => RETURN "ItkRaisesSetAbsent"
      | 190 => RETURN "ItkRaisesANY"
      | 191 => RETURN "ItkResultTypeAbsent"
      | 192 => RETURN "ItkProcBodyAbsent"
      | 193 => RETURN "ItkScopeEmpty"
      | 194 => RETURN "ItkDeclScopeLt"
      | 195 => RETURN "ItkDeclScopeRt"
      | 196 => RETURN "ItkLookupScopeLt"
      | 197 => RETURN "ItkLookupScopeRt"
      | 198 => RETURN "ItkReservedId"
      | 199 => RETURN "ItkDuplDeclId"
      | 200 => RETURN "ItkDeclId"
      | 201 => RETURN "ItkDeclNo"
      | 202 => RETURN "ItkIdRefAtom"
      | 203 => RETURN "ItkIdRefDeclNo"
      | 204 => RETURN "ItkQualIdAtoms"
      | 205 => RETURN "ItkQualIdDeclNoAtom"
      | 206 => RETURN "ItkQualIdDeclNos"
      | 207 => RETURN "ItkInvalidRef"
      | 208 => RETURN "ItkFuncSignatureLt"
      | 209 => RETURN "ItkFuncSignatureLtTemp"
      | 210 => RETURN "ItkFuncSignatureLtPatch"
      | 211 => RETURN "ItkFuncSignatureRt"
      | 212 => RETURN "ItkProcSignatureLt"
      | 213 => RETURN "ItkProcSignatureLtTemp"
      | 214 => RETURN "ItkProcSignatureLtPatch"
      | 215 => RETURN "ItkProcSignatureRt"
      | 216 => RETURN "ItkFormalTypeLt"
      | 217 => RETURN "ItkFormalTypeLtTemp"
      | 218 => RETURN "ItkFormalTypeLtPatch"
      | 219 => RETURN "ItkFormalTypeRt"
      | 220 => RETURN "ItkFormalExprLt"
      | 221 => RETURN "ItkFormalExprLtTemp"
      | 222 => RETURN "ItkFormalExprLtPatch"
      | 223 => RETURN "ItkFormalExprRt"
      | 224 => RETURN "ItkResultTypeLt"
      | 225 => RETURN "ItkResultTypeLtTemp"
      | 226 => RETURN "ItkResultTypeLtPatch"
      | 227 => RETURN "ItkResultTypeRt"
      | 228 => RETURN "ItkRaisesSetLt"
      | 229 => RETURN "ItkRaisesSetLtTemp"
      | 230 => RETURN "ItkRaisesSetLtPatch"
      | 231 => RETURN "ItkRaisesSetRt"
      | 232 => RETURN "ItkProcNoBodyLt"
      | 233 => RETURN "ItkProcNoBodyLtTemp"
      | 234 => RETURN "ItkProcNoBodyLtPatch"
      | 235 => RETURN "ItkProcNoBodyRt"
      | 236 => RETURN "ItkProcWBodyLt"
      | 237 => RETURN "ItkProcWBodyLtTemp"
      | 238 => RETURN "ItkProcWBodyLtPatch"
      | 239 => RETURN "ItkProcWBodyRt"
      | 240 => RETURN "ItkProcBodyLt"
      | 241 => RETURN "ItkProcBodyLtTemp"
      | 242 => RETURN "ItkProcBodyLtPatch"
      | 243 => RETURN "ItkProcBodyRt"
      | 244 => RETURN "ItkProcTypeLt"
      | 245 => RETURN "ItkProcTypeLtTemp"
      | 246 => RETURN "ItkProcTypeLtPatch"
      | 247 => RETURN "ItkProcTypeRt"
      | 248 => RETURN "ItkBlockLt"
      | 249 => RETURN "ItkBlockLtTemp"
      | 250 => RETURN "ItkBlockLtPatch"
      | 251 => RETURN "ItkBlockRt"
      | 252 => RETURN "ItkBlockBEGIN"
      | 253 => RETURN "ItkBlockBEGINTemp"
      | 254 => RETURN "ItkBlockBEGINPatch"
      | 255 => RETURN "ItkConstDeclLt"
      | 256 => RETURN "ItkConstDeclLtTemp"
      | 257 => RETURN "ItkConstDeclLtPatch"
      | 258 => RETURN "ItkConstDeclRt"
      | 259 => RETURN "ItkConstDeclColon"
      | 260 => RETURN "ItkConstDeclColonTemp"
      | 261 => RETURN "ItkConstDeclColonPatch"
      | 262 => RETURN "ItkConstDeclEquals"
      | 263 => RETURN "ItkConstDeclEqualsTemp"
      | 264 => RETURN "ItkConstDeclEqualsPatch"
      | 265 => RETURN "ItkTypeDeclLt"
      | 266 => RETURN "ItkTypeDeclLtTemp"
      | 267 => RETURN "ItkTypeDeclLtPatch"
      | 268 => RETURN "ItkTypeDeclRt"
      | 269 => RETURN "ItkTypeDeclEquals"
      | 270 => RETURN "ItkTypeDeclEqualsTemp"
      | 271 => RETURN "ItkTypeDeclEqualsPatch"
      | 272 => RETURN "ItkSubtypeDeclLt"
      | 273 => RETURN "ItkSubtypeDeclLtTemp"
      | 274 => RETURN "ItkSubtypeDeclLtPatch"
      | 275 => RETURN "ItkSubtypeDeclRt"
      | 276 => RETURN "ItkSubtypeDeclSubtype"
      | 277 => RETURN "ItkSubtypeDeclSubtypeTemp"
      | 278 => RETURN "ItkSubtypeDeclSubtypePatch"
      | 279 => RETURN "ItkFullRevealLt"
      | 280 => RETURN "ItkFullRevealLtTemp"
      | 281 => RETURN "ItkFullRevealLtPatch"
      | 282 => RETURN "ItkFullRevealRt"
      | 283 => RETURN "ItkFullRevealEquals"
      | 284 => RETURN "ItkFullRevealEqualsTemp"
      | 285 => RETURN "ItkFullRevealEqualsPatch"
      | 286 => RETURN "ItkPartialRevealLt"
      | 287 => RETURN "ItkPartialRevealLtTemp"
      | 288 => RETURN "ItkPartialRevealLtPatch"
      | 289 => RETURN "ItkPartialRevealRt"
      | 290 => RETURN "ItkPartialRevealSubtype"
      | 291 => RETURN "ItkPartialRevealSubtypeTemp"
      | 292 => RETURN "ItkPartialRevealSubtypePatch"
      | 293 => RETURN "ItkInvalidType"
      | 294 => RETURN "ItkREFDefLt"
      | 295 => RETURN "ItkREFDefLtTemp"
      | 296 => RETURN "ItkREFDefLtPatch"
      | 297 => RETURN "ItkREFDefRt"
      | 298 => RETURN "ItkRecDefLt"
      | 299 => RETURN "ItkRecDefLtTemp"
      | 300 => RETURN "ItkRecDefLtPatch"
      | 301 => RETURN "ItkRecDefRt"
      | 302 => RETURN "ItkRecFieldLt"
      | 303 => RETURN "ItkRecFieldLtTemp"
      | 304 => RETURN "ItkRecFieldLtPatch"
      | 305 => RETURN "ItkRecFieldRt"
      | 306 => RETURN "ItkRecFieldType"
      | 307 => RETURN "ItkRecFieldTypeTemp"
      | 308 => RETURN "ItkRecFieldTypePatch"
      | 309 => RETURN "ItkRecFieldVal"
      | 310 => RETURN "ItkRecFieldValTemp"
      | 311 => RETURN "ItkRecFieldValPatch"
      | 312 => RETURN "ItkFieldDeclListLt"
      | 313 => RETURN "ItkFieldDeclListLtTemp"
      | 314 => RETURN "ItkFieldDeclListLtPatch"
      | 315 => RETURN "ItkFieldDeclListRt"
      | 316 => RETURN "ItkFieldDeclListSep"
      | 317 => RETURN "ItkFieldDeclListSepTemp"
      | 318 => RETURN "ItkFieldDeclListSepPatch"
      | 319 => RETURN "ItkFieldDeclListElem"
      | 320 => RETURN "ItkVarDeclListLt"
      | 321 => RETURN "ItkVarDeclListLtTemp"
      | 322 => RETURN "ItkVarDeclListLtPatch"
      | 323 => RETURN "ItkVarDeclListRt"
      | 324 => RETURN "ItkVarDeclListSep"
      | 325 => RETURN "ItkVarDeclListSepTemp"
      | 326 => RETURN "ItkVarDeclListSepPatch"
      | 327 => RETURN "ItkVarDeclListElem"
      | 328 => RETURN "ItkTypeDeclListLt"
      | 329 => RETURN "ItkTypeDeclListLtTemp"
      | 330 => RETURN "ItkTypeDeclListLtPatch"
      | 331 => RETURN "ItkTypeDeclListRt"
      | 332 => RETURN "ItkTypeDeclListSep"
      | 333 => RETURN "ItkTypeDeclListSepTemp"
      | 334 => RETURN "ItkTypeDeclListSepPatch"
      | 335 => RETURN "ItkTypeDeclListElem"
      | 336 => RETURN "ItkFieldDeclLt"
      | 337 => RETURN "ItkFieldDeclLtTemp"
      | 338 => RETURN "ItkFieldDeclLtPatch"
      | 339 => RETURN "ItkFieldDeclRt"
      | 340 => RETURN "ItkFieldDeclType"
      | 341 => RETURN "ItkFieldDeclTypeTemp"
      | 342 => RETURN "ItkFieldDeclTypePatch"
      | 343 => RETURN "ItkFieldDeclVal"
      | 344 => RETURN "ItkFieldDeclValTemp"
      | 345 => RETURN "ItkFieldDeclValPatch"
      | 346 => RETURN "ItkVarDeclLt"
      | 347 => RETURN "ItkVarDeclLtTemp"
      | 348 => RETURN "ItkVarDeclLtPatch"
      | 349 => RETURN "ItkVarDeclRt"
      | 350 => RETURN "ItkVarDeclType"
      | 351 => RETURN "ItkVarDeclTypeTemp"
      | 352 => RETURN "ItkVarDeclTypePatch"
      | 353 => RETURN "ItkVarDeclVal"
      | 354 => RETURN "ItkVarDeclValTemp"
      | 355 => RETURN "ItkVarDeclValPatch"
      | 356 => RETURN "ItkVALUEFormalLt"
      | 357 => RETURN "ItkVALUEFormalLtTemp"
      | 358 => RETURN "ItkVALUEFormalLtPatch"
      | 359 => RETURN "ItkVALUEFormalRt"
      | 360 => RETURN "ItkVALUEFormalType"
      | 361 => RETURN "ItkVALUEFormalTypeTemp"
      | 362 => RETURN "ItkVALUEFormalTypePatch"
      | 363 => RETURN "ItkVALUEFormalVal"
      | 364 => RETURN "ItkVALUEFormalValTemp"
      | 365 => RETURN "ItkVALUEFormalValPatch"
      | 366 => RETURN "ItkVARFormalLt"
      | 367 => RETURN "ItkVARFormalLtTemp"
      | 368 => RETURN "ItkVARFormalLtPatch"
      | 369 => RETURN "ItkVARFormalRt"
      | 370 => RETURN "ItkVARFormalType"
      | 371 => RETURN "ItkVARFormalTypeTemp"
      | 372 => RETURN "ItkVARFormalTypePatch"
      | 373 => RETURN "ItkVARFormalVal"
      | 374 => RETURN "ItkVARFormalValTemp"
      | 375 => RETURN "ItkVARFormalValPatch"
      | 376 => RETURN "ItkROFormalLt"
      | 377 => RETURN "ItkROFormalLtTemp"
      | 378 => RETURN "ItkROFormalLtPatch"
      | 379 => RETURN "ItkROFormalRt"
      | 380 => RETURN "ItkROFormalType"
      | 381 => RETURN "ItkROFormalTypeTemp"
      | 382 => RETURN "ItkROFormalTypePatch"
      | 383 => RETURN "ItkROFormalVal"
      | 384 => RETURN "ItkROFormalValTemp"
      | 385 => RETURN "ItkROFormalValPatch"
      | 386 => RETURN "ItkExportIdListLt"
      | 387 => RETURN "ItkExportIdListLtTemp"
      | 388 => RETURN "ItkExportIdListLtPatch"
      | 389 => RETURN "ItkExportIdListRt"
      | 390 => RETURN "ItkExportIdListSep"
      | 391 => RETURN "ItkExportIdListSepTemp"
      | 392 => RETURN "ItkExportIdListSepPatch"
      | 393 => RETURN "ItkExportIdListElem"
      | 394 => RETURN "ItkGenFormalIdListLt"
      | 395 => RETURN "ItkGenFormalIdListLtTemp"
      | 396 => RETURN "ItkGenFormalIdListLtPatch"
      | 397 => RETURN "ItkGenFormalIdListRt"
      | 398 => RETURN "ItkGenFormalIdListSep"
      | 399 => RETURN "ItkGenFormalIdListSepTemp"
      | 400 => RETURN "ItkGenFormalIdListSepPatch"
      | 401 => RETURN "ItkGenFormalIdListElem"
      | 402 => RETURN "ItkGenActualIdListLt"
      | 403 => RETURN "ItkGenActualIdListLtTemp"
      | 404 => RETURN "ItkGenActualIdListLtPatch"
      | 405 => RETURN "ItkGenActualIdListRt"
      | 406 => RETURN "ItkGenActualIdListSep"
      | 407 => RETURN "ItkGenActualIdListSepTemp"
      | 408 => RETURN "ItkGenActualIdListSepPatch"
      | 409 => RETURN "ItkGenActualIdListElem"
      | 410 => RETURN "ItkVarDeclIdListLt"
      | 411 => RETURN "ItkVarDeclIdListLtTemp"
      | 412 => RETURN "ItkVarDeclIdListLtPatch"
      | 413 => RETURN "ItkVarDeclIdListRt"
      | 414 => RETURN "ItkVarDeclIdListSep"
      | 415 => RETURN "ItkVarDeclIdListSepTemp"
      | 416 => RETURN "ItkVarDeclIdListSepPatch"
      | 417 => RETURN "ItkVarDeclIdListElem"
      | 418 => RETURN "ItkTypeDeclIdListLt"
      | 419 => RETURN "ItkTypeDeclIdListLtTemp"
      | 420 => RETURN "ItkTypeDeclIdListLtPatch"
      | 421 => RETURN "ItkTypeDeclIdListRt"
      | 422 => RETURN "ItkTypeDeclIdListSep"
      | 423 => RETURN "ItkTypeDeclIdListSepTemp"
      | 424 => RETURN "ItkTypeDeclIdListSepPatch"
      | 425 => RETURN "ItkTypeDeclIdListElem"
      | 426 => RETURN "ItkRevealIdListLt"
      | 427 => RETURN "ItkRevealIdListLtTemp"
      | 428 => RETURN "ItkRevealIdListLtPatch"
      | 429 => RETURN "ItkRevealIdListRt"
      | 430 => RETURN "ItkRevealIdListSep"
      | 431 => RETURN "ItkRevealIdListSepTemp"
      | 432 => RETURN "ItkRevealIdListSepPatch"
      | 433 => RETURN "ItkRevealIdListElem"
      | 434 => RETURN "ItkFieldDeclIdListLt"
      | 435 => RETURN "ItkFieldDeclIdListLtTemp"
      | 436 => RETURN "ItkFieldDeclIdListLtPatch"
      | 437 => RETURN "ItkFieldDeclIdListRt"
      | 438 => RETURN "ItkFieldDeclIdListSep"
      | 439 => RETURN "ItkFieldDeclIdListSepTemp"
      | 440 => RETURN "ItkFieldDeclIdListSepPatch"
      | 441 => RETURN "ItkFieldDeclIdListElem"
      | 442 => RETURN "ItkFormalsListLt"
      | 443 => RETURN "ItkFormalsListLtTemp"
      | 444 => RETURN "ItkFormalsListLtPatch"
      | 445 => RETURN "ItkFormalsListRt"
      | 446 => RETURN "ItkFormalsListSep"
      | 447 => RETURN "ItkFormalsListSepTemp"
      | 448 => RETURN "ItkFormalsListSepPatch"
      | 449 => RETURN "ItkFormalsListElem"
      | 450 => RETURN "ItkVALUEFormalIdListLt"
      | 451 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 452 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 453 => RETURN "ItkVALUEFormalIdListRt"
      | 454 => RETURN "ItkVALUEFormalIdListSep"
      | 455 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 456 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 457 => RETURN "ItkVALUEFormalIdListElem"
      | 458 => RETURN "ItkVARFormalIdListLt"
      | 459 => RETURN "ItkVARFormalIdListLtTemp"
      | 460 => RETURN "ItkVARFormalIdListLtPatch"
      | 461 => RETURN "ItkVARFormalIdListRt"
      | 462 => RETURN "ItkVARFormalIdListSep"
      | 463 => RETURN "ItkVARFormalIdListSepTemp"
      | 464 => RETURN "ItkVARFormalIdListSepPatch"
      | 465 => RETURN "ItkVARFormalIdListElem"
      | 466 => RETURN "ItkROFormalIdListLt"
      | 467 => RETURN "ItkROFormalIdListLtTemp"
      | 468 => RETURN "ItkROFormalIdListLtPatch"
      | 469 => RETURN "ItkROFormalIdListRt"
      | 470 => RETURN "ItkROFormalIdListSep"
      | 471 => RETURN "ItkROFormalIdListSepTemp"
      | 472 => RETURN "ItkROFormalIdListSepPatch"
      | 473 => RETURN "ItkROFormalIdListElem"
      | 474 => RETURN "ItkBlockDeclListLt"
      | 475 => RETURN "ItkBlockDeclListLtTemp"
      | 476 => RETURN "ItkBlockDeclListLtPatch"
      | 477 => RETURN "ItkBlockDeclListRt"
      | 478 => RETURN "ItkBlockDeclListSep"
      | 479 => RETURN "ItkBlockDeclListSepTemp"
      | 480 => RETURN "ItkBlockDeclListSepPatch"
      | 481 => RETURN "ItkBlockDeclListElem"
      | 482 => RETURN "ItkStmtListLt"
      | 483 => RETURN "ItkStmtListLtTemp"
      | 484 => RETURN "ItkStmtListLtPatch"
      | 485 => RETURN "ItkStmtListRt"
      | 486 => RETURN "ItkStmtListSep"
      | 487 => RETURN "ItkStmtListSepTemp"
      | 488 => RETURN "ItkStmtListSepPatch"
      | 489 => RETURN "ItkStmtListElem"
      | 490 => RETURN "ItkBecomesLt"
      | 491 => RETURN "ItkBecomesLtTemp"
      | 492 => RETURN "ItkBecomesLtPatch"
      | 493 => RETURN "ItkBecomesRt"
      | 494 => RETURN "ItkBecomesInfix"
      | 495 => RETURN "ItkBecomesInfixTemp"
      | 496 => RETURN "ItkBecomesInfixPatch"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

(*EXPORTED*)
; PROCEDURE Operands ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 160 => RETURN ""
      | 161 => RETURN ""
      | 162 => RETURN ""
      | 163 => RETURN ""
      | 164 => RETURN ""
      | 165 => RETURN ""
      | 166 => RETURN ""
      | 167 => RETURN "_L"
      | 168 => RETURN "_L"
      | 169 => RETURN "_C_L"
      | 170 => RETURN "_L"
      | 171 => RETURN "_L_P"
      | 172 => RETURN "_L_P"
      | 173 => RETURN "_C_L_P"
      | 174 => RETURN "_L_P"
      | 175 => RETURN "_L_P"
      | 176 => RETURN "_L_P"
      | 177 => RETURN "_C_L_P"
      | 178 => RETURN "_L_P"
      | 179 => RETURN "_L_P"
      | 180 => RETURN "_L_P"
      | 181 => RETURN "_C_L_P"
      | 182 => RETURN "_I_P"
      | 183 => RETURN "_I_P"
      | 184 => RETURN "_I_P_I_P"
      | 185 => RETURN "_I_P_I_P"
      | 186 => RETURN ""
      | 187 => RETURN "_P"
      | 188 => RETURN "_P"
      | 189 => RETURN "_P"
      | 190 => RETURN "_P"
      | 191 => RETURN "_P"
      | 192 => RETURN "_P"
      | 193 => RETURN "_L"
      | 194 => RETURN "_L"
      | 195 => RETURN "_L"
      | 196 => RETURN "_L"
      | 197 => RETURN "_L"
      | 198 => RETURN "_I_P"
      | 199 => RETURN "_I_P"
      | 200 => RETURN "_L_I_P"
      | 201 => RETURN "_D_P"
      | 202 => RETURN "_I_P"
      | 203 => RETURN "_D_P"
      | 204 => RETURN "_I_I_P_P"
      | 205 => RETURN "_D_I_P_P"
      | 206 => RETURN "_D_D_P_P"
      | 207 => RETURN "_P"
      | 208 => RETURN "_P"
      | 209 => RETURN "_P"
      | 210 => RETURN "_C_P"
      | 211 => RETURN "_P"
      | 212 => RETURN "_P"
      | 213 => RETURN "_P"
      | 214 => RETURN "_C_P"
      | 215 => RETURN "_P"
      | 216 => RETURN "_P"
      | 217 => RETURN "_P"
      | 218 => RETURN "_C_P"
      | 219 => RETURN "_P"
      | 220 => RETURN "_P"
      | 221 => RETURN "_P"
      | 222 => RETURN "_C_P"
      | 223 => RETURN "_P"
      | 224 => RETURN "_P"
      | 225 => RETURN "_P"
      | 226 => RETURN "_C_P"
      | 227 => RETURN "_P"
      | 228 => RETURN "_P"
      | 229 => RETURN "_P"
      | 230 => RETURN "_C_P"
      | 231 => RETURN "_P"
      | 232 => RETURN "_L"
      | 233 => RETURN "_L"
      | 234 => RETURN "_C_L"
      | 235 => RETURN "_L"
      | 236 => RETURN "_L"
      | 237 => RETURN "_L"
      | 238 => RETURN "_C_L"
      | 239 => RETURN "_L"
      | 240 => RETURN "_P"
      | 241 => RETURN "_P"
      | 242 => RETURN "_C_P"
      | 243 => RETURN "_P"
      | 244 => RETURN "_P"
      | 245 => RETURN "_P"
      | 246 => RETURN "_C_P"
      | 247 => RETURN "_P"
      | 248 => RETURN "_L_P"
      | 249 => RETURN "_L_P"
      | 250 => RETURN "_C_L_P"
      | 251 => RETURN "_L_P"
      | 252 => RETURN "_L_P"
      | 253 => RETURN "_L_P"
      | 254 => RETURN "_C_L_P"
      | 255 => RETURN "_P"
      | 256 => RETURN "_P"
      | 257 => RETURN "_C_P"
      | 258 => RETURN "_P"
      | 259 => RETURN "_P_B"
      | 260 => RETURN "_P_B"
      | 261 => RETURN "_C_P_B"
      | 262 => RETURN "_P"
      | 263 => RETURN "_P"
      | 264 => RETURN "_C_P"
      | 265 => RETURN "_P"
      | 266 => RETURN "_P"
      | 267 => RETURN "_C_P"
      | 268 => RETURN "_P"
      | 269 => RETURN "_P"
      | 270 => RETURN "_P"
      | 271 => RETURN "_C_P"
      | 272 => RETURN "_P"
      | 273 => RETURN "_P"
      | 274 => RETURN "_C_P"
      | 275 => RETURN "_P"
      | 276 => RETURN "_P"
      | 277 => RETURN "_P"
      | 278 => RETURN "_C_P"
      | 279 => RETURN "_P"
      | 280 => RETURN "_P"
      | 281 => RETURN "_C_P"
      | 282 => RETURN "_P"
      | 283 => RETURN "_P"
      | 284 => RETURN "_P"
      | 285 => RETURN "_C_P"
      | 286 => RETURN "_P"
      | 287 => RETURN "_P"
      | 288 => RETURN "_C_P"
      | 289 => RETURN "_P"
      | 290 => RETURN "_P"
      | 291 => RETURN "_P"
      | 292 => RETURN "_C_P"
      | 293 => RETURN "_P"
      | 294 => RETURN "_P"
      | 295 => RETURN "_P"
      | 296 => RETURN "_C_P"
      | 297 => RETURN "_P"
      | 298 => RETURN "_L_P"
      | 299 => RETURN "_L_P"
      | 300 => RETURN "_C_L_P"
      | 301 => RETURN "_L_P"
      | 302 => RETURN "_P"
      | 303 => RETURN "_P"
      | 304 => RETURN "_C_P"
      | 305 => RETURN "_P"
      | 306 => RETURN "_P"
      | 307 => RETURN "_P"
      | 308 => RETURN "_C_P"
      | 309 => RETURN "_P"
      | 310 => RETURN "_P"
      | 311 => RETURN "_C_P"
      | 312 => RETURN "_L_P"
      | 313 => RETURN "_L_P"
      | 314 => RETURN "_C_L_P"
      | 315 => RETURN "_L_P"
      | 316 => RETURN "_L_P"
      | 317 => RETURN "_L_P"
      | 318 => RETURN "_C_L_P"
      | 319 => RETURN "_I_P"
      | 320 => RETURN "_L_P"
      | 321 => RETURN "_L_P"
      | 322 => RETURN "_C_L_P"
      | 323 => RETURN "_L_P"
      | 324 => RETURN "_L_P"
      | 325 => RETURN "_L_P"
      | 326 => RETURN "_C_L_P"
      | 327 => RETURN "_I_P"
      | 328 => RETURN "_L_P"
      | 329 => RETURN "_L_P"
      | 330 => RETURN "_C_L_P"
      | 331 => RETURN "_L_P"
      | 332 => RETURN "_L_P"
      | 333 => RETURN "_L_P"
      | 334 => RETURN "_C_L_P"
      | 335 => RETURN "_I_P"
      | 336 => RETURN "_P"
      | 337 => RETURN "_P"
      | 338 => RETURN "_C_P"
      | 339 => RETURN "_P"
      | 340 => RETURN "_P"
      | 341 => RETURN "_P"
      | 342 => RETURN "_C_P"
      | 343 => RETURN "_P"
      | 344 => RETURN "_P"
      | 345 => RETURN "_C_P"
      | 346 => RETURN "_P"
      | 347 => RETURN "_P"
      | 348 => RETURN "_C_P"
      | 349 => RETURN "_P"
      | 350 => RETURN "_P"
      | 351 => RETURN "_P"
      | 352 => RETURN "_C_P"
      | 353 => RETURN "_P"
      | 354 => RETURN "_P"
      | 355 => RETURN "_C_P"
      | 356 => RETURN "_P"
      | 357 => RETURN "_P"
      | 358 => RETURN "_C_P"
      | 359 => RETURN "_P"
      | 360 => RETURN "_P"
      | 361 => RETURN "_P"
      | 362 => RETURN "_C_P"
      | 363 => RETURN "_P"
      | 364 => RETURN "_P"
      | 365 => RETURN "_C_P"
      | 366 => RETURN "_P"
      | 367 => RETURN "_P"
      | 368 => RETURN "_C_P"
      | 369 => RETURN "_P"
      | 370 => RETURN "_P"
      | 371 => RETURN "_P"
      | 372 => RETURN "_C_P"
      | 373 => RETURN "_P"
      | 374 => RETURN "_P"
      | 375 => RETURN "_C_P"
      | 376 => RETURN "_P"
      | 377 => RETURN "_P"
      | 378 => RETURN "_C_P"
      | 379 => RETURN "_P"
      | 380 => RETURN "_P"
      | 381 => RETURN "_P"
      | 382 => RETURN "_C_P"
      | 383 => RETURN "_P"
      | 384 => RETURN "_P"
      | 385 => RETURN "_C_P"
      | 386 => RETURN "_L_P"
      | 387 => RETURN "_L_P"
      | 388 => RETURN "_C_L_P"
      | 389 => RETURN "_L_P"
      | 390 => RETURN "_L_P"
      | 391 => RETURN "_L_P"
      | 392 => RETURN "_C_L_P"
      | 393 => RETURN "_I_P"
      | 394 => RETURN "_L_P"
      | 395 => RETURN "_L_P"
      | 396 => RETURN "_C_L_P"
      | 397 => RETURN "_L_P"
      | 398 => RETURN "_L_P"
      | 399 => RETURN "_L_P"
      | 400 => RETURN "_C_L_P"
      | 401 => RETURN "_I_P"
      | 402 => RETURN "_L_P"
      | 403 => RETURN "_L_P"
      | 404 => RETURN "_C_L_P"
      | 405 => RETURN "_L_P"
      | 406 => RETURN "_L_P"
      | 407 => RETURN "_L_P"
      | 408 => RETURN "_C_L_P"
      | 409 => RETURN "_I_P"
      | 410 => RETURN "_L_P"
      | 411 => RETURN "_L_P"
      | 412 => RETURN "_C_L_P"
      | 413 => RETURN "_L_P"
      | 414 => RETURN "_L_P"
      | 415 => RETURN "_L_P"
      | 416 => RETURN "_C_L_P"
      | 417 => RETURN "_I_P"
      | 418 => RETURN "_L_P"
      | 419 => RETURN "_L_P"
      | 420 => RETURN "_C_L_P"
      | 421 => RETURN "_L_P"
      | 422 => RETURN "_L_P"
      | 423 => RETURN "_L_P"
      | 424 => RETURN "_C_L_P"
      | 425 => RETURN "_I_P"
      | 426 => RETURN "_L_P"
      | 427 => RETURN "_L_P"
      | 428 => RETURN "_C_L_P"
      | 429 => RETURN "_L_P"
      | 430 => RETURN "_L_P"
      | 431 => RETURN "_L_P"
      | 432 => RETURN "_C_L_P"
      | 433 => RETURN "_I_P"
      | 434 => RETURN "_L_P"
      | 435 => RETURN "_L_P"
      | 436 => RETURN "_C_L_P"
      | 437 => RETURN "_L_P"
      | 438 => RETURN "_L_P"
      | 439 => RETURN "_L_P"
      | 440 => RETURN "_C_L_P"
      | 441 => RETURN "_I_P"
      | 442 => RETURN "_L_P"
      | 443 => RETURN "_L_P"
      | 444 => RETURN "_C_L_P"
      | 445 => RETURN "_L_P"
      | 446 => RETURN "_L_P"
      | 447 => RETURN "_L_P"
      | 448 => RETURN "_C_L_P"
      | 449 => RETURN "_I_P"
      | 450 => RETURN "_L_P"
      | 451 => RETURN "_L_P"
      | 452 => RETURN "_C_L_P"
      | 453 => RETURN "_L_P"
      | 454 => RETURN "_L_P"
      | 455 => RETURN "_L_P"
      | 456 => RETURN "_C_L_P"
      | 457 => RETURN "_I_P"
      | 458 => RETURN "_L_P"
      | 459 => RETURN "_L_P"
      | 460 => RETURN "_C_L_P"
      | 461 => RETURN "_L_P"
      | 462 => RETURN "_L_P"
      | 463 => RETURN "_L_P"
      | 464 => RETURN "_C_L_P"
      | 465 => RETURN "_I_P"
      | 466 => RETURN "_L_P"
      | 467 => RETURN "_L_P"
      | 468 => RETURN "_C_L_P"
      | 469 => RETURN "_L_P"
      | 470 => RETURN "_L_P"
      | 471 => RETURN "_L_P"
      | 472 => RETURN "_C_L_P"
      | 473 => RETURN "_I_P"
      | 474 => RETURN "_L_P"
      | 475 => RETURN "_L_P"
      | 476 => RETURN "_C_L_P"
      | 477 => RETURN "_L_P"
      | 478 => RETURN "_L_P"
      | 479 => RETURN "_L_P"
      | 480 => RETURN "_C_L_P"
      | 481 => RETURN "_I_P"
      | 482 => RETURN "_L_P"
      | 483 => RETURN "_L_P"
      | 484 => RETURN "_C_L_P"
      | 485 => RETURN "_L_P"
      | 486 => RETURN "_L_P"
      | 487 => RETURN "_L_P"
      | 488 => RETURN "_C_L_P"
      | 489 => RETURN "_I_P"
      | 490 => RETURN "_P"
      | 491 => RETURN "_P"
      | 492 => RETURN "_C_P"
      | 493 => RETURN "_P"
      | 494 => RETURN "_P"
      | 495 => RETURN "_P"
      | 496 => RETURN "_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

