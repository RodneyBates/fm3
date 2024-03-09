
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
      | 189 => RETURN "ItkRaisesANY"
      | 190 => RETURN "ItkResultTypeAbsent"
      | 191 => RETURN "ItkProcBodyAbsent"
      | 192 => RETURN "ItkScopeEmpty"
      | 193 => RETURN "ItkDeclScopeLt"
      | 194 => RETURN "ItkDeclScopeRt"
      | 195 => RETURN "ItkLookupScopeLt"
      | 196 => RETURN "ItkLookupScopeRt"
      | 197 => RETURN "ItkReservedId"
      | 198 => RETURN "ItkDuplDeclId"
      | 199 => RETURN "ItkDeclId"
      | 200 => RETURN "ItkDeclNo"
      | 201 => RETURN "ItkIdRefAtom"
      | 202 => RETURN "ItkIdRefDeclNo"
      | 203 => RETURN "ItkQualIdAtoms"
      | 204 => RETURN "ItkQualIdDeclNoAtom"
      | 205 => RETURN "ItkQualIdDeclNos"
      | 206 => RETURN "ItkInvalidRef"
      | 207 => RETURN "ItkSignatureProperLt"
      | 208 => RETURN "ItkSignatureProperLtTemp"
      | 209 => RETURN "ItkSignatureProperLtPatch"
      | 210 => RETURN "ItkSignatureProperRt"
      | 211 => RETURN "ItkSignatureFuncLt"
      | 212 => RETURN "ItkSignatureFuncLtTemp"
      | 213 => RETURN "ItkSignatureFuncLtPatch"
      | 214 => RETURN "ItkSignatureFuncRt"
      | 215 => RETURN "ItkMethodDeclLt"
      | 216 => RETURN "ItkMethodDeclLtTemp"
      | 217 => RETURN "ItkMethodDeclLtPatch"
      | 218 => RETURN "ItkMethodDeclRt"
      | 219 => RETURN "ItkFormalTypeLt"
      | 220 => RETURN "ItkFormalTypeLtTemp"
      | 221 => RETURN "ItkFormalTypeLtPatch"
      | 222 => RETURN "ItkFormalTypeRt"
      | 223 => RETURN "ItkFormalExprLt"
      | 224 => RETURN "ItkFormalExprLtTemp"
      | 225 => RETURN "ItkFormalExprLtPatch"
      | 226 => RETURN "ItkFormalExprRt"
      | 227 => RETURN "ItkResultTypeLt"
      | 228 => RETURN "ItkResultTypeLtTemp"
      | 229 => RETURN "ItkResultTypeLtPatch"
      | 230 => RETURN "ItkResultTypeRt"
      | 231 => RETURN "ItkRaisesSetLt"
      | 232 => RETURN "ItkRaisesSetLtTemp"
      | 233 => RETURN "ItkRaisesSetLtPatch"
      | 234 => RETURN "ItkRaisesSetRt"
      | 235 => RETURN "ItkProcNoBodyLt"
      | 236 => RETURN "ItkProcNoBodyLtTemp"
      | 237 => RETURN "ItkProcNoBodyLtPatch"
      | 238 => RETURN "ItkProcNoBodyRt"
      | 239 => RETURN "ItkProcWBodyLt"
      | 240 => RETURN "ItkProcWBodyLtTemp"
      | 241 => RETURN "ItkProcWBodyLtPatch"
      | 242 => RETURN "ItkProcWBodyRt"
      | 243 => RETURN "ItkProcBodyLt"
      | 244 => RETURN "ItkProcBodyLtTemp"
      | 245 => RETURN "ItkProcBodyLtPatch"
      | 246 => RETURN "ItkProcBodyRt"
      | 247 => RETURN "ItkProcTypeLt"
      | 248 => RETURN "ItkProcTypeLtTemp"
      | 249 => RETURN "ItkProcTypeLtPatch"
      | 250 => RETURN "ItkProcTypeRt"
      | 251 => RETURN "ItkOverrideLt"
      | 252 => RETURN "ItkOverrideLtTemp"
      | 253 => RETURN "ItkOverrideLtPatch"
      | 254 => RETURN "ItkOverrideRt"
      | 255 => RETURN "ItkOverrideEquals"
      | 256 => RETURN "ItkOverrideEqualsTemp"
      | 257 => RETURN "ItkOverrideEqualsPatch"
      | 258 => RETURN "ItkBlockLt"
      | 259 => RETURN "ItkBlockLtTemp"
      | 260 => RETURN "ItkBlockLtPatch"
      | 261 => RETURN "ItkBlockRt"
      | 262 => RETURN "ItkBlockBEGIN"
      | 263 => RETURN "ItkBlockBEGINTemp"
      | 264 => RETURN "ItkBlockBEGINPatch"
      | 265 => RETURN "ItkConstDeclLt"
      | 266 => RETURN "ItkConstDeclLtTemp"
      | 267 => RETURN "ItkConstDeclLtPatch"
      | 268 => RETURN "ItkConstDeclRt"
      | 269 => RETURN "ItkConstDeclColon"
      | 270 => RETURN "ItkConstDeclColonTemp"
      | 271 => RETURN "ItkConstDeclColonPatch"
      | 272 => RETURN "ItkConstDeclEquals"
      | 273 => RETURN "ItkConstDeclEqualsTemp"
      | 274 => RETURN "ItkConstDeclEqualsPatch"
      | 275 => RETURN "ItkTypeDeclLt"
      | 276 => RETURN "ItkTypeDeclLtTemp"
      | 277 => RETURN "ItkTypeDeclLtPatch"
      | 278 => RETURN "ItkTypeDeclRt"
      | 279 => RETURN "ItkTypeDeclEquals"
      | 280 => RETURN "ItkTypeDeclEqualsTemp"
      | 281 => RETURN "ItkTypeDeclEqualsPatch"
      | 282 => RETURN "ItkSubtypeDeclLt"
      | 283 => RETURN "ItkSubtypeDeclLtTemp"
      | 284 => RETURN "ItkSubtypeDeclLtPatch"
      | 285 => RETURN "ItkSubtypeDeclRt"
      | 286 => RETURN "ItkSubtypeDeclSubtype"
      | 287 => RETURN "ItkSubtypeDeclSubtypeTemp"
      | 288 => RETURN "ItkSubtypeDeclSubtypePatch"
      | 289 => RETURN "ItkFullRevealLt"
      | 290 => RETURN "ItkFullRevealLtTemp"
      | 291 => RETURN "ItkFullRevealLtPatch"
      | 292 => RETURN "ItkFullRevealRt"
      | 293 => RETURN "ItkFullRevealEquals"
      | 294 => RETURN "ItkFullRevealEqualsTemp"
      | 295 => RETURN "ItkFullRevealEqualsPatch"
      | 296 => RETURN "ItkPartialRevealLt"
      | 297 => RETURN "ItkPartialRevealLtTemp"
      | 298 => RETURN "ItkPartialRevealLtPatch"
      | 299 => RETURN "ItkPartialRevealRt"
      | 300 => RETURN "ItkPartialRevealSubtype"
      | 301 => RETURN "ItkPartialRevealSubtypeTemp"
      | 302 => RETURN "ItkPartialRevealSubtypePatch"
      | 303 => RETURN "ItkInvalidType"
      | 304 => RETURN "ItkREFDefLt"
      | 305 => RETURN "ItkREFDefLtTemp"
      | 306 => RETURN "ItkREFDefLtPatch"
      | 307 => RETURN "ItkREFDefRt"
      | 308 => RETURN "ItkRecDefLt"
      | 309 => RETURN "ItkRecDefLtTemp"
      | 310 => RETURN "ItkRecDefLtPatch"
      | 311 => RETURN "ItkRecDefRt"
      | 312 => RETURN "ItkObjDefLt"
      | 313 => RETURN "ItkObjDefLtTemp"
      | 314 => RETURN "ItkObjDefLtPatch"
      | 315 => RETURN "ItkObjDefRt"
      | 316 => RETURN "ItkSupertypeLt"
      | 317 => RETURN "ItkSupertypeLtTemp"
      | 318 => RETURN "ItkSupertypeLtPatch"
      | 319 => RETURN "ItkSupertypeRt"
      | 320 => RETURN "ItkBrandLt"
      | 321 => RETURN "ItkBrandLtTemp"
      | 322 => RETURN "ItkBrandLtPatch"
      | 323 => RETURN "ItkBrandRt"
      | 324 => RETURN "ItkAbsentSupertype"
      | 325 => RETURN "ItkAbsentBrand"
      | 326 => RETURN "ItkBrandAnon"
      | 327 => RETURN "ItkFieldDeclListLt"
      | 328 => RETURN "ItkFieldDeclListLtTemp"
      | 329 => RETURN "ItkFieldDeclListLtPatch"
      | 330 => RETURN "ItkFieldDeclListRt"
      | 331 => RETURN "ItkFieldDeclListSep"
      | 332 => RETURN "ItkFieldDeclListSepTemp"
      | 333 => RETURN "ItkFieldDeclListSepPatch"
      | 334 => RETURN "ItkFieldDeclListElem"
      | 335 => RETURN "ItkVarDeclListLt"
      | 336 => RETURN "ItkVarDeclListLtTemp"
      | 337 => RETURN "ItkVarDeclListLtPatch"
      | 338 => RETURN "ItkVarDeclListRt"
      | 339 => RETURN "ItkVarDeclListSep"
      | 340 => RETURN "ItkVarDeclListSepTemp"
      | 341 => RETURN "ItkVarDeclListSepPatch"
      | 342 => RETURN "ItkVarDeclListElem"
      | 343 => RETURN "ItkTypeDeclListLt"
      | 344 => RETURN "ItkTypeDeclListLtTemp"
      | 345 => RETURN "ItkTypeDeclListLtPatch"
      | 346 => RETURN "ItkTypeDeclListRt"
      | 347 => RETURN "ItkTypeDeclListSep"
      | 348 => RETURN "ItkTypeDeclListSepTemp"
      | 349 => RETURN "ItkTypeDeclListSepPatch"
      | 350 => RETURN "ItkTypeDeclListElem"
      | 351 => RETURN "ItkMethodDeclListLt"
      | 352 => RETURN "ItkMethodDeclListLtTemp"
      | 353 => RETURN "ItkMethodDeclListLtPatch"
      | 354 => RETURN "ItkMethodDeclListRt"
      | 355 => RETURN "ItkMethodDeclListSep"
      | 356 => RETURN "ItkMethodDeclListSepTemp"
      | 357 => RETURN "ItkMethodDeclListSepPatch"
      | 358 => RETURN "ItkMethodDeclListElem"
      | 359 => RETURN "ItkOverrideListLt"
      | 360 => RETURN "ItkOverrideListLtTemp"
      | 361 => RETURN "ItkOverrideListLtPatch"
      | 362 => RETURN "ItkOverrideListRt"
      | 363 => RETURN "ItkOverrideListSep"
      | 364 => RETURN "ItkOverrideListSepTemp"
      | 365 => RETURN "ItkOverrideListSepPatch"
      | 366 => RETURN "ItkOverrideListElem"
      | 367 => RETURN "ItkFieldDeclLt"
      | 368 => RETURN "ItkFieldDeclLtTemp"
      | 369 => RETURN "ItkFieldDeclLtPatch"
      | 370 => RETURN "ItkFieldDeclRt"
      | 371 => RETURN "ItkFieldDeclType"
      | 372 => RETURN "ItkFieldDeclTypeTemp"
      | 373 => RETURN "ItkFieldDeclTypePatch"
      | 374 => RETURN "ItkFieldDeclVal"
      | 375 => RETURN "ItkFieldDeclValTemp"
      | 376 => RETURN "ItkFieldDeclValPatch"
      | 377 => RETURN "ItkVarDeclLt"
      | 378 => RETURN "ItkVarDeclLtTemp"
      | 379 => RETURN "ItkVarDeclLtPatch"
      | 380 => RETURN "ItkVarDeclRt"
      | 381 => RETURN "ItkVarDeclType"
      | 382 => RETURN "ItkVarDeclTypeTemp"
      | 383 => RETURN "ItkVarDeclTypePatch"
      | 384 => RETURN "ItkVarDeclVal"
      | 385 => RETURN "ItkVarDeclValTemp"
      | 386 => RETURN "ItkVarDeclValPatch"
      | 387 => RETURN "ItkDeclTypeAbsent"
      | 388 => RETURN "ItkDeclValAbsent"
      | 389 => RETURN "ItkVALUEFormalLt"
      | 390 => RETURN "ItkVALUEFormalLtTemp"
      | 391 => RETURN "ItkVALUEFormalLtPatch"
      | 392 => RETURN "ItkVALUEFormalRt"
      | 393 => RETURN "ItkVALUEFormalType"
      | 394 => RETURN "ItkVALUEFormalTypeTemp"
      | 395 => RETURN "ItkVALUEFormalTypePatch"
      | 396 => RETURN "ItkVALUEFormalVal"
      | 397 => RETURN "ItkVALUEFormalValTemp"
      | 398 => RETURN "ItkVALUEFormalValPatch"
      | 399 => RETURN "ItkVARFormalLt"
      | 400 => RETURN "ItkVARFormalLtTemp"
      | 401 => RETURN "ItkVARFormalLtPatch"
      | 402 => RETURN "ItkVARFormalRt"
      | 403 => RETURN "ItkVARFormalType"
      | 404 => RETURN "ItkVARFormalTypeTemp"
      | 405 => RETURN "ItkVARFormalTypePatch"
      | 406 => RETURN "ItkVARFormalVal"
      | 407 => RETURN "ItkVARFormalValTemp"
      | 408 => RETURN "ItkVARFormalValPatch"
      | 409 => RETURN "ItkROFormalLt"
      | 410 => RETURN "ItkROFormalLtTemp"
      | 411 => RETURN "ItkROFormalLtPatch"
      | 412 => RETURN "ItkROFormalRt"
      | 413 => RETURN "ItkROFormalType"
      | 414 => RETURN "ItkROFormalTypeTemp"
      | 415 => RETURN "ItkROFormalTypePatch"
      | 416 => RETURN "ItkROFormalVal"
      | 417 => RETURN "ItkROFormalValTemp"
      | 418 => RETURN "ItkROFormalValPatch"
      | 419 => RETURN "ItkExportIdListLt"
      | 420 => RETURN "ItkExportIdListLtTemp"
      | 421 => RETURN "ItkExportIdListLtPatch"
      | 422 => RETURN "ItkExportIdListRt"
      | 423 => RETURN "ItkExportIdListSep"
      | 424 => RETURN "ItkExportIdListSepTemp"
      | 425 => RETURN "ItkExportIdListSepPatch"
      | 426 => RETURN "ItkExportIdListElem"
      | 427 => RETURN "ItkGenFormalIdListLt"
      | 428 => RETURN "ItkGenFormalIdListLtTemp"
      | 429 => RETURN "ItkGenFormalIdListLtPatch"
      | 430 => RETURN "ItkGenFormalIdListRt"
      | 431 => RETURN "ItkGenFormalIdListSep"
      | 432 => RETURN "ItkGenFormalIdListSepTemp"
      | 433 => RETURN "ItkGenFormalIdListSepPatch"
      | 434 => RETURN "ItkGenFormalIdListElem"
      | 435 => RETURN "ItkGenActualIdListLt"
      | 436 => RETURN "ItkGenActualIdListLtTemp"
      | 437 => RETURN "ItkGenActualIdListLtPatch"
      | 438 => RETURN "ItkGenActualIdListRt"
      | 439 => RETURN "ItkGenActualIdListSep"
      | 440 => RETURN "ItkGenActualIdListSepTemp"
      | 441 => RETURN "ItkGenActualIdListSepPatch"
      | 442 => RETURN "ItkGenActualIdListElem"
      | 443 => RETURN "ItkVarDeclIdListLt"
      | 444 => RETURN "ItkVarDeclIdListLtTemp"
      | 445 => RETURN "ItkVarDeclIdListLtPatch"
      | 446 => RETURN "ItkVarDeclIdListRt"
      | 447 => RETURN "ItkVarDeclIdListSep"
      | 448 => RETURN "ItkVarDeclIdListSepTemp"
      | 449 => RETURN "ItkVarDeclIdListSepPatch"
      | 450 => RETURN "ItkVarDeclIdListElem"
      | 451 => RETURN "ItkTypeDeclIdListLt"
      | 452 => RETURN "ItkTypeDeclIdListLtTemp"
      | 453 => RETURN "ItkTypeDeclIdListLtPatch"
      | 454 => RETURN "ItkTypeDeclIdListRt"
      | 455 => RETURN "ItkTypeDeclIdListSep"
      | 456 => RETURN "ItkTypeDeclIdListSepTemp"
      | 457 => RETURN "ItkTypeDeclIdListSepPatch"
      | 458 => RETURN "ItkTypeDeclIdListElem"
      | 459 => RETURN "ItkRevealIdListLt"
      | 460 => RETURN "ItkRevealIdListLtTemp"
      | 461 => RETURN "ItkRevealIdListLtPatch"
      | 462 => RETURN "ItkRevealIdListRt"
      | 463 => RETURN "ItkRevealIdListSep"
      | 464 => RETURN "ItkRevealIdListSepTemp"
      | 465 => RETURN "ItkRevealIdListSepPatch"
      | 466 => RETURN "ItkRevealIdListElem"
      | 467 => RETURN "ItkFieldDeclIdListLt"
      | 468 => RETURN "ItkFieldDeclIdListLtTemp"
      | 469 => RETURN "ItkFieldDeclIdListLtPatch"
      | 470 => RETURN "ItkFieldDeclIdListRt"
      | 471 => RETURN "ItkFieldDeclIdListSep"
      | 472 => RETURN "ItkFieldDeclIdListSepTemp"
      | 473 => RETURN "ItkFieldDeclIdListSepPatch"
      | 474 => RETURN "ItkFieldDeclIdListElem"
      | 475 => RETURN "ItkFormalsListLt"
      | 476 => RETURN "ItkFormalsListLtTemp"
      | 477 => RETURN "ItkFormalsListLtPatch"
      | 478 => RETURN "ItkFormalsListRt"
      | 479 => RETURN "ItkFormalsListSep"
      | 480 => RETURN "ItkFormalsListSepTemp"
      | 481 => RETURN "ItkFormalsListSepPatch"
      | 482 => RETURN "ItkFormalsListElem"
      | 483 => RETURN "ItkVALUEFormalIdListLt"
      | 484 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 485 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 486 => RETURN "ItkVALUEFormalIdListRt"
      | 487 => RETURN "ItkVALUEFormalIdListSep"
      | 488 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 489 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 490 => RETURN "ItkVALUEFormalIdListElem"
      | 491 => RETURN "ItkVARFormalIdListLt"
      | 492 => RETURN "ItkVARFormalIdListLtTemp"
      | 493 => RETURN "ItkVARFormalIdListLtPatch"
      | 494 => RETURN "ItkVARFormalIdListRt"
      | 495 => RETURN "ItkVARFormalIdListSep"
      | 496 => RETURN "ItkVARFormalIdListSepTemp"
      | 497 => RETURN "ItkVARFormalIdListSepPatch"
      | 498 => RETURN "ItkVARFormalIdListElem"
      | 499 => RETURN "ItkROFormalIdListLt"
      | 500 => RETURN "ItkROFormalIdListLtTemp"
      | 501 => RETURN "ItkROFormalIdListLtPatch"
      | 502 => RETURN "ItkROFormalIdListRt"
      | 503 => RETURN "ItkROFormalIdListSep"
      | 504 => RETURN "ItkROFormalIdListSepTemp"
      | 505 => RETURN "ItkROFormalIdListSepPatch"
      | 506 => RETURN "ItkROFormalIdListElem"
      | 507 => RETURN "ItkBlockDeclListLt"
      | 508 => RETURN "ItkBlockDeclListLtTemp"
      | 509 => RETURN "ItkBlockDeclListLtPatch"
      | 510 => RETURN "ItkBlockDeclListRt"
      | 511 => RETURN "ItkBlockDeclListSep"
      | 512 => RETURN "ItkBlockDeclListSepTemp"
      | 513 => RETURN "ItkBlockDeclListSepPatch"
      | 514 => RETURN "ItkBlockDeclListElem"
      | 515 => RETURN "ItkStmtListLt"
      | 516 => RETURN "ItkStmtListLtTemp"
      | 517 => RETURN "ItkStmtListLtPatch"
      | 518 => RETURN "ItkStmtListRt"
      | 519 => RETURN "ItkStmtListSep"
      | 520 => RETURN "ItkStmtListSepTemp"
      | 521 => RETURN "ItkStmtListSepPatch"
      | 522 => RETURN "ItkStmtListElem"
      | 523 => RETURN "ItkBecomesLt"
      | 524 => RETURN "ItkBecomesLtTemp"
      | 525 => RETURN "ItkBecomesLtPatch"
      | 526 => RETURN "ItkBecomesRt"
      | 527 => RETURN "ItkBecomesInfix"
      | 528 => RETURN "ItkBecomesInfixTemp"
      | 529 => RETURN "ItkBecomesInfixPatch"
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
      | 192 => RETURN "_L"
      | 193 => RETURN "_L"
      | 194 => RETURN "_L"
      | 195 => RETURN "_L"
      | 196 => RETURN "_L"
      | 197 => RETURN "_I_P"
      | 198 => RETURN "_I_P"
      | 199 => RETURN "_L_I_P"
      | 200 => RETURN "_D_P"
      | 201 => RETURN "_I_P"
      | 202 => RETURN "_D_P"
      | 203 => RETURN "_I_I_P_P"
      | 204 => RETURN "_D_I_P_P"
      | 205 => RETURN "_D_D_P_P"
      | 206 => RETURN "_P"
      | 207 => RETURN "_P"
      | 208 => RETURN "_P"
      | 209 => RETURN "_C_P"
      | 210 => RETURN "_P"
      | 211 => RETURN "_P"
      | 212 => RETURN "_P"
      | 213 => RETURN "_C_P"
      | 214 => RETURN "_P"
      | 215 => RETURN "_I_P"
      | 216 => RETURN "_I_P"
      | 217 => RETURN "_C_I_P"
      | 218 => RETURN "_I_P"
      | 219 => RETURN "_P"
      | 220 => RETURN "_P"
      | 221 => RETURN "_C_P"
      | 222 => RETURN "_P"
      | 223 => RETURN "_P"
      | 224 => RETURN "_P"
      | 225 => RETURN "_C_P"
      | 226 => RETURN "_P"
      | 227 => RETURN "_P"
      | 228 => RETURN "_P"
      | 229 => RETURN "_C_P"
      | 230 => RETURN "_P"
      | 231 => RETURN "_L_P"
      | 232 => RETURN "_L_P"
      | 233 => RETURN "_C_L_P"
      | 234 => RETURN "_L_P"
      | 235 => RETURN "_I"
      | 236 => RETURN "_I"
      | 237 => RETURN "_C_I"
      | 238 => RETURN "_I"
      | 239 => RETURN "_I"
      | 240 => RETURN "_I"
      | 241 => RETURN "_C_I"
      | 242 => RETURN "_I"
      | 243 => RETURN "_P"
      | 244 => RETURN "_P"
      | 245 => RETURN "_C_P"
      | 246 => RETURN "_P"
      | 247 => RETURN "_P"
      | 248 => RETURN "_P"
      | 249 => RETURN "_C_P"
      | 250 => RETURN "_P"
      | 251 => RETURN "_P"
      | 252 => RETURN "_P"
      | 253 => RETURN "_C_P"
      | 254 => RETURN "_P"
      | 255 => RETURN "_P"
      | 256 => RETURN "_P"
      | 257 => RETURN "_C_P"
      | 258 => RETURN "_L_P"
      | 259 => RETURN "_L_P"
      | 260 => RETURN "_C_L_P"
      | 261 => RETURN "_L_P"
      | 262 => RETURN "_L_P"
      | 263 => RETURN "_L_P"
      | 264 => RETURN "_C_L_P"
      | 265 => RETURN "_P"
      | 266 => RETURN "_P"
      | 267 => RETURN "_C_P"
      | 268 => RETURN "_P"
      | 269 => RETURN "_P_B"
      | 270 => RETURN "_P_B"
      | 271 => RETURN "_C_P_B"
      | 272 => RETURN "_P"
      | 273 => RETURN "_P"
      | 274 => RETURN "_C_P"
      | 275 => RETURN "_P"
      | 276 => RETURN "_P"
      | 277 => RETURN "_C_P"
      | 278 => RETURN "_P"
      | 279 => RETURN "_P"
      | 280 => RETURN "_P"
      | 281 => RETURN "_C_P"
      | 282 => RETURN "_P"
      | 283 => RETURN "_P"
      | 284 => RETURN "_C_P"
      | 285 => RETURN "_P"
      | 286 => RETURN "_P"
      | 287 => RETURN "_P"
      | 288 => RETURN "_C_P"
      | 289 => RETURN "_P"
      | 290 => RETURN "_P"
      | 291 => RETURN "_C_P"
      | 292 => RETURN "_P"
      | 293 => RETURN "_P"
      | 294 => RETURN "_P"
      | 295 => RETURN "_C_P"
      | 296 => RETURN "_P"
      | 297 => RETURN "_P"
      | 298 => RETURN "_C_P"
      | 299 => RETURN "_P"
      | 300 => RETURN "_P"
      | 301 => RETURN "_P"
      | 302 => RETURN "_C_P"
      | 303 => RETURN "_P"
      | 304 => RETURN "_P"
      | 305 => RETURN "_P"
      | 306 => RETURN "_C_P"
      | 307 => RETURN "_P"
      | 308 => RETURN "_L_P"
      | 309 => RETURN "_L_P"
      | 310 => RETURN "_C_L_P"
      | 311 => RETURN "_L_P"
      | 312 => RETURN "_P"
      | 313 => RETURN "_P"
      | 314 => RETURN "_C_P"
      | 315 => RETURN "_P"
      | 316 => RETURN "_P"
      | 317 => RETURN "_P"
      | 318 => RETURN "_C_P"
      | 319 => RETURN "_P"
      | 320 => RETURN "_P"
      | 321 => RETURN "_P"
      | 322 => RETURN "_C_P"
      | 323 => RETURN "_P"
      | 324 => RETURN "_P"
      | 325 => RETURN "_P"
      | 326 => RETURN "_P"
      | 327 => RETURN "_L_P"
      | 328 => RETURN "_L_P"
      | 329 => RETURN "_C_L_P"
      | 330 => RETURN "_L_P"
      | 331 => RETURN "_L_P"
      | 332 => RETURN "_L_P"
      | 333 => RETURN "_C_L_P"
      | 334 => RETURN "_I_P"
      | 335 => RETURN "_L_P"
      | 336 => RETURN "_L_P"
      | 337 => RETURN "_C_L_P"
      | 338 => RETURN "_L_P"
      | 339 => RETURN "_L_P"
      | 340 => RETURN "_L_P"
      | 341 => RETURN "_C_L_P"
      | 342 => RETURN "_I_P"
      | 343 => RETURN "_L_P"
      | 344 => RETURN "_L_P"
      | 345 => RETURN "_C_L_P"
      | 346 => RETURN "_L_P"
      | 347 => RETURN "_L_P"
      | 348 => RETURN "_L_P"
      | 349 => RETURN "_C_L_P"
      | 350 => RETURN "_I_P"
      | 351 => RETURN "_L_P"
      | 352 => RETURN "_L_P"
      | 353 => RETURN "_C_L_P"
      | 354 => RETURN "_L_P"
      | 355 => RETURN "_L_P"
      | 356 => RETURN "_L_P"
      | 357 => RETURN "_C_L_P"
      | 358 => RETURN "_I_P"
      | 359 => RETURN "_L_P"
      | 360 => RETURN "_L_P"
      | 361 => RETURN "_C_L_P"
      | 362 => RETURN "_L_P"
      | 363 => RETURN "_L_P"
      | 364 => RETURN "_L_P"
      | 365 => RETURN "_C_L_P"
      | 366 => RETURN "_I_P"
      | 367 => RETURN "_P"
      | 368 => RETURN "_P"
      | 369 => RETURN "_C_P"
      | 370 => RETURN "_P"
      | 371 => RETURN "_P"
      | 372 => RETURN "_P"
      | 373 => RETURN "_C_P"
      | 374 => RETURN "_P"
      | 375 => RETURN "_P"
      | 376 => RETURN "_C_P"
      | 377 => RETURN "_P"
      | 378 => RETURN "_P"
      | 379 => RETURN "_C_P"
      | 380 => RETURN "_P"
      | 381 => RETURN "_P"
      | 382 => RETURN "_P"
      | 383 => RETURN "_C_P"
      | 384 => RETURN "_P"
      | 385 => RETURN "_P"
      | 386 => RETURN "_C_P"
      | 387 => RETURN ""
      | 388 => RETURN ""
      | 389 => RETURN "_P"
      | 390 => RETURN "_P"
      | 391 => RETURN "_C_P"
      | 392 => RETURN "_P"
      | 393 => RETURN "_P"
      | 394 => RETURN "_P"
      | 395 => RETURN "_C_P"
      | 396 => RETURN "_P"
      | 397 => RETURN "_P"
      | 398 => RETURN "_C_P"
      | 399 => RETURN "_P"
      | 400 => RETURN "_P"
      | 401 => RETURN "_C_P"
      | 402 => RETURN "_P"
      | 403 => RETURN "_P"
      | 404 => RETURN "_P"
      | 405 => RETURN "_C_P"
      | 406 => RETURN "_P"
      | 407 => RETURN "_P"
      | 408 => RETURN "_C_P"
      | 409 => RETURN "_P"
      | 410 => RETURN "_P"
      | 411 => RETURN "_C_P"
      | 412 => RETURN "_P"
      | 413 => RETURN "_P"
      | 414 => RETURN "_P"
      | 415 => RETURN "_C_P"
      | 416 => RETURN "_P"
      | 417 => RETURN "_P"
      | 418 => RETURN "_C_P"
      | 419 => RETURN "_L_P"
      | 420 => RETURN "_L_P"
      | 421 => RETURN "_C_L_P"
      | 422 => RETURN "_L_P"
      | 423 => RETURN "_L_P"
      | 424 => RETURN "_L_P"
      | 425 => RETURN "_C_L_P"
      | 426 => RETURN "_I_P"
      | 427 => RETURN "_L_P"
      | 428 => RETURN "_L_P"
      | 429 => RETURN "_C_L_P"
      | 430 => RETURN "_L_P"
      | 431 => RETURN "_L_P"
      | 432 => RETURN "_L_P"
      | 433 => RETURN "_C_L_P"
      | 434 => RETURN "_I_P"
      | 435 => RETURN "_L_P"
      | 436 => RETURN "_L_P"
      | 437 => RETURN "_C_L_P"
      | 438 => RETURN "_L_P"
      | 439 => RETURN "_L_P"
      | 440 => RETURN "_L_P"
      | 441 => RETURN "_C_L_P"
      | 442 => RETURN "_I_P"
      | 443 => RETURN "_L_P"
      | 444 => RETURN "_L_P"
      | 445 => RETURN "_C_L_P"
      | 446 => RETURN "_L_P"
      | 447 => RETURN "_L_P"
      | 448 => RETURN "_L_P"
      | 449 => RETURN "_C_L_P"
      | 450 => RETURN "_I_P"
      | 451 => RETURN "_L_P"
      | 452 => RETURN "_L_P"
      | 453 => RETURN "_C_L_P"
      | 454 => RETURN "_L_P"
      | 455 => RETURN "_L_P"
      | 456 => RETURN "_L_P"
      | 457 => RETURN "_C_L_P"
      | 458 => RETURN "_I_P"
      | 459 => RETURN "_L_P"
      | 460 => RETURN "_L_P"
      | 461 => RETURN "_C_L_P"
      | 462 => RETURN "_L_P"
      | 463 => RETURN "_L_P"
      | 464 => RETURN "_L_P"
      | 465 => RETURN "_C_L_P"
      | 466 => RETURN "_I_P"
      | 467 => RETURN "_L_P"
      | 468 => RETURN "_L_P"
      | 469 => RETURN "_C_L_P"
      | 470 => RETURN "_L_P"
      | 471 => RETURN "_L_P"
      | 472 => RETURN "_L_P"
      | 473 => RETURN "_C_L_P"
      | 474 => RETURN "_I_P"
      | 475 => RETURN "_L_P"
      | 476 => RETURN "_L_P"
      | 477 => RETURN "_C_L_P"
      | 478 => RETURN "_L_P"
      | 479 => RETURN "_L_P"
      | 480 => RETURN "_L_P"
      | 481 => RETURN "_C_L_P"
      | 482 => RETURN "_I_P"
      | 483 => RETURN "_L_P"
      | 484 => RETURN "_L_P"
      | 485 => RETURN "_C_L_P"
      | 486 => RETURN "_L_P"
      | 487 => RETURN "_L_P"
      | 488 => RETURN "_L_P"
      | 489 => RETURN "_C_L_P"
      | 490 => RETURN "_I_P"
      | 491 => RETURN "_L_P"
      | 492 => RETURN "_L_P"
      | 493 => RETURN "_C_L_P"
      | 494 => RETURN "_L_P"
      | 495 => RETURN "_L_P"
      | 496 => RETURN "_L_P"
      | 497 => RETURN "_C_L_P"
      | 498 => RETURN "_I_P"
      | 499 => RETURN "_L_P"
      | 500 => RETURN "_L_P"
      | 501 => RETURN "_C_L_P"
      | 502 => RETURN "_L_P"
      | 503 => RETURN "_L_P"
      | 504 => RETURN "_L_P"
      | 505 => RETURN "_C_L_P"
      | 506 => RETURN "_I_P"
      | 507 => RETURN "_L_P"
      | 508 => RETURN "_L_P"
      | 509 => RETURN "_C_L_P"
      | 510 => RETURN "_L_P"
      | 511 => RETURN "_L_P"
      | 512 => RETURN "_L_P"
      | 513 => RETURN "_C_L_P"
      | 514 => RETURN "_I_P"
      | 515 => RETURN "_L_P"
      | 516 => RETURN "_L_P"
      | 517 => RETURN "_C_L_P"
      | 518 => RETURN "_L_P"
      | 519 => RETURN "_L_P"
      | 520 => RETURN "_L_P"
      | 521 => RETURN "_C_L_P"
      | 522 => RETURN "_I_P"
      | 523 => RETURN "_P"
      | 524 => RETURN "_P"
      | 525 => RETURN "_C_P"
      | 526 => RETURN "_P"
      | 527 => RETURN "_P"
      | 528 => RETURN "_P"
      | 529 => RETURN "_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

