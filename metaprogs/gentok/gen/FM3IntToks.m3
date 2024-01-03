
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
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
      | 167 => RETURN "ItkUnitId"
      | 168 => RETURN "ItkImport"
      | 169 => RETURN "ItkFromImport"
      | 170 => RETURN "ItkImportAs"
      | 171 => RETURN "ItkFormalsListEmpty"
      | 172 => RETURN "ItkFormalTypeAbsent"
      | 173 => RETURN "ItkFormalExprAbsent"
      | 174 => RETURN "ItkRaisesSetAbsent"
      | 175 => RETURN "ItkRaisesANY"
      | 176 => RETURN "ItkResultTypeAbsent"
      | 177 => RETURN "ItkProcBodyAbsent"
      | 178 => RETURN "ItkScopeLt"
      | 179 => RETURN "ItkScopeRt"
      | 180 => RETURN "ItkIdReserved"
      | 181 => RETURN "ItkIdRefAtom"
      | 182 => RETURN "ItkIdRefDeclNo"
      | 183 => RETURN "ItkDuplDeclId"
      | 184 => RETURN "ItkDeclId"
      | 185 => RETURN "ItkDeclNo"
      | 186 => RETURN "ItkQualIdLt"
      | 187 => RETURN "ItkQualIdLtTemp"
      | 188 => RETURN "ItkQualIdLtPatch"
      | 189 => RETURN "ItkQualIdRt"
      | 190 => RETURN "ItkFuncSignatureLt"
      | 191 => RETURN "ItkFuncSignatureLtTemp"
      | 192 => RETURN "ItkFuncSignatureLtPatch"
      | 193 => RETURN "ItkFuncSignatureRt"
      | 194 => RETURN "ItkProcSignatureLt"
      | 195 => RETURN "ItkProcSignatureLtTemp"
      | 196 => RETURN "ItkProcSignatureLtPatch"
      | 197 => RETURN "ItkProcSignatureRt"
      | 198 => RETURN "ItkFormalTypeLt"
      | 199 => RETURN "ItkFormalTypeLtTemp"
      | 200 => RETURN "ItkFormalTypeLtPatch"
      | 201 => RETURN "ItkFormalTypeRt"
      | 202 => RETURN "ItkFormalExprLt"
      | 203 => RETURN "ItkFormalExprLtTemp"
      | 204 => RETURN "ItkFormalExprLtPatch"
      | 205 => RETURN "ItkFormalExprRt"
      | 206 => RETURN "ItkResultTypeLt"
      | 207 => RETURN "ItkResultTypeLtTemp"
      | 208 => RETURN "ItkResultTypeLtPatch"
      | 209 => RETURN "ItkResultTypeRt"
      | 210 => RETURN "ItkRaisesSetLt"
      | 211 => RETURN "ItkRaisesSetLtTemp"
      | 212 => RETURN "ItkRaisesSetLtPatch"
      | 213 => RETURN "ItkRaisesSetRt"
      | 214 => RETURN "ItkProcNoBodyLt"
      | 215 => RETURN "ItkProcNoBodyLtTemp"
      | 216 => RETURN "ItkProcNoBodyLtPatch"
      | 217 => RETURN "ItkProcNoBodyRt"
      | 218 => RETURN "ItkProcWBodyLt"
      | 219 => RETURN "ItkProcWBodyLtTemp"
      | 220 => RETURN "ItkProcWBodyLtPatch"
      | 221 => RETURN "ItkProcWBodyRt"
      | 222 => RETURN "ItkProcBodyLt"
      | 223 => RETURN "ItkProcBodyLtTemp"
      | 224 => RETURN "ItkProcBodyLtPatch"
      | 225 => RETURN "ItkProcBodyRt"
      | 226 => RETURN "ItkProcTypeLt"
      | 227 => RETURN "ItkProcTypeLtTemp"
      | 228 => RETURN "ItkProcTypeLtPatch"
      | 229 => RETURN "ItkProcTypeRt"
      | 230 => RETURN "ItkBlockLt"
      | 231 => RETURN "ItkBlockLtTemp"
      | 232 => RETURN "ItkBlockLtPatch"
      | 233 => RETURN "ItkBlockRt"
      | 234 => RETURN "ItkBlockBEGIN"
      | 235 => RETURN "ItkBlockBEGINTemp"
      | 236 => RETURN "ItkBlockBEGINPatch"
      | 237 => RETURN "ItkTypeDeclId"
      | 238 => RETURN "ItkTypeDeclLt"
      | 239 => RETURN "ItkTypeDeclLtTemp"
      | 240 => RETURN "ItkTypeDeclLtPatch"
      | 241 => RETURN "ItkTypeDeclRt"
      | 242 => RETURN "ItkTypeDeclEquals"
      | 243 => RETURN "ItkTypeDeclEqualsTemp"
      | 244 => RETURN "ItkTypeDeclEqualsPatch"
      | 245 => RETURN "ItkREFDefLt"
      | 246 => RETURN "ItkREFDefLtTemp"
      | 247 => RETURN "ItkREFDefLtPatch"
      | 248 => RETURN "ItkREFDefRt"
      | 249 => RETURN "ItkRecDefLt"
      | 250 => RETURN "ItkRecDefLtTemp"
      | 251 => RETURN "ItkRecDefLtPatch"
      | 252 => RETURN "ItkRecDefRt"
      | 253 => RETURN "ItkRecFieldLt"
      | 254 => RETURN "ItkRecFieldLtTemp"
      | 255 => RETURN "ItkRecFieldLtPatch"
      | 256 => RETURN "ItkRecFieldRt"
      | 257 => RETURN "ItkRecFieldType"
      | 258 => RETURN "ItkRecFieldTypeTemp"
      | 259 => RETURN "ItkRecFieldTypePatch"
      | 260 => RETURN "ItkRecFieldVal"
      | 261 => RETURN "ItkRecFieldValTemp"
      | 262 => RETURN "ItkRecFieldValPatch"
      | 263 => RETURN "ItkFieldDeclListLt"
      | 264 => RETURN "ItkFieldDeclListLtTemp"
      | 265 => RETURN "ItkFieldDeclListLtPatch"
      | 266 => RETURN "ItkFieldDeclListRt"
      | 267 => RETURN "ItkFieldDeclListSep"
      | 268 => RETURN "ItkFieldDeclListSepTemp"
      | 269 => RETURN "ItkFieldDeclListSepPatch"
      | 270 => RETURN "ItkFieldDeclListElem"
      | 271 => RETURN "ItkVarDeclListLt"
      | 272 => RETURN "ItkVarDeclListLtTemp"
      | 273 => RETURN "ItkVarDeclListLtPatch"
      | 274 => RETURN "ItkVarDeclListRt"
      | 275 => RETURN "ItkVarDeclListSep"
      | 276 => RETURN "ItkVarDeclListSepTemp"
      | 277 => RETURN "ItkVarDeclListSepPatch"
      | 278 => RETURN "ItkVarDeclListElem"
      | 279 => RETURN "ItkFieldDeclLt"
      | 280 => RETURN "ItkFieldDeclLtTemp"
      | 281 => RETURN "ItkFieldDeclLtPatch"
      | 282 => RETURN "ItkFieldDeclRt"
      | 283 => RETURN "ItkFieldDeclType"
      | 284 => RETURN "ItkFieldDeclTypeTemp"
      | 285 => RETURN "ItkFieldDeclTypePatch"
      | 286 => RETURN "ItkFieldDeclVal"
      | 287 => RETURN "ItkFieldDeclValTemp"
      | 288 => RETURN "ItkFieldDeclValPatch"
      | 289 => RETURN "ItkVarDeclLt"
      | 290 => RETURN "ItkVarDeclLtTemp"
      | 291 => RETURN "ItkVarDeclLtPatch"
      | 292 => RETURN "ItkVarDeclRt"
      | 293 => RETURN "ItkVarDeclType"
      | 294 => RETURN "ItkVarDeclTypeTemp"
      | 295 => RETURN "ItkVarDeclTypePatch"
      | 296 => RETURN "ItkVarDeclVal"
      | 297 => RETURN "ItkVarDeclValTemp"
      | 298 => RETURN "ItkVarDeclValPatch"
      | 299 => RETURN "ItkVALUEFormalLt"
      | 300 => RETURN "ItkVALUEFormalLtTemp"
      | 301 => RETURN "ItkVALUEFormalLtPatch"
      | 302 => RETURN "ItkVALUEFormalRt"
      | 303 => RETURN "ItkVALUEFormalType"
      | 304 => RETURN "ItkVALUEFormalTypeTemp"
      | 305 => RETURN "ItkVALUEFormalTypePatch"
      | 306 => RETURN "ItkVALUEFormalVal"
      | 307 => RETURN "ItkVALUEFormalValTemp"
      | 308 => RETURN "ItkVALUEFormalValPatch"
      | 309 => RETURN "ItkVARFormalLt"
      | 310 => RETURN "ItkVARFormalLtTemp"
      | 311 => RETURN "ItkVARFormalLtPatch"
      | 312 => RETURN "ItkVARFormalRt"
      | 313 => RETURN "ItkVARFormalType"
      | 314 => RETURN "ItkVARFormalTypeTemp"
      | 315 => RETURN "ItkVARFormalTypePatch"
      | 316 => RETURN "ItkVARFormalVal"
      | 317 => RETURN "ItkVARFormalValTemp"
      | 318 => RETURN "ItkVARFormalValPatch"
      | 319 => RETURN "ItkROFormalLt"
      | 320 => RETURN "ItkROFormalLtTemp"
      | 321 => RETURN "ItkROFormalLtPatch"
      | 322 => RETURN "ItkROFormalRt"
      | 323 => RETURN "ItkROFormalType"
      | 324 => RETURN "ItkROFormalTypeTemp"
      | 325 => RETURN "ItkROFormalTypePatch"
      | 326 => RETURN "ItkROFormalVal"
      | 327 => RETURN "ItkROFormalValTemp"
      | 328 => RETURN "ItkROFormalValPatch"
      | 329 => RETURN "ItkExportIdListLt"
      | 330 => RETURN "ItkExportIdListLtTemp"
      | 331 => RETURN "ItkExportIdListLtPatch"
      | 332 => RETURN "ItkExportIdListRt"
      | 333 => RETURN "ItkExportIdListSep"
      | 334 => RETURN "ItkExportIdListSepTemp"
      | 335 => RETURN "ItkExportIdListSepPatch"
      | 336 => RETURN "ItkExportIdListElem"
      | 337 => RETURN "ItkGenFormalIdListLt"
      | 338 => RETURN "ItkGenFormalIdListLtTemp"
      | 339 => RETURN "ItkGenFormalIdListLtPatch"
      | 340 => RETURN "ItkGenFormalIdListRt"
      | 341 => RETURN "ItkGenFormalIdListSep"
      | 342 => RETURN "ItkGenFormalIdListSepTemp"
      | 343 => RETURN "ItkGenFormalIdListSepPatch"
      | 344 => RETURN "ItkGenFormalIdListElem"
      | 345 => RETURN "ItkGenActualIdListLt"
      | 346 => RETURN "ItkGenActualIdListLtTemp"
      | 347 => RETURN "ItkGenActualIdListLtPatch"
      | 348 => RETURN "ItkGenActualIdListRt"
      | 349 => RETURN "ItkGenActualIdListSep"
      | 350 => RETURN "ItkGenActualIdListSepTemp"
      | 351 => RETURN "ItkGenActualIdListSepPatch"
      | 352 => RETURN "ItkGenActualIdListElem"
      | 353 => RETURN "ItkVarDeclIdListLt"
      | 354 => RETURN "ItkVarDeclIdListLtTemp"
      | 355 => RETURN "ItkVarDeclIdListLtPatch"
      | 356 => RETURN "ItkVarDeclIdListRt"
      | 357 => RETURN "ItkVarDeclIdListSep"
      | 358 => RETURN "ItkVarDeclIdListSepTemp"
      | 359 => RETURN "ItkVarDeclIdListSepPatch"
      | 360 => RETURN "ItkVarDeclIdListElem"
      | 361 => RETURN "ItkFieldDeclIdListLt"
      | 362 => RETURN "ItkFieldDeclIdListLtTemp"
      | 363 => RETURN "ItkFieldDeclIdListLtPatch"
      | 364 => RETURN "ItkFieldDeclIdListRt"
      | 365 => RETURN "ItkFieldDeclIdListSep"
      | 366 => RETURN "ItkFieldDeclIdListSepTemp"
      | 367 => RETURN "ItkFieldDeclIdListSepPatch"
      | 368 => RETURN "ItkFieldDeclIdListElem"
      | 369 => RETURN "ItkFormalsListLt"
      | 370 => RETURN "ItkFormalsListLtTemp"
      | 371 => RETURN "ItkFormalsListLtPatch"
      | 372 => RETURN "ItkFormalsListRt"
      | 373 => RETURN "ItkFormalsListSep"
      | 374 => RETURN "ItkFormalsListSepTemp"
      | 375 => RETURN "ItkFormalsListSepPatch"
      | 376 => RETURN "ItkFormalsListElem"
      | 377 => RETURN "ItkVALUEFormalIdListLt"
      | 378 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 379 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 380 => RETURN "ItkVALUEFormalIdListRt"
      | 381 => RETURN "ItkVALUEFormalIdListSep"
      | 382 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 383 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 384 => RETURN "ItkVALUEFormalIdListElem"
      | 385 => RETURN "ItkVARFormalIdListLt"
      | 386 => RETURN "ItkVARFormalIdListLtTemp"
      | 387 => RETURN "ItkVARFormalIdListLtPatch"
      | 388 => RETURN "ItkVARFormalIdListRt"
      | 389 => RETURN "ItkVARFormalIdListSep"
      | 390 => RETURN "ItkVARFormalIdListSepTemp"
      | 391 => RETURN "ItkVARFormalIdListSepPatch"
      | 392 => RETURN "ItkVARFormalIdListElem"
      | 393 => RETURN "ItkROFormalIdListLt"
      | 394 => RETURN "ItkROFormalIdListLtTemp"
      | 395 => RETURN "ItkROFormalIdListLtPatch"
      | 396 => RETURN "ItkROFormalIdListRt"
      | 397 => RETURN "ItkROFormalIdListSep"
      | 398 => RETURN "ItkROFormalIdListSepTemp"
      | 399 => RETURN "ItkROFormalIdListSepPatch"
      | 400 => RETURN "ItkROFormalIdListElem"
      | 401 => RETURN "ItkDeclListLt"
      | 402 => RETURN "ItkDeclListLtTemp"
      | 403 => RETURN "ItkDeclListLtPatch"
      | 404 => RETURN "ItkDeclListRt"
      | 405 => RETURN "ItkDeclListSep"
      | 406 => RETURN "ItkDeclListSepTemp"
      | 407 => RETURN "ItkDeclListSepPatch"
      | 408 => RETURN "ItkDeclListElem"
      | 409 => RETURN "ItkBecomesLt"
      | 410 => RETURN "ItkBecomesLtTemp"
      | 411 => RETURN "ItkBecomesLtPatch"
      | 412 => RETURN "ItkBecomesRt"
      | 413 => RETURN "ItkBecomesInfix"
      | 414 => RETURN "ItkBecomesInfixTemp"
      | 415 => RETURN "ItkBecomesInfixPatch"
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
      | 167 => RETURN "_I_P"
      | 168 => RETURN "_I_P"
      | 169 => RETURN "_I_P_I_P"
      | 170 => RETURN "_I_P_I_P"
      | 171 => RETURN ""
      | 172 => RETURN "_P"
      | 173 => RETURN "_P"
      | 174 => RETURN "_P"
      | 175 => RETURN "_P"
      | 176 => RETURN "_P"
      | 177 => RETURN "_P"
      | 178 => RETURN "_L"
      | 179 => RETURN "_L"
      | 180 => RETURN "_I_P"
      | 181 => RETURN "_I_P"
      | 182 => RETURN "_D_P"
      | 183 => RETURN "_I_P"
      | 184 => RETURN "_L_I_P"
      | 185 => RETURN "_D_P"
      | 186 => RETURN ""
      | 187 => RETURN ""
      | 188 => RETURN "_C"
      | 189 => RETURN ""
      | 190 => RETURN "_P"
      | 191 => RETURN "_P"
      | 192 => RETURN "_C_P"
      | 193 => RETURN "_P"
      | 194 => RETURN "_P"
      | 195 => RETURN "_P"
      | 196 => RETURN "_C_P"
      | 197 => RETURN "_P"
      | 198 => RETURN "_P"
      | 199 => RETURN "_P"
      | 200 => RETURN "_C_P"
      | 201 => RETURN "_P"
      | 202 => RETURN "_P"
      | 203 => RETURN "_P"
      | 204 => RETURN "_C_P"
      | 205 => RETURN "_P"
      | 206 => RETURN "_P"
      | 207 => RETURN "_P"
      | 208 => RETURN "_C_P"
      | 209 => RETURN "_P"
      | 210 => RETURN "_P"
      | 211 => RETURN "_P"
      | 212 => RETURN "_C_P"
      | 213 => RETURN "_P"
      | 214 => RETURN "_L"
      | 215 => RETURN "_L"
      | 216 => RETURN "_C_L"
      | 217 => RETURN "_L"
      | 218 => RETURN "_L"
      | 219 => RETURN "_L"
      | 220 => RETURN "_C_L"
      | 221 => RETURN "_L"
      | 222 => RETURN "_P"
      | 223 => RETURN "_P"
      | 224 => RETURN "_C_P"
      | 225 => RETURN "_P"
      | 226 => RETURN "_P"
      | 227 => RETURN "_P"
      | 228 => RETURN "_C_P"
      | 229 => RETURN "_P"
      | 230 => RETURN "_L_P"
      | 231 => RETURN "_L_P"
      | 232 => RETURN "_C_L_P"
      | 233 => RETURN "_L_P"
      | 234 => RETURN "_L_P"
      | 235 => RETURN "_L_P"
      | 236 => RETURN "_C_L_P"
      | 237 => RETURN "_I_P"
      | 238 => RETURN "_P"
      | 239 => RETURN "_P"
      | 240 => RETURN "_C_P"
      | 241 => RETURN "_P"
      | 242 => RETURN "_P"
      | 243 => RETURN "_P"
      | 244 => RETURN "_C_P"
      | 245 => RETURN "_P"
      | 246 => RETURN "_P"
      | 247 => RETURN "_C_P"
      | 248 => RETURN "_P"
      | 249 => RETURN "_L_P"
      | 250 => RETURN "_L_P"
      | 251 => RETURN "_C_L_P"
      | 252 => RETURN "_L_P"
      | 253 => RETURN "_P"
      | 254 => RETURN "_P"
      | 255 => RETURN "_C_P"
      | 256 => RETURN "_P"
      | 257 => RETURN "_P"
      | 258 => RETURN "_P"
      | 259 => RETURN "_C_P"
      | 260 => RETURN "_P"
      | 261 => RETURN "_P"
      | 262 => RETURN "_C_P"
      | 263 => RETURN "_L_P"
      | 264 => RETURN "_L_P"
      | 265 => RETURN "_C_L_P"
      | 266 => RETURN "_L_P"
      | 267 => RETURN "_L_P"
      | 268 => RETURN "_L_P"
      | 269 => RETURN "_C_L_P"
      | 270 => RETURN "_I_P"
      | 271 => RETURN "_L_P"
      | 272 => RETURN "_L_P"
      | 273 => RETURN "_C_L_P"
      | 274 => RETURN "_L_P"
      | 275 => RETURN "_L_P"
      | 276 => RETURN "_L_P"
      | 277 => RETURN "_C_L_P"
      | 278 => RETURN "_I_P"
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
      | 301 => RETURN "_C_P"
      | 302 => RETURN "_P"
      | 303 => RETURN "_P"
      | 304 => RETURN "_P"
      | 305 => RETURN "_C_P"
      | 306 => RETURN "_P"
      | 307 => RETURN "_P"
      | 308 => RETURN "_C_P"
      | 309 => RETURN "_P"
      | 310 => RETURN "_P"
      | 311 => RETURN "_C_P"
      | 312 => RETURN "_P"
      | 313 => RETURN "_P"
      | 314 => RETURN "_P"
      | 315 => RETURN "_C_P"
      | 316 => RETURN "_P"
      | 317 => RETURN "_P"
      | 318 => RETURN "_C_P"
      | 319 => RETURN "_P"
      | 320 => RETURN "_P"
      | 321 => RETURN "_C_P"
      | 322 => RETURN "_P"
      | 323 => RETURN "_P"
      | 324 => RETURN "_P"
      | 325 => RETURN "_C_P"
      | 326 => RETURN "_P"
      | 327 => RETURN "_P"
      | 328 => RETURN "_C_P"
      | 329 => RETURN "_L_P"
      | 330 => RETURN "_L_P"
      | 331 => RETURN "_C_L_P"
      | 332 => RETURN "_L_P"
      | 333 => RETURN "_L_P"
      | 334 => RETURN "_L_P"
      | 335 => RETURN "_C_L_P"
      | 336 => RETURN "_I_P"
      | 337 => RETURN "_L_P"
      | 338 => RETURN "_L_P"
      | 339 => RETURN "_C_L_P"
      | 340 => RETURN "_L_P"
      | 341 => RETURN "_L_P"
      | 342 => RETURN "_L_P"
      | 343 => RETURN "_C_L_P"
      | 344 => RETURN "_I_P"
      | 345 => RETURN "_L_P"
      | 346 => RETURN "_L_P"
      | 347 => RETURN "_C_L_P"
      | 348 => RETURN "_L_P"
      | 349 => RETURN "_L_P"
      | 350 => RETURN "_L_P"
      | 351 => RETURN "_C_L_P"
      | 352 => RETURN "_I_P"
      | 353 => RETURN "_L_P"
      | 354 => RETURN "_L_P"
      | 355 => RETURN "_C_L_P"
      | 356 => RETURN "_L_P"
      | 357 => RETURN "_L_P"
      | 358 => RETURN "_L_P"
      | 359 => RETURN "_C_L_P"
      | 360 => RETURN "_I_P"
      | 361 => RETURN "_L_P"
      | 362 => RETURN "_L_P"
      | 363 => RETURN "_C_L_P"
      | 364 => RETURN "_L_P"
      | 365 => RETURN "_L_P"
      | 366 => RETURN "_L_P"
      | 367 => RETURN "_C_L_P"
      | 368 => RETURN "_I_P"
      | 369 => RETURN "_L_P"
      | 370 => RETURN "_L_P"
      | 371 => RETURN "_C_L_P"
      | 372 => RETURN "_L_P"
      | 373 => RETURN "_L_P"
      | 374 => RETURN "_L_P"
      | 375 => RETURN "_C_L_P"
      | 376 => RETURN "_I_P"
      | 377 => RETURN "_L_P"
      | 378 => RETURN "_L_P"
      | 379 => RETURN "_C_L_P"
      | 380 => RETURN "_L_P"
      | 381 => RETURN "_L_P"
      | 382 => RETURN "_L_P"
      | 383 => RETURN "_C_L_P"
      | 384 => RETURN "_I_P"
      | 385 => RETURN "_L_P"
      | 386 => RETURN "_L_P"
      | 387 => RETURN "_C_L_P"
      | 388 => RETURN "_L_P"
      | 389 => RETURN "_L_P"
      | 390 => RETURN "_L_P"
      | 391 => RETURN "_C_L_P"
      | 392 => RETURN "_I_P"
      | 393 => RETURN "_L_P"
      | 394 => RETURN "_L_P"
      | 395 => RETURN "_C_L_P"
      | 396 => RETURN "_L_P"
      | 397 => RETURN "_L_P"
      | 398 => RETURN "_L_P"
      | 399 => RETURN "_C_L_P"
      | 400 => RETURN "_I_P"
      | 401 => RETURN "_L_P"
      | 402 => RETURN "_L_P"
      | 403 => RETURN "_C_L_P"
      | 404 => RETURN "_L_P"
      | 405 => RETURN "_L_P"
      | 406 => RETURN "_L_P"
      | 407 => RETURN "_C_L_P"
      | 408 => RETURN "_I_P"
      | 409 => RETURN "_P"
      | 410 => RETURN "_P"
      | 411 => RETURN "_C_P"
      | 412 => RETURN "_P"
      | 413 => RETURN "_P"
      | 414 => RETURN "_P"
      | 415 => RETURN "_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

