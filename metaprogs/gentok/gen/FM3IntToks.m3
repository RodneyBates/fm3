
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
; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 128 => RETURN "ItkNull"
      | 129 => RETURN "ItkBOF"
      | 130 => RETURN "ItkEOF"
      | 131 => RETURN "ItkLeftEnd"
      | 132 => RETURN "ItkRightEnd"
      | 133 => RETURN "ItkLeftEndIncomplete"
      | 134 => RETURN "ItkRightEndIncomplete"
      | 135 => RETURN "ItkImport"
      | 136 => RETURN "ItkFromImport"
      | 137 => RETURN "ItkImportAs"
      | 138 => RETURN "ItkFormalsListEmpty"
      | 139 => RETURN "ItkFormalTypeAbsent"
      | 140 => RETURN "ItkFormalExprAbsent"
      | 141 => RETURN "ItkRaisesSetAbsent"
      | 142 => RETURN "ItkRaisesANY"
      | 143 => RETURN "ItkResultTypeAbsent"
      | 144 => RETURN "ItkProcBodyAbsent"
      | 145 => RETURN "ItkScopeLt"
      | 146 => RETURN "ItkScopeRt"
      | 147 => RETURN "ItkRefId"
      | 148 => RETURN "ItkRefNo"
      | 149 => RETURN "ItkDuplDeclId"
      | 150 => RETURN "ItkDeclId"
      | 151 => RETURN "ItkDeclNo"
      | 152 => RETURN "ItkQualIdLt"
      | 153 => RETURN "ItkQualIdLtTemp"
      | 154 => RETURN "ItkQualIdLtPatch"
      | 155 => RETURN "ItkQualIdRt"
      | 156 => RETURN "ItkFuncSignatureLt"
      | 157 => RETURN "ItkFuncSignatureLtTemp"
      | 158 => RETURN "ItkFuncSignatureLtPatch"
      | 159 => RETURN "ItkFuncSignatureRt"
      | 160 => RETURN "ItkProcSignatureLt"
      | 161 => RETURN "ItkProcSignatureLtTemp"
      | 162 => RETURN "ItkProcSignatureLtPatch"
      | 163 => RETURN "ItkProcSignatureRt"
      | 164 => RETURN "ItkFormalTypeLt"
      | 165 => RETURN "ItkFormalTypeLtTemp"
      | 166 => RETURN "ItkFormalTypeLtPatch"
      | 167 => RETURN "ItkFormalTypeRt"
      | 168 => RETURN "ItkFormalExprLt"
      | 169 => RETURN "ItkFormalExprLtTemp"
      | 170 => RETURN "ItkFormalExprLtPatch"
      | 171 => RETURN "ItkFormalExprRt"
      | 172 => RETURN "ItkResultTypeLt"
      | 173 => RETURN "ItkResultTypeLtTemp"
      | 174 => RETURN "ItkResultTypeLtPatch"
      | 175 => RETURN "ItkResultTypeRt"
      | 176 => RETURN "ItkRaisesSetLt"
      | 177 => RETURN "ItkRaisesSetLtTemp"
      | 178 => RETURN "ItkRaisesSetLtPatch"
      | 179 => RETURN "ItkRaisesSetRt"
      | 180 => RETURN "ItkProcNoBodyLt"
      | 181 => RETURN "ItkProcNoBodyLtTemp"
      | 182 => RETURN "ItkProcNoBodyLtPatch"
      | 183 => RETURN "ItkProcNoBodyRt"
      | 184 => RETURN "ItkProcWBodyLt"
      | 185 => RETURN "ItkProcWBodyLtTemp"
      | 186 => RETURN "ItkProcWBodyLtPatch"
      | 187 => RETURN "ItkProcWBodyRt"
      | 188 => RETURN "ItkProcBodyLt"
      | 189 => RETURN "ItkProcBodyLtTemp"
      | 190 => RETURN "ItkProcBodyLtPatch"
      | 191 => RETURN "ItkProcBodyRt"
      | 192 => RETURN "ItkProcTypeLt"
      | 193 => RETURN "ItkProcTypeLtTemp"
      | 194 => RETURN "ItkProcTypeLtPatch"
      | 195 => RETURN "ItkProcTypeRt"
      | 196 => RETURN "ItkBlockLt"
      | 197 => RETURN "ItkBlockLtTemp"
      | 198 => RETURN "ItkBlockLtPatch"
      | 199 => RETURN "ItkBlockRt"
      | 200 => RETURN "ItkBlockBeg"
      | 201 => RETURN "ItkBlockBegTemp"
      | 202 => RETURN "ItkBlockBegPatch"
      | 203 => RETURN "ItkExportId"
      | 204 => RETURN "ItkExportIdSep"
      | 205 => RETURN "ItkVarDeclId"
      | 206 => RETURN "ItkRecFieldId"
      | 207 => RETURN "ItkVALUEFormalId"
      | 208 => RETURN "ItkVARFormalId"
      | 209 => RETURN "ItkROFormalId"
      | 210 => RETURN "ItkRecDefLt"
      | 211 => RETURN "ItkRecDefLtTemp"
      | 212 => RETURN "ItkRecDefLtPatch"
      | 213 => RETURN "ItkRecDefRt"
      | 214 => RETURN "ItkRecFieldLt"
      | 215 => RETURN "ItkRecFieldLtTemp"
      | 216 => RETURN "ItkRecFieldLtPatch"
      | 217 => RETURN "ItkRecFieldRt"
      | 218 => RETURN "ItkRecFieldType"
      | 219 => RETURN "ItkRecFieldTypeTemp"
      | 220 => RETURN "ItkRecFieldTypePatch"
      | 221 => RETURN "ItkRecFieldVal"
      | 222 => RETURN "ItkRecFieldValTemp"
      | 223 => RETURN "ItkRecFieldValPatch"
      | 224 => RETURN "ItkTypeDeclLt"
      | 225 => RETURN "ItkTypeDeclLtTemp"
      | 226 => RETURN "ItkTypeDeclLtPatch"
      | 227 => RETURN "ItkTypeDeclRt"
      | 228 => RETURN "ItkTypeDeclEq"
      | 229 => RETURN "ItkTypeDeclEqTemp"
      | 230 => RETURN "ItkTypeDeclEqPatch"
      | 231 => RETURN "ItkVarDeclListLt"
      | 232 => RETURN "ItkVarDeclListLtTemp"
      | 233 => RETURN "ItkVarDeclListLtPatch"
      | 234 => RETURN "ItkVarDeclListRt"
      | 235 => RETURN "ItkVarDeclListSep"
      | 236 => RETURN "ItkVarDeclListSepTemp"
      | 237 => RETURN "ItkVarDeclListSepPatch"
      | 238 => RETURN "ItkVarDeclListElem"
      | 239 => RETURN "ItkVarDeclLt"
      | 240 => RETURN "ItkVarDeclLtTemp"
      | 241 => RETURN "ItkVarDeclLtPatch"
      | 242 => RETURN "ItkVarDeclRt"
      | 243 => RETURN "ItkVarDeclType"
      | 244 => RETURN "ItkVarDeclTypeTemp"
      | 245 => RETURN "ItkVarDeclTypePatch"
      | 246 => RETURN "ItkVarDeclVal"
      | 247 => RETURN "ItkVarDeclValTemp"
      | 248 => RETURN "ItkVarDeclValPatch"
      | 249 => RETURN "ItkVALUEFormalLt"
      | 250 => RETURN "ItkVALUEFormalLtTemp"
      | 251 => RETURN "ItkVALUEFormalLtPatch"
      | 252 => RETURN "ItkVALUEFormalRt"
      | 253 => RETURN "ItkVALUEFormalType"
      | 254 => RETURN "ItkVALUEFormalTypeTemp"
      | 255 => RETURN "ItkVALUEFormalTypePatch"
      | 256 => RETURN "ItkVALUEFormalVal"
      | 257 => RETURN "ItkVALUEFormalValTemp"
      | 258 => RETURN "ItkVALUEFormalValPatch"
      | 259 => RETURN "ItkVARFormalLt"
      | 260 => RETURN "ItkVARFormalLtTemp"
      | 261 => RETURN "ItkVARFormalLtPatch"
      | 262 => RETURN "ItkVARFormalRt"
      | 263 => RETURN "ItkVARFormalType"
      | 264 => RETURN "ItkVARFormalTypeTemp"
      | 265 => RETURN "ItkVARFormalTypePatch"
      | 266 => RETURN "ItkVARFormalVal"
      | 267 => RETURN "ItkVARFormalValTemp"
      | 268 => RETURN "ItkVARFormalValPatch"
      | 269 => RETURN "ItkROFormalLt"
      | 270 => RETURN "ItkROFormalLtTemp"
      | 271 => RETURN "ItkROFormalLtPatch"
      | 272 => RETURN "ItkROFormalRt"
      | 273 => RETURN "ItkROFormalType"
      | 274 => RETURN "ItkROFormalTypeTemp"
      | 275 => RETURN "ItkROFormalTypePatch"
      | 276 => RETURN "ItkROFormalVal"
      | 277 => RETURN "ItkROFormalValTemp"
      | 278 => RETURN "ItkROFormalValPatch"
      | 279 => RETURN "ItkExportIdListLt"
      | 280 => RETURN "ItkExportIdListLtTemp"
      | 281 => RETURN "ItkExportIdListLtPatch"
      | 282 => RETURN "ItkExportIdListRt"
      | 283 => RETURN "ItkExportIdListSep"
      | 284 => RETURN "ItkExportIdListSepTemp"
      | 285 => RETURN "ItkExportIdListSepPatch"
      | 286 => RETURN "ItkExportIdListElem"
      | 287 => RETURN "ItkGenFormalIdListLt"
      | 288 => RETURN "ItkGenFormalIdListLtTemp"
      | 289 => RETURN "ItkGenFormalIdListLtPatch"
      | 290 => RETURN "ItkGenFormalIdListRt"
      | 291 => RETURN "ItkGenFormalIdListSep"
      | 292 => RETURN "ItkGenFormalIdListSepTemp"
      | 293 => RETURN "ItkGenFormalIdListSepPatch"
      | 294 => RETURN "ItkGenFormalIdListElem"
      | 295 => RETURN "ItkGenActualIdListLt"
      | 296 => RETURN "ItkGenActualIdListLtTemp"
      | 297 => RETURN "ItkGenActualIdListLtPatch"
      | 298 => RETURN "ItkGenActualIdListRt"
      | 299 => RETURN "ItkGenActualIdListSep"
      | 300 => RETURN "ItkGenActualIdListSepTemp"
      | 301 => RETURN "ItkGenActualIdListSepPatch"
      | 302 => RETURN "ItkGenActualIdListElem"
      | 303 => RETURN "ItkVarDeclIdListLt"
      | 304 => RETURN "ItkVarDeclIdListLtTemp"
      | 305 => RETURN "ItkVarDeclIdListLtPatch"
      | 306 => RETURN "ItkVarDeclIdListRt"
      | 307 => RETURN "ItkVarDeclIdListSep"
      | 308 => RETURN "ItkVarDeclIdListSepTemp"
      | 309 => RETURN "ItkVarDeclIdListSepPatch"
      | 310 => RETURN "ItkVarDeclIdListElem"
      | 311 => RETURN "ItkRecFieldIdListLt"
      | 312 => RETURN "ItkRecFieldIdListLtTemp"
      | 313 => RETURN "ItkRecFieldIdListLtPatch"
      | 314 => RETURN "ItkRecFieldIdListRt"
      | 315 => RETURN "ItkRecFieldIdListSep"
      | 316 => RETURN "ItkRecFieldIdListSepTemp"
      | 317 => RETURN "ItkRecFieldIdListSepPatch"
      | 318 => RETURN "ItkRecFieldIdListElem"
      | 319 => RETURN "ItkFormalsListLt"
      | 320 => RETURN "ItkFormalsListLtTemp"
      | 321 => RETURN "ItkFormalsListLtPatch"
      | 322 => RETURN "ItkFormalsListRt"
      | 323 => RETURN "ItkFormalsListSep"
      | 324 => RETURN "ItkFormalsListSepTemp"
      | 325 => RETURN "ItkFormalsListSepPatch"
      | 326 => RETURN "ItkFormalsListElem"
      | 327 => RETURN "ItkVALUEFormalIdListLt"
      | 328 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 329 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 330 => RETURN "ItkVALUEFormalIdListRt"
      | 331 => RETURN "ItkVALUEFormalIdListSep"
      | 332 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 333 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 334 => RETURN "ItkVALUEFormalIdListElem"
      | 335 => RETURN "ItkVARFormalIdListLt"
      | 336 => RETURN "ItkVARFormalIdListLtTemp"
      | 337 => RETURN "ItkVARFormalIdListLtPatch"
      | 338 => RETURN "ItkVARFormalIdListRt"
      | 339 => RETURN "ItkVARFormalIdListSep"
      | 340 => RETURN "ItkVARFormalIdListSepTemp"
      | 341 => RETURN "ItkVARFormalIdListSepPatch"
      | 342 => RETURN "ItkVARFormalIdListElem"
      | 343 => RETURN "ItkROFormalIdListLt"
      | 344 => RETURN "ItkROFormalIdListLtTemp"
      | 345 => RETURN "ItkROFormalIdListLtPatch"
      | 346 => RETURN "ItkROFormalIdListRt"
      | 347 => RETURN "ItkROFormalIdListSep"
      | 348 => RETURN "ItkROFormalIdListSepTemp"
      | 349 => RETURN "ItkROFormalIdListSepPatch"
      | 350 => RETURN "ItkROFormalIdListElem"
      | 351 => RETURN "ItkDeclListLt"
      | 352 => RETURN "ItkDeclListLtTemp"
      | 353 => RETURN "ItkDeclListLtPatch"
      | 354 => RETURN "ItkDeclListRt"
      | 355 => RETURN "ItkDeclListSep"
      | 356 => RETURN "ItkDeclListSepTemp"
      | 357 => RETURN "ItkDeclListSepPatch"
      | 358 => RETURN "ItkDeclListElem"
      | 359 => RETURN "ItkBecomesLt"
      | 360 => RETURN "ItkBecomesLtTemp"
      | 361 => RETURN "ItkBecomesLtPatch"
      | 362 => RETURN "ItkBecomesRt"
      | 363 => RETURN "ItkBecomesInfix"
      | 364 => RETURN "ItkBecomesInfixTemp"
      | 365 => RETURN "ItkBecomesInfixPatch"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Image

(*EXPORTED*)
; PROCEDURE Operands ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 128 => RETURN ""
      | 129 => RETURN ""
      | 130 => RETURN ""
      | 131 => RETURN ""
      | 132 => RETURN ""
      | 133 => RETURN ""
      | 134 => RETURN ""
      | 135 => RETURN "_I_P"
      | 136 => RETURN "_I_P_I_P"
      | 137 => RETURN "_I_P_I_P"
      | 138 => RETURN ""
      | 139 => RETURN "_P"
      | 140 => RETURN "_P"
      | 141 => RETURN "_P"
      | 142 => RETURN "_P"
      | 143 => RETURN "_P"
      | 144 => RETURN "_P"
      | 145 => RETURN "_L"
      | 146 => RETURN "_L"
      | 147 => RETURN "_I_P"
      | 148 => RETURN "_D_P"
      | 149 => RETURN "_I_P"
      | 150 => RETURN "_L_I_P"
      | 151 => RETURN "_D_P"
      | 152 => RETURN ""
      | 153 => RETURN ""
      | 154 => RETURN "_C"
      | 155 => RETURN ""
      | 156 => RETURN "_P"
      | 157 => RETURN "_P"
      | 158 => RETURN "_C_P"
      | 159 => RETURN "_P"
      | 160 => RETURN "_P"
      | 161 => RETURN "_P"
      | 162 => RETURN "_C_P"
      | 163 => RETURN "_P"
      | 164 => RETURN "_P"
      | 165 => RETURN "_P"
      | 166 => RETURN "_C_P"
      | 167 => RETURN "_P"
      | 168 => RETURN "_P"
      | 169 => RETURN "_P"
      | 170 => RETURN "_C_P"
      | 171 => RETURN "_P"
      | 172 => RETURN "_P"
      | 173 => RETURN "_P"
      | 174 => RETURN "_C_P"
      | 175 => RETURN "_P"
      | 176 => RETURN "_P"
      | 177 => RETURN "_P"
      | 178 => RETURN "_C_P"
      | 179 => RETURN "_P"
      | 180 => RETURN "_L"
      | 181 => RETURN "_L"
      | 182 => RETURN "_C_L"
      | 183 => RETURN "_L"
      | 184 => RETURN "_L"
      | 185 => RETURN "_L"
      | 186 => RETURN "_C_L"
      | 187 => RETURN "_L"
      | 188 => RETURN "_P"
      | 189 => RETURN "_P"
      | 190 => RETURN "_C_P"
      | 191 => RETURN "_P"
      | 192 => RETURN "_P"
      | 193 => RETURN "_P"
      | 194 => RETURN "_C_P"
      | 195 => RETURN "_P"
      | 196 => RETURN "_P"
      | 197 => RETURN "_P"
      | 198 => RETURN "_C_P"
      | 199 => RETURN "_P"
      | 200 => RETURN "_P"
      | 201 => RETURN "_P"
      | 202 => RETURN "_C_P"
      | 203 => RETURN "_I_P"
      | 204 => RETURN "_I_P"
      | 205 => RETURN "_I_P"
      | 206 => RETURN "_I_P"
      | 207 => RETURN "_I_P"
      | 208 => RETURN "_I_P"
      | 209 => RETURN "_I_P"
      | 210 => RETURN "_P_L"
      | 211 => RETURN "_P_L"
      | 212 => RETURN "_C_P_L"
      | 213 => RETURN "_P_L"
      | 214 => RETURN "_P"
      | 215 => RETURN "_P"
      | 216 => RETURN "_C_P"
      | 217 => RETURN "_P"
      | 218 => RETURN "_P"
      | 219 => RETURN "_P"
      | 220 => RETURN "_C_P"
      | 221 => RETURN "_P"
      | 222 => RETURN "_P"
      | 223 => RETURN "_C_P"
      | 224 => RETURN "_P"
      | 225 => RETURN "_P"
      | 226 => RETURN "_C_P"
      | 227 => RETURN "_P"
      | 228 => RETURN ""
      | 229 => RETURN ""
      | 230 => RETURN "_C"
      | 231 => RETURN "_L_P"
      | 232 => RETURN "_L_P"
      | 233 => RETURN "_C_L_P"
      | 234 => RETURN "_L_P"
      | 235 => RETURN "_L_P"
      | 236 => RETURN "_L_P"
      | 237 => RETURN "_C_L_P"
      | 238 => RETURN "_I_P"
      | 239 => RETURN "_P"
      | 240 => RETURN "_P"
      | 241 => RETURN "_C_P"
      | 242 => RETURN "_P"
      | 243 => RETURN "_P"
      | 244 => RETURN "_P"
      | 245 => RETURN "_C_P"
      | 246 => RETURN "_P"
      | 247 => RETURN "_P"
      | 248 => RETURN "_C_P"
      | 249 => RETURN "_P"
      | 250 => RETURN "_P"
      | 251 => RETURN "_C_P"
      | 252 => RETURN "_P"
      | 253 => RETURN "_P"
      | 254 => RETURN "_P"
      | 255 => RETURN "_C_P"
      | 256 => RETURN "_P"
      | 257 => RETURN "_P"
      | 258 => RETURN "_C_P"
      | 259 => RETURN "_P"
      | 260 => RETURN "_P"
      | 261 => RETURN "_C_P"
      | 262 => RETURN "_P"
      | 263 => RETURN "_P"
      | 264 => RETURN "_P"
      | 265 => RETURN "_C_P"
      | 266 => RETURN "_P"
      | 267 => RETURN "_P"
      | 268 => RETURN "_C_P"
      | 269 => RETURN "_P"
      | 270 => RETURN "_P"
      | 271 => RETURN "_C_P"
      | 272 => RETURN "_P"
      | 273 => RETURN "_P"
      | 274 => RETURN "_P"
      | 275 => RETURN "_C_P"
      | 276 => RETURN "_P"
      | 277 => RETURN "_P"
      | 278 => RETURN "_C_P"
      | 279 => RETURN "_L_P"
      | 280 => RETURN "_L_P"
      | 281 => RETURN "_C_L_P"
      | 282 => RETURN "_L_P"
      | 283 => RETURN "_L_P"
      | 284 => RETURN "_L_P"
      | 285 => RETURN "_C_L_P"
      | 286 => RETURN "_I_P"
      | 287 => RETURN "_L_P"
      | 288 => RETURN "_L_P"
      | 289 => RETURN "_C_L_P"
      | 290 => RETURN "_L_P"
      | 291 => RETURN "_L_P"
      | 292 => RETURN "_L_P"
      | 293 => RETURN "_C_L_P"
      | 294 => RETURN "_I_P"
      | 295 => RETURN "_L_P"
      | 296 => RETURN "_L_P"
      | 297 => RETURN "_C_L_P"
      | 298 => RETURN "_L_P"
      | 299 => RETURN "_L_P"
      | 300 => RETURN "_L_P"
      | 301 => RETURN "_C_L_P"
      | 302 => RETURN "_I_P"
      | 303 => RETURN "_L_P"
      | 304 => RETURN "_L_P"
      | 305 => RETURN "_C_L_P"
      | 306 => RETURN "_L_P"
      | 307 => RETURN "_L_P"
      | 308 => RETURN "_L_P"
      | 309 => RETURN "_C_L_P"
      | 310 => RETURN "_I_P"
      | 311 => RETURN "_L_P"
      | 312 => RETURN "_L_P"
      | 313 => RETURN "_C_L_P"
      | 314 => RETURN "_L_P"
      | 315 => RETURN "_L_P"
      | 316 => RETURN "_L_P"
      | 317 => RETURN "_C_L_P"
      | 318 => RETURN "_I_P"
      | 319 => RETURN "_L_P"
      | 320 => RETURN "_L_P"
      | 321 => RETURN "_C_L_P"
      | 322 => RETURN "_L_P"
      | 323 => RETURN "_L_P"
      | 324 => RETURN "_L_P"
      | 325 => RETURN "_C_L_P"
      | 326 => RETURN "_I_P"
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
      | 359 => RETURN "_P"
      | 360 => RETURN "_P"
      | 361 => RETURN "_C_P"
      | 362 => RETURN "_P"
      | 363 => RETURN "_P"
      | 364 => RETURN "_P"
      | 365 => RETURN "_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

