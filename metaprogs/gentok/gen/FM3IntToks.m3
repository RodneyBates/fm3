
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
      | 147 => RETURN "ItkIdRefAtom"
      | 148 => RETURN "ItkIdRefDeclNo"
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
      | 203 => RETURN "ItkTypeDeclId"
      | 204 => RETURN "ItkTypeDeclLt"
      | 205 => RETURN "ItkTypeDeclLtTemp"
      | 206 => RETURN "ItkTypeDeclLtPatch"
      | 207 => RETURN "ItkTypeDeclRt"
      | 208 => RETURN "ItkTypeDeclDef"
      | 209 => RETURN "ItkTypeDeclDefTemp"
      | 210 => RETURN "ItkTypeDeclDefPatch"
      | 211 => RETURN "ItkREFDefLt"
      | 212 => RETURN "ItkREFDefLtTemp"
      | 213 => RETURN "ItkREFDefLtPatch"
      | 214 => RETURN "ItkREFDefRt"
      | 215 => RETURN "ItkRecDefLt"
      | 216 => RETURN "ItkRecDefLtTemp"
      | 217 => RETURN "ItkRecDefLtPatch"
      | 218 => RETURN "ItkRecDefRt"
      | 219 => RETURN "ItkRecFieldLt"
      | 220 => RETURN "ItkRecFieldLtTemp"
      | 221 => RETURN "ItkRecFieldLtPatch"
      | 222 => RETURN "ItkRecFieldRt"
      | 223 => RETURN "ItkRecFieldType"
      | 224 => RETURN "ItkRecFieldTypeTemp"
      | 225 => RETURN "ItkRecFieldTypePatch"
      | 226 => RETURN "ItkRecFieldVal"
      | 227 => RETURN "ItkRecFieldValTemp"
      | 228 => RETURN "ItkRecFieldValPatch"
      | 229 => RETURN "ItkVarDeclListLt"
      | 230 => RETURN "ItkVarDeclListLtTemp"
      | 231 => RETURN "ItkVarDeclListLtPatch"
      | 232 => RETURN "ItkVarDeclListRt"
      | 233 => RETURN "ItkVarDeclListSep"
      | 234 => RETURN "ItkVarDeclListSepTemp"
      | 235 => RETURN "ItkVarDeclListSepPatch"
      | 236 => RETURN "ItkVarDeclListElem"
      | 237 => RETURN "ItkVarDeclLt"
      | 238 => RETURN "ItkVarDeclLtTemp"
      | 239 => RETURN "ItkVarDeclLtPatch"
      | 240 => RETURN "ItkVarDeclRt"
      | 241 => RETURN "ItkVarDeclType"
      | 242 => RETURN "ItkVarDeclTypeTemp"
      | 243 => RETURN "ItkVarDeclTypePatch"
      | 244 => RETURN "ItkVarDeclVal"
      | 245 => RETURN "ItkVarDeclValTemp"
      | 246 => RETURN "ItkVarDeclValPatch"
      | 247 => RETURN "ItkVALUEFormalLt"
      | 248 => RETURN "ItkVALUEFormalLtTemp"
      | 249 => RETURN "ItkVALUEFormalLtPatch"
      | 250 => RETURN "ItkVALUEFormalRt"
      | 251 => RETURN "ItkVALUEFormalType"
      | 252 => RETURN "ItkVALUEFormalTypeTemp"
      | 253 => RETURN "ItkVALUEFormalTypePatch"
      | 254 => RETURN "ItkVALUEFormalVal"
      | 255 => RETURN "ItkVALUEFormalValTemp"
      | 256 => RETURN "ItkVALUEFormalValPatch"
      | 257 => RETURN "ItkVARFormalLt"
      | 258 => RETURN "ItkVARFormalLtTemp"
      | 259 => RETURN "ItkVARFormalLtPatch"
      | 260 => RETURN "ItkVARFormalRt"
      | 261 => RETURN "ItkVARFormalType"
      | 262 => RETURN "ItkVARFormalTypeTemp"
      | 263 => RETURN "ItkVARFormalTypePatch"
      | 264 => RETURN "ItkVARFormalVal"
      | 265 => RETURN "ItkVARFormalValTemp"
      | 266 => RETURN "ItkVARFormalValPatch"
      | 267 => RETURN "ItkROFormalLt"
      | 268 => RETURN "ItkROFormalLtTemp"
      | 269 => RETURN "ItkROFormalLtPatch"
      | 270 => RETURN "ItkROFormalRt"
      | 271 => RETURN "ItkROFormalType"
      | 272 => RETURN "ItkROFormalTypeTemp"
      | 273 => RETURN "ItkROFormalTypePatch"
      | 274 => RETURN "ItkROFormalVal"
      | 275 => RETURN "ItkROFormalValTemp"
      | 276 => RETURN "ItkROFormalValPatch"
      | 277 => RETURN "ItkExportIdListLt"
      | 278 => RETURN "ItkExportIdListLtTemp"
      | 279 => RETURN "ItkExportIdListLtPatch"
      | 280 => RETURN "ItkExportIdListRt"
      | 281 => RETURN "ItkExportIdListSep"
      | 282 => RETURN "ItkExportIdListSepTemp"
      | 283 => RETURN "ItkExportIdListSepPatch"
      | 284 => RETURN "ItkExportIdListElem"
      | 285 => RETURN "ItkGenFormalIdListLt"
      | 286 => RETURN "ItkGenFormalIdListLtTemp"
      | 287 => RETURN "ItkGenFormalIdListLtPatch"
      | 288 => RETURN "ItkGenFormalIdListRt"
      | 289 => RETURN "ItkGenFormalIdListSep"
      | 290 => RETURN "ItkGenFormalIdListSepTemp"
      | 291 => RETURN "ItkGenFormalIdListSepPatch"
      | 292 => RETURN "ItkGenFormalIdListElem"
      | 293 => RETURN "ItkGenActualIdListLt"
      | 294 => RETURN "ItkGenActualIdListLtTemp"
      | 295 => RETURN "ItkGenActualIdListLtPatch"
      | 296 => RETURN "ItkGenActualIdListRt"
      | 297 => RETURN "ItkGenActualIdListSep"
      | 298 => RETURN "ItkGenActualIdListSepTemp"
      | 299 => RETURN "ItkGenActualIdListSepPatch"
      | 300 => RETURN "ItkGenActualIdListElem"
      | 301 => RETURN "ItkVarDeclIdListLt"
      | 302 => RETURN "ItkVarDeclIdListLtTemp"
      | 303 => RETURN "ItkVarDeclIdListLtPatch"
      | 304 => RETURN "ItkVarDeclIdListRt"
      | 305 => RETURN "ItkVarDeclIdListSep"
      | 306 => RETURN "ItkVarDeclIdListSepTemp"
      | 307 => RETURN "ItkVarDeclIdListSepPatch"
      | 308 => RETURN "ItkVarDeclIdListElem"
      | 309 => RETURN "ItkRecFieldIdListLt"
      | 310 => RETURN "ItkRecFieldIdListLtTemp"
      | 311 => RETURN "ItkRecFieldIdListLtPatch"
      | 312 => RETURN "ItkRecFieldIdListRt"
      | 313 => RETURN "ItkRecFieldIdListSep"
      | 314 => RETURN "ItkRecFieldIdListSepTemp"
      | 315 => RETURN "ItkRecFieldIdListSepPatch"
      | 316 => RETURN "ItkRecFieldIdListElem"
      | 317 => RETURN "ItkFormalsListLt"
      | 318 => RETURN "ItkFormalsListLtTemp"
      | 319 => RETURN "ItkFormalsListLtPatch"
      | 320 => RETURN "ItkFormalsListRt"
      | 321 => RETURN "ItkFormalsListSep"
      | 322 => RETURN "ItkFormalsListSepTemp"
      | 323 => RETURN "ItkFormalsListSepPatch"
      | 324 => RETURN "ItkFormalsListElem"
      | 325 => RETURN "ItkVALUEFormalIdListLt"
      | 326 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 327 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 328 => RETURN "ItkVALUEFormalIdListRt"
      | 329 => RETURN "ItkVALUEFormalIdListSep"
      | 330 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 331 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 332 => RETURN "ItkVALUEFormalIdListElem"
      | 333 => RETURN "ItkVARFormalIdListLt"
      | 334 => RETURN "ItkVARFormalIdListLtTemp"
      | 335 => RETURN "ItkVARFormalIdListLtPatch"
      | 336 => RETURN "ItkVARFormalIdListRt"
      | 337 => RETURN "ItkVARFormalIdListSep"
      | 338 => RETURN "ItkVARFormalIdListSepTemp"
      | 339 => RETURN "ItkVARFormalIdListSepPatch"
      | 340 => RETURN "ItkVARFormalIdListElem"
      | 341 => RETURN "ItkROFormalIdListLt"
      | 342 => RETURN "ItkROFormalIdListLtTemp"
      | 343 => RETURN "ItkROFormalIdListLtPatch"
      | 344 => RETURN "ItkROFormalIdListRt"
      | 345 => RETURN "ItkROFormalIdListSep"
      | 346 => RETURN "ItkROFormalIdListSepTemp"
      | 347 => RETURN "ItkROFormalIdListSepPatch"
      | 348 => RETURN "ItkROFormalIdListElem"
      | 349 => RETURN "ItkDeclListLt"
      | 350 => RETURN "ItkDeclListLtTemp"
      | 351 => RETURN "ItkDeclListLtPatch"
      | 352 => RETURN "ItkDeclListRt"
      | 353 => RETURN "ItkDeclListSep"
      | 354 => RETURN "ItkDeclListSepTemp"
      | 355 => RETURN "ItkDeclListSepPatch"
      | 356 => RETURN "ItkDeclListElem"
      | 357 => RETURN "ItkBecomesLt"
      | 358 => RETURN "ItkBecomesLtTemp"
      | 359 => RETURN "ItkBecomesLtPatch"
      | 360 => RETURN "ItkBecomesRt"
      | 361 => RETURN "ItkBecomesInfix"
      | 362 => RETURN "ItkBecomesInfixTemp"
      | 363 => RETURN "ItkBecomesInfixPatch"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

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
      | 204 => RETURN "_P"
      | 205 => RETURN "_P"
      | 206 => RETURN "_C_P"
      | 207 => RETURN "_P"
      | 208 => RETURN "_P"
      | 209 => RETURN "_P"
      | 210 => RETURN "_C_P"
      | 211 => RETURN "_P"
      | 212 => RETURN "_P"
      | 213 => RETURN "_C_P"
      | 214 => RETURN "_P"
      | 215 => RETURN "_L_P"
      | 216 => RETURN "_L_P"
      | 217 => RETURN "_C_L_P"
      | 218 => RETURN "_L_P"
      | 219 => RETURN "_P"
      | 220 => RETURN "_P"
      | 221 => RETURN "_C_P"
      | 222 => RETURN "_P"
      | 223 => RETURN "_P"
      | 224 => RETURN "_P"
      | 225 => RETURN "_C_P"
      | 226 => RETURN "_P"
      | 227 => RETURN "_P"
      | 228 => RETURN "_C_P"
      | 229 => RETURN "_L_P"
      | 230 => RETURN "_L_P"
      | 231 => RETURN "_C_L_P"
      | 232 => RETURN "_L_P"
      | 233 => RETURN "_L_P"
      | 234 => RETURN "_L_P"
      | 235 => RETURN "_C_L_P"
      | 236 => RETURN "_I_P"
      | 237 => RETURN "_P"
      | 238 => RETURN "_P"
      | 239 => RETURN "_C_P"
      | 240 => RETURN "_P"
      | 241 => RETURN "_P"
      | 242 => RETURN "_P"
      | 243 => RETURN "_C_P"
      | 244 => RETURN "_P"
      | 245 => RETURN "_P"
      | 246 => RETURN "_C_P"
      | 247 => RETURN "_P"
      | 248 => RETURN "_P"
      | 249 => RETURN "_C_P"
      | 250 => RETURN "_P"
      | 251 => RETURN "_P"
      | 252 => RETURN "_P"
      | 253 => RETURN "_C_P"
      | 254 => RETURN "_P"
      | 255 => RETURN "_P"
      | 256 => RETURN "_C_P"
      | 257 => RETURN "_P"
      | 258 => RETURN "_P"
      | 259 => RETURN "_C_P"
      | 260 => RETURN "_P"
      | 261 => RETURN "_P"
      | 262 => RETURN "_P"
      | 263 => RETURN "_C_P"
      | 264 => RETURN "_P"
      | 265 => RETURN "_P"
      | 266 => RETURN "_C_P"
      | 267 => RETURN "_P"
      | 268 => RETURN "_P"
      | 269 => RETURN "_C_P"
      | 270 => RETURN "_P"
      | 271 => RETURN "_P"
      | 272 => RETURN "_P"
      | 273 => RETURN "_C_P"
      | 274 => RETURN "_P"
      | 275 => RETURN "_P"
      | 276 => RETURN "_C_P"
      | 277 => RETURN "_L_P"
      | 278 => RETURN "_L_P"
      | 279 => RETURN "_C_L_P"
      | 280 => RETURN "_L_P"
      | 281 => RETURN "_L_P"
      | 282 => RETURN "_L_P"
      | 283 => RETURN "_C_L_P"
      | 284 => RETURN "_I_P"
      | 285 => RETURN "_L_P"
      | 286 => RETURN "_L_P"
      | 287 => RETURN "_C_L_P"
      | 288 => RETURN "_L_P"
      | 289 => RETURN "_L_P"
      | 290 => RETURN "_L_P"
      | 291 => RETURN "_C_L_P"
      | 292 => RETURN "_I_P"
      | 293 => RETURN "_L_P"
      | 294 => RETURN "_L_P"
      | 295 => RETURN "_C_L_P"
      | 296 => RETURN "_L_P"
      | 297 => RETURN "_L_P"
      | 298 => RETURN "_L_P"
      | 299 => RETURN "_C_L_P"
      | 300 => RETURN "_I_P"
      | 301 => RETURN "_L_P"
      | 302 => RETURN "_L_P"
      | 303 => RETURN "_C_L_P"
      | 304 => RETURN "_L_P"
      | 305 => RETURN "_L_P"
      | 306 => RETURN "_L_P"
      | 307 => RETURN "_C_L_P"
      | 308 => RETURN "_I_P"
      | 309 => RETURN "_L_P"
      | 310 => RETURN "_L_P"
      | 311 => RETURN "_C_L_P"
      | 312 => RETURN "_L_P"
      | 313 => RETURN "_L_P"
      | 314 => RETURN "_L_P"
      | 315 => RETURN "_C_L_P"
      | 316 => RETURN "_I_P"
      | 317 => RETURN "_L_P"
      | 318 => RETURN "_L_P"
      | 319 => RETURN "_C_L_P"
      | 320 => RETURN "_L_P"
      | 321 => RETURN "_L_P"
      | 322 => RETURN "_L_P"
      | 323 => RETURN "_C_L_P"
      | 324 => RETURN "_I_P"
      | 325 => RETURN "_L_P"
      | 326 => RETURN "_L_P"
      | 327 => RETURN "_C_L_P"
      | 328 => RETURN "_L_P"
      | 329 => RETURN "_L_P"
      | 330 => RETURN "_L_P"
      | 331 => RETURN "_C_L_P"
      | 332 => RETURN "_I_P"
      | 333 => RETURN "_L_P"
      | 334 => RETURN "_L_P"
      | 335 => RETURN "_C_L_P"
      | 336 => RETURN "_L_P"
      | 337 => RETURN "_L_P"
      | 338 => RETURN "_L_P"
      | 339 => RETURN "_C_L_P"
      | 340 => RETURN "_I_P"
      | 341 => RETURN "_L_P"
      | 342 => RETURN "_L_P"
      | 343 => RETURN "_C_L_P"
      | 344 => RETURN "_L_P"
      | 345 => RETURN "_L_P"
      | 346 => RETURN "_L_P"
      | 347 => RETURN "_C_L_P"
      | 348 => RETURN "_I_P"
      | 349 => RETURN "_L_P"
      | 350 => RETURN "_L_P"
      | 351 => RETURN "_C_L_P"
      | 352 => RETURN "_L_P"
      | 353 => RETURN "_L_P"
      | 354 => RETURN "_L_P"
      | 355 => RETURN "_C_L_P"
      | 356 => RETURN "_I_P"
      | 357 => RETURN "_P"
      | 358 => RETURN "_P"
      | 359 => RETURN "_C_P"
      | 360 => RETURN "_P"
      | 361 => RETURN "_P"
      | 362 => RETURN "_P"
      | 363 => RETURN "_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

