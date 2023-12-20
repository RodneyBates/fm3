
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
      | 203 => RETURN "ItkRecDefLt"
      | 204 => RETURN "ItkRecDefLtTemp"
      | 205 => RETURN "ItkRecDefLtPatch"
      | 206 => RETURN "ItkRecDefRt"
      | 207 => RETURN "ItkRecFieldLt"
      | 208 => RETURN "ItkRecFieldLtTemp"
      | 209 => RETURN "ItkRecFieldLtPatch"
      | 210 => RETURN "ItkRecFieldRt"
      | 211 => RETURN "ItkRecFieldType"
      | 212 => RETURN "ItkRecFieldTypeTemp"
      | 213 => RETURN "ItkRecFieldTypePatch"
      | 214 => RETURN "ItkRecFieldVal"
      | 215 => RETURN "ItkRecFieldValTemp"
      | 216 => RETURN "ItkRecFieldValPatch"
      | 217 => RETURN "ItkTypeDeclLt"
      | 218 => RETURN "ItkTypeDeclLtTemp"
      | 219 => RETURN "ItkTypeDeclLtPatch"
      | 220 => RETURN "ItkTypeDeclRt"
      | 221 => RETURN "ItkTypeDeclEq"
      | 222 => RETURN "ItkTypeDeclEqTemp"
      | 223 => RETURN "ItkTypeDeclEqPatch"
      | 224 => RETURN "ItkVarDeclListLt"
      | 225 => RETURN "ItkVarDeclListLtTemp"
      | 226 => RETURN "ItkVarDeclListLtPatch"
      | 227 => RETURN "ItkVarDeclListRt"
      | 228 => RETURN "ItkVarDeclListSep"
      | 229 => RETURN "ItkVarDeclListSepTemp"
      | 230 => RETURN "ItkVarDeclListSepPatch"
      | 231 => RETURN "ItkVarDeclListElem"
      | 232 => RETURN "ItkVarDeclLt"
      | 233 => RETURN "ItkVarDeclLtTemp"
      | 234 => RETURN "ItkVarDeclLtPatch"
      | 235 => RETURN "ItkVarDeclRt"
      | 236 => RETURN "ItkVarDeclType"
      | 237 => RETURN "ItkVarDeclTypeTemp"
      | 238 => RETURN "ItkVarDeclTypePatch"
      | 239 => RETURN "ItkVarDeclVal"
      | 240 => RETURN "ItkVarDeclValTemp"
      | 241 => RETURN "ItkVarDeclValPatch"
      | 242 => RETURN "ItkVALUEFormalLt"
      | 243 => RETURN "ItkVALUEFormalLtTemp"
      | 244 => RETURN "ItkVALUEFormalLtPatch"
      | 245 => RETURN "ItkVALUEFormalRt"
      | 246 => RETURN "ItkVALUEFormalType"
      | 247 => RETURN "ItkVALUEFormalTypeTemp"
      | 248 => RETURN "ItkVALUEFormalTypePatch"
      | 249 => RETURN "ItkVALUEFormalVal"
      | 250 => RETURN "ItkVALUEFormalValTemp"
      | 251 => RETURN "ItkVALUEFormalValPatch"
      | 252 => RETURN "ItkVARFormalLt"
      | 253 => RETURN "ItkVARFormalLtTemp"
      | 254 => RETURN "ItkVARFormalLtPatch"
      | 255 => RETURN "ItkVARFormalRt"
      | 256 => RETURN "ItkVARFormalType"
      | 257 => RETURN "ItkVARFormalTypeTemp"
      | 258 => RETURN "ItkVARFormalTypePatch"
      | 259 => RETURN "ItkVARFormalVal"
      | 260 => RETURN "ItkVARFormalValTemp"
      | 261 => RETURN "ItkVARFormalValPatch"
      | 262 => RETURN "ItkROFormalLt"
      | 263 => RETURN "ItkROFormalLtTemp"
      | 264 => RETURN "ItkROFormalLtPatch"
      | 265 => RETURN "ItkROFormalRt"
      | 266 => RETURN "ItkROFormalType"
      | 267 => RETURN "ItkROFormalTypeTemp"
      | 268 => RETURN "ItkROFormalTypePatch"
      | 269 => RETURN "ItkROFormalVal"
      | 270 => RETURN "ItkROFormalValTemp"
      | 271 => RETURN "ItkROFormalValPatch"
      | 272 => RETURN "ItkExportIdListLt"
      | 273 => RETURN "ItkExportIdListLtTemp"
      | 274 => RETURN "ItkExportIdListLtPatch"
      | 275 => RETURN "ItkExportIdListRt"
      | 276 => RETURN "ItkExportIdListSep"
      | 277 => RETURN "ItkExportIdListSepTemp"
      | 278 => RETURN "ItkExportIdListSepPatch"
      | 279 => RETURN "ItkExportIdListElem"
      | 280 => RETURN "ItkGenFormalIdListLt"
      | 281 => RETURN "ItkGenFormalIdListLtTemp"
      | 282 => RETURN "ItkGenFormalIdListLtPatch"
      | 283 => RETURN "ItkGenFormalIdListRt"
      | 284 => RETURN "ItkGenFormalIdListSep"
      | 285 => RETURN "ItkGenFormalIdListSepTemp"
      | 286 => RETURN "ItkGenFormalIdListSepPatch"
      | 287 => RETURN "ItkGenFormalIdListElem"
      | 288 => RETURN "ItkGenActualIdListLt"
      | 289 => RETURN "ItkGenActualIdListLtTemp"
      | 290 => RETURN "ItkGenActualIdListLtPatch"
      | 291 => RETURN "ItkGenActualIdListRt"
      | 292 => RETURN "ItkGenActualIdListSep"
      | 293 => RETURN "ItkGenActualIdListSepTemp"
      | 294 => RETURN "ItkGenActualIdListSepPatch"
      | 295 => RETURN "ItkGenActualIdListElem"
      | 296 => RETURN "ItkVarDeclIdListLt"
      | 297 => RETURN "ItkVarDeclIdListLtTemp"
      | 298 => RETURN "ItkVarDeclIdListLtPatch"
      | 299 => RETURN "ItkVarDeclIdListRt"
      | 300 => RETURN "ItkVarDeclIdListSep"
      | 301 => RETURN "ItkVarDeclIdListSepTemp"
      | 302 => RETURN "ItkVarDeclIdListSepPatch"
      | 303 => RETURN "ItkVarDeclIdListElem"
      | 304 => RETURN "ItkRecFieldIdListLt"
      | 305 => RETURN "ItkRecFieldIdListLtTemp"
      | 306 => RETURN "ItkRecFieldIdListLtPatch"
      | 307 => RETURN "ItkRecFieldIdListRt"
      | 308 => RETURN "ItkRecFieldIdListSep"
      | 309 => RETURN "ItkRecFieldIdListSepTemp"
      | 310 => RETURN "ItkRecFieldIdListSepPatch"
      | 311 => RETURN "ItkRecFieldIdListElem"
      | 312 => RETURN "ItkFormalsListLt"
      | 313 => RETURN "ItkFormalsListLtTemp"
      | 314 => RETURN "ItkFormalsListLtPatch"
      | 315 => RETURN "ItkFormalsListRt"
      | 316 => RETURN "ItkFormalsListSep"
      | 317 => RETURN "ItkFormalsListSepTemp"
      | 318 => RETURN "ItkFormalsListSepPatch"
      | 319 => RETURN "ItkFormalsListElem"
      | 320 => RETURN "ItkVALUEFormalIdListLt"
      | 321 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 322 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 323 => RETURN "ItkVALUEFormalIdListRt"
      | 324 => RETURN "ItkVALUEFormalIdListSep"
      | 325 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 326 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 327 => RETURN "ItkVALUEFormalIdListElem"
      | 328 => RETURN "ItkVARFormalIdListLt"
      | 329 => RETURN "ItkVARFormalIdListLtTemp"
      | 330 => RETURN "ItkVARFormalIdListLtPatch"
      | 331 => RETURN "ItkVARFormalIdListRt"
      | 332 => RETURN "ItkVARFormalIdListSep"
      | 333 => RETURN "ItkVARFormalIdListSepTemp"
      | 334 => RETURN "ItkVARFormalIdListSepPatch"
      | 335 => RETURN "ItkVARFormalIdListElem"
      | 336 => RETURN "ItkROFormalIdListLt"
      | 337 => RETURN "ItkROFormalIdListLtTemp"
      | 338 => RETURN "ItkROFormalIdListLtPatch"
      | 339 => RETURN "ItkROFormalIdListRt"
      | 340 => RETURN "ItkROFormalIdListSep"
      | 341 => RETURN "ItkROFormalIdListSepTemp"
      | 342 => RETURN "ItkROFormalIdListSepPatch"
      | 343 => RETURN "ItkROFormalIdListElem"
      | 344 => RETURN "ItkDeclListLt"
      | 345 => RETURN "ItkDeclListLtTemp"
      | 346 => RETURN "ItkDeclListLtPatch"
      | 347 => RETURN "ItkDeclListRt"
      | 348 => RETURN "ItkDeclListSep"
      | 349 => RETURN "ItkDeclListSepTemp"
      | 350 => RETURN "ItkDeclListSepPatch"
      | 351 => RETURN "ItkDeclListElem"
      | 352 => RETURN "ItkBecomesLt"
      | 353 => RETURN "ItkBecomesLtTemp"
      | 354 => RETURN "ItkBecomesLtPatch"
      | 355 => RETURN "ItkBecomesRt"
      | 356 => RETURN "ItkBecomesInfix"
      | 357 => RETURN "ItkBecomesInfixTemp"
      | 358 => RETURN "ItkBecomesInfixPatch"
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
      | 203 => RETURN "_L_P"
      | 204 => RETURN "_L_P"
      | 205 => RETURN "_C_L_P"
      | 206 => RETURN "_L_P"
      | 207 => RETURN "_P"
      | 208 => RETURN "_P"
      | 209 => RETURN "_C_P"
      | 210 => RETURN "_P"
      | 211 => RETURN "_P"
      | 212 => RETURN "_P"
      | 213 => RETURN "_C_P"
      | 214 => RETURN "_P"
      | 215 => RETURN "_P"
      | 216 => RETURN "_C_P"
      | 217 => RETURN "_P"
      | 218 => RETURN "_P"
      | 219 => RETURN "_C_P"
      | 220 => RETURN "_P"
      | 221 => RETURN ""
      | 222 => RETURN ""
      | 223 => RETURN "_C"
      | 224 => RETURN "_L_P"
      | 225 => RETURN "_L_P"
      | 226 => RETURN "_C_L_P"
      | 227 => RETURN "_L_P"
      | 228 => RETURN "_L_P"
      | 229 => RETURN "_L_P"
      | 230 => RETURN "_C_L_P"
      | 231 => RETURN "_I_P"
      | 232 => RETURN "_P"
      | 233 => RETURN "_P"
      | 234 => RETURN "_C_P"
      | 235 => RETURN "_P"
      | 236 => RETURN "_P"
      | 237 => RETURN "_P"
      | 238 => RETURN "_C_P"
      | 239 => RETURN "_P"
      | 240 => RETURN "_P"
      | 241 => RETURN "_C_P"
      | 242 => RETURN "_P"
      | 243 => RETURN "_P"
      | 244 => RETURN "_C_P"
      | 245 => RETURN "_P"
      | 246 => RETURN "_P"
      | 247 => RETURN "_P"
      | 248 => RETURN "_C_P"
      | 249 => RETURN "_P"
      | 250 => RETURN "_P"
      | 251 => RETURN "_C_P"
      | 252 => RETURN "_P"
      | 253 => RETURN "_P"
      | 254 => RETURN "_C_P"
      | 255 => RETURN "_P"
      | 256 => RETURN "_P"
      | 257 => RETURN "_P"
      | 258 => RETURN "_C_P"
      | 259 => RETURN "_P"
      | 260 => RETURN "_P"
      | 261 => RETURN "_C_P"
      | 262 => RETURN "_P"
      | 263 => RETURN "_P"
      | 264 => RETURN "_C_P"
      | 265 => RETURN "_P"
      | 266 => RETURN "_P"
      | 267 => RETURN "_P"
      | 268 => RETURN "_C_P"
      | 269 => RETURN "_P"
      | 270 => RETURN "_P"
      | 271 => RETURN "_C_P"
      | 272 => RETURN "_L_P"
      | 273 => RETURN "_L_P"
      | 274 => RETURN "_C_L_P"
      | 275 => RETURN "_L_P"
      | 276 => RETURN "_L_P"
      | 277 => RETURN "_L_P"
      | 278 => RETURN "_C_L_P"
      | 279 => RETURN "_I_P"
      | 280 => RETURN "_L_P"
      | 281 => RETURN "_L_P"
      | 282 => RETURN "_C_L_P"
      | 283 => RETURN "_L_P"
      | 284 => RETURN "_L_P"
      | 285 => RETURN "_L_P"
      | 286 => RETURN "_C_L_P"
      | 287 => RETURN "_I_P"
      | 288 => RETURN "_L_P"
      | 289 => RETURN "_L_P"
      | 290 => RETURN "_C_L_P"
      | 291 => RETURN "_L_P"
      | 292 => RETURN "_L_P"
      | 293 => RETURN "_L_P"
      | 294 => RETURN "_C_L_P"
      | 295 => RETURN "_I_P"
      | 296 => RETURN "_L_P"
      | 297 => RETURN "_L_P"
      | 298 => RETURN "_C_L_P"
      | 299 => RETURN "_L_P"
      | 300 => RETURN "_L_P"
      | 301 => RETURN "_L_P"
      | 302 => RETURN "_C_L_P"
      | 303 => RETURN "_I_P"
      | 304 => RETURN "_L_P"
      | 305 => RETURN "_L_P"
      | 306 => RETURN "_C_L_P"
      | 307 => RETURN "_L_P"
      | 308 => RETURN "_L_P"
      | 309 => RETURN "_L_P"
      | 310 => RETURN "_C_L_P"
      | 311 => RETURN "_I_P"
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
      | 336 => RETURN "_L_P"
      | 337 => RETURN "_L_P"
      | 338 => RETURN "_C_L_P"
      | 339 => RETURN "_L_P"
      | 340 => RETURN "_L_P"
      | 341 => RETURN "_L_P"
      | 342 => RETURN "_C_L_P"
      | 343 => RETURN "_I_P"
      | 344 => RETURN "_L_P"
      | 345 => RETURN "_L_P"
      | 346 => RETURN "_C_L_P"
      | 347 => RETURN "_L_P"
      | 348 => RETURN "_L_P"
      | 349 => RETURN "_L_P"
      | 350 => RETURN "_C_L_P"
      | 351 => RETURN "_I_P"
      | 352 => RETURN "_P"
      | 353 => RETURN "_P"
      | 354 => RETURN "_C_P"
      | 355 => RETURN "_P"
      | 356 => RETURN "_P"
      | 357 => RETURN "_P"
      | 358 => RETURN "_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

