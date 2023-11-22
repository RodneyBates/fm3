
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
      | 133 => RETURN "ItkImport"
      | 134 => RETURN "ItkFromImport"
      | 135 => RETURN "ItkImportAs"
      | 136 => RETURN "ItkFormalsListEmpty"
      | 137 => RETURN "ItkFormalTypeAbsent"
      | 138 => RETURN "ItkFormalExprAbsent"
      | 139 => RETURN "ItkRaisesSetAbsent"
      | 140 => RETURN "ItkRaisesANY"
      | 141 => RETURN "ItkResultTypeAbsent"
      | 142 => RETURN "ItkProcBodyAbsent"
      | 143 => RETURN "ItkScopeLt"
      | 144 => RETURN "ItkScopeRt"
      | 145 => RETURN "ItkRefId"
      | 146 => RETURN "ItkRefNo"
      | 147 => RETURN "ItkDuplDeclId"
      | 148 => RETURN "ItkDeclId"
      | 149 => RETURN "ItkDeclNo"
      | 150 => RETURN "ItkQualIdLt"
      | 151 => RETURN "ItkQualIdLtTemp"
      | 152 => RETURN "ItkQualIdLtPatch"
      | 153 => RETURN "ItkQualIdRt"
      | 154 => RETURN "ItkFuncSignatureLt"
      | 155 => RETURN "ItkFuncSignatureLtTemp"
      | 156 => RETURN "ItkFuncSignatureLtPatch"
      | 157 => RETURN "ItkFuncSignatureRt"
      | 158 => RETURN "ItkProcSignatureLt"
      | 159 => RETURN "ItkProcSignatureLtTemp"
      | 160 => RETURN "ItkProcSignatureLtPatch"
      | 161 => RETURN "ItkProcSignatureRt"
      | 162 => RETURN "ItkFormalTypeLt"
      | 163 => RETURN "ItkFormalTypeLtTemp"
      | 164 => RETURN "ItkFormalTypeLtPatch"
      | 165 => RETURN "ItkFormalTypeRt"
      | 166 => RETURN "ItkFormalExprLt"
      | 167 => RETURN "ItkFormalExprLtTemp"
      | 168 => RETURN "ItkFormalExprLtPatch"
      | 169 => RETURN "ItkFormalExprRt"
      | 170 => RETURN "ItkResultTypeLt"
      | 171 => RETURN "ItkResultTypeLtTemp"
      | 172 => RETURN "ItkResultTypeLtPatch"
      | 173 => RETURN "ItkResultTypeRt"
      | 174 => RETURN "ItkRaisesSetLt"
      | 175 => RETURN "ItkRaisesSetLtTemp"
      | 176 => RETURN "ItkRaisesSetLtPatch"
      | 177 => RETURN "ItkRaisesSetRt"
      | 178 => RETURN "ItkProcNoBodyLt"
      | 179 => RETURN "ItkProcNoBodyLtTemp"
      | 180 => RETURN "ItkProcNoBodyLtPatch"
      | 181 => RETURN "ItkProcNoBodyRt"
      | 182 => RETURN "ItkProcWBodyLt"
      | 183 => RETURN "ItkProcWBodyLtTemp"
      | 184 => RETURN "ItkProcWBodyLtPatch"
      | 185 => RETURN "ItkProcWBodyRt"
      | 186 => RETURN "ItkProcBodyLt"
      | 187 => RETURN "ItkProcBodyLtTemp"
      | 188 => RETURN "ItkProcBodyLtPatch"
      | 189 => RETURN "ItkProcBodyRt"
      | 190 => RETURN "ItkProcTypeLt"
      | 191 => RETURN "ItkProcTypeLtTemp"
      | 192 => RETURN "ItkProcTypeLtPatch"
      | 193 => RETURN "ItkProcTypeRt"
      | 194 => RETURN "ItkBlockLt"
      | 195 => RETURN "ItkBlockLtTemp"
      | 196 => RETURN "ItkBlockLtPatch"
      | 197 => RETURN "ItkBlockRt"
      | 198 => RETURN "ItkBlockBeg"
      | 199 => RETURN "ItkBlockBegTemp"
      | 200 => RETURN "ItkBlockBegPatch"
      | 201 => RETURN "ItkVarDeclId"
      | 202 => RETURN "ItkRecFieldId"
      | 203 => RETURN "ItkVALUEFormalId"
      | 204 => RETURN "ItkVARFormalId"
      | 205 => RETURN "ItkROFormalId"
      | 206 => RETURN "ItkRecDefLt"
      | 207 => RETURN "ItkRecDefLtTemp"
      | 208 => RETURN "ItkRecDefLtPatch"
      | 209 => RETURN "ItkRecDefRt"
      | 210 => RETURN "ItkRecFieldLt"
      | 211 => RETURN "ItkRecFieldLtTemp"
      | 212 => RETURN "ItkRecFieldLtPatch"
      | 213 => RETURN "ItkRecFieldRt"
      | 214 => RETURN "ItkRecFieldType"
      | 215 => RETURN "ItkRecFieldTypeTemp"
      | 216 => RETURN "ItkRecFieldTypePatch"
      | 217 => RETURN "ItkRecFieldVal"
      | 218 => RETURN "ItkRecFieldValTemp"
      | 219 => RETURN "ItkRecFieldValPatch"
      | 220 => RETURN "ItkTypeDeclLt"
      | 221 => RETURN "ItkTypeDeclLtTemp"
      | 222 => RETURN "ItkTypeDeclLtPatch"
      | 223 => RETURN "ItkTypeDeclRt"
      | 224 => RETURN "ItkTypeDeclEq"
      | 225 => RETURN "ItkTypeDeclEqTemp"
      | 226 => RETURN "ItkTypeDeclEqPatch"
      | 227 => RETURN "ItkVarDeclLt"
      | 228 => RETURN "ItkVarDeclLtTemp"
      | 229 => RETURN "ItkVarDeclLtPatch"
      | 230 => RETURN "ItkVarDeclRt"
      | 231 => RETURN "ItkVarDeclType"
      | 232 => RETURN "ItkVarDeclTypeTemp"
      | 233 => RETURN "ItkVarDeclTypePatch"
      | 234 => RETURN "ItkVarDeclVal"
      | 235 => RETURN "ItkVarDeclValTemp"
      | 236 => RETURN "ItkVarDeclValPatch"
      | 237 => RETURN "ItkExportIdListLt"
      | 238 => RETURN "ItkExportIdListLtTemp"
      | 239 => RETURN "ItkExportIdListLtPatch"
      | 240 => RETURN "ItkExportIdListRt"
      | 241 => RETURN "ItkExportIdListSep"
      | 242 => RETURN "ItkExportIdListSepTemp"
      | 243 => RETURN "ItkExportIdListSepPatch"
      | 244 => RETURN "ItkGenFormalIdListLt"
      | 245 => RETURN "ItkGenFormalIdListLtTemp"
      | 246 => RETURN "ItkGenFormalIdListLtPatch"
      | 247 => RETURN "ItkGenFormalIdListRt"
      | 248 => RETURN "ItkGenFormalIdListSep"
      | 249 => RETURN "ItkGenFormalIdListSepTemp"
      | 250 => RETURN "ItkGenFormalIdListSepPatch"
      | 251 => RETURN "ItkGenActualIdListLt"
      | 252 => RETURN "ItkGenActualIdListLtTemp"
      | 253 => RETURN "ItkGenActualIdListLtPatch"
      | 254 => RETURN "ItkGenActualIdListRt"
      | 255 => RETURN "ItkGenActualIdListSep"
      | 256 => RETURN "ItkGenActualIdListSepTemp"
      | 257 => RETURN "ItkGenActualIdListSepPatch"
      | 258 => RETURN "ItkVarDeclIdListLt"
      | 259 => RETURN "ItkVarDeclIdListLtTemp"
      | 260 => RETURN "ItkVarDeclIdListLtPatch"
      | 261 => RETURN "ItkVarDeclIdListRt"
      | 262 => RETURN "ItkVarDeclIdListSep"
      | 263 => RETURN "ItkVarDeclIdListSepTemp"
      | 264 => RETURN "ItkVarDeclIdListSepPatch"
      | 265 => RETURN "ItkRecFieldIdListLt"
      | 266 => RETURN "ItkRecFieldIdListLtTemp"
      | 267 => RETURN "ItkRecFieldIdListLtPatch"
      | 268 => RETURN "ItkRecFieldIdListRt"
      | 269 => RETURN "ItkRecFieldIdListSep"
      | 270 => RETURN "ItkRecFieldIdListSepTemp"
      | 271 => RETURN "ItkRecFieldIdListSepPatch"
      | 272 => RETURN "ItkFormalsListLt"
      | 273 => RETURN "ItkFormalsListLtTemp"
      | 274 => RETURN "ItkFormalsListLtPatch"
      | 275 => RETURN "ItkFormalsListRt"
      | 276 => RETURN "ItkFormalsListSep"
      | 277 => RETURN "ItkFormalsListSepTemp"
      | 278 => RETURN "ItkFormalsListSepPatch"
      | 279 => RETURN "ItkVALUEFormalIdListLt"
      | 280 => RETURN "ItkVALUEFormalIdListLtTemp"
      | 281 => RETURN "ItkVALUEFormalIdListLtPatch"
      | 282 => RETURN "ItkVALUEFormalIdListRt"
      | 283 => RETURN "ItkVALUEFormalIdListSep"
      | 284 => RETURN "ItkVALUEFormalIdListSepTemp"
      | 285 => RETURN "ItkVALUEFormalIdListSepPatch"
      | 286 => RETURN "ItkVARFormalIdListLt"
      | 287 => RETURN "ItkVARFormalIdListLtTemp"
      | 288 => RETURN "ItkVARFormalIdListLtPatch"
      | 289 => RETURN "ItkVARFormalIdListRt"
      | 290 => RETURN "ItkVARFormalIdListSep"
      | 291 => RETURN "ItkVARFormalIdListSepTemp"
      | 292 => RETURN "ItkVARFormalIdListSepPatch"
      | 293 => RETURN "ItkROFormalIdListLt"
      | 294 => RETURN "ItkROFormalIdListLtTemp"
      | 295 => RETURN "ItkROFormalIdListLtPatch"
      | 296 => RETURN "ItkROFormalIdListRt"
      | 297 => RETURN "ItkROFormalIdListSep"
      | 298 => RETURN "ItkROFormalIdListSepTemp"
      | 299 => RETURN "ItkROFormalIdListSepPatch"
      | 300 => RETURN "ItkDeclListLt"
      | 301 => RETURN "ItkDeclListLtTemp"
      | 302 => RETURN "ItkDeclListLtPatch"
      | 303 => RETURN "ItkDeclListRt"
      | 304 => RETURN "ItkDeclListSep"
      | 305 => RETURN "ItkDeclListSepTemp"
      | 306 => RETURN "ItkDeclListSepPatch"
      | 307 => RETURN "ItkBecomesLt"
      | 308 => RETURN "ItkBecomesLtTemp"
      | 309 => RETURN "ItkBecomesLtPatch"
      | 310 => RETURN "ItkBecomesRt"
      | 311 => RETURN "ItkBecomesInfix"
      | 312 => RETURN "ItkBecomesInfixTemp"
      | 313 => RETURN "ItkBecomesInfixPatch"
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
      | 133 => RETURN "_I_P"
      | 134 => RETURN "_I_P_I_P"
      | 135 => RETURN "_I_P_I_P"
      | 136 => RETURN ""
      | 137 => RETURN "_P"
      | 138 => RETURN "_P"
      | 139 => RETURN "_P"
      | 140 => RETURN "_P"
      | 141 => RETURN "_P"
      | 142 => RETURN "_P"
      | 143 => RETURN "_I"
      | 144 => RETURN "_I"
      | 145 => RETURN "_I_P"
      | 146 => RETURN "_D_P"
      | 147 => RETURN "_I_P"
      | 148 => RETURN "_L_I_P"
      | 149 => RETURN "_D_P"
      | 150 => RETURN ""
      | 151 => RETURN ""
      | 152 => RETURN "_L_C"
      | 153 => RETURN ""
      | 154 => RETURN "_P"
      | 155 => RETURN "_P"
      | 156 => RETURN "_L_C_P"
      | 157 => RETURN "_P"
      | 158 => RETURN "_P"
      | 159 => RETURN "_P"
      | 160 => RETURN "_L_C_P"
      | 161 => RETURN "_P"
      | 162 => RETURN "_P"
      | 163 => RETURN "_P"
      | 164 => RETURN "_L_C_P"
      | 165 => RETURN "_P"
      | 166 => RETURN "_P"
      | 167 => RETURN "_P"
      | 168 => RETURN "_L_C_P"
      | 169 => RETURN "_P"
      | 170 => RETURN "_P"
      | 171 => RETURN "_P"
      | 172 => RETURN "_L_C_P"
      | 173 => RETURN "_P"
      | 174 => RETURN "_P"
      | 175 => RETURN "_P"
      | 176 => RETURN "_L_C_P"
      | 177 => RETURN "_P"
      | 178 => RETURN "_I"
      | 179 => RETURN "_I"
      | 180 => RETURN "_L_C_I"
      | 181 => RETURN "_I"
      | 182 => RETURN "_I"
      | 183 => RETURN "_I"
      | 184 => RETURN "_L_C_I"
      | 185 => RETURN "_I"
      | 186 => RETURN "_P"
      | 187 => RETURN "_P"
      | 188 => RETURN "_L_C_P"
      | 189 => RETURN "_P"
      | 190 => RETURN "_P"
      | 191 => RETURN "_P"
      | 192 => RETURN "_L_C_P"
      | 193 => RETURN "_P"
      | 194 => RETURN "_P"
      | 195 => RETURN "_P"
      | 196 => RETURN "_L_C_P"
      | 197 => RETURN "_P"
      | 198 => RETURN "_P"
      | 199 => RETURN "_P"
      | 200 => RETURN "_L_C_P"
      | 201 => RETURN "_L_I_P"
      | 202 => RETURN "_L_I_P"
      | 203 => RETURN "_L_I_P"
      | 204 => RETURN "_L_I_P"
      | 205 => RETURN "_L_I_P"
      | 206 => RETURN "_P_L"
      | 207 => RETURN "_P_L"
      | 208 => RETURN "_L_C_P_L"
      | 209 => RETURN "_P_L"
      | 210 => RETURN "_P"
      | 211 => RETURN "_P"
      | 212 => RETURN "_L_C_P"
      | 213 => RETURN "_P"
      | 214 => RETURN "_P"
      | 215 => RETURN "_P"
      | 216 => RETURN "_L_C_P"
      | 217 => RETURN "_P"
      | 218 => RETURN "_P"
      | 219 => RETURN "_L_C_P"
      | 220 => RETURN "_P"
      | 221 => RETURN "_P"
      | 222 => RETURN "_L_C_P"
      | 223 => RETURN "_P"
      | 224 => RETURN ""
      | 225 => RETURN ""
      | 226 => RETURN "_L_C"
      | 227 => RETURN "_P"
      | 228 => RETURN "_P"
      | 229 => RETURN "_L_C_P"
      | 230 => RETURN "_P"
      | 231 => RETURN "_P"
      | 232 => RETURN "_P"
      | 233 => RETURN "_L_C_P"
      | 234 => RETURN "_P"
      | 235 => RETURN "_P"
      | 236 => RETURN "_L_C_P"
      | 237 => RETURN "_L_P"
      | 238 => RETURN "_L_P"
      | 239 => RETURN "_C_L_P"
      | 240 => RETURN "_L_P"
      | 241 => RETURN "_L_P"
      | 242 => RETURN "_L_P"
      | 243 => RETURN "_C_L_P"
      | 244 => RETURN "_L_P"
      | 245 => RETURN "_L_P"
      | 246 => RETURN "_C_L_P"
      | 247 => RETURN "_L_P"
      | 248 => RETURN "_L_P"
      | 249 => RETURN "_L_P"
      | 250 => RETURN "_C_L_P"
      | 251 => RETURN "_L_P"
      | 252 => RETURN "_L_P"
      | 253 => RETURN "_C_L_P"
      | 254 => RETURN "_L_P"
      | 255 => RETURN "_L_P"
      | 256 => RETURN "_L_P"
      | 257 => RETURN "_C_L_P"
      | 258 => RETURN "_L_P"
      | 259 => RETURN "_L_P"
      | 260 => RETURN "_C_L_P"
      | 261 => RETURN "_L_P"
      | 262 => RETURN "_L_P"
      | 263 => RETURN "_L_P"
      | 264 => RETURN "_C_L_P"
      | 265 => RETURN "_L_P"
      | 266 => RETURN "_L_P"
      | 267 => RETURN "_C_L_P"
      | 268 => RETURN "_L_P"
      | 269 => RETURN "_L_P"
      | 270 => RETURN "_L_P"
      | 271 => RETURN "_C_L_P"
      | 272 => RETURN "_L_P"
      | 273 => RETURN "_L_P"
      | 274 => RETURN "_C_L_P"
      | 275 => RETURN "_L_P"
      | 276 => RETURN "_L_P"
      | 277 => RETURN "_L_P"
      | 278 => RETURN "_C_L_P"
      | 279 => RETURN "_L_P"
      | 280 => RETURN "_L_P"
      | 281 => RETURN "_C_L_P"
      | 282 => RETURN "_L_P"
      | 283 => RETURN "_L_P"
      | 284 => RETURN "_L_P"
      | 285 => RETURN "_C_L_P"
      | 286 => RETURN "_L_P"
      | 287 => RETURN "_L_P"
      | 288 => RETURN "_C_L_P"
      | 289 => RETURN "_L_P"
      | 290 => RETURN "_L_P"
      | 291 => RETURN "_L_P"
      | 292 => RETURN "_C_L_P"
      | 293 => RETURN "_L_P"
      | 294 => RETURN "_L_P"
      | 295 => RETURN "_C_L_P"
      | 296 => RETURN "_L_P"
      | 297 => RETURN "_L_P"
      | 298 => RETURN "_L_P"
      | 299 => RETURN "_C_L_P"
      | 300 => RETURN "_L_P"
      | 301 => RETURN "_L_P"
      | 302 => RETURN "_C_L_P"
      | 303 => RETURN "_L_P"
      | 304 => RETURN "_L_P"
      | 305 => RETURN "_L_P"
      | 306 => RETURN "_C_L_P"
      | 307 => RETURN "_P"
      | 308 => RETURN "_P"
      | 309 => RETURN "_L_C_P"
      | 310 => RETURN "_P"
      | 311 => RETURN "_P"
      | 312 => RETURN "_P"
      | 313 => RETURN "_L_C_P"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Operands

; BEGIN 
  END FM3IntToks
.

