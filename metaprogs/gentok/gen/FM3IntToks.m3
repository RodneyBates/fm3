
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3IntToks.gentok", with command line 
     "./gentok -T -t -c -n FM3IntToks.gentok". *)

MODULE FM3IntToks

; IMPORT IntSets

(*EXPORTED*)
; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 128 => RETURN "ItkNullLt"
      | 129 => RETURN "ItkNullLtTemp"
      | 130 => RETURN "ItkNullLtPatch"
      | 131 => RETURN "ItkNullRt"
      | 132 => RETURN "ItkBOFLt"
      | 133 => RETURN "ItkBOFLtTemp"
      | 134 => RETURN "ItkBOFLtPatch"
      | 135 => RETURN "ItkBOFRt"
      | 136 => RETURN "ItkEOFLt"
      | 137 => RETURN "ItkEOFLtTemp"
      | 138 => RETURN "ItkEOFLtPatch"
      | 139 => RETURN "ItkEOFRt"
      | 140 => RETURN "ItkImport"
      | 141 => RETURN "ItkFromImport"
      | 142 => RETURN "ItkImportAs"
      | 143 => RETURN "ItkFormalsListEmpty"
      | 144 => RETURN "ItkFormalTypeAbsent"
      | 145 => RETURN "ItkFormalExprAbsent"
      | 146 => RETURN "ItkRaisesSetAbsent"
      | 147 => RETURN "ItkRaisesANY"
      | 148 => RETURN "ItkProcBodyAbsent"
      | 149 => RETURN "ItkResultTypeAbsent"
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
      | 190 => RETURN "ItkExportIdListLt"
      | 191 => RETURN "ItkExportIdListLtTemp"
      | 192 => RETURN "ItkExportIdListLtPatch"
      | 193 => RETURN "ItkExportIdListRt"
      | 194 => RETURN "ItkGenFormalIdListLt"
      | 195 => RETURN "ItkGenFormalIdListLtTemp"
      | 196 => RETURN "ItkGenFormalIdListLtPatch"
      | 197 => RETURN "ItkGenFormalIdListRt"
      | 198 => RETURN "ItkGenActualIdListLt"
      | 199 => RETURN "ItkGenActualIdListLtTemp"
      | 200 => RETURN "ItkGenActualIdListLtPatch"
      | 201 => RETURN "ItkGenActualIdListRt"
      | 202 => RETURN "ItkFormalsListLt"
      | 203 => RETURN "ItkFormalsListLtTemp"
      | 204 => RETURN "ItkFormalsListLtPatch"
      | 205 => RETURN "ItkFormalsListRt"
      | 206 => RETURN "ItkFormalVALUEIdListLt"
      | 207 => RETURN "ItkFormalVALUEIdListLtTemp"
      | 208 => RETURN "ItkFormalVALUEIdListLtPatch"
      | 209 => RETURN "ItkFormalVALUEIdListRt"
      | 210 => RETURN "ItkFormalVARIdListLt"
      | 211 => RETURN "ItkFormalVARIdListLtTemp"
      | 212 => RETURN "ItkFormalVARIdListLtPatch"
      | 213 => RETURN "ItkFormalVARIdListRt"
      | 214 => RETURN "ItkFormalREADONLYIdListLt"
      | 215 => RETURN "ItkFormalREADONLYIdListLtTemp"
      | 216 => RETURN "ItkFormalREADONLYIdListLtPatch"
      | 217 => RETURN "ItkFormalREADONLYIdListRt"
      | 218 => RETURN "ItkDeclListLt"
      | 219 => RETURN "ItkDeclListLtTemp"
      | 220 => RETURN "ItkDeclListLtPatch"
      | 221 => RETURN "ItkDeclListRt"
      ELSE RETURN "<TokUndef>"
      END (*CASE*) 
    END Image 

; BEGIN 
  END FM3IntToks
.

