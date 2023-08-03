
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
      | 125 => RETURN "ItkNullLt"
      | 126 => RETURN "ItkNullLtTemp"
      | 127 => RETURN "ItkNullLtPatch"
      | 128 => RETURN "ItkNullRt"
      | 129 => RETURN "ItkBOFLt"
      | 130 => RETURN "ItkBOFLtTemp"
      | 131 => RETURN "ItkBOFLtPatch"
      | 132 => RETURN "ItkBOFRt"
      | 133 => RETURN "ItkEOFLt"
      | 134 => RETURN "ItkEOFLtTemp"
      | 135 => RETURN "ItkEOFLtPatch"
      | 136 => RETURN "ItkEOFRt"
      | 137 => RETURN "ItkModuleLt"
      | 138 => RETURN "ItkModuleLtTemp"
      | 139 => RETURN "ItkModuleLtPatch"
      | 140 => RETURN "ItkModuleRt"
      | 141 => RETURN "ItkModuleBegin"
      | 142 => RETURN "ItkModuleBeginTemp"
      | 143 => RETURN "ItkModuleBeginPatch"
      | 144 => RETURN "ItkExportsListLt"
      | 145 => RETURN "ItkExportsListLtTemp"
      | 146 => RETURN "ItkExportsListLtPatch"
      | 147 => RETURN "ItkExportsListRt"
      | 148 => RETURN "ItkExportsListElemLt"
      | 149 => RETURN "ItkExportsListElemLtTemp"
      | 150 => RETURN "ItkExportsListElemLtPatch"
      | 151 => RETURN "ItkExportsListElemRt"
      | 152 => RETURN "ItkIdPlusListLt"
      | 153 => RETURN "ItkIdPlusListLtTemp"
      | 154 => RETURN "ItkIdPlusListLtPatch"
      | 155 => RETURN "ItkIdPlusListRt"
      | 156 => RETURN "ItkIdPlusListElemLt"
      | 157 => RETURN "ItkIdPlusListElemLtTemp"
      | 158 => RETURN "ItkIdPlusListElemLtPatch"
      | 159 => RETURN "ItkIdPlusListElemRt"
      | 160 => RETURN "ItkExportLt"
      | 161 => RETURN "ItkExportLtTemp"
      | 162 => RETURN "ItkExportLtPatch"
      | 163 => RETURN "ItkExportRt"
      ELSE RETURN "<TokUndef>"
      END (*CASE*) 
    END Image 

; BEGIN 
  END FM3IntToks
.

