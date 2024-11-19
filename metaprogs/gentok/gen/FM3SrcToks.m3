
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This file was generated by FM3's GenTok metaprogram,
   from input file "FM3SrcToks.gentok", with command line 
     "./gentok -S -L -i -n FM3SrcToks.gentok". *)

MODULE FM3SrcToks

(*EXPORTED*)
; PROCEDURE Image ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 0 => RETURN "StkEOF"
      | 1 => RETURN "StkUnknown"
      | 2 => RETURN "StkBOF"
      | 5 => RETURN "AND"
      | 6 => RETURN "ANY"
      | 7 => RETURN "ARRAY"
      | 8 => RETURN "AS"
      | 9 => RETURN "BEGIN"
      | 10 => RETURN "BITS"
      | 11 => RETURN "BRANDED"
      | 12 => RETURN "BY"
      | 13 => RETURN "CASE"
      | 14 => RETURN "CONST"
      | 15 => RETURN "DIV"
      | 16 => RETURN "DO"
      | 17 => RETURN "ELSE"
      | 18 => RETURN "ELSIF"
      | 19 => RETURN "END"
      | 20 => RETURN "EVAL"
      | 21 => RETURN "EXCEPT"
      | 22 => RETURN "EXCEPTION"
      | 23 => RETURN "EXIT"
      | 24 => RETURN "EXPORTS"
      | 25 => RETURN "FINALLY"
      | 26 => RETURN "FOR"
      | 27 => RETURN "FROM"
      | 28 => RETURN "GENERIC"
      | 29 => RETURN "IF"
      | 30 => RETURN "IMPORT"
      | 31 => RETURN "IN"
      | 32 => RETURN "INTERFACE"
      | 33 => RETURN "LOCK"
      | 34 => RETURN "LOOP"
      | 35 => RETURN "METHODS"
      | 36 => RETURN "MOD"
      | 37 => RETURN "MODULE"
      | 38 => RETURN "NOT"
      | 39 => RETURN "OBJECT"
      | 40 => RETURN "OF"
      | 41 => RETURN "OR"
      | 42 => RETURN "OVERRIDES"
      | 43 => RETURN "PROCEDURE"
      | 44 => RETURN "RAISE"
      | 45 => RETURN "RAISES"
      | 46 => RETURN "READONLY"
      | 47 => RETURN "RECORD"
      | 48 => RETURN "REF"
      | 49 => RETURN "REPEAT"
      | 50 => RETURN "RETURN"
      | 51 => RETURN "REVEAL"
      | 52 => RETURN "ROOT"
      | 53 => RETURN "SET"
      | 54 => RETURN "THEN"
      | 55 => RETURN "TO"
      | 56 => RETURN "TRY"
      | 57 => RETURN "TYPE"
      | 58 => RETURN "TYPECASE"
      | 59 => RETURN "UNSAFE"
      | 60 => RETURN "UNTIL"
      | 61 => RETURN "UNTRACED"
      | 62 => RETURN "VALUE"
      | 63 => RETURN "VAR"
      | 64 => RETURN "WHILE"
      | 65 => RETURN "WITH"
      | 66 => RETURN ";"
      | 67 => RETURN "."
      | 68 => RETURN "="
      | 69 => RETURN "("
      | 70 => RETURN ")"
      | 71 => RETURN ","
      | 72 => RETURN ":"
      | 73 => RETURN "<:"
      | 74 => RETURN ":="
      | 75 => RETURN "{"
      | 76 => RETURN "}"
      | 77 => RETURN "|"
      | 78 => RETURN "=>"
      | 79 => RETURN ".."
      | 80 => RETURN "["
      | 81 => RETURN "]"
      | 82 => RETURN "#"
      | 83 => RETURN "<"
      | 84 => RETURN ">"
      | 85 => RETURN "<="
      | 86 => RETURN ">="
      | 87 => RETURN "+"
      | 88 => RETURN "-"
      | 89 => RETURN "&"
      | 90 => RETURN "*"
      | 91 => RETURN "/"
      | 92 => RETURN "^"
      | 93 => RETURN "<*"
      | 94 => RETURN "*>"
      | 95 => RETURN "StkIdent"
      | 96 => RETURN "StkIntLit"
      | 97 => RETURN "StkLongIntLit"
      | 98 => RETURN "StkBasedLit"
      | 99 => RETURN "StkLongBasedLit"
      | 100 => RETURN "StkRealLit"
      | 101 => RETURN "StkLongRealLit"
      | 102 => RETURN "StkExtendedLit"
      | 103 => RETURN "StkTextLit"
      | 104 => RETURN "StkWideTextLit"
      | 105 => RETURN "StkCharLit"
      | 106 => RETURN "StkWideCharLit"
      | 107 => RETURN "StkLexErrChars"
      | 108 => RETURN "StkPragmaId"
      | 109 => RETURN "<TokMinPredef>"
      | 110 => RETURN "<RidNull>"
      | 111 => RETURN "ABS"
      | 112 => RETURN "ADDRESS"
      | 113 => RETURN "ADR"
      | 114 => RETURN "ADRSIZE"
      | 115 => RETURN "BITSIZE"
      | 116 => RETURN "BOOLEAN"
      | 117 => RETURN "BYTESIZE"
      | 118 => RETURN "CARDINAL"
      | 119 => RETURN "CEILING"
      | 120 => RETURN "CHAR"
      | 121 => RETURN "DEC"
      | 122 => RETURN "DISPOSE"
      | 123 => RETURN "EXTENDED"
      | 124 => RETURN "FALSE"
      | 125 => RETURN "FIRST"
      | 126 => RETURN "FLOAT"
      | 127 => RETURN "FLOOR"
      | 128 => RETURN "INC"
      | 129 => RETURN "INTEGER"
      | 130 => RETURN "ISTYPE"
      | 131 => RETURN "LAST"
      | 132 => RETURN "LONGCARD"
      | 133 => RETURN "LONGINT"
      | 134 => RETURN "LONGREAL"
      | 135 => RETURN "LOOPHOLE"
      | 136 => RETURN "MAX"
      | 137 => RETURN "MIN"
      | 138 => RETURN "MUTEX"
      | 139 => RETURN "NARROW"
      | 140 => RETURN "NEW"
      | 141 => RETURN "NIL"
      | 142 => RETURN "NULL"
      | 143 => RETURN "NUMBER"
      | 144 => RETURN "ORD"
      | 145 => RETURN "REAL"
      | 146 => RETURN "REFANY"
      | 147 => RETURN "ROUND"
      | 148 => RETURN "SUBARRAY"
      | 149 => RETURN "TEXT"
      | 150 => RETURN "TRUE"
      | 151 => RETURN "TRUNC"
      | 152 => RETURN "TYPECODE"
      | 153 => RETURN "VAL"
      | 154 => RETURN "WIDECHAR"
      | 155 => RETURN "<ROOT>"
      | 156 => RETURN "<UNTRACED_ROOT>"
      | 157 => RETURN "<MaxReservedId>"
      | 158 => RETURN "<TokMinPredef>"
      | 159 => RETURN "T"
      | 160 => RETURN "Size"
      | 161 => RETURN "Plus"
      | 162 => RETURN "Times"
      | 163 => RETURN "Minus"
      | 164 => RETURN "Divide"
      | 165 => RETURN "Mod"
      | 166 => RETURN "LT"
      | 167 => RETURN "LE"
      | 168 => RETURN "GT"
      | 169 => RETURN "GE"
      | 170 => RETURN "And"
      | 171 => RETURN "Or"
      | 172 => RETURN "Xor"
      | 173 => RETURN "Not"
      | 174 => RETURN "Shift"
      | 175 => RETURN "LeftShift"
      | 176 => RETURN "RightShift"
      | 177 => RETURN "Rotate"
      | 178 => RETURN "LeftRotate"
      | 179 => RETURN "RightRotate"
      | 180 => RETURN "Extract"
      | 181 => RETURN "Insert"
      | 187 => RETURN "Long.T"
      | 188 => RETURN "Long.Size"
      | 189 => RETURN "Long.Plus"
      | 190 => RETURN "Long.Times"
      | 191 => RETURN "Long.Minus"
      | 192 => RETURN "Long.Divide"
      | 193 => RETURN "Long.Mod"
      | 194 => RETURN "Long.LT"
      | 195 => RETURN "Long.LE"
      | 196 => RETURN "Long.GT"
      | 197 => RETURN "Long.GE"
      | 198 => RETURN "Long.And"
      | 199 => RETURN "Long.Or"
      | 200 => RETURN "Long.Xor"
      | 201 => RETURN "Long.Not"
      | 202 => RETURN "Long.Shift"
      | 203 => RETURN "Long.LeftShift"
      | 204 => RETURN "Long.RightShift"
      | 205 => RETURN "Long.Rotate"
      | 206 => RETURN "Long.LeftRotate"
      | 207 => RETURN "Long.RightRotate"
      | 208 => RETURN "Long.Extract"
      | 209 => RETURN "Long.Insert"
      | 210 => RETURN "<TokMaxPredef>"
      | 211 => RETURN "\XFF"
      | 212 => RETURN "\XFF"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Image

(*EXPORTED*)
; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | 0 => RETURN "StkEOF"
      | 1 => RETURN "StkUnknown"
      | 2 => RETURN "StkBOF"
      | 5 => RETURN "StkRwAND"
      | 6 => RETURN "StkRwANY"
      | 7 => RETURN "StkRwARRAY"
      | 8 => RETURN "StkRwAS"
      | 9 => RETURN "StkRwBEGIN"
      | 10 => RETURN "StkRwBITS"
      | 11 => RETURN "StkRwBRANDED"
      | 12 => RETURN "StkRwBY"
      | 13 => RETURN "StkRwCASE"
      | 14 => RETURN "StkRwCONST"
      | 15 => RETURN "StkRwDIV"
      | 16 => RETURN "StkRwDO"
      | 17 => RETURN "StkRwELSE"
      | 18 => RETURN "StkRwELSIF"
      | 19 => RETURN "StkRwEND"
      | 20 => RETURN "StkRwEVAL"
      | 21 => RETURN "StkRwEXCEPT"
      | 22 => RETURN "StkRwEXCEPTION"
      | 23 => RETURN "StkRwEXIT"
      | 24 => RETURN "StkRwEXPORTS"
      | 25 => RETURN "StkRwFINALLY"
      | 26 => RETURN "StkRwFOR"
      | 27 => RETURN "StkRwFROM"
      | 28 => RETURN "StkRwGENERIC"
      | 29 => RETURN "StkRwIF"
      | 30 => RETURN "StkRwIMPORT"
      | 31 => RETURN "StkRwIN"
      | 32 => RETURN "StkRwINTERFACE"
      | 33 => RETURN "StkRwLOCK"
      | 34 => RETURN "StkRwLOOP"
      | 35 => RETURN "StkRwMETHODS"
      | 36 => RETURN "StkRwMOD"
      | 37 => RETURN "StkRwMODULE"
      | 38 => RETURN "StkRwNOT"
      | 39 => RETURN "StkRwOBJECT"
      | 40 => RETURN "StkRwOF"
      | 41 => RETURN "StkRwOR"
      | 42 => RETURN "StkRwOVERRIDES"
      | 43 => RETURN "StkRwPROCEDURE"
      | 44 => RETURN "StkRwRAISE"
      | 45 => RETURN "StkRwRAISES"
      | 46 => RETURN "StkRwREADONLY"
      | 47 => RETURN "StkRwRECORD"
      | 48 => RETURN "StkRwREF"
      | 49 => RETURN "StkRwREPEAT"
      | 50 => RETURN "StkRwRETURN"
      | 51 => RETURN "StkRwREVEAL"
      | 52 => RETURN "StkRwROOT"
      | 53 => RETURN "StkRwSET"
      | 54 => RETURN "StkRwTHEN"
      | 55 => RETURN "StkRwTO"
      | 56 => RETURN "StkRwTRY"
      | 57 => RETURN "StkRwTYPE"
      | 58 => RETURN "StkRwTYPECASE"
      | 59 => RETURN "StkRwUNSAFE"
      | 60 => RETURN "StkRwUNTIL"
      | 61 => RETURN "StkRwUNTRACED"
      | 62 => RETURN "StkRwVALUE"
      | 63 => RETURN "StkRwVAR"
      | 64 => RETURN "StkRwWHILE"
      | 65 => RETURN "StkRwWITH"
      | 66 => RETURN "StkSemicolon"
      | 67 => RETURN "StkDot"
      | 68 => RETURN "StkEqual"
      | 69 => RETURN "StkOpenParen"
      | 70 => RETURN "StkCloseParen"
      | 71 => RETURN "StkComma"
      | 72 => RETURN "StkColon"
      | 73 => RETURN "StkSubtype"
      | 74 => RETURN "StkBecomes"
      | 75 => RETURN "StkOpenBrace"
      | 76 => RETURN "StkCloseBrace"
      | 77 => RETURN "StkStroke"
      | 78 => RETURN "StkArrow"
      | 79 => RETURN "StkEllipsis"
      | 80 => RETURN "StkOpenBracket"
      | 81 => RETURN "StkCloseBracket"
      | 82 => RETURN "StkUnequal"
      | 83 => RETURN "StkLess"
      | 84 => RETURN "StkGreater"
      | 85 => RETURN "StkLessEqual"
      | 86 => RETURN "StkGreaterEqual"
      | 87 => RETURN "StkPlus"
      | 88 => RETURN "StkMinus"
      | 89 => RETURN "StkAmpersand"
      | 90 => RETURN "StkStar"
      | 91 => RETURN "StkSlash"
      | 92 => RETURN "StkDeref"
      | 93 => RETURN "StkOpenPragma"
      | 94 => RETURN "StkClosePragma"
      | 95 => RETURN "StkIdent"
      | 96 => RETURN "StkIntLit"
      | 97 => RETURN "StkLongIntLit"
      | 98 => RETURN "StkBasedLit"
      | 99 => RETURN "StkLongBasedLit"
      | 100 => RETURN "StkRealLit"
      | 101 => RETURN "StkLongRealLit"
      | 102 => RETURN "StkExtendedLit"
      | 103 => RETURN "StkTextLit"
      | 104 => RETURN "StkWideTextLit"
      | 105 => RETURN "StkCharLit"
      | 106 => RETURN "StkWideCharLit"
      | 107 => RETURN "StkLexErrChars"
      | 108 => RETURN "StkPragmaId"
      | 109 => RETURN "TokMinReservedId"
      | 110 => RETURN "RidNull"
      | 111 => RETURN "RidABS"
      | 112 => RETURN "RidADDRESS"
      | 113 => RETURN "RidADR"
      | 114 => RETURN "RidADRSIZE"
      | 115 => RETURN "RidBITSIZE"
      | 116 => RETURN "RidBOOLEAN"
      | 117 => RETURN "RidBYTESIZE"
      | 118 => RETURN "RidCARDINAL"
      | 119 => RETURN "RidCEILING"
      | 120 => RETURN "RidCHAR"
      | 121 => RETURN "RidDEC"
      | 122 => RETURN "RidDISPOSE"
      | 123 => RETURN "RidEXTENDED"
      | 124 => RETURN "RidFALSE"
      | 125 => RETURN "RidFIRST"
      | 126 => RETURN "RidFLOAT"
      | 127 => RETURN "RidFLOOR"
      | 128 => RETURN "RidINC"
      | 129 => RETURN "RidINTEGER"
      | 130 => RETURN "RidISTYPE"
      | 131 => RETURN "RidLAST"
      | 132 => RETURN "RidLONGCARD"
      | 133 => RETURN "RidLONGINT"
      | 134 => RETURN "RidLONGREAL"
      | 135 => RETURN "RidLOOPHOLE"
      | 136 => RETURN "RidMAX"
      | 137 => RETURN "RidMIN"
      | 138 => RETURN "RidMUTEX"
      | 139 => RETURN "RidNARROW"
      | 140 => RETURN "RidNEW"
      | 141 => RETURN "RidNIL"
      | 142 => RETURN "RidNULL"
      | 143 => RETURN "RidNUMBER"
      | 144 => RETURN "RidORD"
      | 145 => RETURN "RidREAL"
      | 146 => RETURN "RidREFANY"
      | 147 => RETURN "RidROUND"
      | 148 => RETURN "RidSUBARRAY"
      | 149 => RETURN "RidTEXT"
      | 150 => RETURN "RidTRUE"
      | 151 => RETURN "RidTRUNC"
      | 152 => RETURN "RidTYPECODE"
      | 153 => RETURN "RidVAL"
      | 154 => RETURN "RidWIDECHAR"
      | 155 => RETURN "RidROOT"
      | 156 => RETURN "RidUNTRACEDROOT"
      | 157 => RETURN "TokMaxReservedId"
      | 158 => RETURN "TokMinPredef"
      | 159 => RETURN "Word_T"
      | 160 => RETURN "Word_Size"
      | 161 => RETURN "Word_Plus"
      | 162 => RETURN "Word_Times"
      | 163 => RETURN "Word_Minus"
      | 164 => RETURN "Word_Divide"
      | 165 => RETURN "Word_Mod"
      | 166 => RETURN "Word_LT"
      | 167 => RETURN "Word_LE"
      | 168 => RETURN "Word_GT"
      | 169 => RETURN "Word_GE"
      | 170 => RETURN "Word_And"
      | 171 => RETURN "Word_Or"
      | 172 => RETURN "Word_Xor"
      | 173 => RETURN "Word_Not"
      | 174 => RETURN "Word_Shift"
      | 175 => RETURN "Word_LeftShift"
      | 176 => RETURN "Word_RightShift"
      | 177 => RETURN "Word_Rotate"
      | 178 => RETURN "Word_LeftRotate"
      | 179 => RETURN "Word_RightRotate"
      | 180 => RETURN "Word_Extract"
      | 181 => RETURN "Word_Insert"
      | 187 => RETURN "Long_T"
      | 188 => RETURN "Long_Size"
      | 189 => RETURN "Long_Plus"
      | 190 => RETURN "Long_Times"
      | 191 => RETURN "Long_Minus"
      | 192 => RETURN "Long_Divide"
      | 193 => RETURN "Long_Mod"
      | 194 => RETURN "Long_LT"
      | 195 => RETURN "Long_LE"
      | 196 => RETURN "Long_GT"
      | 197 => RETURN "Long_GE"
      | 198 => RETURN "Long_And"
      | 199 => RETURN "Long_Or"
      | 200 => RETURN "Long_Xor"
      | 201 => RETURN "Long_Not"
      | 202 => RETURN "Long_Shift"
      | 203 => RETURN "Long_LeftShift"
      | 204 => RETURN "Long_RightShift"
      | 205 => RETURN "Long_Rotate"
      | 206 => RETURN "Long_LeftRotate"
      | 207 => RETURN "Long_RightRotate"
      | 208 => RETURN "Long_Extract"
      | 209 => RETURN "Long_Insert"
      | 210 => RETURN "TokMaxPredef"
      | 211 => RETURN "StkUnaryPlus"
      | 212 => RETURN "StkUnaryMinus"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

; BEGIN 
  END FM3SrcToks
.

