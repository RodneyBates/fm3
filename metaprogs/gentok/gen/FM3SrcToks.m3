
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
      | 109 => RETURN "<RidNull>"
      | 110 => RETURN "ABS"
      | 111 => RETURN "ADDRESS"
      | 112 => RETURN "ADR"
      | 113 => RETURN "ADRSIZE"
      | 114 => RETURN "BITSIZE"
      | 115 => RETURN "BOOLEAN"
      | 116 => RETURN "BYTESIZE"
      | 117 => RETURN "CARDINAL"
      | 118 => RETURN "CEILING"
      | 119 => RETURN "CHAR"
      | 120 => RETURN "DEC"
      | 121 => RETURN "DISPOSE"
      | 122 => RETURN "EXTENDED"
      | 123 => RETURN "FALSE"
      | 124 => RETURN "FIRST"
      | 125 => RETURN "FLOAT"
      | 126 => RETURN "FLOOR"
      | 127 => RETURN "INC"
      | 128 => RETURN "INTEGER"
      | 129 => RETURN "ISTYPE"
      | 130 => RETURN "LAST"
      | 131 => RETURN "LONGCARD"
      | 132 => RETURN "LONGINT"
      | 133 => RETURN "LONGREAL"
      | 134 => RETURN "LOOPHOLE"
      | 135 => RETURN "MAX"
      | 136 => RETURN "MIN"
      | 137 => RETURN "MUTEX"
      | 138 => RETURN "NARROW"
      | 139 => RETURN "NEW"
      | 140 => RETURN "NIL"
      | 141 => RETURN "NULL"
      | 142 => RETURN "NUMBER"
      | 143 => RETURN "ORD"
      | 144 => RETURN "REAL"
      | 145 => RETURN "REFANY"
      | 146 => RETURN "ROUND"
      | 147 => RETURN "SUBARRAY"
      | 148 => RETURN "TEXT"
      | 149 => RETURN "TRUE"
      | 150 => RETURN "TRUNC"
      | 151 => RETURN "TYPECODE"
      | 152 => RETURN "VAL"
      | 153 => RETURN "WIDECHAR"
      | 154 => RETURN "<ROOT>"
      | 155 => RETURN "<UNTRACED_ROOT>"
      | 156 => RETURN "T"
      | 157 => RETURN "Size"
      | 158 => RETURN "Plus"
      | 159 => RETURN "Times"
      | 160 => RETURN "Minus"
      | 161 => RETURN "Divide"
      | 162 => RETURN "Mod"
      | 163 => RETURN "LT"
      | 164 => RETURN "LE"
      | 165 => RETURN "GT"
      | 166 => RETURN "GE"
      | 167 => RETURN "And"
      | 168 => RETURN "Or"
      | 169 => RETURN "Xor"
      | 170 => RETURN "Not"
      | 171 => RETURN "Shift"
      | 172 => RETURN "LeftShift"
      | 173 => RETURN "RightShift"
      | 174 => RETURN "Rotate"
      | 175 => RETURN "LeftRotate"
      | 176 => RETURN "RightRotate"
      | 177 => RETURN "Extract"
      | 178 => RETURN "Insert"
      | 184 => RETURN "Long.T"
      | 185 => RETURN "Long.Size"
      | 186 => RETURN "Long.Plus"
      | 187 => RETURN "Long.Times"
      | 188 => RETURN "Long.Minus"
      | 189 => RETURN "Long.Divide"
      | 190 => RETURN "Long.Mod"
      | 191 => RETURN "Long.LT"
      | 192 => RETURN "Long.LE"
      | 193 => RETURN "Long.GT"
      | 194 => RETURN "Long.GE"
      | 195 => RETURN "Long.And"
      | 196 => RETURN "Long.Or"
      | 197 => RETURN "Long.Xor"
      | 198 => RETURN "Long.Not"
      | 199 => RETURN "Long.Shift"
      | 200 => RETURN "Long.LeftShift"
      | 201 => RETURN "Long.RightShift"
      | 202 => RETURN "Long.Rotate"
      | 203 => RETURN "Long.LeftRotate"
      | 204 => RETURN "Long.RightRotate"
      | 205 => RETURN "Long.Extract"
      | 206 => RETURN "Long.Insert"
      | 207 => RETURN "\XFF"
      | 208 => RETURN "\XFF"
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
      | 109 => RETURN "RidNull"
      | 110 => RETURN "RidABS"
      | 111 => RETURN "RidADDRESS"
      | 112 => RETURN "RidADR"
      | 113 => RETURN "RidADRSIZE"
      | 114 => RETURN "RidBITSIZE"
      | 115 => RETURN "RidBOOLEAN"
      | 116 => RETURN "RidBYTESIZE"
      | 117 => RETURN "RidCARDINAL"
      | 118 => RETURN "RidCEILING"
      | 119 => RETURN "RidCHAR"
      | 120 => RETURN "RidDEC"
      | 121 => RETURN "RidDISPOSE"
      | 122 => RETURN "RidEXTENDED"
      | 123 => RETURN "RidFALSE"
      | 124 => RETURN "RidFIRST"
      | 125 => RETURN "RidFLOAT"
      | 126 => RETURN "RidFLOOR"
      | 127 => RETURN "RidINC"
      | 128 => RETURN "RidINTEGER"
      | 129 => RETURN "RidISTYPE"
      | 130 => RETURN "RidLAST"
      | 131 => RETURN "RidLONGCARD"
      | 132 => RETURN "RidLONGINT"
      | 133 => RETURN "RidLONGREAL"
      | 134 => RETURN "RidLOOPHOLE"
      | 135 => RETURN "RidMAX"
      | 136 => RETURN "RidMIN"
      | 137 => RETURN "RidMUTEX"
      | 138 => RETURN "RidNARROW"
      | 139 => RETURN "RidNEW"
      | 140 => RETURN "RidNIL"
      | 141 => RETURN "RidNULL"
      | 142 => RETURN "RidNUMBER"
      | 143 => RETURN "RidORD"
      | 144 => RETURN "RidREAL"
      | 145 => RETURN "RidREFANY"
      | 146 => RETURN "RidROUND"
      | 147 => RETURN "RidSUBARRAY"
      | 148 => RETURN "RidTEXT"
      | 149 => RETURN "RidTRUE"
      | 150 => RETURN "RidTRUNC"
      | 151 => RETURN "RidTYPECODE"
      | 152 => RETURN "RidVAL"
      | 153 => RETURN "RidWIDECHAR"
      | 154 => RETURN "RidROOT"
      | 155 => RETURN "RidUNTRACEDROOT"
      | 156 => RETURN "Word_T"
      | 157 => RETURN "Word_Size"
      | 158 => RETURN "Word_Plus"
      | 159 => RETURN "Word_Times"
      | 160 => RETURN "Word_Minus"
      | 161 => RETURN "Word_Divide"
      | 162 => RETURN "Word_Mod"
      | 163 => RETURN "Word_LT"
      | 164 => RETURN "Word_LE"
      | 165 => RETURN "Word_GT"
      | 166 => RETURN "Word_GE"
      | 167 => RETURN "Word_And"
      | 168 => RETURN "Word_Or"
      | 169 => RETURN "Word_Xor"
      | 170 => RETURN "Word_Not"
      | 171 => RETURN "Word_Shift"
      | 172 => RETURN "Word_LeftShift"
      | 173 => RETURN "Word_RightShift"
      | 174 => RETURN "Word_Rotate"
      | 175 => RETURN "Word_LeftRotate"
      | 176 => RETURN "Word_RightRotate"
      | 177 => RETURN "Word_Extract"
      | 178 => RETURN "Word_Insert"
      | 184 => RETURN "Long_T"
      | 185 => RETURN "Long_Size"
      | 186 => RETURN "Long_Plus"
      | 187 => RETURN "Long_Times"
      | 188 => RETURN "Long_Minus"
      | 189 => RETURN "Long_Divide"
      | 190 => RETURN "Long_Mod"
      | 191 => RETURN "Long_LT"
      | 192 => RETURN "Long_LE"
      | 193 => RETURN "Long_GT"
      | 194 => RETURN "Long_GE"
      | 195 => RETURN "Long_And"
      | 196 => RETURN "Long_Or"
      | 197 => RETURN "Long_Xor"
      | 198 => RETURN "Long_Not"
      | 199 => RETURN "Long_Shift"
      | 200 => RETURN "Long_LeftShift"
      | 201 => RETURN "Long_RightShift"
      | 202 => RETURN "Long_Rotate"
      | 203 => RETURN "Long_LeftRotate"
      | 204 => RETURN "Long_RightRotate"
      | 205 => RETURN "Long_Extract"
      | 206 => RETURN "Long_Insert"
      | 207 => RETURN "StkUnaryPlus"
      | 208 => RETURN "StkUnaryMinus"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

; BEGIN 
  END FM3SrcToks
.

