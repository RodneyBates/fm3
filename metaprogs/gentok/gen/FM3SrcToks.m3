
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
      | 109 => RETURN "%UnaryPlus"
      | 110 => RETURN "%UnaryMinus"
      | 111 => RETURN "<RidNull>"
      | 112 => RETURN "ABS"
      | 113 => RETURN "ADDRESS"
      | 114 => RETURN "ADR"
      | 115 => RETURN "ADRSIZE"
      | 116 => RETURN "BITSIZE"
      | 117 => RETURN "BOOLEAN"
      | 118 => RETURN "BYTESIZE"
      | 119 => RETURN "CARDINAL"
      | 120 => RETURN "CEILING"
      | 121 => RETURN "CHAR"
      | 122 => RETURN "DEC"
      | 123 => RETURN "DISPOSE"
      | 124 => RETURN "EXTENDED"
      | 125 => RETURN "FALSE"
      | 126 => RETURN "FIRST"
      | 127 => RETURN "FLOAT"
      | 128 => RETURN "FLOOR"
      | 129 => RETURN "INC"
      | 130 => RETURN "INTEGER"
      | 131 => RETURN "ISTYPE"
      | 132 => RETURN "LAST"
      | 133 => RETURN "LONGCARD"
      | 134 => RETURN "LONGINT"
      | 135 => RETURN "LONGREAL"
      | 136 => RETURN "LOOPHOLE"
      | 137 => RETURN "MAX"
      | 138 => RETURN "MIN"
      | 139 => RETURN "MUTEX"
      | 140 => RETURN "NARROW"
      | 141 => RETURN "NEW"
      | 142 => RETURN "NIL"
      | 143 => RETURN "NULL"
      | 144 => RETURN "NUMBER"
      | 145 => RETURN "ORD"
      | 146 => RETURN "REAL"
      | 147 => RETURN "REFANY"
      | 148 => RETURN "ROUND"
      | 149 => RETURN "SUBARRAY"
      | 150 => RETURN "TEXT"
      | 151 => RETURN "TRUE"
      | 152 => RETURN "TRUNC"
      | 153 => RETURN "TYPECODE"
      | 154 => RETURN "VAL"
      | 155 => RETURN "WIDECHAR"
      | 156 => RETURN "%ROOT"
      | 157 => RETURN "%UNTRACED_ROOT"
      | 158 => RETURN "Main"
      | 159 => RETURN "Text"
      | 160 => RETURN "Thread"
      | 161 => RETURN "Word"
      | 162 => RETURN "Long"
      | 163 => RETURN "Real"
      | 164 => RETURN "LongReal"
      | 165 => RETURN "Extended"
      | 166 => RETURN "RealFloat"
      | 167 => RETURN "LongRealFloat"
      | 168 => RETURN "ExtendedFloat"
      | 169 => RETURN "FloatMode"
      | 170 => RETURN "Lex"
      | 171 => RETURN "Fmt"
      | 172 => RETURN "T"
      | 173 => RETURN "Size"
      | 174 => RETURN "Plus"
      | 175 => RETURN "Times"
      | 176 => RETURN "Minus"
      | 177 => RETURN "Divide"
      | 178 => RETURN "Mod"
      | 179 => RETURN "LT"
      | 180 => RETURN "LE"
      | 181 => RETURN "GT"
      | 182 => RETURN "GE"
      | 183 => RETURN "And"
      | 184 => RETURN "Or"
      | 185 => RETURN "Xor"
      | 186 => RETURN "Not"
      | 187 => RETURN "Shift"
      | 188 => RETURN "LeftShift"
      | 189 => RETURN "RightShift"
      | 190 => RETURN "Rotate"
      | 191 => RETURN "LeftRotate"
      | 192 => RETURN "RightRotate"
      | 193 => RETURN "Extract"
      | 194 => RETURN "Insert"
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
      | 109 => RETURN "StkUnaryPlus"
      | 110 => RETURN "StkUnaryMinus"
      | 111 => RETURN "RidNull"
      | 112 => RETURN "RidABS"
      | 113 => RETURN "RidADDRESS"
      | 114 => RETURN "RidADR"
      | 115 => RETURN "RidADRSIZE"
      | 116 => RETURN "RidBITSIZE"
      | 117 => RETURN "RidBOOLEAN"
      | 118 => RETURN "RidBYTESIZE"
      | 119 => RETURN "RidCARDINAL"
      | 120 => RETURN "RidCEILING"
      | 121 => RETURN "RidCHAR"
      | 122 => RETURN "RidDEC"
      | 123 => RETURN "RidDISPOSE"
      | 124 => RETURN "RidEXTENDED"
      | 125 => RETURN "RidFALSE"
      | 126 => RETURN "RidFIRST"
      | 127 => RETURN "RidFLOAT"
      | 128 => RETURN "RidFLOOR"
      | 129 => RETURN "RidINC"
      | 130 => RETURN "RidINTEGER"
      | 131 => RETURN "RidISTYPE"
      | 132 => RETURN "RidLAST"
      | 133 => RETURN "RidLONGCARD"
      | 134 => RETURN "RidLONGINT"
      | 135 => RETURN "RidLONGREAL"
      | 136 => RETURN "RidLOOPHOLE"
      | 137 => RETURN "RidMAX"
      | 138 => RETURN "RidMIN"
      | 139 => RETURN "RidMUTEX"
      | 140 => RETURN "RidNARROW"
      | 141 => RETURN "RidNEW"
      | 142 => RETURN "RidNIL"
      | 143 => RETURN "RidNULL"
      | 144 => RETURN "RidNUMBER"
      | 145 => RETURN "RidORD"
      | 146 => RETURN "RidREAL"
      | 147 => RETURN "RidREFANY"
      | 148 => RETURN "RidROUND"
      | 149 => RETURN "RidSUBARRAY"
      | 150 => RETURN "RidTEXT"
      | 151 => RETURN "RidTRUE"
      | 152 => RETURN "RidTRUNC"
      | 153 => RETURN "RidTYPECODE"
      | 154 => RETURN "RidVAL"
      | 155 => RETURN "RidWIDECHAR"
      | 156 => RETURN "RidROOT"
      | 157 => RETURN "RidUNTRACEDROOT"
      | 158 => RETURN "StkPdMain"
      | 159 => RETURN "StkPdText"
      | 160 => RETURN "StkThread"
      | 161 => RETURN "StkPdWord"
      | 162 => RETURN "StkPdLong"
      | 163 => RETURN "StkPdReal"
      | 164 => RETURN "StkPdLongReal"
      | 165 => RETURN "StkPdExtended"
      | 166 => RETURN "StkPdRealFloat"
      | 167 => RETURN "StkPdLongRealFloat"
      | 168 => RETURN "StkPdExtendedFloat"
      | 169 => RETURN "StkPdFloatMode"
      | 170 => RETURN "StkPdLex"
      | 171 => RETURN "StkPdFmt"
      | 172 => RETURN "StkPdT"
      | 173 => RETURN "StkPdSize"
      | 174 => RETURN "StkPdPlus"
      | 175 => RETURN "StkPdTimes"
      | 176 => RETURN "StkPdMinus"
      | 177 => RETURN "StkPdDivide"
      | 178 => RETURN "StkPdMod"
      | 179 => RETURN "StkPdLT"
      | 180 => RETURN "StkPdLE"
      | 181 => RETURN "StkPdGT"
      | 182 => RETURN "StkPdGE"
      | 183 => RETURN "StkPdAnd"
      | 184 => RETURN "StkPdOr"
      | 185 => RETURN "StkPdXor"
      | 186 => RETURN "StkPdNot"
      | 187 => RETURN "StkPdShift"
      | 188 => RETURN "StkPdLeftShift"
      | 189 => RETURN "StkPdRightShift"
      | 190 => RETURN "StkPdRotate"
      | 191 => RETURN "StkPdLeftRotate"
      | 192 => RETURN "StkPdRightRotate"
      | 193 => RETURN "StkPdExtract"
      | 194 => RETURN "StkPdInsert"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

; BEGIN 
  END FM3SrcToks
.

