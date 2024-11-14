
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
      | -203 => RETURN "<TokMinPredef>"
      | -202 => RETURN "Insert"
      | -201 => RETURN "Extract"
      | -200 => RETURN "RightRotate"
      | -199 => RETURN "LeftRotate"
      | -198 => RETURN "Rotate"
      | -197 => RETURN "RightShift"
      | -196 => RETURN "LeftShift"
      | -195 => RETURN "Shift"
      | -194 => RETURN "Not"
      | -193 => RETURN "Xor"
      | -192 => RETURN "Or"
      | -191 => RETURN "And"
      | -190 => RETURN "GE"
      | -189 => RETURN "GT"
      | -188 => RETURN "LE"
      | -187 => RETURN "LT"
      | -186 => RETURN "Mod"
      | -185 => RETURN "Divide"
      | -184 => RETURN "Minus"
      | -183 => RETURN "Times"
      | -182 => RETURN "Plus"
      | -181 => RETURN "Size"
      | -180 => RETURN "T"
      | -179 => RETURN "Insert"
      | -178 => RETURN "Extract"
      | -177 => RETURN "RightRotate"
      | -176 => RETURN "LeftRotate"
      | -175 => RETURN "Rotate"
      | -174 => RETURN "RightShift"
      | -173 => RETURN "LeftShift"
      | -172 => RETURN "Shift"
      | -171 => RETURN "Not"
      | -170 => RETURN "Xor"
      | -169 => RETURN "Or"
      | -168 => RETURN "And"
      | -167 => RETURN "GE"
      | -166 => RETURN "GT"
      | -165 => RETURN "LE"
      | -164 => RETURN "LT"
      | -163 => RETURN "Mod"
      | -162 => RETURN "Divide"
      | -161 => RETURN "Minus"
      | -160 => RETURN "Times"
      | -159 => RETURN "Plus"
      | -158 => RETURN "Size"
      | -157 => RETURN "T"
      | -156 => RETURN "<UNTRACED_ROOT>"
      | -155 => RETURN "<ROOT>"
      | -154 => RETURN "WIDECHAR"
      | -153 => RETURN "VAL"
      | -152 => RETURN "TYPECODE"
      | -151 => RETURN "TRUNC"
      | -150 => RETURN "TRUE"
      | -149 => RETURN "TEXT"
      | -148 => RETURN "SUBARRAY"
      | -147 => RETURN "ROUND"
      | -146 => RETURN "REFANY"
      | -145 => RETURN "REAL"
      | -144 => RETURN "ORD"
      | -143 => RETURN "NUMBER"
      | -142 => RETURN "NULL"
      | -141 => RETURN "NIL"
      | -140 => RETURN "NEW"
      | -139 => RETURN "NARROW"
      | -138 => RETURN "MUTEX"
      | -137 => RETURN "MIN"
      | -136 => RETURN "MAX"
      | -135 => RETURN "LOOPHOLE"
      | -134 => RETURN "LONGREAL"
      | -133 => RETURN "LONGINT"
      | -132 => RETURN "LONGCARD"
      | -131 => RETURN "LAST"
      | -130 => RETURN "ISTYPE"
      | -129 => RETURN "INTEGER"
      | -128 => RETURN "INC"
      | -127 => RETURN "FLOOR"
      | -126 => RETURN "FLOAT"
      | -125 => RETURN "FIRST"
      | -124 => RETURN "FALSE"
      | -123 => RETURN "EXTENDED"
      | -122 => RETURN "DISPOSE"
      | -121 => RETURN "DEC"
      | -120 => RETURN "CHAR"
      | -119 => RETURN "CEILING"
      | -118 => RETURN "CARDINAL"
      | -117 => RETURN "BYTESIZE"
      | -116 => RETURN "BOOLEAN"
      | -115 => RETURN "BITSIZE"
      | -114 => RETURN "ADRSIZE"
      | -113 => RETURN "ADR"
      | -112 => RETURN "ADDRESS"
      | -111 => RETURN "ABS"
      | -110 => RETURN "<RidNull>"
      | -109 => RETURN "<TokMaxPredef>"
      | -108 => RETURN "StkPragmaId"
      | -107 => RETURN "StkLexErrChars"
      | -106 => RETURN "StkWideCharLit"
      | -105 => RETURN "StkCharLit"
      | -104 => RETURN "StkWideTextLit"
      | -103 => RETURN "StkTextLit"
      | -102 => RETURN "StkExtendedLit"
      | -101 => RETURN "StkLongRealLit"
      | -100 => RETURN "StkRealLit"
      | -99 => RETURN "StkLongBasedLit"
      | -98 => RETURN "StkBasedLit"
      | -97 => RETURN "StkLongIntLit"
      | -96 => RETURN "StkIntLit"
      | -95 => RETURN "StkIdent"
      | -94 => RETURN "*>"
      | -93 => RETURN "<*"
      | -92 => RETURN "^"
      | -91 => RETURN "/"
      | -90 => RETURN "*"
      | -89 => RETURN "&"
      | -88 => RETURN "-"
      | -87 => RETURN "+"
      | -86 => RETURN ">="
      | -85 => RETURN "<="
      | -84 => RETURN ">"
      | -83 => RETURN "<"
      | -82 => RETURN "#"
      | -81 => RETURN "]"
      | -80 => RETURN "["
      | -79 => RETURN ".."
      | -78 => RETURN "=>"
      | -77 => RETURN "|"
      | -76 => RETURN "}"
      | -75 => RETURN "{"
      | -74 => RETURN ":="
      | -73 => RETURN "<:"
      | -72 => RETURN ":"
      | -71 => RETURN ","
      | -70 => RETURN ")"
      | -69 => RETURN "("
      | -68 => RETURN "="
      | -67 => RETURN "."
      | -66 => RETURN ";"
      | -65 => RETURN "WITH"
      | -64 => RETURN "WHILE"
      | -63 => RETURN "VAR"
      | -62 => RETURN "VALUE"
      | -61 => RETURN "UNTRACED"
      | -60 => RETURN "UNTIL"
      | -59 => RETURN "UNSAFE"
      | -58 => RETURN "TYPECASE"
      | -57 => RETURN "TYPE"
      | -56 => RETURN "TRY"
      | -55 => RETURN "TO"
      | -54 => RETURN "THEN"
      | -53 => RETURN "SET"
      | -52 => RETURN "ROOT"
      | -51 => RETURN "REVEAL"
      | -50 => RETURN "RETURN"
      | -49 => RETURN "REPEAT"
      | -48 => RETURN "REF"
      | -47 => RETURN "RECORD"
      | -46 => RETURN "READONLY"
      | -45 => RETURN "RAISES"
      | -44 => RETURN "RAISE"
      | -43 => RETURN "PROCEDURE"
      | -42 => RETURN "OVERRIDES"
      | -41 => RETURN "OR"
      | -40 => RETURN "OF"
      | -39 => RETURN "OBJECT"
      | -38 => RETURN "NOT"
      | -37 => RETURN "MODULE"
      | -36 => RETURN "MOD"
      | -35 => RETURN "METHODS"
      | -34 => RETURN "LOOP"
      | -33 => RETURN "LOCK"
      | -32 => RETURN "INTERFACE"
      | -31 => RETURN "IN"
      | -30 => RETURN "IMPORT"
      | -29 => RETURN "IF"
      | -28 => RETURN "GENERIC"
      | -27 => RETURN "FROM"
      | -26 => RETURN "FOR"
      | -25 => RETURN "FINALLY"
      | -24 => RETURN "EXPORTS"
      | -23 => RETURN "EXIT"
      | -22 => RETURN "EXCEPTION"
      | -21 => RETURN "EXCEPT"
      | -20 => RETURN "EVAL"
      | -19 => RETURN "END"
      | -18 => RETURN "ELSIF"
      | -17 => RETURN "ELSE"
      | -16 => RETURN "DO"
      | -15 => RETURN "DIV"
      | -14 => RETURN "CONST"
      | -13 => RETURN "CASE"
      | -12 => RETURN "BY"
      | -11 => RETURN "BRANDED"
      | -10 => RETURN "BITS"
      | -9 => RETURN "BEGIN"
      | -8 => RETURN "AS"
      | -7 => RETURN "ARRAY"
      | -6 => RETURN "ANY"
      | -5 => RETURN "AND"
      | -2 => RETURN "StkBOF"
      | -1 => RETURN "StkUnknown"
      | 0 => RETURN "StkEOF"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Image

(*EXPORTED*)
; PROCEDURE Name ( TokNo : TokTyp ) : TEXT 

  = BEGIN 
      CASE TokNo OF 
      | -203 => RETURN "TokMinPredef"
      | -202 => RETURN "Long_Insert"
      | -201 => RETURN "Long_Extract"
      | -200 => RETURN "Long_RightRotate"
      | -199 => RETURN "Long_LeftRotate"
      | -198 => RETURN "Long_Rotate"
      | -197 => RETURN "Long_RightShift"
      | -196 => RETURN "Long_LeftShift"
      | -195 => RETURN "Long_Shift"
      | -194 => RETURN "Long_Not"
      | -193 => RETURN "Long_Xor"
      | -192 => RETURN "Long_Or"
      | -191 => RETURN "Long_And"
      | -190 => RETURN "Long_GE"
      | -189 => RETURN "Long_GT"
      | -188 => RETURN "Long_LE"
      | -187 => RETURN "Long_LT"
      | -186 => RETURN "Long_Mod"
      | -185 => RETURN "Long_Divide"
      | -184 => RETURN "Long_Minus"
      | -183 => RETURN "Long_Times"
      | -182 => RETURN "Long_Plus"
      | -181 => RETURN "Long_Size"
      | -180 => RETURN "Long_T"
      | -179 => RETURN "Word_Insert"
      | -178 => RETURN "Word_Extract"
      | -177 => RETURN "Word_RightRotate"
      | -176 => RETURN "Word_LeftRotate"
      | -175 => RETURN "Word_Rotate"
      | -174 => RETURN "Word_RightShift"
      | -173 => RETURN "Word_LeftShift"
      | -172 => RETURN "Word_Shift"
      | -171 => RETURN "Word_Not"
      | -170 => RETURN "Word_Xor"
      | -169 => RETURN "Word_Or"
      | -168 => RETURN "Word_And"
      | -167 => RETURN "Word_GE"
      | -166 => RETURN "Word_GT"
      | -165 => RETURN "Word_LE"
      | -164 => RETURN "Word_LT"
      | -163 => RETURN "Word_Mod"
      | -162 => RETURN "Word_Divide"
      | -161 => RETURN "Word_Minus"
      | -160 => RETURN "Word_Times"
      | -159 => RETURN "Word_Plus"
      | -158 => RETURN "Word_Size"
      | -157 => RETURN "Word_T"
      | -156 => RETURN "RidUNTRACEDROOT"
      | -155 => RETURN "RidROOT"
      | -154 => RETURN "RidWIDECHAR"
      | -153 => RETURN "RidVAL"
      | -152 => RETURN "RidTYPECODE"
      | -151 => RETURN "RidTRUNC"
      | -150 => RETURN "RidTRUE"
      | -149 => RETURN "RidTEXT"
      | -148 => RETURN "RidSUBARRAY"
      | -147 => RETURN "RidROUND"
      | -146 => RETURN "RidREFANY"
      | -145 => RETURN "RidREAL"
      | -144 => RETURN "RidORD"
      | -143 => RETURN "RidNUMBER"
      | -142 => RETURN "RidNULL"
      | -141 => RETURN "RidNIL"
      | -140 => RETURN "RidNEW"
      | -139 => RETURN "RidNARROW"
      | -138 => RETURN "RidMUTEX"
      | -137 => RETURN "RidMIN"
      | -136 => RETURN "RidMAX"
      | -135 => RETURN "RidLOOPHOLE"
      | -134 => RETURN "RidLONGREAL"
      | -133 => RETURN "RidLONGINT"
      | -132 => RETURN "RidLONGCARD"
      | -131 => RETURN "RidLAST"
      | -130 => RETURN "RidISTYPE"
      | -129 => RETURN "RidINTEGER"
      | -128 => RETURN "RidINC"
      | -127 => RETURN "RidFLOOR"
      | -126 => RETURN "RidFLOAT"
      | -125 => RETURN "RidFIRST"
      | -124 => RETURN "RidFALSE"
      | -123 => RETURN "RidEXTENDED"
      | -122 => RETURN "RidDISPOSE"
      | -121 => RETURN "RidDEC"
      | -120 => RETURN "RidCHAR"
      | -119 => RETURN "RidCEILING"
      | -118 => RETURN "RidCARDINAL"
      | -117 => RETURN "RidBYTESIZE"
      | -116 => RETURN "RidBOOLEAN"
      | -115 => RETURN "RidBITSIZE"
      | -114 => RETURN "RidADRSIZE"
      | -113 => RETURN "RidADR"
      | -112 => RETURN "RidADDRESS"
      | -111 => RETURN "RidABS"
      | -110 => RETURN "RidNull"
      | -109 => RETURN "TokMaxPredef"
      | -108 => RETURN "StkPragmaId"
      | -107 => RETURN "StkLexErrChars"
      | -106 => RETURN "StkWideCharLit"
      | -105 => RETURN "StkCharLit"
      | -104 => RETURN "StkWideTextLit"
      | -103 => RETURN "StkTextLit"
      | -102 => RETURN "StkExtendedLit"
      | -101 => RETURN "StkLongRealLit"
      | -100 => RETURN "StkRealLit"
      | -99 => RETURN "StkLongBasedLit"
      | -98 => RETURN "StkBasedLit"
      | -97 => RETURN "StkLongIntLit"
      | -96 => RETURN "StkIntLit"
      | -95 => RETURN "StkIdent"
      | -94 => RETURN "StkClosePragma"
      | -93 => RETURN "StkOpenPragma"
      | -92 => RETURN "StkDeref"
      | -91 => RETURN "StkSlash"
      | -90 => RETURN "StkStar"
      | -89 => RETURN "StkAmpersand"
      | -88 => RETURN "StkMinus"
      | -87 => RETURN "StkPlus"
      | -86 => RETURN "StkGreaterEqual"
      | -85 => RETURN "StkLessEqual"
      | -84 => RETURN "StkGreater"
      | -83 => RETURN "StkLess"
      | -82 => RETURN "StkUnequal"
      | -81 => RETURN "StkCloseBracket"
      | -80 => RETURN "StkOpenBracket"
      | -79 => RETURN "StkEllipsis"
      | -78 => RETURN "StkArrow"
      | -77 => RETURN "StkStroke"
      | -76 => RETURN "StkCloseBrace"
      | -75 => RETURN "StkOpenBrace"
      | -74 => RETURN "StkBecomes"
      | -73 => RETURN "StkSubtype"
      | -72 => RETURN "StkColon"
      | -71 => RETURN "StkComma"
      | -70 => RETURN "StkCloseParen"
      | -69 => RETURN "StkOpenParen"
      | -68 => RETURN "StkEqual"
      | -67 => RETURN "StkDot"
      | -66 => RETURN "StkSemicolon"
      | -65 => RETURN "StkRwWITH"
      | -64 => RETURN "StkRwWHILE"
      | -63 => RETURN "StkRwVAR"
      | -62 => RETURN "StkRwVALUE"
      | -61 => RETURN "StkRwUNTRACED"
      | -60 => RETURN "StkRwUNTIL"
      | -59 => RETURN "StkRwUNSAFE"
      | -58 => RETURN "StkRwTYPECASE"
      | -57 => RETURN "StkRwTYPE"
      | -56 => RETURN "StkRwTRY"
      | -55 => RETURN "StkRwTO"
      | -54 => RETURN "StkRwTHEN"
      | -53 => RETURN "StkRwSET"
      | -52 => RETURN "StkRwROOT"
      | -51 => RETURN "StkRwREVEAL"
      | -50 => RETURN "StkRwRETURN"
      | -49 => RETURN "StkRwREPEAT"
      | -48 => RETURN "StkRwREF"
      | -47 => RETURN "StkRwRECORD"
      | -46 => RETURN "StkRwREADONLY"
      | -45 => RETURN "StkRwRAISES"
      | -44 => RETURN "StkRwRAISE"
      | -43 => RETURN "StkRwPROCEDURE"
      | -42 => RETURN "StkRwOVERRIDES"
      | -41 => RETURN "StkRwOR"
      | -40 => RETURN "StkRwOF"
      | -39 => RETURN "StkRwOBJECT"
      | -38 => RETURN "StkRwNOT"
      | -37 => RETURN "StkRwMODULE"
      | -36 => RETURN "StkRwMOD"
      | -35 => RETURN "StkRwMETHODS"
      | -34 => RETURN "StkRwLOOP"
      | -33 => RETURN "StkRwLOCK"
      | -32 => RETURN "StkRwINTERFACE"
      | -31 => RETURN "StkRwIN"
      | -30 => RETURN "StkRwIMPORT"
      | -29 => RETURN "StkRwIF"
      | -28 => RETURN "StkRwGENERIC"
      | -27 => RETURN "StkRwFROM"
      | -26 => RETURN "StkRwFOR"
      | -25 => RETURN "StkRwFINALLY"
      | -24 => RETURN "StkRwEXPORTS"
      | -23 => RETURN "StkRwEXIT"
      | -22 => RETURN "StkRwEXCEPTION"
      | -21 => RETURN "StkRwEXCEPT"
      | -20 => RETURN "StkRwEVAL"
      | -19 => RETURN "StkRwEND"
      | -18 => RETURN "StkRwELSIF"
      | -17 => RETURN "StkRwELSE"
      | -16 => RETURN "StkRwDO"
      | -15 => RETURN "StkRwDIV"
      | -14 => RETURN "StkRwCONST"
      | -13 => RETURN "StkRwCASE"
      | -12 => RETURN "StkRwBY"
      | -11 => RETURN "StkRwBRANDED"
      | -10 => RETURN "StkRwBITS"
      | -9 => RETURN "StkRwBEGIN"
      | -8 => RETURN "StkRwAS"
      | -7 => RETURN "StkRwARRAY"
      | -6 => RETURN "StkRwANY"
      | -5 => RETURN "StkRwAND"
      | -2 => RETURN "StkBOF"
      | -1 => RETURN "StkUnknown"
      | 0 => RETURN "StkEOF"
      ELSE RETURN "<Undef>"
      END (*CASE*) 
    END Name

; BEGIN 
  END FM3SrcToks
.

