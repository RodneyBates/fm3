(* file Rm3Scanner.rex *) 


EXPORT  {

IMPORT Rm3SrcToks ; 

FROM StringMem  IMPORT tStringRef;
FROM Idents     IMPORT tIdent;





FROM Tokens     IMPORT TokIdent, TokDecConst, TokOctalConst, TokHexConst,
                       TokCharConst, TokRealConst, TokStringConst;
FROM Positions  IMPORT tPosition;

TYPE
  tScanAttribute        = RECORD        (* type for token attributes    *)
                          Position      : tPosition     ;
        CASE : SHORTCARD OF
        | TokIdent      : Ident         : tIdent        ;
        | TokDecConst   ,
          TokOctalConst ,
          TokHexConst   : IntValue      : CARDINAL      ;
        | TokCharConst  : CharValue     : CHAR          ;
        | TokRealConst  : RealValue     : tStringRef    ;
        | TokStringConst: StringValue   : tStringRef    ;
        END;
  END;

PROCEDURE ErrorAttribute (Token: CARDINAL; VAR Attribute: tScanAttribute);
                         (* Returns in parameter 'Attribute' default    *)
                         (* values for the attributes of token 'Token'  *)
}

GLOBAL  {
FROM Strings    IMPORT
  tString       , AssignEmpty   , Concatenate   , Append        ,
  SubString     , Length        , StringToNumber, ArrayToString ;

FROM StringMem  IMPORT
  tStringRef    , PutString     ;

FROM Idents     IMPORT
  tIdent        , NoIdent       , MakeIdent     ;

FROM Errors     IMPORT
  ErrorMessageP , Error         , Warning       ,
  IllegalChar   , UnclosedComment,UnclosedString;

VAR
  NestingLevel  : CARDINAL      ;
  DefaultString ,                       (* empty string         *)
  DefaultReal   : tStringRef    ;       (* 1.0                  *)
  String        : tString       ;

PROCEDURE ErrorAttribute (Token: CARDINAL; VAR Attribute: tScanAttribute);
BEGIN
  CASE Token OF
  | TokIdent            : Attribute.Ident       := NoIdent;
  | TokDecConst         ,
    TokOctalConst       ,
    TokHexConst         : Attribute.IntValue    := 1;
  | TokCharConst        : Attribute.CharValue   := CHR (0);
  | TokRealConst        : Attribute.RealValue   := DefaultReal;
  | TokStringConst      : Attribute.StringValue := DefaultString;
  ELSE ;
  END;
END ErrorAttribute;
}

LOCAL   { VAR String, S, Word: tString; }

BEGIN   {
  NestingLevel := 0;

  AssignEmpty (String);
  DefaultString := PutString (String);

  ArrayToString ("1.0", String);
  DefaultReal := PutString (String);
}

DEFAULT { ErrorMessageP (IllegalChar, Error, Attribute.Position); }

EOF     {
  IF yyStartState = Comment THEN
    ErrorMessageP (UnclosedComment, Error, Attribute.Position);
    NestingLevel := 0;
  END;
  IF yyStartState # STD THEN
    yyStart (STD);
  END;
}
   
DEFINE

  digit         = {0-9}.
  octalDigit    = {0-7}.
  hexDigit      = {0-9 a-f A-F}.
  letter        = {a-z A-Z}.
  CommentCh     = - {*(\t\n}.
  CharLitCh     = - {'\t\n}.
  TextLitCh     = - {"\t\n}.
  Blank         = " ".
  TAB           = \t.
  LF            = \n.
  CR            = \r.
  VT            = \v.
  FF            = \f. 

START   Comment, CharLit, TextLit

RULES

(* Comments: *) 
#STD#      "(*"         :  {NestingLevel := 1; yyStart (Comment);}

#Comment#  "(*"         :- {INC (NestingLevel);}
#Comment#  "*)"         :- {DEC (NestingLevel);
                            IF NestingLevel = 0 THEN yyStart (STD); END;
                           }
#Comment#  "(" | "*" | CommentCh + :- {}

(* Whitespace: *) 
(* These are as in Fmt.Blanks. *) 
#STD# Blank | TAB | LF | CR | VT | FF :- {} 

(* Character and Text literals: *) 

DEFINE 
  OctalEscape = "\" octalDigit  [3].
  WideOctalEscape = "\" octalDigit  [6].
  HexEscape = "\" ("h" | "\H" )  (hexDigit) [2] 
  WideHexEscape = "\" ("h" | "\H" ) (hexDigit) [4] 
  UnicodeHexEscape = "\U" (hexDigit) [6] 

#STD# digit +           ,
#STD# digit + / ".."    : {GetWord (Word);
                           Attribute.IntValue   := Lex . StringToNumber (Word, 10);
                           RETURN TokDecConst;}
#STD# octalDigit + B    : {GetWord (Word);
                           SubString (Word, 1, Length (Word) - 1, String);
                           Attribute.IntValue   := StringToNumber (String, 8);
                           RETURN TokOctalConst;}
#STD# octalDigit + C            : {GetWord (Word);
                           SubString (Word, 1, Length (Word) - 1, String);
                           Attribute.CharValue  := CHR (StringToNumber (String, 8));
                           RETURN TokCharConst;}
#STD# digit hexDigit * H : {
                           GetWord (Word);
                           SubString (Word, 1, Length (Word) - 1, String);
                           Attribute.IntValue   := StringToNumber (String, 16);
                           RETURN TokHexConst;}
#STD# digit + "." digit * (E {+\-} ? digit +) ? : {
                           GetWord (Word);
                           Attribute.RealValue  := PutString (Word);
                           RETURN TokRealConst;}

#STD#   '               :  {AssignEmpty (String); yyStart (Str1);}
#Str1#  StrCh1 +        :- {GetWord (S); Concatenate (String, S);}
#Str1#  '               :- {yyStart (STD);
                            Attribute.StringValue:= PutString (String);
                            RETURN TokStringConst;}

#STD#   \"              :  {AssignEmpty (String); yyStart (Str2);}
#Str2#  StrCh2 +        :- {GetWord (S); Concatenate (String, S);}
#Str2#  \"              :- {yyStart (STD);
                            Attribute.StringValue:= PutString (String);
                            RETURN TokStringConst;}

#Str1, Str2# \t         :- {Append (String, 11C); yyTab;}
#Str1, Str2# \n         :- {yyEol (0); yyStart (STD);
                            ErrorMessageP (UnclosedString, Error, Attribute.Position);
                            Attribute.StringValue:= PutString (String);
                            RETURN TokStringConst;}


(* Reserved words: *) 
"STD" "AND" : { RETURN Rm3SrcToks . RwAND ; }  
"STD" "ANY" : { RETURN Rm3SrcToks . RwANY ; }  
"STD" "ARRAY" : { RETURN Rm3SrcToks . RwARRAY ; }  
"STD" "AS" : { RETURN Rm3SrcToks . RwAS ; }  
"STD" "BEGIN" : { RETURN Rm3SrcToks . RwBEGIN ; }  
"STD" "BITS" : { RETURN Rm3SrcToks . RwBITS ; }  
"STD" "BRANDED" : { RETURN Rm3SrcToks . RwBRANDED ; }  
"STD" "BY" : { RETURN Rm3SrcToks . RwBY ; }  
"STD" "CASE" : { RETURN Rm3SrcToks . RwCASE ; }  
"STD" "CONST" : { RETURN Rm3SrcToks . RwCONST ; }  
"STD" "DIV" : { RETURN Rm3SrcToks . RwDIV ; }  
"STD" "DO" : { RETURN Rm3SrcToks . RwDO ; }  
"STD" "ELSE" : { RETURN Rm3SrcToks . RwELSE ; }  
"STD" "ELSIF" : { RETURN Rm3SrcToks . RwELSIF ; }  
"STD" "END" : { RETURN Rm3SrcToks . RwEND ; }  
"STD" "EVAL" : { RETURN Rm3SrcToks . RwEVAL ; }  
"STD" "EXCEPT" : { RETURN Rm3SrcToks . RwEXCEPT ; }  
"STD" "EXCEPTION" : { RETURN Rm3SrcToks . RwEXCEPTION ; }  
"STD" "EXIT" : { RETURN Rm3SrcToks . RwEXIT ; }  
"STD" "EXPORTS" : { RETURN Rm3SrcToks . RwEXPORTS ; }  
"STD" "FINALLY" : { RETURN Rm3SrcToks . RwFINALLY ; }  
"STD" "FOR" : { RETURN Rm3SrcToks . RwFOR ; }  
"STD" "FROM" : { RETURN Rm3SrcToks . RwFROM ; }  
"STD" "GENERIC" : { RETURN Rm3SrcToks . RwGENERIC ; }  
"STD" "IF" : { RETURN Rm3SrcToks . RwIF ; }  
"STD" "IMPORT" : { RETURN Rm3SrcToks . RwIMPORT ; }  
"STD" "IN" : { RETURN Rm3SrcToks . RwIN ; }  
"STD" "INTERFACE" : { RETURN Rm3SrcToks . RwINTERFACE ; }  
"STD" "LOCK" : { RETURN Rm3SrcToks . RwLOCK ; }  
"STD" "LOOP" : { RETURN Rm3SrcToks . RwLOOP ; }  
"STD" "METHODS" : { RETURN Rm3SrcToks . RwMETHODS ; }  
"STD" "MOD" : { RETURN Rm3SrcToks . RwMOD ; }  
"STD" "MODULE" : { RETURN Rm3SrcToks . RwMODULE ; }  
"STD" "NOT" : { RETURN Rm3SrcToks . RwNOT ; }  
"STD" "OBJECT" : { RETURN Rm3SrcToks . RwOBJECT ; }  
"STD" "OF" : { RETURN Rm3SrcToks . RwOF ; }  
"STD" "OR" : { RETURN Rm3SrcToks . RwOR ; }  
"STD" "OVERRIDES" : { RETURN Rm3SrcToks . RwOVERRIDES ; }  
"STD" "PROCEDURE" : { RETURN Rm3SrcToks . RwPROCEDURE ; }  
"STD" "RAISE" : { RETURN Rm3SrcToks . RwRAISE ; }  
"STD" "RAISES" : { RETURN Rm3SrcToks . RwRAISES ; }  
"STD" "READONLY" : { RETURN Rm3SrcToks . RwREADONLY ; }  
"STD" "RECORD" : { RETURN Rm3SrcToks . RwRECORD ; }  
"STD" "REF" : { RETURN Rm3SrcToks . RwREF ; }  
"STD" "REPEAT" : { RETURN Rm3SrcToks . RwREPEAT ; }  
"STD" "RETURN" : { RETURN Rm3SrcToks . RwRETURN Rm3SrcToks . ; }  
"STD" "REVEAL" : { RETURN Rm3SrcToks . RwREVEAL ; }  
"STD" "ROOT" : { RETURN Rm3SrcToks . RwROOT ; }  
"STD" "SET" : { RETURN Rm3SrcToks . RwSET ; }  
"STD" "THEN" : { RETURN Rm3SrcToks . RwTHEN ; }  
"STD" "TO" : { RETURN Rm3SrcToks . RwTO ; }  
"STD" "TRY" : { RETURN Rm3SrcToks . RwTRY ; }  
"STD" "TYPE" : { RETURN Rm3SrcToks . RwTYPE ; }  
"STD" "TYPECASE" : { RETURN Rm3SrcToks . RwTYPECASE ; }  
"STD" "UNSAFE" : { RETURN Rm3SrcToks . RwUNSAFE ; }  
"STD" "UNTIL" : { RETURN Rm3SrcToks . RwUNTIL ; }  
"STD" "UNTRACED" : { RETURN Rm3SrcToks . RwUNTRACED ; }  
"STD" "VALUE" : { RETURN Rm3SrcToks . RwVALUE ; }  
"STD" "VAR" : { RETURN Rm3SrcToks . RwVAR ; }  
"STD" "WHILE" : { RETURN Rm3SrcToks . RwWHILE ; }  
"STD" "WITH" : { RETURN Rm3SrcToks . RwWITH ; } 

(* Source code delimiters: *) 
"STD" ";" : { RETURN Rm3SrcToks . SdSemicolon ; }  
"STD" "." : { RETURN Rm3SrcToks . SdDot ; }  
"STD" "=" : { RETURN Rm3SrcToks . SdEqual ; }  
"STD" "(" : { RETURN Rm3SrcToks . SdOpenParen ; }  
"STD" ")" : { RETURN Rm3SrcToks . SdCloseParen ; }  
"STD" "," : { RETURN Rm3SrcToks . SdComma ; }  
"STD" ":" : { RETURN Rm3SrcToks . SdColon ; }  
"STD" "<:" : { RETURN Rm3SrcToks . SdSubtype ; }  
"STD" ":=" : { RETURN Rm3SrcToks . SdBecomes ; }  
"STD" "{" : { RETURN Rm3SrcToks . SdOpenBrace ; }  
"STD" "}" : { RETURN Rm3SrcToks . SdCloseBrace ; }  
"STD" "|" : { RETURN Rm3SrcToks . SdStroke ; }  
"STD" "=>" : { RETURN Rm3SrcToks . SdArrow ; }  
"STD" ".." : { RETURN Rm3SrcToks . SdEllipsis ; }  
"STD" "[" : { RETURN Rm3SrcToks . SdOpenBracket ; }  
"STD" "]" : { RETURN Rm3SrcToks . SdCloseBracket ; }  
"STD" "#" : { RETURN Rm3SrcToks . SdUnequal ; }  
"STD" "<" : { RETURN Rm3SrcToks . SdLess ; }  
"STD" ">" : { RETURN Rm3SrcToks . SdGreater ; }  
"STD" "<=" : { RETURN Rm3SrcToks . SdLessEqual ; }  
"STD" ">=" : { RETURN Rm3SrcToks . SdGreaterEqual ; }  
"STD" "+" : { RETURN Rm3SrcToks . SdPlus ; }  
"STD" "-" : { RETURN Rm3SrcToks . SdMinus ; }  
"STD" "&" : { RETURN Rm3SrcToks . SdAmpersand ; }  
"STD" "*" : { RETURN Rm3SrcToks . SdStar ; }  
"STD" "/" : { RETURN Rm3SrcToks . SdSlash ; }  
"STD" "^" : { RETURN Rm3SrcToks . SdDeref ; }  
"STD" ";" : { RETURN Rm3SrcToks . SdSemicolon ; }  
"STD" "." : { RETURN Rm3SrcToks . SdDot ; }  
"STD" "=" : { RETURN Rm3SrcToks . SdEqual ; }  
"STD" "(" : { RETURN Rm3SrcToks . SdOpenParen ; }  
"STD" ")" : { RETURN Rm3SrcToks . SdCloseParen ; }  
"STD" "," : { RETURN Rm3SrcToks . SdComma ; }  
"STD" ":" : { RETURN Rm3SrcToks . SdColon ; }  
"STD" "<:" : { RETURN Rm3SrcToks . SdSubtype ; }  
"STD" ":=" : { RETURN Rm3SrcToks . SdBecomes ; }  
"STD" "{" : { RETURN Rm3SrcToks . SdOpenBrace ; }  
"STD" "}" : { RETURN Rm3SrcToks . SdCloseBrace ; }  
"STD" "|" : { RETURN Rm3SrcToks . SdStroke ; }  
"STD" "=>" : { RETURN Rm3SrcToks . SdArrow ; }  
"STD" ".." : { RETURN Rm3SrcToks . SdEllipsis ; }  
"STD" "[" : { RETURN Rm3SrcToks . SdOpenBracket ; }  
"STD" "]" : { RETURN Rm3SrcToks . SdCloseBracket ; }  
"STD" "#" : { RETURN Rm3SrcToks . SdUnequal ; }  
"STD" "<" : { RETURN Rm3SrcToks . SdLess ; }  
"STD" ">" : { RETURN Rm3SrcToks . SdGreater ; }  
"STD" "<=" : { RETURN Rm3SrcToks . SdLessEqual ; }  
"STD" ">=" : { RETURN Rm3SrcToks . SdGreaterEqual ; }  
"STD" "+" : { RETURN Rm3SrcToks . SdPlus ; }  
"STD" "-" : { RETURN Rm3SrcToks . SdMinus ; }  
"STD" "&" : { RETURN Rm3SrcToks . SdAmpersand ; }  
"STD" "*" : { RETURN Rm3SrcToks . SdStar ; }  
"STD" "/" : { RETURN Rm3SrcToks . SdSlash ; }  
"STD" "^" : { RETURN Rm3SrcToks . SdDeref ; }  

(* Identifiers: *) 
#STD# letter (letter | digit | "_" ) * 
          : { GetWord (Word);
              Attribute.StringNo := NewStringNo (Word);
              RETURN Rm3SrcToks . TokIdent;
            }

