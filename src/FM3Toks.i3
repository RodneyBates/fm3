
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Toks

; TYPE TokTyp = [ 0 .. 16_FFFFF ] 
; CONST TkEOF = 108574 (* = 16_03FFE *)

(* Keep within 2^14-1, which will compress to at most two bytes.*) 

(* Variable terminal tokens.  These occur in both source code and
   intermediate streams, and at least some are likely to be
   especially common, especially TkIdent.
*)
   
; CONST TkIdent = 1
; CONST TkIntLit = 2 
; CONST TkLongIntLit = 3 
; CONST TkBasedLit = 4  
; CONST TkLongBasedLit = 5 
; CONST TkRealLit = 6
; CONST TkLongRealLit = 7
; CONST TkExtendedLit = 8 
; CONST TkTextLit = 9 
; CONST TkWideTextLit = 10 
; CONST TkCharLit = 11 
; CONST TkWideCharLit = 12
; CONST TkLexErrChars = 13
; CONST TkUnknown = 14 

(* Source code tokens.  These are near the high end of 2^14-1.
   They will not occur in intermediate streams.  This seemingly strange
   numbering was easy to produce, without individual manual editing.
*) 
; CONST TkRwAND = 433 (* = "AND" *) 
; CONST TkRwANY = 434 (* = "ANY" *) 
; CONST TkRwARRAY = 435 (* = "ARRAY" *) 
; CONST TkRwAS = 436 (* = "AS" *) 
; CONST TkRwBEGIN = 437 (* = "BEGIN" *) 
; CONST TkRwBITS = 438 (* = "BITS" *) 
; CONST TkRwBRANDED = 439 (* = "BRANDED" *) 
; CONST TkRwBY = 440 (* = "BY" *) 
; CONST TkRwCASE = 441 (* = "CASE" *) 
; CONST TkRwCONST = 442 (* = "CONST" *) 
; CONST TkRwDIV = 443 (* = "DIV" *) 
; CONST TkRwDO = 444 (* = "DO" *) 
; CONST TkRwELSE = 445 (* = "ELSE" *) 
; CONST TkRwELSIF = 446 (* = "ELSIF" *) 
; CONST TkRwEND = 447 (* = "END" *) 
; CONST TkRwEVAL = 448 (* = "EVAL" *) 
; CONST TkRwEXCEPT = 449 (* = "EXCEPT" *) 
; CONST TkRwEXCEPTION = 450 (* = "EXCEPTION" *) 
; CONST TkRwEXIT = 451 (* = "EXIT" *) 
; CONST TkRwEXPORTS = 452 (* = "EXPORTS" *) 
; CONST TkRwFINALLY = 453 (* = "FINALLY" *) 
; CONST TkRwFOR = 454 (* = "FOR" *) 
; CONST TkRwFROM = 455 (* = "FROM" *) 
; CONST TkRwGENERIC = 456 (* = "GENERIC" *) 
; CONST TkRwIF = 457 (* = "IF" *) 
; CONST TkRwIMPORT = 458 (* = "IMPORT" *) 
; CONST TkRwIN = 459 (* = "IN" *) 
; CONST TkRwINTERFACE = 460 (* = "INTERFACE" *) 
; CONST TkRwLOCK = 461 (* = "LOCK" *) 
; CONST TkRwLOOP = 462 (* = "LOOP" *) 
; CONST TkRwMETHODS = 463 (* = "METHODS" *) 
; CONST TkRwMOD = 464 (* = "MOD" *) 
; CONST TkRwMODULE = 465 (* = "MODULE" *) 
; CONST TkRwNOT = 466 (* = "NOT" *) 
; CONST TkRwOBJECT = 467 (* = "OBJECT" *) 
; CONST TkRwOF = 468 (* = "OF" *) 
; CONST TkRwOR = 469 (* = "OR" *) 
; CONST TkRwOVERRIDES = 470 (* = "OVERRIDES" *) 
; CONST TkRwPROCEDURE = 471 (* = "PROCEDURE" *) 
; CONST TkRwRAISE = 472 (* = "RAISE" *) 
; CONST TkRwRAISES = 473 (* = "RAISES" *) 
; CONST TkRwREADONLY = 474 (* = "READONLY" *) 
; CONST TkRwRECORD = 475 (* = "RECORD" *) 
; CONST TkRwREF = 476 (* = "REF" *) 
; CONST TkRwREPEAT = 477 (* = "REPEAT" *) 
; CONST TkRwRETURN = 478 (* = "RETURN" *) 
; CONST TkRwREVEAL = 479 (* = "REVEAL" *) 
; CONST TkRwROOT = 480 (* = "ROOT" *) 
; CONST TkRwSET = 481 (* = "SET" *) 
; CONST TkRwTHEN = 482 (* = "THEN" *) 
; CONST TkRwTO = 483 (* = "TO" *) 
; CONST TkRwTRY = 484 (* = "TRY" *) 
; CONST TkRwTYPE = 485 (* = "TYPE" *) 
; CONST TkRwTYPECASE = 486 (* = "TYPECASE" *) 
; CONST TkRwUNSAFE = 487 (* = "UNSAFE" *) 
; CONST TkRwUNTIL = 488 (* = "UNTIL" *) 
; CONST TkRwUNTRACED = 489 (* = "UNTRACED" *) 
; CONST TkRwVALUE = 490 (* = "VALUE" *) 
; CONST TkRwVAR = 491 (* = "VAR" *) 
; CONST TkRwWHILE = 492 (* = "WHILE" *) 
; CONST TkRwWITH = 493 (* = "WITH" *) 
; CONST TkSemicolon = 494 (* = ";" *) 
; CONST TkDot = 495 (* = "." *) 
; CONST TkEqual = 496 (* = "=" *) 
; CONST TkOpenParen = 497 (* = "(" *) 
; CONST TkCloseParen = 498 (* = ")" *) 
; CONST TkComma = 499 (* = "," *) 
; CONST TkColon = 500 (* = ":" *) 
; CONST TkSubtype = 501 (* = "<:" *) 
; CONST TkBecomes = 502 (* = ":=" *) 
; CONST TkOpenBrace = 503 (* = "{" *) 
; CONST TkCloseBrace = 504 (* = "}" *) 
; CONST TkStroke = 505 (* = "|" *) 
; CONST TkArrow = 506 (* = "=>" *) 
; CONST TkEllipsis = 507 (* = ".." *) 
; CONST TkOpenBracket = 508 (* = "[" *) 
; CONST TkCloseBracket = 509 (* = "]" *) 
; CONST TkUnequal = 510 (* = "#" *) 
; CONST TkLess = 511 (* = "<" *) 
; CONST TkGreater = 512 (* = ">" *) 
; CONST TkLessEqual = 513 (* = "<=" *) 
; CONST TkGreaterEqual = 514 (* = ">=" *) 
; CONST TkPlus = 515 (* = "+" *) 
; CONST TkMinus = 516 (* = "-" *) 
; CONST TkAmpersand = 517 (* = "&" *) 
; CONST TkStar = 518 (* = "*" *) 
; CONST TkSlash = 519 (* = "/" *) 
; CONST TkDeref = 520 (* = "^" *)
; CONST TkOpenPragma = 521 (* = "<*" *) 
; CONST TkClosePragma = 522 (* = "*>" *) 

; END FM3Toks 
. 