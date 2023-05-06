(* Derived from: ** $Id: Parser.mi,v 2.8 1992/08/12 06:54:05 grosch rel $ *)
(* Skeleton parser for Cocktail lalr. *)
(* Modified Rodney M. Bates. Various times. rodney.m.bates@acm.org*)
(* Beginning 3-2023: 
    3-2023 Add generation of Module-3 code. 
    Further changes in https://github.com/RodneyBates/cocktail
*) 

(* Parser.m3. *) 

  UNSAFE MODULE FM3Parser;
(*^ BEWARE! lalr loses this character. *) 

  IMPORT FM3Scanner;

IMPORT Fmt, OSError, Rd, Thread, Text, Word, Wr;

IMPORT Positions, FrontErrors, Strings, IntSets, System;

IMPORT Errors (* From Reusem3. *);

(* line 24 "FM3Parser.lalr" *)
 FROM FM3ParseSem IMPORT PushTok , PushTokPatch;
    IMPORT FM3TokDef AS TD;
  

CONST
   yyInitStackSize      = 100;
   yyStackExpansionFactor = 2.0;
   yyNoState            = 0;

   yyFirstTerminal          = 0;
   yyLastTerminal           = 522;
   yyFirstSymbol            = 0;
   yyLastSymbol             = 545;
   yyTableMax               = 540;
   yyNTableMax              = 583;
   yyFirstReadState         = 1;
   yyLastReadState          = 51;
   yyFirstReadTermState             = 52;
   yyLastReadTermState              = 57;
   yyLastReadNontermState           = 67;
   yyFirstReduceState               = 68;
   yyLastReduceState                = 92;
   yyStartState             = 1;
   yyStopState              = 68;

TYPE
   M2SHORTCARD = [ 0 .. 16_FFFF ];
   yyTableElmt = M2SHORTCARD;
CONST yyTableElmtBits = BITSIZE ( yyTableElmt );

   (* The conversion to Modula-3 is very fragile, in part due to the
      use of unsafe address arithmetic.
      On the one hand, some types, in some contexts, need to be
      subranges (particularly, as fixed array subscript types),
      and other times, need to have the same size as in Modula-2, to avoid
      undermining various unsafe address arithmetic.  Modula-3 infers
      its own sizes from subranges, except for fields and elements when
      BITS FOR is used.  But assignments involving scalars with BIT FOR
      types present problems and even CM3 code generator failures.

      So if it is a BITS FOR type, its name ends in "Packed", otherwise
      not, the relevant ones ending in "Range".

      Additionally, CM3 has code generator failures assigning between two
      BIT FOR types, at times.  Actual cases where this has happened are
      replaced by two-step copies with an intermediate, unpacked temporary.
  
      These BITS FOR types must occupy exactly a Modula2-SHORTCARD, when used
      as elements or fields, but must their have subrange bounds when
      used as array subscript types. There a few places where a scalar
      of one of these also must occupy exactly a Modula2-SHORTCARD. 
   *)
CONST
   yyFirstFinalState    = yyFirstReadTermState;
   yyLastState          = yyLastReduceState;

TYPE
   yyTCombRangePacked      = BITS yyTableElmtBits FOR [0 .. yyTableMax];
   yyNCombRangePacked      = BITS yyTableElmtBits
                             FOR [yyLastTerminal + 1 .. yyNTableMax];
   yyStateRange            = [0 .. yyLastState];
   yyStateRangePacked      = BITS yyTableElmtBits FOR yyStateRange;
   yyReadRange             = [yyFirstReadState .. yyLastReadState];
   yyReadRangePacked       = BITS yyTableElmtBits FOR yyReadRange;
   yyReadReduceRangePacked = BITS yyTableElmtBits
                             FOR [yyFirstReadTermState .. yyLastReadNontermState];
   yyReduceRangePacked     = BITS yyTableElmtBits
                             FOR [yyFirstReduceState .. yyLastReduceState];
   yySymbolRange           = [yyFirstSymbol .. yyLastSymbol];
   yySymbolRangePacked     = BITS yyTableElmtBits FOR yySymbolRange;
   yyTCombType          = RECORD Check, Next: yyStateRangePacked; END;
   yyNCombType          = yyStateRangePacked;
   yyTCombTypePtr       = UNTRACED BRANDED REF  yyTCombType;
   yyNCombTypePtr       = UNTRACED BRANDED REF  yyNCombType;
   yyStackPtrType       = BITS yyTableElmtBits FOR yyTableElmt;
   yyStackType          = REF  ARRAY OF yyStateRangePacked;
   yyAttributeStackType = REF  ARRAY OF tParsAttribute;

VAR
   yyTBasePtr           : ARRAY [0 .. yyLastReadState] OF yyTCombTypePtr;
   yyNBasePtr           : ARRAY [0 .. yyLastReadState] OF yyNCombTypePtr;
   yyDefault            : ARRAY [0 .. yyLastReadState] OF yyReadRangePacked;
   yyTComb              : ARRAY yyTCombRangePacked OF yyTCombType;
   yyNComb              : ARRAY yyNCombRangePacked OF yyNCombType;
   yyLength             : ARRAY yyReduceRangePacked OF yyTableElmt;
   yyLeftHandSide       : ARRAY yyReduceRangePacked OF yySymbolRangePacked;
   yyContinuation       : ARRAY [0 .. yyLastReadState] OF yySymbolRangePacked;
   yyFinalToProd        : ARRAY yyReadReduceRangePacked OF yyReduceRangePacked;
   yyIsInitialized      : BOOLEAN;
   yyTableFile          : System.tFile;
   
(* From Parser.m30.orig: *) 
    PROCEDURE ExpandStateStack ( VAR Stack : yyStackType ; ToSize : INTEGER ) =

      VAR LOldStack : yyStackType;
      VAR LStackNumber : INTEGER; 
      BEGIN
        LStackNumber := NUMBER ( Stack ^ );
        IF LStackNumber < ToSize
        THEN
          LOldStack := Stack; 
          Stack := NEW ( yyStackType , ToSize );
          SUBARRAY ( Stack ^ , 0 , LStackNumber ) := LOldStack ^;
          LOldStack := NIL; 
        END;
      END ExpandStateStack; 

    PROCEDURE ExpandAttributeStack
      ( VAR Stack : yyAttributeStackType ; ToSize : INTEGER ) =

      VAR LOldStack : yyAttributeStackType;
      VAR LStackNumber : INTEGER; 
      BEGIN
        LStackNumber := NUMBER ( Stack ^ );
        IF LStackNumber < ToSize
        THEN
          LOldStack := Stack; 
          Stack := NEW ( yyAttributeStackType , ToSize );
          SUBARRAY ( Stack ^ , 0 , LStackNumber ) := LOldStack ^;
          LOldStack := NIL; 
        END; 
      END ExpandAttributeStack; 
(* END From Parser.m30.orig: *) 

(*EXPORTED*)
PROCEDURE TokenName (Token: INTEGER; VAR Name: TEXT) =
   BEGIN
      CASE Token OF
      | 0 => Name := "_EndOfFile";
      | 433 => Name := "TkRwAND";
      | 434 => Name := "TkRwANY";
      | 435 => Name := "TkRwARRAY";
      | 436 => Name := "TkRwAS";
      | 437 => Name := "TkRwBEGIN";
      | 438 => Name := "TkRwBITS";
      | 439 => Name := "TkRwBRANDED";
      | 440 => Name := "TkRwBY";
      | 441 => Name := "TkRwCASE";
      | 442 => Name := "TkRwCONST";
      | 443 => Name := "TkRwDIV";
      | 444 => Name := "TkRwDO";
      | 445 => Name := "TkRwELSE";
      | 446 => Name := "TkRwELSIF";
      | 447 => Name := "TkRwEND";
      | 448 => Name := "TkRwEVAL";
      | 449 => Name := "TkRwEXCEPT";
      | 450 => Name := "TkRwEXCEPTION";
      | 451 => Name := "TkRwEXIT";
      | 452 => Name := "TkRwEXPORTS";
      | 453 => Name := "TkRwFINALLY";
      | 454 => Name := "TkRwFOR";
      | 455 => Name := "TkRwFROM";
      | 456 => Name := "TkRwGENERIC";
      | 457 => Name := "TkRwIF";
      | 458 => Name := "TkRwIMPORT";
      | 459 => Name := "TkRwIN";
      | 460 => Name := "TkRwINTERFACE";
      | 461 => Name := "TkRwLOCK";
      | 462 => Name := "TkRwLOOP";
      | 463 => Name := "TkRwMETHODS";
      | 464 => Name := "TkRwMOD";
      | 465 => Name := "TkRwMODULE";
      | 466 => Name := "TkRwNOT";
      | 467 => Name := "TkRwOBJECT";
      | 468 => Name := "TkRwOF";
      | 469 => Name := "TkRwOR";
      | 470 => Name := "TkRwOVERRIDES";
      | 471 => Name := "TkRwPROCEDURE";
      | 472 => Name := "TkRwRAISE";
      | 473 => Name := "TkRwRAISES";
      | 474 => Name := "TkRwREADONLY";
      | 475 => Name := "TkRwRECORD";
      | 476 => Name := "TkRwREF";
      | 477 => Name := "TkRwREPEAT";
      | 478 => Name := "TkRwRETURN";
      | 479 => Name := "TkRwREVEAL";
      | 480 => Name := "TkRwROOT";
      | 481 => Name := "TkRwSET";
      | 482 => Name := "TkRwTHEN";
      | 483 => Name := "TkRwTO";
      | 484 => Name := "TkRwTRY";
      | 485 => Name := "TkRwTYPE";
      | 486 => Name := "TkRwTYPECASE";
      | 487 => Name := "TkRwUNSAFE";
      | 488 => Name := "TkRwUNTIL";
      | 489 => Name := "TkRwUNTRACED";
      | 490 => Name := "TkRwVALUE";
      | 491 => Name := "TkRwVAR";
      | 492 => Name := "TkRwWHILE";
      | 493 => Name := "TkRwWITH";
      | 494 => Name := "TkSemicolon";
      | 495 => Name := "TkDot";
      | 496 => Name := "TkEqual";
      | 497 => Name := "TkOpenParen";
      | 498 => Name := "TkCloseParen";
      | 499 => Name := "TkComma";
      | 500 => Name := "TkColon";
      | 501 => Name := "TkSubtype";
      | 502 => Name := "TkBecomes";
      | 503 => Name := "TkOpenBrace";
      | 504 => Name := "TkCloseBrace";
      | 505 => Name := "TkStroke";
      | 506 => Name := "TkArrow";
      | 507 => Name := "TkEllipsis";
      | 508 => Name := "TkOpenBracket";
      | 509 => Name := "TkCloseBracket";
      | 510 => Name := "TkUnequal";
      | 511 => Name := "TkLess";
      | 512 => Name := "TkGreater";
      | 513 => Name := "TkLessEqual";
      | 514 => Name := "TkGreaterEqual";
      | 515 => Name := "TkPlus";
      | 516 => Name := "TkMinus";
      | 517 => Name := "TkAmpersand";
      | 518 => Name := "TkStar";
      | 519 => Name := "TkSlash";
      | 520 => Name := "TkDeref";
      | 521 => Name := "TkOpenPragma";
      | 522 => Name := "TkClosePragma";
      ELSE Name := "" 
      END;
   END TokenName;

(*EXPORTED*)
  PROCEDURE FM3Parser (): CARDINAL =
(* line 29 "FM3Parser.lalr" *)
 
   VAR
      yyState           : yyStateRange;
      yyTerminal        : yySymbolRange;
      yyNonterminal     : yySymbolRange;        (* left-hand side symbol *)
      yyStackPtr        : yyStackPtrType;
      yyStackLAST       : INTEGER;
      yyStateStackSize  : INTEGER;
      yyAttrStackSize := yyStateStackSize; 
      (* yyStackPtr, yyStackLAST, and yyStateStackSize always apply equally
         to yyStateStack and yyAttributeStack. *)
      yyStateStack      : yyStackType;
      yyAttributeStack  : yyAttributeStackType;
      yySynAttribute    : tParsAttribute;       (* synthesized attribute *)
     yyRepairAttribute : FM3Scanner.tScanAttribute;
      yyRepairToken     : yySymbolRange;
      yyTCombPtr        : yyTCombTypePtr;
      yyNCombPtr        : yyNCombTypePtr;
      yyIsRepairing     : BOOLEAN;
      yyErrorCount      : CARDINAL;
      yyTokenString     : TEXT (*ARRAY [0..127] OF CHAR*);

   BEGIN (* FM3Parser *) 
     BeginFM3Parser ();
      yyState           := yyStartState;
     yyTerminal        := FM3Scanner.GetToken ();
      yyStateStackSize  := yyInitStackSize;
      yyAttrStackSize   := yyInitStackSize;
      yyStateStack := NEW ( yyStackType , yyStateStackSize );
      yyAttributeStack := NEW ( yyAttributeStackType , yyStateStackSize ); 
      yyStackLAST := LAST ( yyStateStack ^ ) (* Of yyAttributeStack too. *);
      yyStackPtr        := 0;
      yyErrorCount      := 0;
      yyIsRepairing     := FALSE;

      LOOP (* Through parsing actions. One iteration does:
              1) Any token deletions called for by an error.
              2) Any continuation token insertions called for 
                 after an error.
              3) Either: 
                 a) Possibly one read-reduce followed by 
                    a sequence of reduces
              or b) One read
           *)  
         (* Push state stack. *) 
         IF yyStackPtr >= yyStackLAST 
         THEN
            yyStateStackSize
              := MAX ( NUMBER ( yyStateStack ^ ) * 2 , yyStackPtr + 2 ); 
            ExpandStateStack ( yyStateStack , yyStateStackSize ); 
            ExpandAttributeStack ( yyAttributeStack , yyStateStackSize );
            yyStackLAST
              := LAST ( yyStateStack ^ ) (* Of yyAttributeStack too. *);
         END (* IF *) ;
         yyStateStack^ [yyStackPtr] := yyState;

         LOOP (* Through all continuation pushes, plus compute the state
                 after that. This loop also goes through the default state
                 computations. *) 
            (* SPEC State := Next (State, Terminal); terminal transition *)
            
            yyTCombPtr := LOOPHOLE 
                            ( LOOPHOLE ( yyTBasePtr [yyState] ,INTEGER) 
                              + yyTerminal * BYTESIZE (yyTCombType)
                            , yyTCombTypePtr
                            );
            IF yyTCombPtr^.Check = yyState 
            THEN
               yyState := yyTCombPtr^.Next;
               EXIT;
            END (* IF *) ;
            yyState := yyDefault [yyState];

            IF yyState = yyNoState 
            THEN (* syntax error *)
               yyState := yyStateStack^ [yyStackPtr];
               IF yyIsRepairing 
               THEN (* repair *)
                  yyRepairToken := yyContinuation [yyState];
                  yyState := Next (yyState, yyRepairToken);
                  IF yyState <= yyLastReadTermState 
                  THEN (* read or read terminal reduce ? *)
                    FM3Scanner.ErrorAttribute 
                       (yyRepairToken, yyRepairAttribute);
                     TokenName (yyRepairToken, yyTokenString);
                     FrontErrors.ErrorMessageI 
                       (FrontErrors.TokenInserted, FrontErrors.Repair,
                       FM3Scanner.Attribute.Position, FrontErrors.eArray, 
                        ADR (yyTokenString[FIRST(yyTokenString)])
                       );
                     IF yyState >= yyFirstFinalState 
                     THEN (* avoid second push *)
                        yyState := yyFinalToProd [yyState];
                     END (* IF *) ;
                     INC (yyStackPtr);
                     yyAttributeStack^ [yyStackPtr].Scan := yyRepairAttribute;
                     yyStateStack^     [yyStackPtr] := yyState;
                  END (* IF *) ;
                  IF yyState >= yyFirstFinalState 
                  THEN (* final state ? *)
                    EXIT;
                  END (* IF *) ;
               ELSE (* report and recover *)
                  INC (yyErrorCount);
                  ErrorRecovery 
                    (yyTerminal, yyStateStack,
                     NUMBER ( yyStateStack ^ ), yyStackPtr);
                  yyIsRepairing := TRUE;
               END (* IF *) ;
            END (* IF *) ;
         END (* LOOP *) ;

         IF yyState >= yyFirstFinalState 
         THEN (* final state ? *)
            IF yyState <= yyLastReadTermState 
            THEN (* read terminal reduce ? *)
               INC (yyStackPtr);
              yyAttributeStack^ [yyStackPtr].Scan := FM3Scanner.Attribute;
              yyTerminal := FM3Scanner.GetToken ();
               yyIsRepairing := FALSE;
            END (* IF *) ;

            LOOP (* Through successive reductions *)
CASE yyState OF
  | 68 =>  (* _0000_ : Compilation _EndOfFile .*)
  yyStateStack := NIL;
  yyAttributeStack := NIL;
  RETURN yyErrorCount;

  | 69,60 =>  (* Compilation : GenInterface .*)
  DEC (yyStackPtr, 1); yyNonterminal := 524;

  | 70,61 =>  (* Compilation : InstInterface .*)
  DEC (yyStackPtr, 1); yyNonterminal := 524;

  | 71,62 =>  (* Compilation : GenModule .*)
  DEC (yyStackPtr, 1); yyNonterminal := 524;

  | 72,63 =>  (* Compilation : InstModule .*)
  DEC (yyStackPtr, 1); yyNonterminal := 524;

  | 73,64 =>  (* Compilation : Interface .*)
  DEC (yyStackPtr, 1); yyNonterminal := 524;

  | 74,65 =>  (* Compilation : Module .*)
  DEC (yyStackPtr, 1); yyNonterminal := 524;

  | 75,52 =>  (* GenInterface : TkRwGENERIC TkRwINTERFACE TkIdent GenFormalsList TkSemicolon ImportList DeclList TkRwEND TkIdent TkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 523;

  | 76,55 =>  (* InstInterface : OptUnsafe TkRwINTERFACE TkIdent TkEqual TkIdent GenActals TkRwEND TkIdent TkDot .*)
  DEC (yyStackPtr, 9); yyNonterminal := 525;

  | 77,53 =>  (* GenModule : TkRwGENERIC TkRwMODULE TkIdent GenFormalsList TkSemicolon ImportList Block TkRwEND TkIdent TkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 526;

  | 78,57 =>  (* InstModule : OptUnsafeTkRwMODULE TkIdent Exports TkEqual TkIdent GenActals TkRwEND TkIdent TkDot .*)
  DEC (yyStackPtr, 9); yyNonterminal := 527;

  | 79,54 =>  (* Interface : OptUnsafe TkRwINTERFACE TkIdent TkSemicolon ImportList DeclList TkRwEND TkIdent TkDot .*)
  DEC (yyStackPtr, 9); yyNonterminal := 528;

  | 80,56 =>  (* Module : OptUnsafe TkRwMODULE TkIdent Exports TkSemicolon ImportList Block TkRwEND TkIdent TkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 529;

  | 81,67 =>  (* OptUnsafe : TkUNSAFE .*)
  DEC (yyStackPtr, 1); yyNonterminal := 534;
(* line 166 of "FM3Parser.lalr" *)
   yySynAttribute . PaBool := TRUE; 
  | 82 =>  (* OptUnsafe : .*)
yyNonterminal := 534;
(* line 167 of "FM3Parser.lalr" *)
   yySynAttribute . PaBool := FALSE; 
  | 83 =>  (* Exports : .*)
yyNonterminal := 538;
(* line 169 of "FM3Parser.lalr" *)
   
  | 84,66 =>  (* Exports : StkRwExports IdentList .*)
  DEC (yyStackPtr, 2); yyNonterminal := 538;
(* line 170 of "FM3Parser.lalr" *)
   
  | 85 =>  (* IdentList : .*)
yyNonterminal := 541;

  | 86 =>  (* IdentList : IdentListSub .*)
  DEC (yyStackPtr, 1); yyNonterminal := 541;
(* line 174 of "FM3Parser.lalr" *)
   WITH i = yyAttributeStack^[yyStackPtr+1] . PaInt
        DO PushTok ( TD . TkIdentListRt , i+1);
          PushTokPatch ( TD . TkIdentListLtPatch , 0 , i+1);
        END (*WITH*); 
      
  | 87 =>  (* IdentListSub : .*)
yyNonterminal := 542;
(* line 180 of "FM3Parser.lalr" *)
   yySynAttribute . PaInt := 0; 
  | 88,59 =>  (* IdentListSub : IdentListSub TkComma Ident .*)
  DEC (yyStackPtr, 3); yyNonterminal := 542;
(* line 182 of "FM3Parser.lalr" *)
   WITH i = yyAttributeStack^[yyStackPtr+1] . PaInt
        DO PushTok ( TD . TkIdentListRtElem , i );
          PushTokPatch ( TD . TkIdentListRtElemPatch , 0 , i );
          yySynAttribute . PaInt := i+1;
        END (*WITH*); 
      
  | 89,58 =>  (* GenFormalsList : IdentList .*)
  DEC (yyStackPtr, 1); yyNonterminal := 531;

  | 90 =>  (* GenActuals : IdentList .*)
  DEC (yyStackPtr, 1); yyNonterminal := 544;

  | 91 =>  (* DeclList : .*)
yyNonterminal := 533;

  | 92 =>  (* Block : .*)
yyNonterminal := 536;

END;
              (* SPEC State 
                   := Next (Top (), Nonterminal); nonterminal transition *)
               yyNCombPtr 
                 := LOOPHOLE 
                      ( LOOPHOLE ( yyNBasePtr [yyStateStack^ [yyStackPtr]]
                                 , INTEGER
                                 )
                        + yyNonterminal * BYTESIZE (yyNCombType)
                      , yyNCombTypePtr
                      );
               yyState := yyNCombPtr^;
               INC (yyStackPtr);
               yyAttributeStack^ [yyStackPtr] := yySynAttribute;
               IF yyState < yyFirstFinalState 
               THEN (* read nonterminal ? *)
                 EXIT 
               END (* IF *) ; 
            END (* LOOP *) ;

         ELSE (* read *)
            INC (yyStackPtr);
           yyAttributeStack^ [yyStackPtr].Scan := FM3Scanner.Attribute;
           yyTerminal := FM3Scanner.GetToken ();
            yyIsRepairing := FALSE;
         END (* IF *);
      END (* LOOP *) ;
   END FM3Parser;

PROCEDURE ErrorRecovery (
      VAR Terminal      : yySymbolRange ;
          StateStack    : yyStackType   ;
          StackSize     : INTEGER       ;
          StackPtr      : yyStackPtrType) =
   VAR
      TokensSkipped     : BOOLEAN;
      ContinueSet       : IntSets . T;
      RestartSet        : IntSets . T;
      Token             : yySymbolRange;
      TokenArray        : ARRAY [0..127] OF CHAR;
      TokenText         : TEXT;
      TokenString       : Strings.tString;
      ContinueString    : Strings.tString;
   BEGIN
   (* 1. report the error *)
         TokenName ( Terminal , TokenText );
         Strings.TextToString (TokenArray, TokenText);
         FrontErrors.ErrorMessageI (FrontErrors.SyntaxError, FrontErrors.Error, 
          FM3Scanner.Attribute.Position, FrontErrors.eString, ADR(TokenString) );

   (* 2. report the set of expected terminal symbols *)
      ContinueSet:= IntSets . Empty ( ); 
      ComputeContinuation (StateStack, StackSize, StackPtr, ContinueSet);
      Strings.AssignEmpty (ContinueString);
      FOR Token := IntSets.Minimum (ContinueSet) TO IntSets.Maximum (ContinueSet) DO
         IF IntSets.IsElement (Token, ContinueSet) THEN
            TokenName (Token, TokenText);
            Strings.TextToString (TokenText, TokenString);
            IF (Strings.Length (ContinueString) + Strings.Length (TokenString) + 1 <= Strings.cMaxStrLength) THEN
               Strings.Concatenate (ContinueString, TokenString);
               Strings.Append (ContinueString, ' ');
            END;
         END;
      END;
      FrontErrors.ErrorMessageI (FrontErrors.ExpectedTokens, FrontErrors.Information,
       FM3Scanner.Attribute.Position, FrontErrors.eString, ADR (ContinueString));
      ContinueSet := NIL;

   (* 3. compute the set of terminal symbols for restart of the parse *)
      RestartSet := IntSets . Empty ( );
      ComputeRestartPoints (StateStack, StackSize, StackPtr, RestartSet);

   (* 4. skip terminal symbols until a restart point is reached *)
      TokensSkipped := FALSE;
      WHILE NOT IntSets.IsElement (Terminal, RestartSet) DO
       Terminal := FM3Scanner.GetToken ();
        TokensSkipped := TRUE;
      END;
      RestartSet := NIL;

   (* 5. report the restart point *)
      IF TokensSkipped THEN
       FrontErrors.ErrorMessage (FrontErrors.RestartPoint, FrontErrors.Information, FM3Scanner.Attribute.Position);
      END;
   END ErrorRecovery;

(*
   compute the set of terminal symbols that can be accepted (read)
   in a given stack configuration (eventually after reduce actions)
*)

PROCEDURE ComputeContinuation (
          Stack         : yyStackType   ;
          StackSize     : INTEGER       ;
          StackPtr      : yyStackPtrType;
      VAR ContinueSet   : IntSets . T     ) =
   VAR Terminal         : yySymbolRange;
   BEGIN
      ContinueSet:= IntSets . Empty ( );
      FOR Terminal := yyFirstTerminal TO yyLastTerminal DO
         IF IsContinuation (Terminal, Stack, StackSize, StackPtr) THEN
            ContinueSet := IntSets . Include ( ContinueSet , Terminal )
         END;
      END;
   END ComputeContinuation;

(*
   check whether a given terminal symbol can be accepted (read)
   in a certain stack configuration (eventually after reduce actions)
*)

PROCEDURE IsContinuation (
      Terminal          : yySymbolRange ;
      ParseStack        : yyStackType   ;
      StackSize         : INTEGER       ;
      StackPtr          : yyStackPtrType): BOOLEAN =
   VAR
      State             : yyStackPtrType;
      Nonterminal       : yySymbolRange;
      Stack             : yyStackType;
   BEGIN
      Stack := NEW (yyStackType, StackSize);
      FOR RStackPtr := 0 TO StackPtr DO
         (* Making this assignment directly crashes CM3: *)
         State := ParseStack^ [RStackPtr];
         Stack^ [RStackPtr] := State
      END;
      State := Stack^ [StackPtr];
      LOOP
         Stack^ [StackPtr] := State;
         State := Next (State, Terminal);
         IF State = yyNoState THEN
            Stack := NIL;
            RETURN FALSE;
         END;
         IF State <= yyLastReadTermState (* read or read terminal reduce ? *)
         THEN
            Stack := NIL;
            RETURN TRUE;
         END;
         
         LOOP (* reduce *)
            IF State =  yyStopState THEN
               Stack := NIL; 
               RETURN TRUE;
            ELSE 
               DEC (StackPtr, yyLength [State]);
               Nonterminal := yyLeftHandSide [State];
            END;

            State := Next (Stack^ [StackPtr], Nonterminal);
            IF StackPtr >= StackSize THEN
              ExpandStateStack
                (Stack, MAX (NUMBER ( Stack ^ ) * 2 , StackPtr + 2 ) );
              StackSize := NUMBER (Stack^); 
            END;
            INC (StackPtr);
            IF State < yyFirstFinalState
            THEN EXIT;
            END; (* read nonterminal ? *)
            State := yyFinalToProd [State]; (* read nonterminal reduce *)
         END;
      END;
    END IsContinuation;
(*
   compute a set of terminal symbols that can be used to restart
   parsing in a given stack configuration. we simulate parsing until
   end of file using a suffix program synthesized by the function
   Continuation. All symbols acceptable in the states reached during
   the simulation can be used to restart parsing.
*)

PROCEDURE ComputeRestartPoints (
          ParseStack    : yyStackType   ;
          StackSize     : INTEGER       ;
          StackPtr      : yyStackPtrType;
      VAR RestartSet    : IntSets.T     ) =
   VAR
      Stack             : yyStackType;
      State             : yyStackPtrType;
      Nonterminal       : yySymbolRange;
      ContinueSet       : IntSets.T;
   BEGIN
      Stack := NEW (yyStackType, StackSize);
      FOR RStackPtr := 0 TO StackPtr DO
         (* Making this assignment directly crashes CM3: *)
         State:= ParseStack^ [RStackPtr];
         Stack^ [RStackPtr] := State;
      END;
      ContinueSet := IntSets . Empty ( );
      State := Stack^ [StackPtr];

      LOOP
         IF StackPtr >= StackSize THEN
            ExpandStateStack
              (Stack, MAX (NUMBER (Stack ^) * 2 , StackPtr + 2 ) );
            StackSize := NUMBER (Stack^); 
         END;
         Stack^ [StackPtr] := State;
         ComputeContinuation (Stack, StackSize, StackPtr, ContinueSet);
         RestartSet := IntSets.Union (RestartSet, ContinueSet);
         State := Next (State, yyContinuation [State]);

          IF State >= yyFirstFinalState THEN (* final state ? *)
            IF State <= yyLastReadTermState THEN (* read terminal reduce ? *)
               INC (StackPtr);
               State := yyFinalToProd [State];
            END;

            LOOP (* reduce *)
               IF State = yyStopState THEN
                  Stack := NIL;
                  ContinueSet := NIL;
                  RETURN;
               ELSE 
                  DEC (StackPtr, yyLength [State]);
                  Nonterminal := yyLeftHandSide [State];
               END;

               State := Next (Stack^ [StackPtr], Nonterminal);
               INC (StackPtr);
               IF State < yyFirstFinalState
               THEN EXIT;
               END; (* read nonterminal ? *)
               State := yyFinalToProd [State]; (* read nonterminal reduce *)
            END;
         ELSE (* read *)
            INC (StackPtr);
         END;
      END;
   END ComputeRestartPoints;

(* access the parse table:   Next : State x Symbol -> State *)

PROCEDURE Next
   (State: yyStateRange; Symbol: yySymbolRange)
   : yyStateRange =
   VAR
      TCombPtr          : yyTCombTypePtr;
      NCombPtr          : yyNCombTypePtr;
   BEGIN
      IF Symbol <= yyLastTerminal THEN
         LOOP
            TCombPtr 
              := LOOPHOLE 
                   ( LOOPHOLE (yyTBasePtr [State],INTEGER) 
                     + Symbol * BYTESIZE (yyTCombType)
                   ,yyTCombTypePtr);
            IF TCombPtr^.Check # State THEN
               State := yyDefault [State];
               IF State = yyNoState THEN RETURN yyNoState; END;
            ELSE
               RETURN TCombPtr^.Next;
            END;
         END;
      ELSE
        NCombPtr 
          := LOOPHOLE 
               ( LOOPHOLE (yyNBasePtr [State],INTEGER) 
                 + Symbol * BYTESIZE (yyNCombType)
               ,yyNCombTypePtr);
        RETURN NCombPtr^;
      END;
   END Next;
   
PROCEDURE yyGetTables() 
  = <* FATAL Rd . Failure *> 
    <* FATAL Wr . Failure *> 
    <* FATAL System . FileNoError *> 
    <* FATAL Thread . Alerted *> 
   VAR
      BlockSize, j, n   : Word.T;
      ReadVal: INTEGER;
      OK: BOOLEAN;
      (* These arrays are read-into by binary IO, thus they have to have
         the right bounds on the one hand and the right representation
         element size on the other.  Otherwise, chaos will ensue. *) 
      TBase     : ARRAY [0 .. yyLastReadState] OF yyTCombRangePacked;
      NBase     : ARRAY [0 .. yyLastReadState] OF yyNCombRangePacked;
   BEGIN
      BlockSize := 64000 DIV BYTESIZE (yyTCombType);
      OK := TRUE;
      TRY
        yyTableFile := System.OpenInputT (ParsTabName);
      EXCEPT
        OSError.E 
        => Errors.ErrLine
             ("Error: Can't open parse table file " & ParsTabName); 
        OK := FALSE;
      END;
      IF OK THEN
        ReadVal := yyGetTable
          (ADR (TBase[FIRST(TBase)])) DIV BYTESIZE (yyTCombRangePacked ) - 1;
        IF ReadVal # yyLastReadState THEN
           Errors.ErrLine
             ( "Error reading " & ParsTabName  
               & ", TBase size = " & Fmt.Int (ReadVal) & ", expected "
               & Fmt.Int (yyLastReadState)
             );
           OK := FALSE;
        ELSE
          ReadVal := yyGetTable
            (ADR (NBase[FIRST(NBase)])) DIV BYTESIZE (yyNCombRangePacked ) - 1;
          IF ReadVal # yyLastReadState THEN
             Errors.ErrLine
               ( "Error reading " & ParsTabName
                 & ", NBase size = " & Fmt.Int (ReadVal) & ", expected "
                 & Fmt.Int (yyLastReadState)
               );
             OK := FALSE;
          ELSE
            ReadVal := yyGetTable
              (ADR (yyDefault[FIRST(yyDefault)]))
               DIV BYTESIZE (yyReadRangePacked) - 1;
            IF ReadVal # yyLastReadState THEN
               Errors.ErrLine
                 ( "Error reading " & ParsTabName
                   & ", yyDefault size = " & Fmt.Int (ReadVal) & ", expected "
                   & Fmt.Int (yyLastReadState)
                 );
               OK := FALSE;
            ELSE
              ReadVal := yyGetTable
                (ADR (yyNComb[FIRST(yyNComb)])) DIV BYTESIZE (yyNCombType);
              IF ReadVal # (yyNTableMax - yyLastTerminal) THEN
                 Errors.ErrLine
                   ( "Error reading " & ParsTabName
                     & ", yyNComb size = " & Fmt.Int (ReadVal) & ", expected "
                     & Fmt.Int (yyNTableMax - yyLastTerminal)
                   );
                 OK := FALSE;
              ELSE
                ReadVal := yyGetTable
                  (ADR (yyLength[FIRST(yyLength)]))
                   DIV BYTESIZE (yyTableElmt  ) - 1;
                IF ReadVal # (yyLastReduceState - yyFirstReduceState) THEN
                   Errors.ErrLine
                     ( "Error reading " & ParsTabName
                       & ", yylength size = " & Fmt.Int (ReadVal)
                       & ", expected "
                       & Fmt.Int (yyLastReduceState - yyFirstReduceState)
                     );
                   OK := FALSE;
                ELSE
                  ReadVal := yyGetTable
                    (ADR (yyLeftHandSide[FIRST(yyLeftHandSide)]))
                     DIV BYTESIZE (yySymbolRangePacked) - 1;
                  IF ReadVal # (yyLastReduceState - yyFirstReduceState) THEN
                     Errors.ErrLine
                       ( "Error reading " & ParsTabName
                         & ", yy LeftHandSide size= " & Fmt.Int (ReadVal)
                         & ", expected "
                         & Fmt.Int (yyLastReduceState - yyFirstReduceState)
                       );
                     OK := FALSE;
                  ELSE
                    ReadVal := yyGetTable
                      (ADR (yyContinuation[FIRST(yyContinuation)]))
                       DIV BYTESIZE (yySymbolRangePacked) - 1;
                    IF ReadVal # yyLastReadState THEN
                       Errors.ErrLine
                         ( " Error reading " & ParsTabName
                           & ", yyContinuation size= " & Fmt.Int (ReadVal)
                           & ", expected " & Fmt.Int (yyLastReadState)
                         );
                       OK := FALSE;
                    ELSE
                      ReadVal := yyGetTable
                        (ADR (yyFinalToProd[FIRST(yyFinalToProd)] ))
                         DIV BYTESIZE (yyReduceRangePacked) - 1;
                      IF ReadVal
                         # (yyLastReadNontermState - yyFirstReadTermState)
                      THEN
                         Errors.ErrLine
                           ( "Error reading " & ParsTabName
                             & ", yyFinalToProd size = " & Fmt.Int (ReadVal)
                             & ", expected "
                             & Fmt.Int
                                 (yyLastReadNontermState - yyFirstReadTermState)
                             );
                         OK := FALSE;
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
      IF NOT OK THEN
        FrontErrors.ErrorMessage 
          (FrontErrors.WrongParseTable, FrontErrors.Fatal, Positions.NoPosition)
      END;
      n := 0;
      j := 0;
      WHILE j <= yyTableMax DO
         INC (n
         , yyGetTable
             (ADR (yyTComb [VAL(j,yyStateRangePacked)]))
              DIV BYTESIZE (yyTCombType)
             );
         INC (j, BlockSize);
      END;
      IF n # (yyTableMax + 1) THEN 
         FrontErrors.ErrorMessage
           (FrontErrors.WrongParseTable, FrontErrors.Fatal, Positions.NoPosition);
      END;
      System.Close (yyTableFile);

      FOR StateF := 1 TO yyLastReadState DO
         yyTBasePtr [StateF] := ADR (yyTComb [TBase [StateF]]);
      END;
      FOR StateF := 1 TO yyLastReadState DO
         yyNBasePtr [StateF] := ADR (yyNComb [NBase [StateF]]);
      END;
   END yyGetTables;

PROCEDURE yyGetTable (Address: ADDRESS): Word.T

  = <* FATAL Rd . Failure *> 
    <* FATAL System . FileNoError *> 
    <* FATAL Thread . Alerted *> 
    
   VAR
      N         : INTEGER;
      Length    : RECORD Field : BITS yyTableElmtBits FOR yyTableElmt END;
      LongLength : Word.T;
   BEGIN
      N := System.Read
             (yyTableFile, ADR (Length.Field), BYTESIZE (yyTableElmt));
      yyErrorCheck (FrontErrors.ReadParseTable, N);
      LongLength := Length.Field;
      N := System.Read (yyTableFile, Address, LongLength);
      yyErrorCheck (FrontErrors.ReadParseTable, N);
      RETURN LongLength;
   END yyGetTable;

PROCEDURE yyErrorCheck (ErrorCode: INTEGER; Info: INTEGER) =
   VAR ErrNo: INTEGER;
   BEGIN
     IF Info < 0 THEN
        ErrNo := System.ErrNum ();
        FrontErrors.ErrorMessageI
          (ErrorCode, FrontErrors.Fatal, Positions.NoPosition,
           FrontErrors.eInteger, ADR (ErrNo));
     END;
   END yyErrorCheck;

  PROCEDURE BeginFM3Parser ()=
   BEGIN
(* line 32 "FM3Parser.lalr" *)
 
      IF NOT yyIsInitialized THEN
         yyIsInitialized := TRUE;
         yyGetTables();
      END;
   END BeginFM3Parser;

(*EXPORTED*)
  PROCEDURE CloseFM3Parser ()=
   BEGIN
(* line 35 "FM3Parser.lalr" *)
 
   END CloseFM3Parser;

BEGIN
    <*ASSERT BYTESIZE (yyTableElmt) = 2 *>
    yyIsInitialized := FALSE;
     ParsTabName := "FM3Parser.Tab";
  END FM3Parser.

