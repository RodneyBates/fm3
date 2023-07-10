


  UNSAFE MODULE FM3Parser;
(*^ BEWARE! lalr loses this character. *) 

  IMPORT FM3Scanner;

IMPORT Fmt, OSError, Rd, Thread, Text, Word, Wr;

IMPORT Positions, FrontErrors, Strings, IntSets, System;

IMPORT Errors (* From Reusem3. *);

(* line 26 "FM3Parser.lalr" *)
 IMPORT FM3IntToks AS Itk;
    FROM FM3ParseSem IMPORT PushTok , PushTokPatch;
  

CONST
   yyInitStackSize      = 100;
   yyStackExpansionFactor = 2.0;
   yyNoState            = 0;

   yyFirstTerminal          = 0;
   yyLastTerminal           = 107;
   yyFirstSymbol            = 0;
   yyLastSymbol             = 126;
   yyTableMax               = 121;
   yyNTableMax              = 148;
   yyFirstReadState         = 1;
   yyLastReadState          = 52;
   yyFirstReadTermState             = 53;
   yyLastReadTermState              = 60;
   yyLastReadNontermState           = 72;
   yyFirstReduceState               = 73;
   yyLastReduceState                = 99;
   yyStartState             = 1;
   yyStopState              = 73;

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
   yyTBasePtr
     := ARRAY [0 .. yyLastReadState] OF yyTCombTypePtr
         {
           (*   0*) ADR (yyTComb [   0]) , (*   1*) ADR (yyTComb [   0])
         , (*   2*) ADR (yyTComb [   1]) , (*   3*) ADR (yyTComb [   0])
         , (*   4*) ADR (yyTComb [   0]) , (*   5*) ADR (yyTComb [   0])
         , (*   6*) ADR (yyTComb [   0]) , (*   7*) ADR (yyTComb [   0])
         , (*   8*) ADR (yyTComb [   0]) , (*   9*) ADR (yyTComb [   1])
         , (*  10*) ADR (yyTComb [   0]) , (*  11*) ADR (yyTComb [   2])
         , (*  12*) ADR (yyTComb [   0]) , (*  13*) ADR (yyTComb [   2])
         , (*  14*) ADR (yyTComb [   0]) , (*  15*) ADR (yyTComb [   0])
         , (*  16*) ADR (yyTComb [   0]) , (*  17*) ADR (yyTComb [   1])
         , (*  18*) ADR (yyTComb [   2]) , (*  19*) ADR (yyTComb [   3])
         , (*  20*) ADR (yyTComb [   3]) , (*  21*) ADR (yyTComb [   2])
         , (*  22*) ADR (yyTComb [   0]) , (*  23*) ADR (yyTComb [   2])
         , (*  24*) ADR (yyTComb [   4]) , (*  25*) ADR (yyTComb [   4])
         , (*  26*) ADR (yyTComb [   0]) , (*  27*) ADR (yyTComb [   4])
         , (*  28*) ADR (yyTComb [   5]) , (*  29*) ADR (yyTComb [   5])
         , (*  30*) ADR (yyTComb [   4]) , (*  31*) ADR (yyTComb [   6])
         , (*  32*) ADR (yyTComb [   0]) , (*  33*) ADR (yyTComb [   6])
         , (*  34*) ADR (yyTComb [   7]) , (*  35*) ADR (yyTComb [   6])
         , (*  36*) ADR (yyTComb [   8]) , (*  37*) ADR (yyTComb [  11])
         , (*  38*) ADR (yyTComb [   9]) , (*  39*) ADR (yyTComb [   0])
         , (*  40*) ADR (yyTComb [  10]) , (*  41*) ADR (yyTComb [  10])
         , (*  42*) ADR (yyTComb [  14]) , (*  43*) ADR (yyTComb [   0])
         , (*  44*) ADR (yyTComb [   1]) , (*  45*) ADR (yyTComb [   7])
         , (*  46*) ADR (yyTComb [  11]) , (*  47*) ADR (yyTComb [   7])
         , (*  48*) ADR (yyTComb [  12]) , (*  49*) ADR (yyTComb [   0])
         , (*  50*) ADR (yyTComb [   8]) , (*  51*) ADR (yyTComb [  13])
         , (*  52*) ADR (yyTComb [   8])
         };
         
   yyNBasePtr
     := ARRAY [0 .. yyLastReadState] OF yyNCombTypePtr
         {
           (*   0*) ADR (yyNComb [ 108]) , (*   1*) ADR (yyNComb [ 108])
         , (*   2*) ADR (yyNComb [ 108]) , (*   3*) ADR (yyNComb [ 108])
         , (*   4*) ADR (yyNComb [ 108]) , (*   5*) ADR (yyNComb [ 108])
         , (*   6*) ADR (yyNComb [ 109]) , (*   7*) ADR (yyNComb [ 109])
         , (*   8*) ADR (yyNComb [ 108]) , (*   9*) ADR (yyNComb [ 108])
         , (*  10*) ADR (yyNComb [ 108]) , (*  11*) ADR (yyNComb [ 108])
         , (*  12*) ADR (yyNComb [ 110]) , (*  13*) ADR (yyNComb [ 108])
         , (*  14*) ADR (yyNComb [ 111]) , (*  15*) ADR (yyNComb [ 112])
         , (*  16*) ADR (yyNComb [ 108]) , (*  17*) ADR (yyNComb [ 111])
         , (*  18*) ADR (yyNComb [ 108]) , (*  19*) ADR (yyNComb [ 108])
         , (*  20*) ADR (yyNComb [ 108]) , (*  21*) ADR (yyNComb [ 108])
         , (*  22*) ADR (yyNComb [ 108]) , (*  23*) ADR (yyNComb [ 108])
         , (*  24*) ADR (yyNComb [ 108]) , (*  25*) ADR (yyNComb [ 108])
         , (*  26*) ADR (yyNComb [ 121]) , (*  27*) ADR (yyNComb [ 121])
         , (*  28*) ADR (yyNComb [ 108]) , (*  29*) ADR (yyNComb [ 108])
         , (*  30*) ADR (yyNComb [ 108]) , (*  31*) ADR (yyNComb [ 108])
         , (*  32*) ADR (yyNComb [ 118]) , (*  33*) ADR (yyNComb [ 108])
         , (*  34*) ADR (yyNComb [ 108]) , (*  35*) ADR (yyNComb [ 108])
         , (*  36*) ADR (yyNComb [ 108]) , (*  37*) ADR (yyNComb [ 122])
         , (*  38*) ADR (yyNComb [ 119]) , (*  39*) ADR (yyNComb [ 120])
         , (*  40*) ADR (yyNComb [ 108]) , (*  41*) ADR (yyNComb [ 121])
         , (*  42*) ADR (yyNComb [ 108]) , (*  43*) ADR (yyNComb [ 130])
         , (*  44*) ADR (yyNComb [ 130]) , (*  45*) ADR (yyNComb [ 108])
         , (*  46*) ADR (yyNComb [ 108]) , (*  47*) ADR (yyNComb [ 108])
         , (*  48*) ADR (yyNComb [ 108]) , (*  49*) ADR (yyNComb [ 127])
         , (*  50*) ADR (yyNComb [ 108]) , (*  51*) ADR (yyNComb [ 108])
         , (*  52*) ADR (yyNComb [ 108])
         };
         
   yyDefault
     := ARRAY [0 .. yyLastReadState] OF [ 0 .. yyLastReadState ]
         {
           (*   0*)    0 , (*   1*)    0 , (*   2*)    0 , (*   3*)    0
         , (*   4*)    0 , (*   5*)    0 , (*   6*)    0 , (*   7*)   27
         , (*   8*)    0 , (*   9*)    0 , (*  10*)    0 , (*  11*)    0
         , (*  12*)    0 , (*  13*)    0 , (*  14*)    0 , (*  15*)   44
         , (*  16*)    0 , (*  17*)    0 , (*  18*)    0 , (*  19*)    0
         , (*  20*)    0 , (*  21*)    0 , (*  22*)    0 , (*  23*)    0
         , (*  24*)    0 , (*  25*)    0 , (*  26*)    0 , (*  27*)    0
         , (*  28*)    0 , (*  29*)    0 , (*  30*)    0 , (*  31*)    0
         , (*  32*)    0 , (*  33*)    0 , (*  34*)    0 , (*  35*)    0
         , (*  36*)    0 , (*  37*)    0 , (*  38*)    0 , (*  39*)   41
         , (*  40*)    0 , (*  41*)    0 , (*  42*)    0 , (*  43*)    0
         , (*  44*)    0 , (*  45*)    0 , (*  46*)    0 , (*  47*)    0
         , (*  48*)    0 , (*  49*)    0 , (*  50*)    0 , (*  51*)    0
         , (*  52*)    0
         };
         
   yyTComb
     := ARRAY yyTCombRangePacked OF yyTCombType
         {
           (*   0*) yyTCombType {  22,   73} , (*   1*) yyTCombType {   0,    0}
         , (*   2*) yyTCombType {   0,    0} , (*   3*) yyTCombType {   0,    0}
         , (*   4*) yyTCombType {   0,    0} , (*   5*) yyTCombType {   0,    0}
         , (*   6*) yyTCombType {   0,    0} , (*   7*) yyTCombType {   0,    0}
         , (*   8*) yyTCombType {   0,    0} , (*   9*) yyTCombType {  16,   17}
         , (*  10*) yyTCombType {  44,   96} , (*  11*) yyTCombType {   0,    0}
         , (*  12*) yyTCombType {   0,    0} , (*  13*) yyTCombType {   0,    0}
         , (*  14*) yyTCombType {   0,    0} , (*  15*) yyTCombType {   0,    0}
         , (*  16*) yyTCombType {   0,    0} , (*  17*) yyTCombType {   0,    0}
         , (*  18*) yyTCombType {   0,    0} , (*  19*) yyTCombType {   8,    9}
         , (*  20*) yyTCombType {  17,   99} , (*  21*) yyTCombType {  18,   54}
         , (*  22*) yyTCombType {  19,   20} , (*  23*) yyTCombType {  27,   96}
         , (*  24*) yyTCombType {  28,   29} , (*  25*) yyTCombType {  33,   34}
         , (*  26*) yyTCombType {  45,   46} , (*  27*) yyTCombType {  50,   51}
         , (*  28*) yyTCombType {   1,    2} , (*  29*) yyTCombType {   0,    0}
         , (*  30*) yyTCombType {   0,    0} , (*  31*) yyTCombType {   0,    0}
         , (*  32*) yyTCombType {   1,   87} , (*  33*) yyTCombType {   2,    3}
         , (*  34*) yyTCombType {  23,   24} , (*  35*) yyTCombType {  37,   38}
         , (*  36*) yyTCombType {   0,    0} , (*  37*) yyTCombType {   1,   87}
         , (*  38*) yyTCombType {   2,   11} , (*  39*) yyTCombType {  23,   36}
         , (*  40*) yyTCombType {   0,    0} , (*  41*) yyTCombType {   0,    0}
         , (*  42*) yyTCombType {   0,    0} , (*  43*) yyTCombType {   0,    0}
         , (*  44*) yyTCombType {   0,    0} , (*  45*) yyTCombType {   0,    0}
         , (*  46*) yyTCombType {   0,    0} , (*  47*) yyTCombType {   0,    0}
         , (*  48*) yyTCombType {   0,    0} , (*  49*) yyTCombType {   0,    0}
         , (*  50*) yyTCombType {   0,    0} , (*  51*) yyTCombType {   0,    0}
         , (*  52*) yyTCombType {   0,    0} , (*  53*) yyTCombType {   0,    0}
         , (*  54*) yyTCombType {   0,    0} , (*  55*) yyTCombType {   0,    0}
         , (*  56*) yyTCombType {   0,    0} , (*  57*) yyTCombType {   0,    0}
         , (*  58*) yyTCombType {   0,    0} , (*  59*) yyTCombType {   1,   56}
         , (*  60*) yyTCombType {   0,    0} , (*  61*) yyTCombType {   0,    0}
         , (*  62*) yyTCombType {   0,    0} , (*  63*) yyTCombType {   0,    0}
         , (*  64*) yyTCombType {   0,    0} , (*  65*) yyTCombType {   0,    0}
         , (*  66*) yyTCombType {   5,    6} , (*  67*) yyTCombType {  10,   53}
         , (*  68*) yyTCombType {  13,   14} , (*  69*) yyTCombType {  21,   55}
         , (*  70*) yyTCombType {  25,   26} , (*  71*) yyTCombType {  30,   57}
         , (*  72*) yyTCombType {  25,   31} , (*  73*) yyTCombType {  35,   58}
         , (*  74*) yyTCombType {  47,   59} , (*  75*) yyTCombType {  52,   60}
         , (*  76*) yyTCombType {  41,   91} , (*  77*) yyTCombType {  37,   88}
         , (*  78*) yyTCombType {  41,   91} , (*  79*) yyTCombType {  37,   88}
         , (*  80*) yyTCombType {  42,   43} , (*  81*) yyTCombType {  41,   40}
         , (*  82*) yyTCombType {  42,   48} , (*  83*) yyTCombType {   0,    0}
         , (*  84*) yyTCombType {   0,    0} , (*  85*) yyTCombType {   0,    0}
         , (*  86*) yyTCombType {   0,    0} , (*  87*) yyTCombType {   0,    0}
         , (*  88*) yyTCombType {   0,    0} , (*  89*) yyTCombType {   0,    0}
         , (*  90*) yyTCombType {   0,    0} , (*  91*) yyTCombType {   0,    0}
         , (*  92*) yyTCombType {   0,    0} , (*  93*) yyTCombType {   0,    0}
         , (*  94*) yyTCombType {   0,    0} , (*  95*) yyTCombType {   3,    4}
         , (*  96*) yyTCombType {   9,   10} , (*  97*) yyTCombType {  11,   12}
         , (*  98*) yyTCombType {  20,   21} , (*  99*) yyTCombType {  24,   25}
         , (* 100*) yyTCombType {  29,   30} , (* 101*) yyTCombType {  31,   32}
         , (* 102*) yyTCombType {  34,   35} , (* 103*) yyTCombType {  36,   37}
         , (* 104*) yyTCombType {  38,   39} , (* 105*) yyTCombType {  40,   41}
         , (* 106*) yyTCombType {  46,   47} , (* 107*) yyTCombType {  48,   49}
         , (* 108*) yyTCombType {  51,   52} , (* 109*) yyTCombType {   0,    0}
         , (* 110*) yyTCombType {   0,    0} , (* 111*) yyTCombType {   0,    0}
         , (* 112*) yyTCombType {   0,    0} , (* 113*) yyTCombType {   0,    0}
         , (* 114*) yyTCombType {   0,    0} , (* 115*) yyTCombType {   0,    0}
         , (* 116*) yyTCombType {   0,    0} , (* 117*) yyTCombType {   0,    0}
         , (* 118*) yyTCombType {   0,    0} , (* 119*) yyTCombType {   0,    0}
         , (* 120*) yyTCombType {   0,    0} , (* 121*) yyTCombType {   0,    0}
         };
         
   yyNComb 
     := ARRAY yyNCombRangePacked OF yyNCombType
         {
           (*NT: 108*)   63 , (*NT: 109*)   22 , (*NT: 110*)   64
         , (*NT: 111*)   65 , (*NT: 112*)   66 , (*NT: 113*)   67
         , (*NT: 114*)   68 , (*NT: 115*)   23 , (*NT: 116*)    0
         , (*NT: 117*)    7 , (*NT: 118*)    8 , (*NT: 119*)   15
         , (*NT: 120*)    5 , (*NT: 121*)   16 , (*NT: 122*)   13
         , (*NT: 123*)   19 , (*NT: 124*)   62 , (*NT: 125*)   61
         , (*NT: 126*)   62 , (*NT: 127*)   61 , (*NT: 128*)   18
         , (*NT: 129*)   27 , (*NT: 130*)   28 , (*NT: 131*)   33
         , (*NT: 132*)   42 , (*NT: 133*)   72 , (*NT: 134*)   69
         , (*NT: 135*)   71 , (*NT: 136*)   70 , (*NT: 137*)   61
         , (*NT: 138*)   44 , (*NT: 139*)   16 , (*NT: 140*)   50
         , (*NT: 141*)   45 , (*NT: 142*)    0 , (*NT: 143*)   69
         , (*NT: 144*)    0 , (*NT: 145*)    0 , (*NT: 146*)   61
         , (*NT: 147*)    0 , (*NT: 148*)    0
         };
         
   yyLength
     := ARRAY yyReduceRangePacked OF yyTableElmt
         {
           (*State:  73*)    2 , (*State:  74*)    1 , (*State:  75*)    1
         , (*State:  76*)    1 , (*State:  77*)    1 , (*State:  78*)    1
         , (*State:  79*)    1 , (*State:  80*)    9 , (*State:  81*)   10
         , (*State:  82*)   10 , (*State:  83*)   10 , (*State:  84*)    9
         , (*State:  85*)   10 , (*State:  86*)    1 , (*State:  87*)    0
         , (*State:  88*)    0 , (*State:  89*)    2 , (*State:  90*)    2
         , (*State:  91*)    0 , (*State:  92*)    3 , (*State:  93*)    1
         , (*State:  94*)    1 , (*State:  95*)    1 , (*State:  96*)    0
         , (*State:  97*)    4 , (*State:  98*)    0 , (*State:  99*)    0
         };
         
   yyLeftHandSide
     := ARRAY yyReduceRangePacked OF yySymbolRangePacked
         {
           (*State:  73*)  126 , (*State:  74*)  109 , (*State:  75*)  109
         , (*State:  76*)  109 , (*State:  77*)  109 , (*State:  78*)  109
         , (*State:  79*)  109 , (*State:  80*)  108 , (*State:  81*)  110
         , (*State:  82*)  111 , (*State:  83*)  112 , (*State:  84*)  113
         , (*State:  85*)  114 , (*State:  86*)  115 , (*State:  87*)  115
         , (*State:  88*)  118 , (*State:  89*)  118 , (*State:  90*)  122
         , (*State:  91*)  123 , (*State:  92*)  123 , (*State:  93*)  116
         , (*State:  94*)  120 , (*State:  95*)  121 , (*State:  96*)  117
         , (*State:  97*)  119 , (*State:  98*)  117 , (*State:  99*)  125
         };
         
   yyContinuation
     := ARRAY [0 .. yyLastReadState] OF yySymbolRangePacked
         {
           (*State:   0*)    0 , (*State:   1*)   32 , (*State:   2*)   32
         , (*State:   3*)   95 , (*State:   4*)   95 , (*State:   5*)   66
         , (*State:   6*)   66 , (*State:   7*)   19 , (*State:   8*)   19
         , (*State:   9*)   95 , (*State:  10*)   67 , (*State:  11*)   95
         , (*State:  12*)   95 , (*State:  13*)   66 , (*State:  14*)   66
         , (*State:  15*)    9 , (*State:  16*)    9 , (*State:  17*)   19
         , (*State:  18*)   19 , (*State:  19*)   19 , (*State:  20*)   95
         , (*State:  21*)   67 , (*State:  22*)    0 , (*State:  23*)   32
         , (*State:  24*)   95 , (*State:  25*)   66 , (*State:  26*)   66
         , (*State:  27*)   19 , (*State:  28*)   19 , (*State:  29*)   95
         , (*State:  30*)   67 , (*State:  31*)   95 , (*State:  32*)   95
         , (*State:  33*)   19 , (*State:  34*)   95 , (*State:  35*)   67
         , (*State:  36*)   95 , (*State:  37*)   66 , (*State:  38*)   95
         , (*State:  39*)   66 , (*State:  40*)   95 , (*State:  41*)   66
         , (*State:  42*)   66 , (*State:  43*)   66 , (*State:  44*)    9
         , (*State:  45*)   19 , (*State:  46*)   95 , (*State:  47*)   67
         , (*State:  48*)   95 , (*State:  49*)   95 , (*State:  50*)   19
         , (*State:  51*)   95 , (*State:  52*)   67
         };
         
   yyFinalToProd
     := ARRAY yyReadReduceRangePacked OF yyReduceRangePacked
         {
           (*State:  53)*)   82 , (*State:  54)*)   97
         , (*State:  55)*)   83 , (*State:  56)*)   86
         , (*State:  57)*)   80 , (*State:  58)*)   84
         , (*State:  59)*)   81 , (*State:  60)*)   85
         , (*State:  61)*)   93 , (*State:  62)*)   94
         , (*State:  63)*)   74 , (*State:  64)*)   75
         , (*State:  65)*)   76 , (*State:  66)*)   77
         , (*State:  67)*)   78 , (*State:  68)*)   79
         , (*State:  69)*)   95 , (*State:  70)*)   92
         , (*State:  71)*)   90 , (*State:  72)*)   89
         }; 

VAR
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
      | 5 => Name := "StkRwAND";
      | 6 => Name := "StkRwANY";
      | 7 => Name := "StkRwARRAY";
      | 8 => Name := "StkRwAS";
      | 9 => Name := "StkRwBEGIN";
      | 10 => Name := "StkRwBITS";
      | 11 => Name := "StkRwBRANDED";
      | 12 => Name := "StkRwBY";
      | 13 => Name := "StkRwCASE";
      | 14 => Name := "StkRwCONST";
      | 15 => Name := "StkRwDIV";
      | 16 => Name := "StkRwDO";
      | 17 => Name := "StkRwELSE";
      | 18 => Name := "StkRwELSIF";
      | 19 => Name := "StkRwEND";
      | 20 => Name := "StkRwEVAL";
      | 21 => Name := "StkRwEXCEPT";
      | 22 => Name := "StkRwEXCEPTION";
      | 23 => Name := "StkRwEXIT";
      | 24 => Name := "StkRwEXPORTS";
      | 25 => Name := "StkRwFINALLY";
      | 26 => Name := "StkRwFOR";
      | 27 => Name := "StkRwFROM";
      | 28 => Name := "StkRwGENERIC";
      | 29 => Name := "StkRwIF";
      | 30 => Name := "StkRwIMPORT";
      | 31 => Name := "StkRwIN";
      | 32 => Name := "StkRwINTERFACE";
      | 33 => Name := "StkRwLOCK";
      | 34 => Name := "StkRwLOOP";
      | 35 => Name := "StkRwMETHODS";
      | 36 => Name := "StkRwMOD";
      | 37 => Name := "StkRwMODULE";
      | 38 => Name := "StkRwNOT";
      | 39 => Name := "StkRwOBJECT";
      | 40 => Name := "StkRwOF";
      | 41 => Name := "StkRwOR";
      | 42 => Name := "StkRwOVERRIDES";
      | 43 => Name := "StkRwPROCEDURE";
      | 44 => Name := "StkRwRAISE";
      | 45 => Name := "StkRwRAISES";
      | 46 => Name := "StkRwREADONLY";
      | 47 => Name := "StkRwRECORD";
      | 48 => Name := "StkRwREF";
      | 49 => Name := "StkRwREPEAT";
      | 50 => Name := "StkRwRETURN";
      | 51 => Name := "StkRwREVEAL";
      | 52 => Name := "StkRwROOT";
      | 53 => Name := "StkRwSET";
      | 54 => Name := "StkRwTHEN";
      | 55 => Name := "StkRwTO";
      | 56 => Name := "StkRwTRY";
      | 57 => Name := "StkRwTYPE";
      | 58 => Name := "StkRwTYPECASE";
      | 59 => Name := "StkRwUNSAFE";
      | 60 => Name := "StkRwUNTIL";
      | 61 => Name := "StkRwUNTRACED";
      | 62 => Name := "StkRwVALUE";
      | 63 => Name := "StkRwVAR";
      | 64 => Name := "StkRwWHILE";
      | 65 => Name := "StkRwWITH";
      | 66 => Name := "StkSemicolon";
      | 67 => Name := "StkDot";
      | 68 => Name := "StkEqual";
      | 69 => Name := "StkOpenParen";
      | 70 => Name := "StkCloseParen";
      | 71 => Name := "StkComma";
      | 72 => Name := "StkColon";
      | 73 => Name := "StkSubtype";
      | 74 => Name := "StkBecomes";
      | 75 => Name := "StkOpenBrace";
      | 76 => Name := "StkCloseBrace";
      | 77 => Name := "StkStroke";
      | 78 => Name := "StkArrow";
      | 79 => Name := "StkEllipsis";
      | 80 => Name := "StkOpenBracket";
      | 81 => Name := "StkCloseBracket";
      | 82 => Name := "StkUnequal";
      | 83 => Name := "StkLess";
      | 84 => Name := "StkGreater";
      | 85 => Name := "StkLessEqual";
      | 86 => Name := "StkGreaterEqual";
      | 87 => Name := "StkPlus";
      | 88 => Name := "StkMinus";
      | 89 => Name := "StkAmpersand";
      | 90 => Name := "StkStar";
      | 91 => Name := "StkSlash";
      | 92 => Name := "StkDeref";
      | 93 => Name := "StkOpenPragma";
      | 94 => Name := "StkClosePragma";
      | 95 => Name := "StkIdent";
      | 96 => Name := "StkIntLit";
      | 97 => Name := "StkLongIntLit";
      | 98 => Name := "StkBasedLit";
      | 99 => Name := "StkLongBasedLit";
      | 100 => Name := "StkRealLit";
      | 101 => Name := "StkLongRealLit";
      | 102 => Name := "StkExtendedLit";
      | 103 => Name := "StkTextLit";
      | 104 => Name := "StkWideTextLit";
      | 105 => Name := "StkCharLit";
      | 106 => Name := "StkWideCharLit";
      | 107 => Name := "StkLexErrChars";
      ELSE Name := "" 
      END;
   END TokenName;

(*EXPORTED*)
  PROCEDURE FM3Parser (): CARDINAL =
(* line 31 "FM3Parser.lalr" *)
 
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
      yyText            : TEXT; 
      yyTokenStringxxx     : TEXT (*ARRAY [0..127] OF CHAR*);

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
                     TokenName (yyRepairToken, yyText);
                     FrontErrors.ErrorMessageTraced
                       (FrontErrors.TokenInserted, FrontErrors.Repair,
                       FM3Scanner.Attribute.Position, FrontErrors.eText, yyText 
                       );
                     IF yyState >= yyFirstFinalState 
                     THEN (* avoid second push *)
                        yyState := yyFinalToProd [yyState];
                     END (* IF *) ;
                     INC (yyStackPtr);
                     yyStateStack^     [yyStackPtr] := yyState;
                     yyAttributeStack^ [yyStackPtr].Scan := yyRepairAttribute;
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
               yyStateStack^ [yyStackPtr] := yyState (*ParserDebug*);
            END (* IF *) ;

            LOOP (* Through successive reductions *)
CASE yyState OF
  | 73 =>  (* _0000_ : Compilation _EndOfFile .*)
  yyStateStack := NIL;
  yyAttributeStack := NIL;
  RETURN yyErrorCount;

  | 74,63 =>  (* Compilation : Interface .*)
  DEC (yyStackPtr, 1); yyNonterminal := 109;

  | 75,64 =>  (* Compilation : Module .*)
  DEC (yyStackPtr, 1); yyNonterminal := 109;

  | 76,65 =>  (* Compilation : GenInterface .*)
  DEC (yyStackPtr, 1); yyNonterminal := 109;

  | 77,66 =>  (* Compilation : GenModule .*)
  DEC (yyStackPtr, 1); yyNonterminal := 109;

  | 78,67 =>  (* Compilation : InstInterface .*)
  DEC (yyStackPtr, 1); yyNonterminal := 109;

  | 79,68 =>  (* Compilation : InstModule .*)
  DEC (yyStackPtr, 1); yyNonterminal := 109;

  | 80,57 =>  (* Interface : OptUnsafe StkRwINTERFACE StkIdent StkSemicolon ImportList DeclList StkRwEND StkIdent StkDot .*)
  DEC (yyStackPtr, 9); yyNonterminal := 108;

  | 81,59 =>  (* Module : OptUnsafe StkRwMODULE StkIdent Exports StkSemicolon ImportList Block StkRwEND StkIdent StkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 110;

  | 82,53 =>  (* GenInterface : StkRwGENERIC StkRwINTERFACE StkIdent GenFormalsList StkSemicolon ImportList DeclList StkRwEND StkIdent StkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 111;

  | 83,55 =>  (* GenModule : StkRwGENERIC StkRwMODULE StkIdent GenFormalsList StkSemicolon ImportList Block StkRwEND StkIdent StkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 112;

  | 84,58 =>  (* InstInterface : OptUnsafe StkRwINTERFACE StkIdent StkEqual StkIdent GenActuals StkRwEND StkIdent StkDot .*)
  DEC (yyStackPtr, 9); yyNonterminal := 113;

  | 85,60 =>  (* InstModule : OptUnsafe StkRwMODULE StkIdent Exports StkEqual StkIdent GenActuals StkRwEND StkIdent StkDot .*)
  DEC (yyStackPtr, 10); yyNonterminal := 114;

  | 86,56 =>  (* OptUnsafe : StkRwUNSAFE .*)
  DEC (yyStackPtr, 1); yyNonterminal := 115;
(* line 207 of "FM3Parser.lalr" *)
   yySynAttribute . PaBool := TRUE; 
  | 87 =>  (* OptUnsafe : .*)
yyNonterminal := 115;
(* line 208 of "FM3Parser.lalr" *)
   yySynAttribute . PaBool := FALSE; 
  | 88 =>  (* Exports : .*)
yyNonterminal := 118;
(* line 210 of "FM3Parser.lalr" *)
   
  | 89,72 =>  (* Exports : StkRwEXPORTS IdList .*)
  DEC (yyStackPtr, 2); yyNonterminal := 118;
(* line 211 of "FM3Parser.lalr" *)
   
  | 90,71 =>  (* IdList : StkIdent IdListTail .*)
  DEC (yyStackPtr, 2); yyNonterminal := 122;

  | 91 =>  (* IdListTail : .*)
yyNonterminal := 123;

  | 92,70 =>  (* IdListTail : StkComma StkIdent IdListTail .*)
  DEC (yyStackPtr, 3); yyNonterminal := 123;

  | 93,61 =>  (* ImportList : ExportsList .*)
  DEC (yyStackPtr, 1); yyNonterminal := 116;

  | 94,62 =>  (* GenFormalsList : ExportsList .*)
  DEC (yyStackPtr, 1); yyNonterminal := 120;

  | 95,69 =>  (* GenActuals : ExportsList .*)
  DEC (yyStackPtr, 1); yyNonterminal := 121;

  | 96 =>  (* DeclList : .*)
yyNonterminal := 117;

  | 97,54 =>  (* Block : DeclList StkRwBEGIN StmtList StkRwEND .*)
  DEC (yyStackPtr, 4); yyNonterminal := 119;

  | 98 =>  (* DeclList : .*)
yyNonterminal := 117;

  | 99 =>  (* StmtList : .*)
yyNonterminal := 125;

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
               yyAttributeStack^ [yyStackPtr].Scan.SaTok := yyNonterminal;
            (* ^This requires that tScanAttribute have field 'SaTok'. *)
               IF yyState < yyFirstFinalState 
               THEN (* read nonterminal ? *)
                 EXIT 
               END (* IF *) ; 
               yyStateStack^ [yyStackPtr] := yyState (*ParserDebug*);
            END (* LOOP *) ;

         ELSE (* read *)
            INC (yyStackPtr);
            yyStateStack^ [yyStackPtr] := yyState (*ParserDebug*);
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
         FrontErrors.ErrorMessageTraced
           (FrontErrors.SyntaxError, FrontErrors.Error, 
          FM3Scanner.Attribute.Position, FrontErrors.eText, TokenText);

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
      FrontErrors.ErrorMessageI
        (FrontErrors.ExpectedTokens, FrontErrors.Information,
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
      State             : yyStateRange;
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
            (* Ensure Stack has room for at least 2 elements. *)
            IF StackPtr >= StackSize THEN
              ExpandStateStack
                (Stack, MAX (NUMBER ( Stack ^ ) * 2 , StackPtr + 2 ) );
              StackSize := NUMBER (Stack^); 
            END;
            Stack^ [StackPtr] := State (*ParserDebug*);
            INC (StackPtr);
            IF State < yyFirstFinalState
            THEN EXIT;
            END; (* read nonterminal ? *)
            State := yyFinalToProd [State]; (* read nonterminal reduce *)
            Stack^ [StackPtr] := State (*ParserDebug*);
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
      State             : yyStateRange;
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
         (* Ensure Stack has room for at least 2 elements. *)
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
               Stack^ [StackPtr] := State (*ParserDebug*);
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
            END (*LOOP*);
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

      FOR StateF := 0 TO yyLastReadState DO
         yyTBasePtr [StateF] := ADR (yyTComb [TBase [StateF]]);
      END;
      FOR StateF := 0 TO yyLastReadState DO
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
(* line 35 "FM3Parser.lalr" *)
 FrontErrors . SetReportMode ( FrontErrors . tReportMode . eImmediate ); 
  
      IF NOT yyIsInitialized THEN
         yyIsInitialized := TRUE;
      (* yyGetTables(); *)
      END;
   END BeginFM3Parser;

(*EXPORTED*)
  PROCEDURE CloseFM3Parser ()=
   BEGIN
(* line 39 "FM3Parser.lalr" *)
 
   END CloseFM3Parser;

BEGIN
    <*ASSERT BYTESIZE (yyTableElmt) = 2 *>
    yyIsInitialized := FALSE;
     ParsTabName := "FM3Parser.Tab";
  END FM3Parser.

