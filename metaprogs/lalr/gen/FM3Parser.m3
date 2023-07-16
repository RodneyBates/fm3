


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
   yyLastSymbol             = 127;
   yyTableMax               = 126;
   yyNTableMax              = 155;
   yyFirstReadState         = 1;
   yyLastReadState          = 54;
   yyFirstReadTermState             = 55;
   yyLastReadTermState              = 63;
   yyLastReadNontermState           = 75;
   yyFirstReduceState               = 76;
   yyLastReduceState                = 106;
   yyStartState             = 1;
   yyStopState              = 76;

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
   (* ^Any action that does a reduce, i.e., readTermReduce, readNTReduce,
       or simple Reduce.  These are are actions, not true item-set states.
   *) 
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
     (* Subscripted by a state-number action, uses in combination
        with a terminal to find the new action after shifting
        the terminal in this state.
     *) 
     := ARRAY [0 .. yyLastReadState] OF yyTCombTypePtr
         {
           (*   0*) ADR (yyTComb [   0]) , (*   1*) ADR (yyTComb [   0])
         , (*   2*) ADR (yyTComb [   1]) , (*   3*) ADR (yyTComb [   0])
         , (*   4*) ADR (yyTComb [   0]) , (*   5*) ADR (yyTComb [   0])
         , (*   6*) ADR (yyTComb [   0]) , (*   7*) ADR (yyTComb [   0])
         , (*   8*) ADR (yyTComb [   0]) , (*   9*) ADR (yyTComb [   1])
         , (*  10*) ADR (yyTComb [   0]) , (*  11*) ADR (yyTComb [   2])
         , (*  12*) ADR (yyTComb [   2]) , (*  13*) ADR (yyTComb [   3])
         , (*  14*) ADR (yyTComb [   3]) , (*  15*) ADR (yyTComb [   4])
         , (*  16*) ADR (yyTComb [   0]) , (*  17*) ADR (yyTComb [   0])
         , (*  18*) ADR (yyTComb [   0]) , (*  19*) ADR (yyTComb [   1])
         , (*  20*) ADR (yyTComb [   3]) , (*  21*) ADR (yyTComb [   4])
         , (*  22*) ADR (yyTComb [   4]) , (*  23*) ADR (yyTComb [   4])
         , (*  24*) ADR (yyTComb [   0]) , (*  25*) ADR (yyTComb [   2])
         , (*  26*) ADR (yyTComb [   5]) , (*  27*) ADR (yyTComb [   9])
         , (*  28*) ADR (yyTComb [   0]) , (*  29*) ADR (yyTComb [   5])
         , (*  30*) ADR (yyTComb [   6]) , (*  31*) ADR (yyTComb [   6])
         , (*  32*) ADR (yyTComb [   5]) , (*  33*) ADR (yyTComb [   7])
         , (*  34*) ADR (yyTComb [   0]) , (*  35*) ADR (yyTComb [   7])
         , (*  36*) ADR (yyTComb [   8]) , (*  37*) ADR (yyTComb [   9])
         , (*  38*) ADR (yyTComb [   9]) , (*  39*) ADR (yyTComb [  12])
         , (*  40*) ADR (yyTComb [  10]) , (*  41*) ADR (yyTComb [   0])
         , (*  42*) ADR (yyTComb [  11]) , (*  43*) ADR (yyTComb [  13])
         , (*  44*) ADR (yyTComb [  17]) , (*  45*) ADR (yyTComb [  18])
         , (*  46*) ADR (yyTComb [   1]) , (*  47*) ADR (yyTComb [  10])
         , (*  48*) ADR (yyTComb [  12]) , (*  49*) ADR (yyTComb [  15])
         , (*  50*) ADR (yyTComb [  13]) , (*  51*) ADR (yyTComb [  16])
         , (*  52*) ADR (yyTComb [  11]) , (*  53*) ADR (yyTComb [  14])
         , (*  54*) ADR (yyTComb [  19])
         };
         
   yyNBasePtr
     (* Subscripted by a state-number action, used in combination
        with a nonterminal to find the new action after shifting
        the nonterminal in this state.
     *) 
     := ARRAY [0 .. yyLastReadState] OF yyNCombTypePtr
         {
           (*   0*) ADR (yyNComb [ 108]) , (*   1*) ADR (yyNComb [ 108])
         , (*   2*) ADR (yyNComb [ 108]) , (*   3*) ADR (yyNComb [ 108])
         , (*   4*) ADR (yyNComb [ 108]) , (*   5*) ADR (yyNComb [ 108])
         , (*   6*) ADR (yyNComb [ 110]) , (*   7*) ADR (yyNComb [ 108])
         , (*   8*) ADR (yyNComb [ 108]) , (*   9*) ADR (yyNComb [ 108])
         , (*  10*) ADR (yyNComb [ 108]) , (*  11*) ADR (yyNComb [ 108])
         , (*  12*) ADR (yyNComb [ 108]) , (*  13*) ADR (yyNComb [ 108])
         , (*  14*) ADR (yyNComb [ 116]) , (*  15*) ADR (yyNComb [ 108])
         , (*  16*) ADR (yyNComb [ 113]) , (*  17*) ADR (yyNComb [ 125])
         , (*  18*) ADR (yyNComb [ 108]) , (*  19*) ADR (yyNComb [ 113])
         , (*  20*) ADR (yyNComb [ 108]) , (*  21*) ADR (yyNComb [ 108])
         , (*  22*) ADR (yyNComb [ 108]) , (*  23*) ADR (yyNComb [ 108])
         , (*  24*) ADR (yyNComb [ 108]) , (*  25*) ADR (yyNComb [ 108])
         , (*  26*) ADR (yyNComb [ 108]) , (*  27*) ADR (yyNComb [ 108])
         , (*  28*) ADR (yyNComb [ 127]) , (*  29*) ADR (yyNComb [ 110])
         , (*  30*) ADR (yyNComb [ 108]) , (*  31*) ADR (yyNComb [ 108])
         , (*  32*) ADR (yyNComb [ 108]) , (*  33*) ADR (yyNComb [ 108])
         , (*  34*) ADR (yyNComb [ 124]) , (*  35*) ADR (yyNComb [ 108])
         , (*  36*) ADR (yyNComb [ 108]) , (*  37*) ADR (yyNComb [ 108])
         , (*  38*) ADR (yyNComb [ 108]) , (*  39*) ADR (yyNComb [ 112])
         , (*  40*) ADR (yyNComb [ 109]) , (*  41*) ADR (yyNComb [ 123])
         , (*  42*) ADR (yyNComb [ 108]) , (*  43*) ADR (yyNComb [ 124])
         , (*  44*) ADR (yyNComb [ 108]) , (*  45*) ADR (yyNComb [ 134])
         , (*  46*) ADR (yyNComb [ 136]) , (*  47*) ADR (yyNComb [ 108])
         , (*  48*) ADR (yyNComb [ 108]) , (*  49*) ADR (yyNComb [ 108])
         , (*  50*) ADR (yyNComb [ 108]) , (*  51*) ADR (yyNComb [ 136])
         , (*  52*) ADR (yyNComb [ 108]) , (*  53*) ADR (yyNComb [ 108])
         , (*  54*) ADR (yyNComb [ 108])
         };
         
   yyDefault
     := ARRAY [0 .. yyLastReadState] OF [ 0 .. yyLastReadState ]
         {
           (*   0*)    0 , (*   1*)    0 , (*   2*)    0 , (*   3*)    0
         , (*   4*)   14 , (*   5*)    0 , (*   6*)   51 , (*   7*)   29
         , (*   8*)    0 , (*   9*)    0 , (*  10*)    0 , (*  11*)    0
         , (*  12*)    0 , (*  13*)    0 , (*  14*)    0 , (*  15*)    0
         , (*  16*)   45 , (*  17*)   46 , (*  18*)    0 , (*  19*)    0
         , (*  20*)    0 , (*  21*)    0 , (*  22*)    0 , (*  23*)    0
         , (*  24*)    0 , (*  25*)    0 , (*  26*)    0 , (*  27*)    0
         , (*  28*)   51 , (*  29*)    0 , (*  30*)    0 , (*  31*)    0
         , (*  32*)    0 , (*  33*)    0 , (*  34*)   51 , (*  35*)    0
         , (*  36*)    0 , (*  37*)    0 , (*  38*)    0 , (*  39*)    0
         , (*  40*)    0 , (*  41*)   43 , (*  42*)    0 , (*  43*)    0
         , (*  44*)    0 , (*  45*)    0 , (*  46*)    0 , (*  47*)    0
         , (*  48*)    0 , (*  49*)    0 , (*  50*)    0 , (*  51*)    0
         , (*  52*)    0 , (*  53*)    0 , (*  54*)    0
         };
         
   yyTComb
     := ARRAY yyTCombRangePacked OF yyTCombType
         {
           (*   0*) yyTCombType {  24,   76} , (*   1*) yyTCombType {   0,    0}
         , (*   2*) yyTCombType {   0,    0} , (*   3*) yyTCombType {   0,    0}
         , (*   4*) yyTCombType {   0,    0} , (*   5*) yyTCombType {   0,    0}
         , (*   6*) yyTCombType {   0,    0} , (*   7*) yyTCombType {   0,    0}
         , (*   8*) yyTCombType {   0,    0} , (*   9*) yyTCombType {  18,   19}
         , (*  10*) yyTCombType {  46,  103} , (*  11*) yyTCombType {  11,   97}
         , (*  12*) yyTCombType {   0,    0} , (*  13*) yyTCombType {   0,    0}
         , (*  14*) yyTCombType {   0,    0} , (*  15*) yyTCombType {   0,    0}
         , (*  16*) yyTCombType {   0,    0} , (*  17*) yyTCombType {   0,    0}
         , (*  18*) yyTCombType {   0,    0} , (*  19*) yyTCombType {   8,    9}
         , (*  20*) yyTCombType {  19,  106} , (*  21*) yyTCombType {  11,   97}
         , (*  22*) yyTCombType {  20,   57} , (*  23*) yyTCombType {  21,   22}
         , (*  24*) yyTCombType {  29,  103} , (*  25*) yyTCombType {  30,   31}
         , (*  26*) yyTCombType {  35,   36} , (*  27*) yyTCombType {  45,   96}
         , (*  28*) yyTCombType {   1,    2} , (*  29*) yyTCombType {  47,   48}
         , (*  30*) yyTCombType {  52,   53} , (*  31*) yyTCombType {   0,    0}
         , (*  32*) yyTCombType {   1,   90} , (*  33*) yyTCombType {   2,    3}
         , (*  34*) yyTCombType {  25,   26} , (*  35*) yyTCombType {  51,   96}
         , (*  36*) yyTCombType {  39,   40} , (*  37*) yyTCombType {   1,   90}
         , (*  38*) yyTCombType {   2,   13} , (*  39*) yyTCombType {  25,   38}
         , (*  40*) yyTCombType {   0,    0} , (*  41*) yyTCombType {   0,    0}
         , (*  42*) yyTCombType {   0,    0} , (*  43*) yyTCombType {   0,    0}
         , (*  44*) yyTCombType {   0,    0} , (*  45*) yyTCombType {   0,    0}
         , (*  46*) yyTCombType {   0,    0} , (*  47*) yyTCombType {   0,    0}
         , (*  48*) yyTCombType {   0,    0} , (*  49*) yyTCombType {   0,    0}
         , (*  50*) yyTCombType {   0,    0} , (*  51*) yyTCombType {   0,    0}
         , (*  52*) yyTCombType {   0,    0} , (*  53*) yyTCombType {   0,    0}
         , (*  54*) yyTCombType {   0,    0} , (*  55*) yyTCombType {   0,    0}
         , (*  56*) yyTCombType {   0,    0} , (*  57*) yyTCombType {   0,    0}
         , (*  58*) yyTCombType {   0,    0} , (*  59*) yyTCombType {   1,   59}
         , (*  60*) yyTCombType {   0,    0} , (*  61*) yyTCombType {   0,    0}
         , (*  62*) yyTCombType {   0,    0} , (*  63*) yyTCombType {   0,    0}
         , (*  64*) yyTCombType {   0,    0} , (*  65*) yyTCombType {   0,    0}
         , (*  66*) yyTCombType {   5,    6} , (*  67*) yyTCombType {  10,   55}
         , (*  68*) yyTCombType {  11,   97} , (*  69*) yyTCombType {  14,   96}
         , (*  70*) yyTCombType {  15,   16} , (*  71*) yyTCombType {  23,   58}
         , (*  72*) yyTCombType {  32,   60} , (*  73*) yyTCombType {  11,   12}
         , (*  74*) yyTCombType {  14,   98} , (*  75*) yyTCombType {  27,   28}
         , (*  76*) yyTCombType {  37,   61} , (*  77*) yyTCombType {  27,   33}
         , (*  78*) yyTCombType {  39,   91} , (*  79*) yyTCombType {  43,   94}
         , (*  80*) yyTCombType {  39,   91} , (*  81*) yyTCombType {  43,   94}
         , (*  82*) yyTCombType {  49,   62} , (*  83*) yyTCombType {  44,   45}
         , (*  84*) yyTCombType {  43,   42} , (*  85*) yyTCombType {  44,   50}
         , (*  86*) yyTCombType {  54,   63} , (*  87*) yyTCombType {  51,   98}
         , (*  88*) yyTCombType {   0,    0} , (*  89*) yyTCombType {  45,   98}
         , (*  90*) yyTCombType {   0,    0} , (*  91*) yyTCombType {   0,    0}
         , (*  92*) yyTCombType {   0,    0} , (*  93*) yyTCombType {   0,    0}
         , (*  94*) yyTCombType {   0,    0} , (*  95*) yyTCombType {   3,    4}
         , (*  96*) yyTCombType {   9,   10} , (*  97*) yyTCombType {  12,   56}
         , (*  98*) yyTCombType {  13,   14} , (*  99*) yyTCombType {  22,   23}
         , (* 100*) yyTCombType {  26,   27} , (* 101*) yyTCombType {  31,   32}
         , (* 102*) yyTCombType {  33,   34} , (* 103*) yyTCombType {  36,   37}
         , (* 104*) yyTCombType {  38,   39} , (* 105*) yyTCombType {  40,   41}
         , (* 106*) yyTCombType {  42,   43} , (* 107*) yyTCombType {  48,   49}
         , (* 108*) yyTCombType {  50,   51} , (* 109*) yyTCombType {  53,   54}
         , (* 110*) yyTCombType {   0,    0} , (* 111*) yyTCombType {   0,    0}
         , (* 112*) yyTCombType {   0,    0} , (* 113*) yyTCombType {   0,    0}
         , (* 114*) yyTCombType {   0,    0} , (* 115*) yyTCombType {   0,    0}
         , (* 116*) yyTCombType {   0,    0} , (* 117*) yyTCombType {   0,    0}
         , (* 118*) yyTCombType {   0,    0} , (* 119*) yyTCombType {   0,    0}
         , (* 120*) yyTCombType {   0,    0} , (* 121*) yyTCombType {   0,    0}
         , (* 122*) yyTCombType {   0,    0} , (* 123*) yyTCombType {   0,    0}
         , (* 124*) yyTCombType {   0,    0} , (* 125*) yyTCombType {   0,    0}
         , (* 126*) yyTCombType {   0,    0}
         };
         
   yyNComb 
     (* This bounds range is arbitrary, unjustified, and confusing.
        It has nothing to do with states, actions, symbols, or anything
        else.  It is just a space for interspersed actions.  Moreover,
        during use, its elements are accessed only by unsafe pointers to
        to them, from elements of yyNBasePtr, not by subscripts, for a bit
        of speed.  It would make the most sense to start it at zero, like
        yyTComb.  But that would require care to make consistent changes.
     *) 
     := ARRAY yyNCombRangePacked OF yyNCombType
         {
           (* 108*)   66 , (* 109*)   24 , (* 110*)   67 , (* 111*)   68
         , (* 112*)   69 , (* 113*)   70 , (* 114*)   71 , (* 115*)   25
         , (* 116*)    0 , (* 117*)    8 , (* 118*)    7 , (* 119*)   30
         , (* 120*)    5 , (* 121*)   17 , (* 122*)   44 , (* 123*)   75
         , (* 124*)   65 , (* 125*)   11 , (* 126*)   64 , (* 127*)   11
         , (* 128*)   15 , (* 129*)   64 , (* 130*)   11 , (* 131*)   20
         , (* 132*)   65 , (* 133*)   11 , (* 134*)   18 , (* 135*)   29
         , (* 136*)   21 , (* 137*)   35 , (* 138*)   74 , (* 139*)   73
         , (* 140*)   72 , (* 141*)   11 , (* 142*)   46 , (* 143*)   64
         , (* 144*)   11 , (* 145*)   18 , (* 146*)    0 , (* 147*)   47
         , (* 148*)    0 , (* 149*)   52 , (* 150*)   64 , (* 151*)   11
         , (* 152*)   72 , (* 153*)   11 , (* 154*)    0 , (* 155*)    0
         };
         
   yyLength
     (* Subscripted by a reduce action, maps to LHS length of the
        production to reduce by. A<a> is a reduce action number.
        P<p> is the correponding production number.
     *)
     := ARRAY yyReduceRangePacked OF yyTableElmt
         {
           (*A  76(P   1)*)    2 , (*A  77(P   2)*)    1 , (*A  78(P   3)*)    1
         , (*A  79(P   4)*)    1 , (*A  80(P   5)*)    1 , (*A  81(P   6)*)    1
         , (*A  82(P   7)*)    1 , (*A  83(P   8)*)    9 , (*A  84(P   9)*)   10
         , (*A  85(P  10)*)   10 , (*A  86(P  11)*)   10 , (*A  87(P  12)*)    9
         , (*A  88(P  13)*)   10 , (*A  89(P  14)*)    1 , (*A  90(P  15)*)    0
         , (*A  91(P  16)*)    0 , (*A  92(P  17)*)    2 , (*A  93(P  18)*)    2
         , (*A  94(P  19)*)    0 , (*A  95(P  20)*)    3 , (*A  96(P  21)*)    0
         , (*A  97(P  22)*)    1 , (*A  98(P  23)*)    0 , (*A  99(P  24)*)    3
         , (*A 100(P  25)*)    1 , (*A 101(P  26)*)    1 , (*A 102(P  27)*)    1
         , (*A 103(P  28)*)    0 , (*A 104(P  29)*)    4 , (*A 105(P  30)*)    0
         , (*A 106(P  31)*)    0
         };
         
   yyLeftHandSide
     (* Subscripted by a reduce action, maps to LHS NT of the
        of production to reduce by.  A<a> is a reduce action number.
        P<p> is the correponding production number.
     *)
     := ARRAY yyReduceRangePacked OF yySymbolRangePacked
         {
           (*A  76(P   1)*)  127 , (*A  77(P   2)*)  109 , (*A  78(P   3)*)  109
         , (*A  79(P   4)*)  109 , (*A  80(P   5)*)  109 , (*A  81(P   6)*)  109
         , (*A  82(P   7)*)  109 , (*A  83(P   8)*)  108 , (*A  84(P   9)*)  110
         , (*A  85(P  10)*)  111 , (*A  86(P  11)*)  112 , (*A  87(P  12)*)  113
         , (*A  88(P  13)*)  114 , (*A  89(P  14)*)  115 , (*A  90(P  15)*)  115
         , (*A  91(P  16)*)  118 , (*A  92(P  17)*)  118 , (*A  93(P  18)*)  122
         , (*A  94(P  19)*)  123 , (*A  95(P  20)*)  123 , (*A  96(P  21)*)  124
         , (*A  97(P  22)*)  124 , (*A  98(P  23)*)  125 , (*A  99(P  24)*)  125
         , (*A 100(P  25)*)  116 , (*A 101(P  26)*)  120 , (*A 102(P  27)*)  121
         , (*A 103(P  28)*)  117 , (*A 104(P  29)*)  119 , (*A 105(P  30)*)  117
         , (*A 106(P  31)*)  126
         };
         
   yyContinuation
     := ARRAY [0 .. yyLastReadState] OF yySymbolRangePacked
         {
           (*:   0*)    0 , (*:   1*)   32 , (*:   2*)   32
         , (*:   3*)   95 , (*:   4*)   66 , (*:   5*)   66
         , (*:   6*)   19 , (*:   7*)   19 , (*:   8*)   19
         , (*:   9*)   95 , (*:  10*)   67 , (*:  11*)    9
         , (*:  12*)   95 , (*:  13*)   95 , (*:  14*)   66
         , (*:  15*)   66 , (*:  16*)    9 , (*:  17*)    9
         , (*:  18*)    9 , (*:  19*)   19 , (*:  20*)   19
         , (*:  21*)   19 , (*:  22*)   95 , (*:  23*)   67
         , (*:  24*)    0 , (*:  25*)   32 , (*:  26*)   95
         , (*:  27*)   66 , (*:  28*)   19 , (*:  29*)   19
         , (*:  30*)   19 , (*:  31*)   95 , (*:  32*)   67
         , (*:  33*)   95 , (*:  34*)   19 , (*:  35*)   19
         , (*:  36*)   95 , (*:  37*)   67 , (*:  38*)   95
         , (*:  39*)   66 , (*:  40*)   95 , (*:  41*)   66
         , (*:  42*)   95 , (*:  43*)   66 , (*:  44*)   68
         , (*:  45*)    9 , (*:  46*)    9 , (*:  47*)   19
         , (*:  48*)   95 , (*:  49*)   67 , (*:  50*)   95
         , (*:  51*)   19 , (*:  52*)   19 , (*:  53*)   95
         , (*:  54*)   67
         };
         
   yyFinalToProd
     (* Subscripted by a read-reduce action, maps to the reduce
        action to take after the read.
     *) 
     := ARRAY yyReadReduceRangePacked OF yyReduceRangePacked
         {
           (*RR:  55)*)   85 (*P9*) , (*RR:  56)*)   99 (*P23*)
         , (*RR:  57)*)  104 (*P28*) , (*RR:  58)*)   86 (*P10*)
         , (*RR:  59)*)   89 (*P13*) , (*RR:  60)*)   83 (*P7*)
         , (*RR:  61)*)   87 (*P11*) , (*RR:  62)*)   84 (*P8*)
         , (*RR:  63)*)   88 (*P12*) , (*RR:  64)*)  100 (*P24*)
         , (*RR:  65)*)  101 (*P25*) , (*RR:  66)*)   77 (*P1*)
         , (*RR:  67)*)   78 (*P2*) , (*RR:  68)*)   79 (*P3*)
         , (*RR:  69)*)   80 (*P4*) , (*RR:  70)*)   81 (*P5*)
         , (*RR:  71)*)   82 (*P6*) , (*RR:  72)*)  102 (*P26*)
         , (*RR:  73)*)   95 (*P19*) , (*RR:  74)*)   93 (*P17*)
         , (*RR:  75)*)   92 (*P16*)
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
         (* Make room and push (true) state onto state stack. *) 
         IF yyStackPtr >= yyStackLAST 
         THEN
            yyStateStackSize
              := MAX ( NUMBER ( yyStateStack ^ ) * 2 , yyStackPtr + 2 ); 
            ExpandStateStack ( yyStateStack , yyStateStackSize ); 
            ExpandAttributeStack ( yyAttributeStack , yyStateStackSize );
            yyStackLAST
              := LAST ( yyStateStack ^ ) (* Of yyAttributeStack too. *);
         END (* IF *) ;
         yyStateStack^ [yyStackPtr] := (*State*)yyState;

         LOOP (* Through all continuation pushes, plus compute the state
                 after that. This loop also goes through the default state
                 computations. *) 
            (* SPEC State := Next (State, Terminal); terminal transition *)
            
            yyTCombPtr := LOOPHOLE 
                            ( LOOPHOLE ( yyTBasePtr [(*State*)yyState] ,INTEGER) 
                              + yyTerminal * BYTESIZE (yyTCombType)
                            , yyTCombTypePtr
                            );
            IF yyTCombPtr^.Check = (*State*)yyState 
            THEN
               (*Either*)yyState := yyTCombPtr^.Next;
               EXIT;
            END (* IF *) ;
            (*State*)yyState := yyDefault [(*State*)yyState];

            IF yyState = yyNoState 
            THEN (* syntax error *)
               (*State*)yyState := yyStateStack^ [yyStackPtr];
               IF yyIsRepairing 
               THEN (* repair *)
                  yyRepairToken := yyContinuation [yyState];
                  (*Either*)yyState := Next (yyState, yyRepairToken);
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
                        (*Reduce*)yyState := yyFinalToProd [(*Action*)yyState];
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
         THEN (* Action *)
            IF yyState <= yyLastReadTermState (* read terminal reduce ? *) 
            THEN (* First do the read terminal. *)
               INC (yyStackPtr);
              yyAttributeStack^ [yyStackPtr].Scan := FM3Scanner.Attribute;
              yyTerminal := FM3Scanner.GetToken ();
               yyIsRepairing := FALSE;
                yyStateStack^ [yyStackPtr] := (*Reduce*)yyState (*ParserDebug*);
            END (* IF *) ;

            LOOP (* Through successive reductions *)
              CASE yyState OF
              | 76 => (* P1 _0000_ (127): Compilation _EndOfFile .*)
                yyStateStack := NIL;
                yyAttributeStack := NIL;
                RETURN yyErrorCount;

              | 77,66 => (* P2 Compilation (109): Interface .*)
                DEC (yyStackPtr, 1); yyNonterminal := 109;

              | 78,67 => (* P3 Compilation (109): Module .*)
                DEC (yyStackPtr, 1); yyNonterminal := 109;

              | 79,68 => (* P4 Compilation (109): GenInterface .*)
                DEC (yyStackPtr, 1); yyNonterminal := 109;

              | 80,69 => (* P5 Compilation (109): GenModule .*)
                DEC (yyStackPtr, 1); yyNonterminal := 109;

              | 81,70 => (* P6 Compilation (109): InstInterface .*)
                DEC (yyStackPtr, 1); yyNonterminal := 109;

              | 82,71 => (* P7 Compilation (109): InstModule .*)
                DEC (yyStackPtr, 1); yyNonterminal := 109;

              | 83,60 => (* P8 Interface (108): OptUnsafe StkRwINTERFACE StkIdent StkSemicolon ImportList DeclList StkRwEND StkIdent StkDot .*)
                DEC (yyStackPtr, 9); yyNonterminal := 108;

              | 84,62 => (* P9 Module (110): OptUnsafe StkRwMODULE StkIdent Exports StkSemicolon ImportList Block StkRwEND StkIdent StkDot .*)
                DEC (yyStackPtr, 10); yyNonterminal := 110;

              | 85,55 => (* P10 GenInterface (111): StkRwGENERIC StkRwINTERFACE StkIdent GenFormalsList StkSemicolon ImportList DeclList StkRwEND StkIdent StkDot .*)
                DEC (yyStackPtr, 10); yyNonterminal := 111;

              | 86,58 => (* P11 GenModule (112): StkRwGENERIC StkRwMODULE StkIdent GenFormalsList StkSemicolon ImportList Block StkRwEND StkIdent StkDot .*)
                DEC (yyStackPtr, 10); yyNonterminal := 112;

              | 87,61 => (* P12 InstInterface (113): OptUnsafe StkRwINTERFACE StkIdent StkEqual StkIdent GenActuals StkRwEND StkIdent StkDot .*)
                DEC (yyStackPtr, 9); yyNonterminal := 113;

              | 88,63 => (* P13 InstModule (114): OptUnsafe StkRwMODULE StkIdent Exports StkEqual StkIdent GenActuals StkRwEND StkIdent StkDot .*)
                DEC (yyStackPtr, 10); yyNonterminal := 114;

              | 89,59 => (* P14 OptUnsafe (115): StkRwUNSAFE .*)
                DEC (yyStackPtr, 1); yyNonterminal := 115;
                (* line 207 of "FM3Parser.lalr" *)
                 yySynAttribute . PaBool := TRUE; 
              | 90 => (* P15 OptUnsafe (115): .*)
                yyNonterminal := 115;
                (* line 208 of "FM3Parser.lalr" *)
                 yySynAttribute . PaBool := FALSE; 
              | 91 => (* P16 Exports (118): .*)
                yyNonterminal := 118;
                (* line 210 of "FM3Parser.lalr" *)
                 
              | 92,75 => (* P17 Exports (118): StkRwEXPORTS IdList .*)
                DEC (yyStackPtr, 2); yyNonterminal := 118;
                (* line 211 of "FM3Parser.lalr" *)
                 
              | 93,74 => (* P18 IdList (122): StkIdent IdListTail .*)
                DEC (yyStackPtr, 2); yyNonterminal := 122;

              | 94 => (* P19 IdListTail (123): .*)
                yyNonterminal := 123;

              | 95,73 => (* P20 IdListTail (123): StkComma StkIdent IdListTail .*)
                DEC (yyStackPtr, 3); yyNonterminal := 123;

              | 96 => (* P21 ExportsList (124): .*)
                yyNonterminal := 124;

              | 97 => (* P22 ExportsList (124): ExportsListSub .*)
                DEC (yyStackPtr, 1); yyNonterminal := 124;
                (* line 224 of "FM3Parser.lalr" *)
                 WITH i = yyAttributeStack^[yyStackPtr+1] . PaLong
                      DO PushTok ( Itk . ItkExportsListRt , i+1L);
                        PushTokPatch ( Itk . ItkExportsListLtPatch , 0L , i+1L);
                      END (*WITH*); 
                    
              | 98 => (* P23 ExportsListSub (125): .*)
                yyNonterminal := 125;
                (* line 230 of "FM3Parser.lalr" *)
                 yySynAttribute . PaLong := 0L; 
              | 99,56 => (* P24 ExportsListSub (125): ExportsListSub StkComma StkIdent .*)
                DEC (yyStackPtr, 3); yyNonterminal := 125;
                (* line 232 of "FM3Parser.lalr" *)
                 WITH i = yyAttributeStack^[yyStackPtr+1] . PaLong
                      DO PushTok ( Itk . ItkExportsListElemRt , i );
                        PushTokPatch ( Itk . ItkExportsListElemLtPatch , 0L , i );
                        yySynAttribute . PaLong := i+1L;
                      END (*WITH*); 
                    
              | 100,64 => (* P25 ImportList (116): ExportsList .*)
                DEC (yyStackPtr, 1); yyNonterminal := 116;

              | 101,65 => (* P26 GenFormalsList (120): ExportsList .*)
                DEC (yyStackPtr, 1); yyNonterminal := 120;

              | 102,72 => (* P27 GenActuals (121): ExportsList .*)
                DEC (yyStackPtr, 1); yyNonterminal := 121;

              | 103 => (* P28 DeclList (117): .*)
                yyNonterminal := 117;

              | 104,57 => (* P29 Block (119): DeclList StkRwBEGIN StmtList StkRwEND .*)
                DEC (yyStackPtr, 4); yyNonterminal := 119;

              | 105 => (* P30 DeclList (117): .*)
                yyNonterminal := 117;

              | 106 => (* P31 StmtList (126): .*)
                yyNonterminal := 126;

              END (*CASE*);

               (* Here, a reduction has been done.  yyStackPtr has been
                  decremented by the RHS length, yyNonterminal has been set to
                  the LHS NT of the reduced-by production, and  Semantic
                  actions have been performed.  Now do a nonterminal read
                  transition on yyStateStack^ [yyStackPtr] (* A state*)
                  and yyNonterminal.
               *) 

               (* SPEC yyState 
                    := Next (Top (), yyNonterminal); nonterminal transition *)
               yyNCombPtr 
                 := LOOPHOLE 
                      ( LOOPHOLE
                          ( yyNBasePtr [yyStateStack^ [yyStackPtr]], INTEGER )
                          + (yyNonterminal-(yyLastTerminal+1))
                            * BYTESIZE (yyNCombType) 
                      , yyNCombTypePtr
                      );
               (*Either*)yyState := yyNCombPtr^;
               INC (yyStackPtr);
               yyAttributeStack^ [yyStackPtr] := yySynAttribute;
                yyAttributeStack^ [yyStackPtr].Scan.SaTok := yyNonterminal;
             (* ^This requires that tScanAttribute have field 'SaTok'. *)
                yyStateStack^ [yyStackPtr] := (*Either*)yyState (*ParserDebug*);
               IF yyState < yyFirstFinalState (* read nonterminal? *) 
               THEN (* A state. *)
                 EXIT 
               END (* IF *) ; 
            END (* LOOP *) ;

         ELSE (* Read NT. *)
            INC (yyStackPtr);
             yyStateStack^ [yyStackPtr] := (*State*)yyState (*ParserDebug*);
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
                  Nonterminal := yyLeftHandSide [State];               END;

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
                 + (Symbol-(yyLastTerminal+1))
                   * BYTESIZE (yyNCombType)
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

