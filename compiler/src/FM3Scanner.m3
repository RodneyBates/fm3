
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Scanner 

; IMPORT Fmt
; IMPORT Pickle2 
; IMPORT Rd
; IMPORT Scan AS FM3Scan
; IMPORT Text
; IMPORT TextIntTbl
; IMPORT Thread 
; IMPORT UniRd 
; IMPORT UnsafeUniRd
; IMPORT Word 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_OAWideChars 
; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3Dict_OAChars_Int
; IMPORT FM3Files
; IMPORT FM3Globals
; IMPORT FM3LexTable
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3PgToks
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3SrcToks 
; IMPORT FM3Units
; IMPORT FM3Utils
; IMPORT FM3UnsafeUtils 

; IMPORT IntRanges 
; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar

; TYPE IntRangeTyp = IntRanges . RangeTyp

(* New line and end-of-file characters: *) 
; CONST LF =  '\x0A' (* = '\n' *)
; CONST VT =  '\x0B' (* = '\v' *) 
; CONST FF =  '\x0C' (* = '\f' *) 
; CONST CR =  '\x0D' (* = '\r' *)

; CONST NEL = '\x85'  
; CONST WLS =  W'\x2028'  
; CONST WPS =  W'\x2029'  
; CONST EOLCHARS = SET OF CHAR { LF , CR , FF , VT , NEL } 
(* Also CR immediately followed by LF *)  

; CONST BS =  '\x08' (* = '\t' *)
; CONST TAB =  '\x09' (* = '\t' *)
; CONST WEOF = W'\X7000' (* Use a unicode app-specific value. *) 
; CONST WNUL = W'\X7001' (* Use a unicode app-specific value. *)
; CONST WCR = W'\r' 
; CONST WLF = W'\n' 
; CONST NUL = '\X00' 

; CONST TopmostLineNo = 1 (* According to emacs. *)  
; CONST LeftmostColumnNo = 0 (* According to emacs. *)  

; CONST WLastOfChar : WIDECHAR = LAST ( CHAR ) 

; CONST M3Chars 
    = SET OF CHAR 
        { ' ' , '.' , ':' 
        , ';' , '*' , '/' , '<' , '>' , '=' , '#' , '|' , '^' , ',' , '&' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"' , '\'' 
        } 
      + EOLCHARS 

; VAR RwTable : TextIntTbl . T 
; VAR PgTable : TextIntTbl . T 

; PROCEDURE InitTables ( ) 

  = BEGIN 
    END InitTables 

; VAR GTopSsRef : ScanStateRefTyp := NIL 
; VAR GScanStateCt := 0
; VAR GScanStateDepth := 0
; VAR GMaxScanStateDepth := 0   

; TYPE ScanStateRefTyp = REF ScanStateTyp 

; TYPE ScanStateTyp 
       = RECORD 
           Position : tPosition   
         ; SsLink : ScanStateRefTyp := NIL
         ; SsSavedAttribute : tScanAttribute
           (* ^Of SsLink's scanner instance.  Possibly SsLink is NIL, 
              in which case, it is just ScanAttributeDefault. *) 
         ; SsUniRd : UniRd . T := NIL 
         ; SsUnitRef : FM3Units . UnitRefTyp 
         ; SsPragmaDepth : INTEGER := 0  
         ; SsWCh : WIDECHAR 
         ; SsCh : CHAR 
         ; SsAtBegOfPragma := FALSE 
           (* ^The immediately-preceding delivered token was "<*". *) 
         ; SsAtEndOfPragma := FALSE (* Similarly for "*>". *)  
         END (* ScanStateTyp *) 

; CONST ScanAttributeDefault = tScanAttribute 
      { Position := tPosition { TopmostLineNo , LeftmostColumnNo } 
      , SaArgValue := 0L 
      , SaHash := FM3Base . HashNull
      , SaAtom := FM3Base . AtomNull
      , SaWideChars := NIL 
      , SaChars := NIL 
      , SaTok := FM3Base . TokNull  
      , SaBuiltinTok := FM3Base . TokNull 
      , SaWCh := W'\X0000'
      } 

(* EXPORTED: *) 
; PROCEDURE PushState 
     ( NewUniRd : UniRd . T ; UnitRef : FM3Units . UnitRefTyp ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

  = VAR LSsRef : ScanStateRefTyp (* Topmost after push. *) 

  ; BEGIN 
      LSsRef := NEW ( ScanStateRefTyp ) 
    ; LSsRef ^ . SsLink := GTopSsRef 
    ; LSsRef ^ . SsSavedAttribute := Attribute 
    ; Attribute := ScanAttributeDefault  
    ; LSsRef ^ . SsUnitRef := UnitRef
    ; LSsRef ^ . SsUniRd := NewUniRd 
    ; LSsRef ^ . Position . Line := TopmostLineNo 
    ; LSsRef ^ . Position . Column := LeftmostColumnNo 
    ; LSsRef ^ . SsAtBegOfPragma := FALSE  
    ; LSsRef ^ . SsAtEndOfPragma := FALSE  
    ; LSsRef ^ . SsPragmaDepth := 0   

    ; TRY 
        LSsRef ^ . SsWCh := UnsafeUniRd . FastGetWideChar( LSsRef ^ . SsUniRd ) 
      ; IF LSsRef ^ . SsWCh <= WLastOfChar 
        THEN LSsRef ^ . SsCh := LSsRef ^ . SsWCh 
        ELSE LSsRef ^ . SsCh := NUL 
        END (*IF*) 
(* What the? 
      ; IF GTopSsRef # NIL 
        THEN 
          IF GTopSsRef . SsWCh <= WLastOfChar 
          THEN GTopSsRef . SsCh := GTopSsRef . SsWCh 
          ELSE GTopSsRef . SsCh := NUL 
          END (*IF*) 
        END (*IF*) 
*)
      EXCEPT 
      | Thread . Alerted => (* Ignore *)  
      | Rd . EndOfFile , Rd . Failure 
      => LSsRef . SsWCh := WEOF 
      ;  LSsRef . SsCh := NUL 
      END (*EXCEPT*) 
    ; GTopSsRef := LSsRef 
    ; INC ( GScanStateCt ) 
    ; INC ( GScanStateDepth ) 
    ; GMaxScanStateDepth := MAX ( GMaxScanStateDepth , GScanStateDepth ) 
    END PushState 

(* EXPORTED: *) 
; PROCEDURE PopState ( ) : UniRd . T (* Previous reader. *)  

  = VAR LSsRef : ScanStateRefTyp (* Topmost before pop. *)

  ; BEGIN 
      IF GTopSsRef = NIL THEN RETURN NIL END (* IF *) 
    ; LSsRef := GTopSsRef 
    ; GTopSsRef := LSsRef ^ . SsLink 
    ; Attribute := LSsRef ^ . SsSavedAttribute 
    ; TRY UniRd . Close ( LSsRef . SsUniRd ) 
      EXCEPT 
      | Thread . Alerted => (* Ignore *)  
      | Rd . Failure 
      => IF GTopSsRef # NIL 
         THEN 
           GTopSsRef . SsWCh := WEOF 
         ; GTopSsRef . SsCh := NUL 
         END (*IF*) 
      END (*EXCEPT*) 
    ; DEC ( GScanStateDepth ) 
    ; RETURN LSsRef ^ . SsUniRd   
    END PopState 

(* EXPORTED: *) 
; PROCEDURE CurrentUnitNo ( ) : FM3Globals . UnitNoTyp  

  = BEGIN 
      IF GTopSsRef = NIL THEN RETURN FM3Globals . UnitNoNull END (*IF*) 
    ; RETURN GTopSsRef . SsUnitRef ^ . UntSelfUnitNo  
    END CurrentUnitNo 

; PROCEDURE ErrorAtPos ( READONLY Frags : ARRAY OF REFANY ; CharPos : INTEGER )
  (* Report at CharPos of the current line. *) 
 
  = VAR LPos := FM3Base . tPosition { Attribute . Position . Line , CharPos }

  ; BEGIN 
      FM3Messages . ErrorArr ( Frags , LPos )  
    END ErrorAtPos 

; PROCEDURE ErrorAtTok ( READONLY Frags : ARRAY OF REFANY ; Adjust := 0 )
  (* Report relative to the beginning of the current token. *) 
 
  = VAR LPos := FM3Base . tPosition            
                  { Attribute . Position . Line 
                  , Attribute . Position . Column + Adjust  
                  }
  ; BEGIN 
      FM3Messages . ErrorArr ( Frags , LPos )  
    END ErrorAtTok 

; PROCEDURE ErrorAtSs ( READONLY Frags : ARRAY OF REFANY ; Adjust := 0 ) 
  (* Report relative to the current spot in the input *) 

  = VAR LPos := FM3Base . tPosition 
                  { GTopSsRef ^ . Position . Line  
                  , GTopSsRef ^ . Position . Column + Adjust  
                  }

  ; BEGIN 
      FM3Messages . ErrorArr ( Frags , LPos )  
    END ErrorAtSs 

; PROCEDURE AppendChar ( VarArr : VarArr_Char . T ; Ch : CHAR ) 

  = VAR LTouchedRange : IntRangeTyp 
  ; VAR LSs : INTEGER 

  ; BEGIN 
      LTouchedRange := VarArr_Char . TouchedRange ( VarArr ) 
    ; IF IntRanges . RangeIsEmpty ( LTouchedRange ) 
      THEN LSs := 0 
      ELSE LSs := LTouchedRange . Hi + 1 
      END (*IF*) 
    ; VarArr_Char . Assign ( VarArr , LSs , Ch ) 
    END AppendChar 

; PROCEDURE AppendWChar ( VarArr : VarArr_WChar . T ; WCh : WIDECHAR ) 

  = VAR LTouchedRange : IntRangeTyp 
  ; VAR LSs : INTEGER 

  ; BEGIN 
      LTouchedRange := VarArr_WChar . TouchedRange ( VarArr ) 
    ; IF IntRanges . RangeIsEmpty ( LTouchedRange ) 
      THEN LSs := 0 
      ELSE LSs := LTouchedRange . Hi + 1 
      END (*IF*) 
    ; VarArr_WChar . Assign ( VarArr , LSs , WCh ) 
    END AppendWChar 

; PROCEDURE ContribToFsm ( Char : CHAR )  

  = BEGIN 
      IF GCurRwValue = FM3LexTable . ValueUnrecognized 
      THEN (* Keep it that way. *) 
      ELSIF GCurRwValue = FM3LexTable . ValueNull 
      THEN (* Need more chars. *) 
        GCurRwValue 
          := FM3LexTable . IncrNext 
               ( GCurRwLexTable , Char , (*IN OUT*) GCurRwState )
      ELSE (* Recognized previously. Keep it that way. *) 
      END (*IF*) 
    END ContribToFsm 

(* EXPORTED: *) 
; PROCEDURE GetToken ( ) : INTEGER (*lalr*)  

  = VAR ScWCharVarArr : VarArr_WChar . T
  ; VAR ScCharVarArr : VarArr_Char . T  
  ; VAR ScHash : FM3Utils . HashTyp 
  ; VAR ScCh : CHAR 
  ; VAR ScAtBegOfPragma := FALSE 

  ; PROCEDURE BegOfPlainTok ( ) 

    = BEGIN (* BegOfPlainTok *) 
        Attribute . SaHash := FM3Utils . HashNull 
      ; Attribute . SaAtom := FM3Base . AtomNull 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := NIL 
      END BegOfPlainTok 

  ; PROCEDURE NextChar ( ) 
    (* Converts any newline char or sequence to LF/WLF. 
       Leaves GTopSsRef.Position.Column denoting the
       left char of the two-char newline sequence.
    *)

    = VAR LWCh : WIDECHAR 

    ; BEGIN (* NextChar *) 
        IF GTopSsRef . SsCh = LF 
        THEN 
(* TODO: Decide how to indicate new line to client. *) 
          INC ( GTopSsRef . Position . Line ) 
        ; GTopSsRef . Position . Column := LeftmostColumnNo 
        ELSE INC ( GTopSsRef . Position . Column )
        END (* IF *) 
      ; TRY GTopSsRef . SsWCh 
              := UnsafeUniRd . FastGetWideChar ( GTopSsRef ^ . SsUniRd ) 
        EXCEPT 
        | Thread . Alerted => (* Ignore *)  
        | Rd . EndOfFile , Rd . Failure 
        => GTopSsRef . SsWCh := WEOF 
        ;  GTopSsRef . SsCh := NUL 
        ; RETURN 
        END (*EXCEPT*)
      ; IF GTopSsRef . SsWCh <= WLastOfChar 
        THEN GTopSsRef . SsCh := GTopSsRef . SsWCh 
        ELSE GTopSsRef . SsCh := NUL 
        END (*IF*) 
      ; IF GTopSsRef . SsWCh = WCR 
        THEN 
          TRY LWCh := UnsafeUniRd . FastGetWideChar ( GTopSsRef ^ . SsUniRd ) 
          EXCEPT 
          | Thread . Alerted => (* Ignore *)  
          | Rd . EndOfFile , Rd . Failure 
          => GTopSsRef . SsWCh := WEOF 
          ;  GTopSsRef . SsCh := NUL 
          ; RETURN 
          END (*EXCEPT*) 
        ; IF LWCh = WLF 
          THEN 
            GTopSsRef . SsWCh := WLF (* Canonical new line. *)
          ; GTopSsRef . SsCh := LF
          ELSE 
            <* ASSERT 
                 UnsafeUniRd . FastUnGetCodePoint ( GTopSsRef ^ . SsUniRd )
            *> 
          END (* IF *) 
        ELSIF GTopSsRef . SsCh IN  SET OF CHAR { LF , FF , VT , NEL } 
              OR GTopSsRef . SsWCh = WPS 
              OR GTopSsRef . SsWCh = WLS
        THEN  
          GTopSsRef . SsWCh := WLF (* Canonical new line. *) 
        ; GTopSsRef . SsCh := LF 
        END (* IF *) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( ) 
    (* PRE: First bad char is in GTopSsRef . SsWCh. *)  

    = VAR LCharCt : INTEGER 
    ; VAR LBadCharText : TEXT 

    ; BEGIN (* LexErrorChars *)  
        ScCharVarArr := NIL 
      ; ScWCharVarArr := VarArr_WChar . New ( NUL , IntRangeTyp { 0 , 120 } ) 
      ; LCharCt := 0 
      ; WHILE ( GTopSsRef . SsWCh > WLastOfChar 
                AND GTopSsRef . SsWCh # WLS 
                AND GTopSsRef . SsWCh # WPS 
              ) OR NOT GTopSsRef . SsCh IN M3Chars 
        DO 
          AppendWChar ( ScWCharVarArr , GTopSsRef . SsWCh ) 
        ; INC ( LCharCt ) 
        ; NextChar ( )
        END (*WHILE*) 
      ; Attribute . SaWideChars 
          := FM3Utils . WCharVarArrayToOAWChar ( ScWCharVarArr ) 
      ; LBadCharText 
          := FM3Utils . WideTextLiteral ( Attribute . SaWideChars ) 
      ; ErrorAtTok 
          ( ARRAY OF REFANY 
              { Fmt . Int ( LCharCt ) , " illegal characters: " , LBadCharText }
          ) 
      END LexErrorChars 

  ; CONST IdentFollowChars 
            = SET OF CHAR { 'A' .. 'Z' , 'a' .. 'z' , '0' .. '9' , '_' } 

  ; PROCEDURE StartIdent ( ) 

    = BEGIN 
        IF ScAtBegOfPragma 
        THEN GCurRwLexTable := GPgRwLexTable 
        ELSE GCurRwLexTable := GM3RwLexTable
        END (*IF*) 
      ; GCurRwState := FM3LexTable . IncrInit ( GCurRwLexTable ) 
      ; GCurRwValue := FM3LexTable . ValueNull 
      END StartIdent 

  ; PROCEDURE IdentSuffix ( ) 
    (* PRE: The identifier is already started, possibly non-empty. *) 

    = BEGIN (* IdentSuffix *) 
        WHILE GTopSsRef . SsCh IN IdentFollowChars 
        DO 
          ContribToFsm ( GTopSsRef . SsCh ) 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            ) 
        ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        END (* WHILE *) 
 
      ; Attribute . SaHash := ScHash 
      ; Attribute . SaChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; IF GCurRwValue = FM3LexTable . ValueNull 
        THEN (* Not recognized, but could be a prefix of longer RW. *) 
(* NOTE: ^This is a workaround for FM3BuilLexMachine's inconsistent habit
          of having a transition on NullChar for the end of a string, IFF
          it is a prefix of a longer string. 
   TODO: Regularize FM3BuildLexMachine and FM3LexTable so this distinction
         need not be accomodated by client code. 
*)   
          GCurRwValue (* Done with string. *) 
            := FM3LexTable . IncrNext 
                 ( GCurRwLexTable , FM3LexTable . NullChar , GCurRwState ) 
        END (*IF*)

      ; IF ScAtBegOfPragma (* Expecting a pragma id? *) 
        THEN (* GCurRwValue will have been recognized only among the known
                pragma ids. 
             *)
          CASE GCurRwValue OF
          | FM3LexTable . ValueUnrecognized , FM3LexTable . ValueNull 
          => (* It's not a pragma ident. Make it an ordinary ident. *) 
             Attribute . SaAtom 
              := FM3Atom_OAChars . MakeAtom 
                   ( GTopSsRef . SsUnitRef ^ . UntIdentAtomDict
                   , Attribute . SaChars 
                   , ScHash 
                   ) 
          ; Attribute . SaTok := FM3SrcToks . StkIdent  
          ; Attribute . SaBuiltinTok := FM3Base . TokNull 
(* TODO: Handle this as an unrecognized pragma and skip the entire thing. *) 

          ELSE (* It's a pragma ident. *) 
            Attribute . SaBuiltinTok := GCurRwValue (* Recognized pragma name. *)
          ; Attribute . SaTok := FM3SrcToks . StkPragmaId 
          END (*CASE*) 

        ELSE (* Expecting a reserved ident, standard, or declared identifier. *) 
          Attribute . SaTok := FM3SrcToks . StkIdent
        ; CASE GCurRwValue OF 
          | FM3SrcToks . StkMinRid .. FM3SrcToks . StkMaxRid 
          => (* Reserved identifier.  It has its builtin meaning and only that
                in every context.  Make this an StkIdent with Null SaAtom
                and SaBuiltinTok set to the reserved Id's lex code. Having
                a single token for reserved and nonreserved identifiers
                simplifies parsing and allows more helpful error messages
                for misused reserved idents.  
             *) 
              Attribute . SaAtom := FM3Base . AtomNull 
            ; Attribute . SaBuiltinTok := GCurRwValue 

          | FM3SrcToks . StkMinStd .. FM3SrcToks . StkMaxStd
          => (* Not reserved but standard identifier.  It can be an ordinary 
                identifier or, in certain contexts, have a standard meaning. 
                Give it both an atom and the lex value of SaBuiltinTok.  
                FM3 will decide later which to use. 
             *) 
              Attribute . SaAtom 
                := FM3Atom_OAChars . MakeAtom 
                     ( GTopSsRef . SsUnitRef ^ . UntIdentAtomDict
                     , Attribute . SaChars 
                     , ScHash 
                     ) 
            ; Attribute . SaBuiltinTok := GCurRwValue 

          | FM3LexTable . ValueUnrecognized , FM3LexTable . ValueNull 
          => (* Plain ol' identifier. Give it an atom in the current unit,
                but Null lex value. 
             *)
              Attribute . SaAtom 
                := FM3Atom_OAChars . MakeAtom 
                     ( GTopSsRef . SsUnitRef ^ . UntIdentAtomDict
                     , Attribute . SaChars 
                     , ScHash 
                     ) 
            ; Attribute . SaBuiltinTok := FM3Base . TokNull  

          ELSE (* Reserved word.  Lex value is the token code. *) 
            Attribute . SaTok := GCurRwValue 
          ; Attribute . SaAtom := FM3Base . TokNull  
          ; Attribute . SaBuiltinTok := FM3Base . TokNull  
          END (*CASE*) 
        END (*IF*) 
      END IdentSuffix 

  ; PROCEDURE NumberArgValue 
      ( Tok : FM3SrcToks . TokTyp ; CharsRef : FM3OpenArray_Char . T ) 
    : LONGINT  

    = VAR LResult : LONGINT 
    ; VAR LLength : INTEGER 

    ; BEGIN 
        CASE Tok OF 
        | FM3SrcToks . StkIntLit
        => LResult 
             := FM3Scan . LongUnsigned 
                  ( Text . FromChars ( CharsRef ^ ) , defaultBase := 10 ) 

        | FM3SrcToks . StkLongIntLit 
        =>  LLength := NUMBER ( CharsRef ^ ) 
          ; LResult 
             := FM3Scan . LongUnsigned 
                  ( Text . FromChars 
                      ( SUBARRAY ( CharsRef ^ , 0 , LLength - 1 ) ) 
                  , defaultBase := 10 
                  )

        | FM3SrcToks . StkBasedLit
        =>  LResult 
             := FM3Scan . LongUnsigned 
                  ( Text . FromChars ( CharsRef ^ ) , defaultBase := 16 )  

        | FM3SrcToks . StkLongBasedLit
        =>  LLength := NUMBER ( CharsRef ^ ) 
          ; LResult 
             := FM3Scan . LongUnsigned 
                  ( Text . FromChars 
                      ( SUBARRAY ( CharsRef ^ , 0 , LLength - 1 ) ) 
                  , defaultBase := 16 
                  ) 

        | FM3SrcToks . StkRealLit 
        =>  LResult 
             := FM3UnsafeUtils . RealToLongInt 
                  ( FM3Scan . Real ( Text . FromChars ( CharsRef ^ ) ) ) 

        | FM3SrcToks . StkLongRealLit 
        =>  LResult 
             := FM3UnsafeUtils . LongRealToLongInt 
                  ( FM3Scan . LongReal ( Text . FromChars ( CharsRef ^ ) ) )

        | FM3SrcToks . StkExtendedLit
        =>  LResult 
             := FM3UnsafeUtils . ExtendedToLongInt 
                  ( FM3Scan . Extended ( Text . FromChars ( CharsRef ^ ) ) ) 
        END (*CASE*) 
      ; RETURN LResult 
      END NumberArgValue 

  ; CONST DigitChars = SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' }

  ; PROCEDURE Number ( ) 
    (* PRE: GTopSsRef . SsCh is a digit.  TextTok not started. *)  

    = VAR LTok : FM3SrcToks . TokTyp 

    ; BEGIN (* Number *) 
        ScHash := FM3Utils . GroundHash ( ) 
      ; Attribute . SaAtom := FM3Base . AtomNull (* Overlaid later? *) 
      ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 40 } ) 
      ; ScWCharVarArr := NIL 
      ; LTok := FM3SrcToks . StkIntLit 
      ; REPEAT 
          FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            )
        ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
        
      ; CASE GTopSsRef . SsCh
        OF 'L' 
        => LTok := FM3SrcToks . StkLongIntLit 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            )
        ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
        ; NextChar ( ) 

        | '_' 
        => LTok := FM3SrcToks . StkBasedLit 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            )
        ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh IN DigitChars  
          THEN 
            REPEAT 
              FM3Utils . ContribToHash 
                ( (*IN OUT*) ScHash 
                , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                )
            ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
            ; NextChar ( )  
            UNTIL NOT GTopSsRef . SsCh IN DigitChars  
          ; IF  GTopSsRef . SsCh = 'L' 
            THEN 
              LTok := FM3SrcToks . StkLongBasedLit 
            ; FM3Utils . ContribToHash 
                ( (*IN OUT*) ScHash 
                , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                )
            ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
            ; NextChar ( ) 
            END (*IF*) 
          ELSE 
            ErrorAtSs ( ARRAY OF REFANY { "Based literal has no digit" } ) 
          END (* IF *) 

        | '.' 
        => NextChar ( ) 
        ; IF GTopSsRef . SsCh = '.' 
          THEN (* "..", a different token. *)
            <* ASSERT 
                 UnsafeUniRd . FastUnGetCodePoint ( GTopSsRef ^ . SsUniRd ) 
            *> 
          ELSE
            LTok := FM3SrcToks . StkRealLit 
          ; FM3Utils . ContribToHash 
              ( (*IN OUT*) ScHash 
              , VAL ( ORD ( '.' ) , FM3Utils . HashTyp ) 
              )
          ; AppendChar ( ScCharVarArr , '.' ) 
          ; IF GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            THEN 
              REPEAT 
                FM3Utils . ContribToHash 
                  ( (*IN OUT*) ScHash 
                  , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                  )
              ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
              ; NextChar ( ) 
              UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            ELSE 
              ErrorAtSs 
                ( ARRAY OF REFANY 
                    {" Floating point literal has no fractional digit" } 
                ) 
            END (* IF *) 
          END (* IF *) 
        ; IF GTopSsRef . SsCh
             IN SET OF CHAR { 'E' , 'e' , 'D' , 'd' , 'X' , 'x' }  
          THEN 
            IF GTopSsRef . SsCh IN SET OF CHAR { 'D' , 'd' }
            THEN LTok := FM3SrcToks . StkLongRealLit 
            ELSIF GTopSsRef . SsCh IN SET OF CHAR { 'X' , 'x' }
            THEN LTok := FM3SrcToks . StkExtendedLit 
            END (*IF*) 
          ; FM3Utils . ContribToHash 
               ( (*IN OUT*) ScHash 
               , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
               )
          ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
          ; NextChar ( ) 
          ; IF GTopSsRef . SsCh IN SET OF CHAR { '+' , '-' }  
            THEN 
               FM3Utils . ContribToHash 
                 ( (*IN OUT*) ScHash 
                 , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                 )
            ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
            ; NextChar ( ) 
            END (* IF *) 
          ; IF GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            THEN 
              REPEAT 
                FM3Utils . ContribToHash 
                  ( (*IN OUT*) ScHash 
                  , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                  )
              ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
              ; NextChar ( ) 
              UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            ELSE 
              ErrorAtSs 
                ( ARRAY OF REFANY 
                    { "Floating point literal has no exponent digit." } 
                ) 
            END (* IF *) 
          ELSE 
          END (*IF*)  
        ELSE 
        END (*CASE*)  
      ; Attribute . SaHash := ScHash 
      ; Attribute . SaChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; Attribute . SaArgValue := NumberArgValue ( LTok , Attribute . SaChars ) 
(* CHECK: Is an atom for a numeric literal character string really needed? 
          Probably only the binary version of the value. *) 
      ; Attribute . SaAtom 
          := FM3Atom_OAChars . MakeAtom 
               ( GTopSsRef ^ . SsUnitRef ^ . UntNumLitAtomDict 
               , Attribute . SaChars 
               , ScHash 
               ) 
      ; Attribute . SaTok := LTok 
      END Number 

  ; CONST LitTypeName
            = ARRAY BOOLEAN (*Wide*) , BOOLEAN (*Text*) OF TEXT
                { ARRAY BOOLEAN (*Text*) OF TEXT { "CHAR" , "TEXT" }
                , ARRAY BOOLEAN (*Text*) OF TEXT { "WIDECHAR" , "Wide TEXT" } 
                } 

; PROCEDURE LineCharExists 
      ( Msg1 : TEXT ; Wide , Text : BOOLEAN ; Msg2 : TEXT := NIL ) 
    : BOOLEAN (* Neither at end-of-file nor end-of-line. *)
    (* PRE: Msg1 # NIL *)

    = VAR LLoc : TEXT 

    ; BEGIN
        IF GTopSsRef . SsWCh # WEOF 
        THEN 
          IF GTopSsRef . SsCh # LF 
          THEN RETURN TRUE 
          ELSE LLoc := " at end of line," 
          END (*IF*)  
        ELSE LLoc := " at end of file,"
        END (*IF*)  
      ; ErrorAtSs 
           ( ARRAY OF REFANY 
              { Msg1 
              , LitTypeName [ Wide , Text ] 
              , Msg2 
              , LLoc 
              , FM3Messages . NLIndent
              , "begins at ("
              , Fmt . Int ( Attribute . Position . Line )
              , ","
              , Fmt . Int ( Attribute . Position . Column )
              , ")"
              } 
           )  
      ; RETURN FALSE 
      END LineCharExists 

  ; CONST EscapeChars 
      = SET OF CHAR { 'n' , 't' , 'r' , 'f' , '\\' , '\'' , '\"' }
  ; CONST OctalDigits = SET OF [ '0' .. 'f' ] { '0' .. '7' } 
  ; CONST HexDigits = SET OF [ '0' .. 'f' ] { '0' .. '9' , 'a' .. 'f' , 'A' .. 'F' }
  ; CONST HexTagChars = SET OF CHAR { 'x' , 'X' } 
  ; CONST UnicodeTagChars = SET OF CHAR { 'u' , 'U' } 

  ; PROCEDURE EscapeSeq ( Wide , Text : BOOLEAN ) : WIDECHAR 
    (* PRE: GTopSsRef . SsCh is the backslash, not yet handled. *)
    (* POST: The sequence, possibly incorrectly short, has been
             scanned. GTopSsRef . SsWCh/SsCh are the next char to scan. *) 

    = VAR LCharPos : INTEGER 
    ; VAR LIntVal : INTEGER 
    ; VAR LMaxIntVal : INTEGER 
    ; VAR LDigitChars : SET OF [ '0' .. 'f' ] 
    ; VAR LBase : INTEGER  
    ; VAR LBaseText : TEXT 
    ; VAR LDigitCount : FM3Base . Card8Typ
    ; VAR LShift : FM3Base . Card8Typ
    ; VAR LChIntVal : FM3Base . Card8Typ
    ; VAR LPadDigitCt : FM3Base . Card8Typ 
    ; VAR LCh : CHAR 

    ; BEGIN 
        IF Wide THEN LMaxIntVal := 16_10FFFF ELSE LMaxIntVal := 16_FF END (*IF*)
      ; LCharPos := GTopSsRef . Position . Column  
      ; NextChar ( ) (* Consume backslash.*) 
      ; IF NOT LineCharExists
                 ( "Empty escape sequence in " , Wide , Text 
                 , Msg2 := " literal."
                 )
        THEN RETURN WNUL
        END (*IF*) 
      ; IF GTopSsRef . SsCh IN EscapeChars 
        THEN 
          LCh := GEscapeCharMap [ GTopSsRef . SsCh ] 
        ; NextChar ( ) (* Consume escaped source char. *) 
        ; RETURN LCh 
        END (*IF*)

      ; IF GTopSsRef . SsCh IN OctalDigits
        THEN 
          IF Wide THEN LDigitCount := 6 ELSE LDigitCount := 3 END (*IF*) 
        ; LDigitChars := OctalDigits 
        ; LShift := 3 (* times 8 *) 
        ; LBase := 8 
        ; LBaseText := "8_" 
        ELSIF GTopSsRef . SsCh IN HexTagChars
        THEN  
          NextChar ( ) (* Consume the hex tag 'x' or 'X' *) 
        ; IF NOT LineCharExists
                   ( "Incomplete hex escape sequence in " , Wide , Text 
                   , Msg2 := " literal"
                   )
          THEN RETURN WNUL
          END (*IF*)
        ; IF Wide THEN LDigitCount := 4 ELSE LDigitCount := 2 END (*IF*)
        ; LDigitChars := HexDigits 
        ; LShift := 4 (* times 16 *)  
        ; LBase := 16 
        ; LBaseText := "16_" 
        ELSIF GTopSsRef . SsCh IN UnicodeTagChars 
        THEN 
          IF NOT Wide 
          THEN  
            ErrorAtSs
              ( ARRAY OF REFANY 
                  { "Unicode escape in non-wide "
                  , LitTypeName [ Wide , (*Text:=*) FALSE ]
                  , " literal." 
                  } 
              )
          END (*IF*) 
        ; NextChar ( ) (* The Unicode escape tag 'u' or 'U'. *) 
        ; IF NOT LineCharExists
                   ( "Incomplete Unicode escape sequence in " , Wide , Text 
                   , Msg2 := " literal" 
                   )
          THEN RETURN WNUL 
          END (*IF*)
        ; LDigitCount := 6 
        ; LDigitChars := HexDigits 
        ; LShift := 4 (* times 16 *) 
        ; LBase := 16 
        ; LBaseText := "16_" 
        END (*IF*) 

      ; LIntVal := 0 
      ; LOOP (* Thru' LDigitCount chars IN LDigitChars. *)
          IF NOT GTopSsRef . SsCh IN LDigitChars
          THEN 
            ErrorAtSs
              ( ARRAY OF REFANY 
                  { "Short escape sequence in "
                  , LitTypeName [ Wide , (*Text:=*) FALSE ]
                  , " literal." 
                  } 
              )
          ; RETURN WNUL  
          ELSE 
          (* INVARIANT: GTopSsRef . SsCh is a digit of the escape sequence. *) 
            LChIntVal := GDigitCharMap [ GTopSsRef . SsCh ] 
          ; LIntVal := Word . LeftShift ( LIntVal , LShift ) + LChIntVal 
          ; NextChar ( ) 
          ; DEC ( LDigitCount )
          ; IF LDigitCount = 0 THEN EXIT END (*IF*) 
          ; IF NOT LineCharExists 
                     ( "Short escape sequence in " , Wide , Text 
                     , Msg2 := " literal"
                     )
            THEN RETURN WNUL  
            END (*IF*) 
          END (*IF*) 
        END (*LOOP*) 
      ; IF LIntVal > LMaxIntVal 
        THEN 
          IF Wide THEN LPadDigitCt := 6 ELSE LPadDigitCt := 2 END (*IF*)
        ; ErrorAtPos 
            ( ARRAY OF REFANY 
              { "Out-of-range escape value in "
              , LitTypeName [ Wide , (*Text:=*) FALSE ]
              , " literal: " 
              , LBaseText 
              , Fmt . Pad 
                  ( Fmt . Int ( LIntVal , base := LBase ) , LPadDigitCt , '0' )
              , ", max value is " 
              , LBaseText 
              , Fmt . Pad 
                  ( Fmt . Int ( LMaxIntVal , base := LBase ) 
                  , LPadDigitCt 
                  , '0' 
                  ) 
              } 
            , LCharPos 
            ) 
        ; LIntVal := 0 
        END (*IF*) 
      ; RETURN VAL ( LIntVal , WIDECHAR ) 
      END EscapeSeq 

  ; PROCEDURE CharLit ( Wide : BOOLEAN ) 
    (* PRE: GTopSsRef . SsCh is opening single quote, nothing done. *) 
    (* (NON-) POST: TrTok is NOT set. *) 

    = VAR LWCh : WIDECHAR 

    ; BEGIN (* CharLit *) 
        BegOfPlainTok ( )  
      ; NextChar ( ) (* Opening quote. *)
      
      ; IF NOT LineCharExists
                 ( "No character in " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN 
          LWCh := WNUL 
        ELSIF GTopSsRef . SsCh = '\\' 
        THEN LWCh := EscapeSeq ( Wide , Text := FALSE ) 
        ELSIF NOT Wide AND GTopSsRef . SsWCh > WLastOfChar 
        THEN 
(* TODO: put this check inside EscapeSeq, and display the value
         in octal, if it was so specified. *) 
          ErrorAtSs 
            ( ARRAY OF REFANY 
                { "Character literal value 16_" 
                , Fmt . Pad 
                    ( Fmt . Int ( ORD ( GTopSsRef . SsWCh ) , base := 16 ) 
                    , 6 
                    , '0' 
                    ) 
                , " is beyond the range of CHAR." 
                } 
            )
        ; LWCh := WNUL 
        ELSE
          LWCh := GTopSsRef . SsWCh 
        ; NextChar ( ) 
        END (*IF*) 
      ; Attribute . SaWCh := LWCh 
      ; Attribute . SaArgValue := VAL ( ORD ( Attribute . SaWCh ) , LONGINT )
      ; Attribute . SaHash := VAL ( ORD ( LWCh ) , LONGINT )   
      ; Attribute . SaAtom := ORD ( LWCh )  
      ; IF NOT LineCharExists
                 ( "No closing quote on " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN
        ELSIF GTopSsRef . SsCh = '\'' 
        THEN NextChar ( ) 
        ELSE 
          ErrorAtSs
            ( ARRAY OF REFANY 
                { "No closing quote on "
                , LitTypeName [ Wide , (*Text:=*) FALSE ]
                , " Literal." 
                } 
            )
        END (* IF *) 
      END CharLit 

  ; PROCEDURE TextLit ( ) 
    (* Only non-wide text lits. *) 
    (* PRE: GTopSsRef . SsCh is the opening double quote. 
            NO Tr fields have been initialized. *) 

    = VAR LBadCharCt : INTEGER 
    ; VAR LCharVal : CHAR 

    ; BEGIN (* TextLit *) 
        ScHash := FM3Utils . GroundHash ( ) 
      ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 200 } ) 
      ; ScWCharVarArr := NIL 
      ; NextChar ( ) (* Consume the opening double quote. *) 

      ; LOOP (* Thru' chars of text literal. *) 
          IF NOT LineCharExists
                   ( "Unclosed " , Wide := FALSE , Text := TRUE 
                   , Msg2 := " literal"
                   ) 
          THEN EXIT 
          ELSIF GTopSsRef . SsCh = '\"' (* Closing quote. *) 
          THEN 
            NextChar ( ) 
          ; EXIT 
          ELSIF GTopSsRef . SsCh = '\\' 
          THEN LCharVal := EscapeSeq ( Wide := FALSE , Text := TRUE ) 
          ELSIF GTopSsRef . SsWCh > WLastOfChar 
          THEN 
            LBadCharCt := 0
          ; REPEAT 
              INC ( LBadCharCt )               
            ; NextChar ( ) 
            UNTIL GTopSsRef . SsWCh = WEOF 
                  OR GTopSsRef . SsWCh <= WLastOfChar 
          ; ErrorAtSs 
              ( ARRAY OF REFANY 
                  { "Text literal has " 
                  , Fmt . Int ( LBadCharCt ) 
                  ,  " characters beyond the range of CHAR." 
                  } 
              , - LBadCharCt
              ) 
          ; LCharVal := NUL (* One NUL for a whole list of bad chars. *) 
          ELSE (* Ordinary character. *) 
            LCharVal := GTopSsRef . SsWCh 
          ; NextChar ( ) 
          END (*IF*) 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( LCharVal ) , FM3Utils . HashTyp ) 
            )
        ; AppendChar ( ScCharVarArr , LCharVal ) 
        END (*LOOP*) 

      ; Attribute . SaHash := ScHash 
      ; Attribute . SaChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; Attribute . SaAtom 
          := FM3Atom_OAChars . MakeAtom 
               ( GTopSsRef ^ . SsUnitRef ^ . UntCharsLitAtomDict 
               , Attribute . SaChars 
               , ScHash 
               ) 
      ; ScCharVarArr := NIL 
      ; Attribute . SaTok := FM3SrcToks . StkTextLit 
      END TextLit 

  ; PROCEDURE WideTextLit ( ) 
    (* PRE: 'w' or 'W' has been consumed. 
            GTopSsRef . SsCh is the opening double quote. 
            NO Tr fields have been initialized. *) 

    = VAR LWCharVal : WIDECHAR 

    ; BEGIN (* WideTextLit *) 
        ScHash := FM3Utils . GroundHash ( ) 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr 
          := VarArr_WChar . New ( WNUL , IntRangeTyp { 0 , 200 } ) 
      ; NextChar ( ) (* Consume the opening double quote. *) 

      ; LOOP (* Thru' chars of wide text literal. *) 
          IF NOT LineCharExists
                   ( "Unclosed " , Wide := TRUE , Text := TRUE 
                   , Msg2 := " literal"
                   ) 
          THEN EXIT 
          ELSIF GTopSsRef . SsCh = '\"' (* Closing quote. *) 
          THEN 
            NextChar ( ) 
          ; EXIT 
          ELSIF GTopSsRef . SsCh = '\\' 
          THEN LWCharVal := EscapeSeq ( Wide := TRUE , Text := TRUE ) 
          ELSE (* Ordinary character. *) 
            LWCharVal := GTopSsRef . SsWCh 
          ; NextChar ( ) 
          END (*IF*) 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( LWCharVal ) , FM3Utils . HashTyp ) 
            ) 
        ; AppendWChar ( ScWCharVarArr , LWCharVal ) 
        END (*LOOP*) 

      ; Attribute . SaHash := ScHash 
      ; Attribute . SaWideChars 
          := FM3Utils . WCharVarArrayToOAWChar ( ScWCharVarArr ) 
      ; Attribute . SaAtom 
          := FM3Atom_OAWideChars . MakeAtom 
               ( GTopSsRef ^ . SsUnitRef ^ . UntWCharsLitAtomDict 
               , Attribute . SaWideChars 
               , ScHash 
               ) 
      ; ScWCharVarArr := NIL 
      ; Attribute . SaTok := FM3SrcToks . StkWideTextLit 
      END WideTextLit 

  ; PROCEDURE CommentSuffix ( ) 
    (* PRE: Initial opening delimiter has been scanned and consumed. *)
    (* Treat delimiters of nested comments (and their other text too) 
       as just text of the outermost comment. 
    *)  

    = VAR LNestingDepth : INTEGER 

    ; BEGIN 
        LNestingDepth := 1 
      ; LOOP (* Thru chars in comment *) 
          IF GTopSsRef . SsWCh = WEOF  
          THEN 
            ErrorAtTok 
              ( ARRAY OF REFANY 
                  { "Unclosed comment at end-of-file," 
                  , FM3Messages . NLIndent 
                  , "begins at ("
                  , Fmt . Int ( Attribute . Position . Line )
                  , ","
                  , Fmt . Int ( Attribute . Position . Column )
                  , ")"
                  } 
              )  
          ; EXIT 
          ELSIF GTopSsRef . SsWCh = W'(' 
          THEN 
            NextChar ( ) 
          ; IF GTopSsRef . SsWCh = W'*'
            THEN 
              INC ( LNestingDepth ) 
            ; NextChar ( )
            END (*IF*) 
          ELSIF GTopSsRef . SsWCh = W'*' 
          THEN 
            NextChar ( ) 
          ; IF GTopSsRef . SsWCh = W')'
            THEN 
              DEC ( LNestingDepth ) 
            ; NextChar ( ) 
            ; IF LNestingDepth <= 0 THEN EXIT END (*IF*) 
            END (*IF*) 
          ELSE NextChar ( ) 
          END (*IF*) 
        END (*LOOP*) 
      END CommentSuffix 

  ; BEGIN (* GetToken *) 
      LOOP 
        ScAtBegOfPragma := GTopSsRef ^ . SsAtBegOfPragma (* From prev. token. *)
      ; INC ( GTopSsRef ^ . SsPragmaDepth , ORD ( ScAtBegOfPragma ) )   
      ; GTopSsRef ^ . SsAtBegOfPragma := FALSE (* For next time. *)  
      ; DEC ( GTopSsRef ^ . SsPragmaDepth 
            , ORD ( GTopSsRef ^ . SsAtEndOfPragma (* From prev. token. *) )  
            ) 
      ; GTopSsRef ^ . SsAtEndOfPragma := FALSE (* For next time. *)
      ; Attribute . SaArgValue := 0L 
      ; Attribute . SaWCh := WNUL  
      ; Attribute . SaAtom := FM3Base . AtomNull 
      ; LOOP (* Skip non-token chars. *) 
          IF GTopSsRef . SsWCh = WEOF 
          THEN 
            Attribute . SaTok := FM3SrcToks . StkEOF 
          ; RETURN FM3SrcToks . StkEOF 
          ELSIF GTopSsRef . SsWCh > WLastOfChar 
          THEN 
            IF GTopSsRef . SsWCh = WLS OR GTopSsRef . SsWCh = WPS 
               (* ^Unicode end-of-line code points, *)  
            THEN NextChar ( ) 
            ELSE LexErrorChars ( ) 
            END (*IF*) 
          ELSIF GTopSsRef . SsCh 
                IN SET OF CHAR { ' ' , CR , LF , FF , VT , TAB }  
          THEN NextChar ( ) 
          ELSIF GTopSsRef . SsCh IN M3Chars  
          THEN EXIT  
          ELSE LexErrorChars ( ) 
          END (*IF*) 
        END (*LOOP*) 

      ; Attribute . Position := GTopSsRef ^ . Position 

      ; CASE GTopSsRef . SsCh 
        OF 'w' , 'W'
        => ScCh := GTopSsRef . SsCh
        ; NextChar ( )
        ; IF GTopSsRef . SsCh = '\''
          THEN (* WIDECHAR literal. *) 
            CharLit ( Wide := TRUE ) 
          ; Attribute . SaTok := FM3SrcToks . StkWideCharLit  
          ELSIF GTopSsRef . SsCh = '\"'
          THEN (* Wide TEXT literal. *)
            WideTextLit ( )
          ELSE (* An identifier starting with w or W. *)
            Attribute . SaWCh := WNUL  
          ; ScHash := FM3Utils . GroundHash ( ) 
          ; ScCharVarArr 
              := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 160 } ) 
          ; ScWCharVarArr := NIL 
          ; StartIdent ( ) 
          ; ContribToFsm ( ScCh ) (* The 'w' or 'W'. *)  
          ; FM3Utils . ContribToHash 
              ( (*IN OUT*) ScHash 
              , VAL ( ORD ( ScCh ) , FM3Utils . HashTyp ) 
              )  
          ; VarArr_Char . Assign ( ScCharVarArr , 0 , ScCh ) 
          ; IdentSuffix ( ) 
          END (*IF*)

        | 'a' .. 'v' , 'x' .. 'z' , 'A' .. 'V' , 'X' .. 'Z' 
          (* Other identifier. *) 
        => Attribute . SaWCh := WNUL  
        ; ScHash := FM3Utils . GroundHash ( ) 
        ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 160 } ) 
        ; ScWCharVarArr := NIL 
        ; StartIdent ( ) 
        ; IdentSuffix ( )

        | '0' .. '9' 
        => Number ( ) 

        | '\'' (* CHAR literal. *)
        => CharLit ( Wide := FALSE )  
        ; Attribute . SaTok := FM3SrcToks . StkCharLit  

        | '"' (* TEXT literal. *)  
        => TextLit ( ) 

        | '(' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '*' (* Opening comment delimiter. *)  
          THEN 
            NextChar ( ) 
          ; CommentSuffix ( )
          ; RETURN GetToken ( )
  (* TOTO: ^Skip comments interatively instead of recursively. *) 
          ELSE 
            Attribute . SaTok := FM3SrcToks . StkOpenParen
          END (* IF *) 

        | '<' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; CASE GTopSsRef . SsCh 
          OF '=' 
          => NextChar ( ) 
          ; Attribute . SaTok := FM3SrcToks . StkLessEqual  
          | ':' 
          => NextChar ( ) 
          ; Attribute . SaTok := FM3SrcToks . StkSubtype  
          | '*' 
          => (* Opening pragma delimiter. *)
            NextChar ( )
          ; GTopSsRef ^ . SsAtBegOfPragma := TRUE 
          ; Attribute . SaTok := FM3SrcToks . StkOpenPragma
          ELSE 
            Attribute . SaTok := FM3SrcToks . StkLess
          END (* CASE *) 

        | '*' 
        => BegOfPlainTok ( )
        ; NextChar ( )
        ; IF GTopSsRef . SsCh = '>'
          THEN (* Closing pragma delimiter. *) 
            NextChar ( )
          ; GTopSsRef ^ . SsAtEndOfPragma := TRUE 
          ; Attribute . SaTok := FM3SrcToks . StkClosePragma
          ELSE Attribute . SaTok := FM3SrcToks . StkStar
          END (*IF*) 

        | ':' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '=' 
          THEN
            NextChar ( )
          ; Attribute . SaTok := FM3SrcToks . StkBecomes  
          ELSE Attribute . SaTok := FM3SrcToks . StkColon  
          END (* IF *) 

        | '.' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '.' 
          THEN
            NextChar ( )
          ; Attribute . SaTok := FM3SrcToks . StkEllipsis  
          ELSE Attribute . SaTok := FM3SrcToks . StkDot 
          END (* IF *) 

        | '=' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '>' 
          THEN
            NextChar ( )
          ; Attribute . SaTok := FM3SrcToks . StkArrow 
          ELSE Attribute . SaTok := FM3SrcToks . StkEqual  
          END (* IF *) 

        | '>' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '=' 
          THEN
            NextChar ( )
          ; Attribute . SaTok := FM3SrcToks . StkGreaterEqual 
          ELSE Attribute . SaTok := FM3SrcToks . StkGreater 
          END (* IF *) 

        | '+' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkPlus  

        | '-' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkMinus  

        | '^' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkDeref  

        | '#' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkUnequal  

        | ';' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkSemicolon  

        | '[' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkOpenBracket  

        | ']' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkCloseBracket  

        | '{' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkOpenBrace  

        | '}' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkCloseBrace  

        | ')' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkCloseParen  

        | ',' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkComma  

        | '&' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkAmpersand  

        | '|' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkStroke  

        | '/' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; Attribute . SaTok := FM3SrcToks . StkSlash  

     (* ELSE Can't happen.  (Other values ruled out earlier). *)  
        END (* CASE *)

      ; IF Attribute . SaTok = FM3SrcToks . StkClosePragma
           AND GTopSsRef ^ . SsPragmaDepth = 0
        THEN
           FM3Messages . ErrorArr
             ( ARRAY OF REFANY
                 { "No pragma is open here." }
             , Attribute . Position
             ) 
(* TODO: Emit error on unclosed pragmas at EOF *) 
        ; GTopSsRef ^ . SsAtEndOfPragma := FALSE
          (* So we won't go negative at the top of the loop. *)
        ELSIF GTopSsRef ^ . SsPragmaDepth = 0
        THEN RETURN Attribute . SaTok
        ELSIF GTopSsRef ^ . SsPragmaDepth = 1
              AND ( Attribute . SaTok = FM3SrcToks . StkClosePragma 
                    OR Attribute . SaTok = FM3SrcToks . StkPragmaId
                  )
        THEN RETURN Attribute . SaTok
        ELSE (* Don't deliver this token.  We are inside a top-level pragma,
                not including its StlOpenPragma, StkPragmaId, or StkClosePragma. 
                This is tricky.
             *) 
        END (*IF*) 
      END (*LOOP*) (* A very long one. *) 
    END GetToken

; PROCEDURE ErrorAttribute
  ( Token : CARDINAL ; VAR ResultAttribute : tScanAttribute )
  (* This is dependent only on the language, not code being compiled, so a 
     single instance is OK. *) 

  = BEGIN
      ResultAttribute := ScanAttributeDefault
    ; ResultAttribute . Position := Attribute . Position 
    ; ResultAttribute . SaTok := Token 
(* COMPLETEME: *) 
    END ErrorAttribute
    
; VAR GEscapeCharMap := ARRAY [ '\"' .. 't' ] OF CHAR { '\X00' , .. } 

; PROCEDURE InitEscapeCharMap ( )

  = BEGIN
(* Do we really want to recognize the capital letters? *) 
      GEscapeCharMap [ 'n' ] :=  '\n'  
    ; GEscapeCharMap [ 'N' ] :=  '\n'  
    ; GEscapeCharMap [ 't' ] :=  '\t' 
    ; GEscapeCharMap [ 'T' ] :=  '\t' 
    ; GEscapeCharMap [ 'r' ] :=  '\r' 
    ; GEscapeCharMap [ 'R' ] :=  '\r' 
    ; GEscapeCharMap [ 'f' ] :=  '\f' 
    ; GEscapeCharMap [ 'F' ] :=  '\f' 
    ; GEscapeCharMap [ '\\' ] :=  '\\' 
    ; GEscapeCharMap [ '\'' ] :=  '\'' 
    ; GEscapeCharMap [ '\"' ] :=  '\"'
    END InitEscapeCharMap 

; VAR GDigitCharMap := ARRAY CHAR OF [ 0 .. 15 ] { 0 , .. } 

; PROCEDURE InitDigitCharMap ( )

  = BEGIN
      GDigitCharMap [ '0' ] := 0
    ; GDigitCharMap [ '1' ] := 1 
    ; GDigitCharMap [ '2' ] := 2 
    ; GDigitCharMap [ '3' ] := 3 
    ; GDigitCharMap [ '4' ] := 4 
    ; GDigitCharMap [ '5' ] := 5 
    ; GDigitCharMap [ '6' ] := 6 
    ; GDigitCharMap [ '7' ] := 7 
    ; GDigitCharMap [ '8' ] := 8 
    ; GDigitCharMap [ '9' ] := 9 
    ; GDigitCharMap [ 'a' ] := 10 
    ; GDigitCharMap [ 'b' ] := 11 
    ; GDigitCharMap [ 'c' ] := 12 
    ; GDigitCharMap [ 'd' ] := 13 
    ; GDigitCharMap [ 'e' ] := 14 
    ; GDigitCharMap [ 'f' ] := 15 
    ; GDigitCharMap [ 'A' ] := 10 
    ; GDigitCharMap [ 'B' ] := 11 
    ; GDigitCharMap [ 'C' ] := 12 
    ; GDigitCharMap [ 'D' ] := 13 
    ; GDigitCharMap [ 'E' ] := 14 
    ; GDigitCharMap [ 'F' ] := 15 
    END InitDigitCharMap

; VAR GCurRwLexTable : FM3LexTable . T
; VAR GCurRwState : FM3LexTable . TransitionTyp
; VAR GCurRwValue : FM3LexTable . ValueTyp 

(* EXPORTED: *) 
; PROCEDURE Init ( )

  = BEGIN
      InitEscapeCharMap ( ) 
    ; InitDigitCharMap ( ) 
    ; GM3RwLexTable
        := FM3Files . ReadFsm ( "M3" , FM3SharedGlobals . FM3FileKindSrcPkl )
    ; GPgRwLexTable
        := FM3Files . ReadFsm ( "Pg" , FM3SharedGlobals . FM3FileKindSrcPkl ) 
    END Init 

; BEGIN (* FM3Scanner *)
  END FM3Scanner 
. 
