
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3canner 

; IMPORT CharVarArray AS CVarArray 
; IMPORT WidecharVarArray AS WCVarArray
; IMPORT TextIntTbl
; IMPORT UniRd 
; IMPORT UnsafeUniRd

; IMPORT FM3Base 

; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT LangUtil  
; IMPORT LbeStd 
; IMPORT M3InitTokStrings 
; IMPORT M3Tok 
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT Strings 

; TYPE AFT = MessageCodes . T 

(* New line and end-of-file characters: *) 
; CONST LF =  '\x0A' (* = '\n' *)
; CONST CR =  '\x0D' (* = '\r' *)
; CONST FF =  '\x0C' (* = '\f' *) 
; CONST VT =  '\x09' (* = '\t' *) 
; CONST NEL = '\x85'  
; CONST WLS =  W'\x2028'  
; CONST WPS =  W'\x2029'  
(* Also CR immediately followed by LF *)  
; CONST WEOF = W'\7000' (* Use a unicode app-specific value. *) 


; CONST WLastOfChar : WIDECHAR := LAST ( CHAR ) 

; CONST EOLCHARS = SET OF CHAR { LF , CR , FF , VT , NEL } 
; CONST M3Chars 
    = SET OF CHAR 
        { ' ' , '.' , ':' 
        , ';' , '*' , '/' , '<' , '>' , '=' , '#' , '|' , '^' , ',' , '&' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"'
        } 
      + EOLCHARS

; PROCEDURE InitTables ( ) 

  = BEGIN 
      RwTable := NEW ( TextIntTbl . Default ) . init ( sizeHint := 61 )
    ; M3InitTokStrings . InitRw ( RwTable )  
    ; PhTable := NEW ( TextIntTbl . Default ) . init ( sizeHint := 235 ) 
    ; M3InitTokStrings . InitPh ( PhTable )  
    END InitTables 

; TYPE ScanStateTyp 
       = RECORD 
           SsHash : FM3Utils . HashTyp 
         ; SsLink : ScanStateRefTyp := NIL 
         ; SsUniRd : UniRd . T := NIL 
         ; SsUnitNo : INTEGER 
         ; SsLineNo : INTEGER 
         ; SsCharPos : INTEGER 
         ; SsCharsAtomDict : FM3CharsAtom . T 
         ; SsWideCharsAtomDict : FM3WideCharsAtom . T 
         ; SsAtBegOfPragma := FALSE 
           (* ^The immediately preceding token was "<*". *)
         ; SsWCh : WIDECHAR 
         ; SsChars : CVarArray . T  
         ; SsWideChars : WCVarArray . T  
         END (* ScanStateTyp *) 

; TYPE ScanStateRefTyp = REF ScanStateTyp 

; VAR GTopSsRef : ScanStateRefTyp := NIL 
  (* SsUniRd of every node on this stack is kept locked until popped. *) 
; VAR GScanStateCt := 0
; VAR GScanStateDepth := 0
; VAR GMaxScanStateDepth := 0   

(* EXPORTED: *) 
; PROCEDURE PushState ( NewUniRd : UniRd . T ; UnitNo : INTEGER ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

  = VAR LSsRef : ScanStateRefTyp 

  ; BEGIN 
      Tread . Acquire ( NewUniRd ) 
    ; LSsRef := NEW ( ScanStateRefTyp ) 
    ; LSsRef ^ . SsLink := GTopSsRef 
    ; LSsRef ^ . SsUniRd := NewUniRd 
    ; LSsRef ^ . SsUnitNo := UnitNo 
    ; LSsRef ^ . SsLineNo := 0 
    ; LSsRef ^ . SsAtBegOfPragma := FALSE 
    ; GTopSsRef . SsLineNo := 0 
    ; LSsRef ^ . SsCharPos : 0   
    ; GTopSsRef := LSsRef 
    ; GTopSsRef . SsWCh := UnsafeUniRd ( LSsRef ^ . SsUniRd := NewUniRd ) 
    ; GTopSsRef . SsCh := MIN ( GTopSsRef . SsWCh , WLastOfChar )  
    ; INC ( GScanStateCt ) 
    ; INC ( GScanStateDepth ) 
    ; GMaxScanStateDepth := MAX ( GMaxScanStateDepth , GScanStateDepth ) 
    END PushState 

(* EXPORTED: *) 
; PROCEDURE PopState ( ) : UniRd . T (* Previous reader. *)  

  = VAR LSsRef : ScanStateRefTyp 

  ; BEGIN 
      IF GTopSsRef = NIL THEN RETURN NIL END (* IF *) 
    ; LSsRef := GTopSsRef 
    ; GTopSsRef := LSsRef . SsLink 
    ; UniRd . Close ( LSsRef . SsUniRd )  
    ; Thread . Release ( LSsRef . SsUniRd ) 
    ; DEC ( GScanStateDepth ) 
    ; RETURN LSsRef 
    END PopState 

(* EXPORTED: *) 
; PROCEDURE CurrentUnitNo ( ) : INTEGER 

  = BEGIN 
      IF GTopSsRef = NIL THEN RETURN - 1 END (* IF *) 
    ; RETURN GTopSsRef . SsUnitNo 
    END CurrentUnitNo 

(* EXPORTED: *) 
; PROCEDURE Scan ( ) 

  = VAR ScWCharVarArr : WidecharVarArray . T
  ; VAR ScCharVarArr : CharVarArray . T  
  ; VAR ScHash : FM3Utils . HashTyp 
  ; VAR ScAtBegOfPragma := FALSE 

  ; VAR Sif : ScannerIf . ScanIfTyp 
  ; VAR InString : Strings . StringTyp 
  ; VAR State : LbeStd . ScanStateTyp 
  ; VAR SaveState : LbeStd . ScanStateTyp 
  ; VAR InLength : Strings . StringSsTyp 
  ; VAR Pos : PortTypes . Int32Typ 
  ; VAR TokString : Strings . StringTyp 
  ; VAR BackCh : WIDECHAR (* Meaningful only when Pos = - 1 *) 

  ; PROCEDURE BegOfPlainTok ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfPlainTok *) 
        GCurTokRef ^ . TrTok := Tok 
      ; GCurTokRef ^ . TrHash := FM3Utils . HashNull 
      ; GCurTokRef ^ . TrAtom := FM3Base . AtomNull 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := NIL 
      END BegOfPlainTok 

  ; PROCEDURE BegOfTokWInfo ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfTokWInfo *) 
        GCurTokRef ^ . TrTok := Tok 
      ; GCurTokRef ^ . TrWCh := WNUL  
      ; GCurTokRef . TrHash := FM3Utils . HashGround ( ) 
      END BegOfTokWInfo 

  ; PROCEDURE NextChar ( ) 
    RAISES { Backout } 

    = VAR LWCh : WIDECHAR 
    ; VAR LWCh2 : CHAR 

    ; BEGIN (* NextChar *) 
        IF GTopSsRef . SsCh = CR 
        THEN 
          INC ( GTopSsRef . SsLineNo ) 
        ; GTopSsRef . SsCharPos := 0 
        END (* IF *) 
      ; TRY GTopSsRef . SsWCh := UnsafeUniRd . GetWideCh ( ) 
        EXCEPT 
        | Rd . EndOfFile , Rd . Failure 
        => GTopSsRef . SsWCh := WEOF 
        ;  GTopSsRef . SsCh := NUL' 
        ; RETURN 
        END (*EXCEPT*) 
      ; GTopSsRef . SsCh := MIN ( GTopSsRef . SsWCh , WLastOfChar )  
      ; INC ( GTopSsRef . SsCharPos )
      ; IF GTopSsRef . SsCh = CR 
        THEN 
          LWCh2 := UnsafeUniRd . FastGetWideCh ( LSsRef ^ . SsUniRd ) 
        ; IF LWCh2 = LF 
          THEN INC ( GTopSsRef . SsCharPos )
          (* Leave GTopSsRef.SsCh and .SsWCh = CR, canonical new line. *)
          ELSE UnsafeUniRd . FastUnGetWideCodePoint ( LSsRef ^ . SsUniRd ) 
          END (* IF *) 
        ELSIF GTopSsRef . SsCh IN  SET OF CHAR { LF , FF , VT , NEL } 
              OR GTopSsRef . SsWCh = WPS 
              OR GTopSsRef . SsWCh = WLS
        THEN  
          GTopSsRef . SsWCh := CR (* Canonical new line. *) 
        ; GTopSsRef . SsCh := CR 
        END (* IF *) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( Code : LbeStd . ErrCodeTyp ) 
    RAISES { Backout } 
    (* PRE: First bad char is in GTopSsRef . SsWCh. *)  

    = BEGIN (* LexErrorChars *) 
        BegOfTokWInfo ( ) 
      ; GCurTokRef ^ . TrAtom := FM3Base . AtomNull 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := WCVarArray . New ( NUL , IntRange . T { 0 , 120 } ) 
      ; NextChar ( ) 
      ; WHILE ( GTopSsRef . SsWCh > WLastOfChar 
                AND GTopSsRef . SsWCh # WLS 
                AND GTopSsRef . SsWCh # WPS 
              ) OR NOT GTopSsRef . SsCh IN M3Chars 
        DO NextChar ( ) END 
        END (*WHILE*) 
      ; GCurTokRef ^ . TrWideChars 
          := FM3Utils . WideCharsFromWCVarArr ( ScCharVarArr ) 
      ; GCurTokRef ^ . TrTok := FM3Toks . TkLexErrChars 
      END LexErrorChars 

  ; PROCEDURE IdentSuffix ( ) 
    RAISES { Backout } 
    (* PRE: The identifier is already started, possibly non-empty. *) 

    = VAR LSs : INTEGER 
    ; VAR LRwTok : FM3Base . TokTyp 
    ; VAR LIsRw : BOOLEAN 

    ; BEGIN (* Ident *) 
        WHILE GTopSsRef . SsCh 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO 
          ContributeToHash 
            ( ScHash , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
        ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; CVarArray . Assign 
            ( ScCharVarArr , LSs , VAL ( GTopSsRef . SsWCh , CHAR ) ) 
        ; NextChar ( ) 
        END (* WHILE *) 
      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef . TrChars := FM3Utils . CharsFromCVarArr ( ScCharVarArr ) 
      ; IF ScAtBegOfPragma 
           AND FM3CharsIntDict . Lookup
                 ( FM3Globals . PgRwDict 
                 , GCurTokRef . TrChars  
                 , ScHash 
                 , (*OUT*) LRwTok 
                 ) 
        THEN GCurTokRef ^ . TrTok := LRwTok 
        ELSIF FM3CharsIntDict . Lookup 
                ( FM3Globals . M3RwDict 
                , GCurTokRef . TrChars 
                , ScHash 
                , (*OUT*) LRwTok 
                ) 
        THEN GCurTokRef ^ . TrTok := LRwTok 
        ELSE 
          GCurTokRef ^ . TrAtom 
            := FM3CharsAtom . MakeAtom 
                 ( FM3Globals . IdentAtomDict 
                 , GCurTokRef ^ . TrChars 
                 , ScHash 
                 ) 
        ; GCurTokRef ^ . TrTok := FM3Tok . TkIdent
        END (* IF *) 
      END IdentSuffix 

  ; PROCEDURE Number ( ) 
    RAISES { Backout } 
    (* PRE: GTopSsRef . SsCh is a digit.  TextTok not started. *)  

    = VAR LSs : INTEGER 

    ; BEGIN (* Number *) 
        BegOfTokWInfo ( FM3Tok . TkNumber ) 
      ; GCurTokRef . TrAtom := FM3Base . AtomNull (* Overlaid later? *) 
      ; ScCharVarArr := CVarArray . New ( NUL , CharRange . T { 0 , 40 } ) 
      ; ScWCharVarArr := NIL 
      ; ScHash := FM3Utils. HashGround ( ) 
      ; WHILE GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
        DO ContribToHash 
             ( (*IN OUT*) ScHash 
             , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
             )
        ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        END (* WHILE *)
      ; CASE GTopSsRef . SsCh
        OF '_' 
        => ContribToHash 
             ( (*IN OUT*) ScHash 
             , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
             )
        ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh 
             IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
          THEN 
            WHILE GTopSsRef . SsCh 
            IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
            DO ContribToHash 
                 ( (*IN OUT*) ScHash 
                 , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
                 )
            ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
            ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
            ; NextChar ( ) 
            END (* WHILE *) 
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoHexDigit ) 
          END (* IF *) 

        | '.' 
        => NextChar ( ) 
        ; IF GTopSsRef . SsCh = '.' 
          THEN UnsafeUniRd . FastUnGetWideCodePoint ( LSsRef ^ . SsUniRd ) 
          ELSE
            IF GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            THEN 
              WHILE GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
              DO ContribToHash 
                   ( (*IN OUT*) ScHash 
                   , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
                   )
              ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
              ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
              ; NextChar ( ) 
              END (* WHILE *) 
            ELSE 
              ScannerIf . LexErr ( Sif , LbeStd . LeNoFractionalDigit ) 
            END (* IF *) 
          END (* IF *) 
        ELSE 
        END (*CASE*)  
      ; IF GTopSsRef . SsCh
           IN SET OF CHAR { 'E' , 'e' , 'D' , 'd' , 'X' , 'x' }  
        => ContribToHash 
             ( (*IN OUT*) ScHash 
             , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
             )
        ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh IN SET OF CHAR { '+' , '-' }  
          THEN 
             ContribToHash 
               ( (*IN OUT*) ScHash 
               , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
               )
          ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
          ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
          ; NextChar ( ) 
          END (* IF *) 
        ; IF GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
          THEN 
            WHILE GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            DO ContribToHash 
                 ( (*IN OUT*) ScHash 
                 , VAL ( GTopSsRef . SsCh , FM3Utils . HashTyp ) 
                 )
            ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
            ; CVarArray . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
            ; NextChar ( ) 
            END (*WHILE*)
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoExponentDigit ) 
          END (* IF *) 
        ELSE 
        END  
      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef ^ . TrChars := FM3Utils . CharsFromCVarArr ( ScCharVarArr ) 
      ; GCurTokRef ^ . TrAtom 
          := FM3CharsAtom . MakeAtom 
               ( GTopSsRef ^ . SsNumberAtomDict 
               , GCurTokRef ^ . TrChars 
               , ScHash 
               ) 
     (* ^Of doubtful use. *) 
      ; GCurTokRef ^ . TrTok := FM3Tok . Number 
      END Number 

  ; CONST LitTypeName
            = ARRAY BOOLEAN (*Wide*) , BOOLEAN (*Text*) OF TEXT
                { ARRAY BOOLEAN (*Text*) OF TEXT { "Wide TEXT" , "WIDECHAR" } 
                , ARRAY BOOLEAN (*Text*) OF TEXT { "TEXT" , "CHAR" }
                } 

; PROCEDURE LineCharExists 
      ( Msg1 : TEXT ; Wide , Text : BOOLEAN ; Msg2 : TEXT := NIL ) 
    : BOOLEAN (* Neither at end-of-file nor end-of-line. *)
    (* PRE: Msg1 # NIL *)

    = VAR LMsg : TEXT 
    = VAR LLoc : TEXT 
    ; VAR LKind : LitKindTyp 

    ; BEGIN
        IF GTopSsRef . SsWCh # WEOF 
        THEN 
          IF GTopSsRef . SsCh # CR 
          THEN RETURN TRUE 
          ELSE LLoc := " at end of line." 
        ELSE LLoc := " at end of file."
        END (*IF*)  
      ; LMsg := Msg & LitTypeName [ Wide , Text ] & Msg2 & LLoc 
      ; Error ( GFileName , GTopSsRef . SsLineNo , GTopSsRef . SsCharPos , LMsg )  
      ; RETURN FALSE 
      END LineCharExists 

  ; CONST EscapeChars = SET OF CHAR { '\t' , '\r' , '\f' , '\\' , '\'' , '\"' }
  ; CONST OctalDigits = SET OF CHAR { '0' .. '7' } 
  ; CONST HexDigits = SET OF CHAR { '0' .. '9' , 'a' .. 'f' , 'A' .. 'F' }
  ; CONST HexTagChars = SET OF CHAR { 'h' , 'H' } 

  ; PROCEDURE IncludeChar ( WCh : WIDECHAR ; Wide :: BOOLEAN ) 

    = VAR LSs : INTEGER 

    ; BEGIN 
        ContribToHash 
          ( (*IN OUT*) ScHash , VAL ( WCh , FM3Utils . HashTyp ) )
      ; IF Wide 
        THEN 
          LSs := WCVarArray . TouchedRange ( ScWCharVarArr ) . Hi + 1 
        ; WCVarArray . Assign ( ScWCharVarArr , LSs , GTopSsRef . SsWCh ) 
        ELSE 
          LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; CVarArray . Assign 
            ( ScCharVarArr , LSs , VAL ( GTopSsRef . SsWCh , CHAR ) ) 
        END (* IF *) 
      ; NextChar ( ) 
      END IncludeChar ) 

  ; PROCEDURE EscapeSeq ( Wide , Text : BOOLEAN ) : WIDECHAR 
      : WIDECHAR 
    (* PRE: GTopSsRef . SsCh is the backslash, not yet handled. *)
    (* POST: The sequence, possibly incorectly short, has been
             scanned. GTopSsRef . SsWCh/GTopSsRef . SsCh are the next char to scan. *) 

    = VAR LChars : SET OF CHAR
    ; VAR IntVal : FM3Base . UInt32 
    ; VAR LWCh : WIDECHAR 
    ; VAR LMaxIntVal : FM3Base . UInt32
    ; VAR LDigitCount : FM3Base . UInt8
    ; VAR LShift : FM3Base . UInt8
    ; VAR LChIntVal : FM3Base . UInt8
    ; VAR LPadDigitCt : FM3Base . UInt8

    ; BEGIN 
        NextChar ( ) (* Backslash.*)  
      ; IF NOT LineCharExists
                 ( "Empty escape sequence in " , Wide , Text 
                 , Msg2 := " literal"
                 )
        THEN RETURN WNUL
        END (*IF*) 
      ; IF GTopSsRef . SsCh IN SET OF CHAR { '\t' , '\r' , '\f' , '\\' , '\'' , '\"' } 
        THEN 
          LWCh := GEscapeCharMap [ GTopSsRef . SsCh ] 
        ; IncludeChar ( LWCh ) 
        ; RETURN LWCh 
        END (*IF*)

      ; IF GTopSsRef . SsCh IN OctalDigits
        THEN 
          IF Wide THEN LDigitCount := 6 ELSE LDigitCount := 3 END (*IF*) 
        ; LChars := OctalDigits 
        ; LShift := 3 
        ELSIF GTopSsRef . SsCh IN HexTagChars
        THEN  
          NextChar ( ) (* The Hex escape tag 'w' or 'W' *) 
        ; IF NOT LineCharExists
                   ( "Incomplete hex escape sequence in " , Wide , Text 
                   , Msg2 := " literal"
                   )
          THEN RETURN WNUL
          END (*IF*)
        ; IF Wide THEN LDigitCount := 4 ELSE LDigitCount := 2 END (*IF*)
        ; LChars := HexDigits 
        ; LShift := 4   
        ELSIF GTopSsRef . SsCh = "U" (*The Unicode escape tag 'U'. *) 
        THEN 
          IF NOT Wide 
          THEN  
            Error
              ( GFileName , GTopSsRef . SsLineNo , GTopSsRef . SsCharPos 
              , "Unicode escape in non-wide "
                & LitTypeName [ Wide , Text := FALSE ]
                & " literal."
              )
          END ((*IF*) 
        ; NextChar ( ) (* The Unicode escape tag 'U' *) 
        ; IF NOT LineCharExists
                   ( "Incomplete Unicode escape sequence in " , Wide , Text 
                   , Msg2 := " literal" 
                   )
          THEN RETURN WNUL 
          END (*IF*)
        ; LDigitCount := 6 
        ; LChars := HexDigits 
        ; LShift := 4
        END (*IF*) 

      ; LIntVal := 0 
      ; LOOP (* Thru' LDigitCount chars in LChars. *)
        (* INVARIANT: GTopSsRef . SsCh is a digit of the escape sequence. ) 
          LChIntVal := GDigitCharMap [ GTopSsRef . SsCh ] 
        ; LIntVal := Word . LeftShift ( LIntVal , LShift ) + LChIntVal 
        ; NextChar ( ) 
        ; DEC ( LDigitCount )
        ; IF LDigitCount = 0 
          THEN EXIT  
        ; ELSIF NOT LineCharExists 
                      ( "Short escape sequence" , Wide , Text 
                      , Msg2 := " literal"
                      )
          THEN RETURN WNUL  
        ; ELSIF IF NOT GTopSsRef . SsCh IN LChars
          THEN 
            Error
              ( GFileName , GTopSsRef . SsLineNo , GTopSsRef . SsCharPos 
              , "Short escape sequence in "
                & LitTypeName [ Wide , Text := FALSE ]
                & " literal."
              )
          ; EXIT 
       (* ELSE Loop *) 
          END (*IF*) 
        END (*LOOP*) 
      ; IF Wide 
        THEN 
          LMaxIntVal = 16_10FFFF 
        ; LPadDigitCt := 6 
        ELSE 
          LMaxIntVal := 16_FF 
        ; LPadDigitCt := 2 
        END (*IF*)
      ; IF LIntVal > LMaxIntVal 
        THEN 
          Error
            ( GFileName , GTopSsRef . SsLineNo , GTopSsRef . SsCharPos 
            , "Out range value in "
            & LitTypeName [ Wide , Text := FALSE ]
            & " literal: 16_"
            & Fmt . Pad 
                ( Fmt . Int ( LIntVal , Base = 16 ) , LPadDigitCt , '0' )
            & ", max value is 16_"
            & Fmt . Pad 
                ( Fmt . Int ( LMaxIntVal , Base = 16 ) , LPadDigitCt , '0' )
            )
        END (*IF*) 
      ; RETURN VAL ( LIntVal , WIDECHAR ) 
      END EscapeSeq 

  ; PROCEDURE CharLit ( Wide : BOOLEAN ) 
    RAISES { Backout }
    (* PRE: GTopSsRef . SsCh is opening single quote, nothing done. *) 
    (* POST: TrTok is NOT set. *) 

    = VAR LMsg : TEXT 
    ; VAR LWCh : WIDECHAR 

    ; BEGIN (* CharLit *) 
        BegOfPlainTok ( 0  
        NextChar ( ) (* Opening quote. *)
      
      ; IF NOT LineCharExists
                 ( "No character in " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN 
        ELSIF GTopSsRef . SsCh = '\\' 
        => LWCh := EscapeSeq ( Wide , Text := FALSE ) 
        ELSE
          LWCh := ORD ( GTopSsRef . SsWCh ) 
        ; NextChar ( ) 
        END (*IF*) 
      ; GCurTokRef ^ . TrWCharVal := LWCh 
      ; GCurTokRef ^ . TrHash := LWCh 
      ; GCurTokRef ^ . TrAtom := LWCh 
      ; IF NOT LineCharExists
                 ( "No closing quote on " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN
        ELSIF GTopSsRef . SsCh = '\'' 
        THEN NextChar ( ) 
        ELSE 
          Error
            ( GFileName , GTopSsRef . SsLineNo , GTopSsRef . SsCharPos
            , "No closing quote on "
              & LitTypeName [ Wide , Text := FALSE ]
              & " Literal"
            )
        END (* IF *) 
      END CharLit 

  ; PROCEDURE WideTextLit ( ) 
    RAISES { Backout } 
    (* PRE: 'w' or 'W' has been consumed. 
            GTopSsRef . SsCh is the opening double quote. 
            NO Tr fields have been initialized. *) 

    = VAR LNextCharSs : INTEGER 
    ; VAR LCharVal WIDECHAR 
    ; VAR LLoc : TEXT 
    ; VAR LMsg : TEXT 

    ; BEGIN (* WideTextLit *) 
        BegOfTokWInfo ( )
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := WCVarArray . New ( WNULL , IntRange . T { 0 , 100 } ) 
      ; LNextCharSs := 0  
      ; NextChar ( ) (* Consume the opening double quote. *) 

      ; LOOP (* Thru' chars of text literal. *) 
          IF NOT LineCharExists
                   ( "Unclosed " , Wide := TRUE , Text := TRUE 
                   , Msg2 := " literal"
                   ) 
          THEN EXIT 
          ELSIF GTopSsRef . SsCh = '\"' 
          THEN 
            NextChar ( ) 
          ; EXIT 
          ELSIF GTopSsRef . SsCh = '\\' 
          THEN 
            LCharVal := EscapeSeq ( Wide := TRUE , Text := TRUE ) 
          ELSE (* Ordinary character. *) 
            ContribToHash 
              ( (*IN OUT*) ScHash , VAL ( LCharVal , FM3Utils . HashTyp ) )
          ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
          ; WCVarArray . Assign ( ScWCharVarArr , LSs , GTopSsRef . SsWCh , WIDECHAR ) 
          ; NextChar ( ) 
          END (*IF*) 
        END (*LOOP*) 

      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef ^ . TrWideChars 
          := FM3Utils . WideCharsToWCArray ( ScWCharVarArr ) 
      ; GCurTokRef ^ . TrAtom 
          := FM3WCharsAtom . MakeAtom 
               ( GTopSsRef ^ . SsWideCharsAtomDict 
               , GCurTokRef ^ . TrWideChars 
               , ScHash 
               ) 
      ; GCurTokRef ^ . Tok := FM3Tok . TkWideTextLit 
      END WideTextLit 

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { Backout } 
    (* PRE: Initial "(*" has been scanned and consumed. * ) 

    = VAR LNestingDepth : INTEGER 

    ; BEGIN 
        LNestingDepthj := 1 
      ; LOOP (* Thru chars in comment *) 
          IF GTopSsRef . SsWCh = WEOF  
          THEN 
            Error 
              ( GFileName , GTopSsRef . SsLineNo , GTopSsRef . SsCharPos 
              , "Comment unclosed at end-of-file"
              )  
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

  ; BEGIN (* Scan *) 
      ScAtBegOfPragma := GCurSsRef ^ . SsAtBegOfPragma 
    ; GCurSsRef ^ . SsAtBegOfPragma := FALSE 

    ; TRY 
        LOOP (* Skip uninteresting chars. *) 
          IF GTopSsRef . SsWCh = WEOF 
          THEN 
            GCurTokRef ^ . Tok := FLbeStd . TkEOF 
          ; RETURN 
          ELSIF GTopSsRef . SsWCh > WLastOfCh 
          THEN 
            IF GTopSsRef . SsWCh = WLS OR GTopSsRef . SsWCh = WPS (* Unicode end-of-line code points, *)  
            THEN NextChar ( ) 
            ELSE 
              LexErrorChars ( LbeStd . LeBadChars ) 
            ; EXIT 
            END (*IF*) 
          ELSIF GTopSsRef . SsCh IN SET OF CHAR { ' ' , CR , LF , FF , VT }  
          THEN NextChar ( ) 
          ELSE EXIT  
          END (*IF*) 
        END (*LOOP*) 
          
      ; CASE GTopSsRef . SsCh 
        OF 'w' , 'W'
        => LCh := GTopSsRef . SsCh
        ; NextChar ( )
        ; IF GTopSsRef . SsCh = '\''
          THEN (* WIDECHAR literal. *) 
            CharLit ( Wide := TRUE ) 
          ; GCurTokRef ^ . Tok := FM3Tok . WidecharLit  
          ELSIF GTopSsRef . SsCh = '\"'
          THEN (* Wide TEXT literal. *)
            WideTextLit ( LCh )
          ELSE (* An identifier starting with w or W. *)
            BegOfTokWInfo ( )
          ; ScCharVarArr := CVarArray . New ( NUL , CharRange . T { 0 , 160 } ) 
          ; ScWCharVarArr := NIL 
          ; ScHash := FM3Utils. HashGround ( ) 
          ; ContribToHash 
              ( (*IN OUT*) ScHash , VAL ( LTok , FM3Utils . HashTyp ) )
          ; LSs := CVarArray . TouchedRange ( ScCharVarArr ) . Hi + 1 
          ; CVarArray . Assign ( ScCharVarArr , LSs , LCh ) 
          ; NextChar ( ) 
          ; IdentSuffix ( ) 

        | 'a' .. 'v' , 'x .. 'z' , 'A' .. 'V' , 'X' .. 'Z' , '_'    
          (* Other identifier. *) 
        => BegOfTokWInfo ( )
        ; ScCharVarArr := CVarArray . New ( NUL , CharRange . T { 0 , 80 } ) 
        ; ScWCharVarArr := NIL 
        ; ScHash := FM3Utils. HashGround ( ) 
        ; IdentSuffix ( )

        | '0' .. '9' 
        => Number ( ) 

        | '\'' (* CHAR literal. *)
        => CharLit ( Wide := FALSE )  
        ; GCurTokRef ^ . Tok := FM3Tok . CharLit  

        | '"' (* TEXT literal. *)  
        => BegOfTokWInfo ( )
        ; ScCharVarArr := CVarArray . New ( NUL , CharRange . T { 0 , 160 } ) 
        ; ScWCharVarArr := NIL
        ; TextLit ( Wide := FALSE ) 

        | '(' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '*' (* Ground-level opening comment delimiter. *)  
          THEN 
            NextChar ( ) 
          ; CommentSuffix ( ) 
          ELSE 
            GCurTokRef ^ . Tok := FM3Tok . TkOpenParen
          END (* IF *) 

        | '<' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; CASE GTopSsRef . SsWCh 
          OF W'=' 
          => NextChar ( ) 
          ; GCurTokRef ^ . Tok := FM3Tok . TkLessEqual  
          | W':' 
          => NextChar ( ) 
          ; GCurTokRef ^ . Tok := FM3Tok . TkSubtype  
          | W'*' 
          => (* Opening pragma delimiter. *)
            NextChar ( )
          ; GTopSsRef ^ . SsAtBegOfPragma := TRUE 
          ; GCurTokRef ^ . Tok := FM3Tok . TkTkPragmaOpen  
          ELSE 
            GCurTokRef ^ . Tok := FM3Tok . TkTkLessThan 
          END (* CASE *) 

        | '*' 
        => BegOfPlainTok ( )
        ; NextChar ( )
        ; IF GTopSsRef . SsWCh = W'>'
          THEN (* Closing pragma delimiter. *) 
            NextChar ( )
          ; GCurTokRef ^ . Tok := FM3Tok . TkPragmaClose 
          ELSE GCurTokRef ^ . Tok := FM3Tok . TkStar
          END (*IF*) 

        | ':' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '=' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . Tok := FM3Tok . TkBecomes  
          ELSE GCurTokRef ^ . Tok := FM3Tok . TkColon  
          END (* IF *) 

        | '.' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '.' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . Tok := FM3Tok . TkEllipsis  
          ELSE GCurTokRef ^ . Tok := FM3Tok . TkDot 
          END (* IF *) 

        | '=' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '>' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . Tok := FM3Tok . TkArrow 
          ELSE GCurTokRef ^ . Tok := FM3Tok . TkEqual  
          END (* IF *) 

        | '>' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '=' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . Tok := FM3Tok . TkGreaterEqual 
          ELSE GCurTokRef ^ . Tok := FM3Tok . TkGreater 
          END (* IF *) 

        | '+' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkPlus  

        | '-' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkMinus  

        | '^' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkDeref  

        | '#' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkUnequal  

        | ';' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkSemicolon  

        | '[' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkOpenBracket  

        | ']' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkCloseBracket  

        | '{' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkOpenBrace  

        | '}' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkCloseBrace  

        | ')' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkCloseParen  

        | ',' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkComma  

        | '&' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkAmpersand  

        | '|' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkStroke  

        | '/' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . Tok := FM3Tok . TkSlash  

        ELSE (* Other values in CHAR *) 
          LexErrorChars ( LbeStd . LeBadChars ) 
        END (* CASE *) 

      EXCEPT Backout 
      => <* FATAL Backout *> 
        BEGIN 
          BegOfPlainTok ( ) 
        ; GCurTokRef ^ . Tok := FLbeStd . Tok__Unknown  
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END Scan 

; VAR GEscapeCharMap := ARRAY CHAR OF CHAR { '\H00' , .. } 

; PROCEDURE InitEscapeCharMap ( )

  = BEGIN
      GEscapeCharMap [ 'n' ] :=  '\n'  
    ; GEscapeCharMap [ 't' ] :=  '\t' 
    ; GEscapeCharMap [ 'r' ] :=  '\r' 
    ; GEscapeCharMap [ 'f' ] :=  '\f' 
    ; GEscapeCharMap [ '\\' ] :=  '\\' 
    ; GEscapeCharMap [ '\'' ] :=  '\'' 
    ; GEscapeCharMap [ '\"' ] :=  '\"'
    END InitEscapeCharMap 

; VAR GDigitCharMap := ARRAY CHAR OF [ 0 .. 15 ] { 0 , .. } 

; PROCEDURE InitDigitMap ( )

  = BEGIN
      GDigitCharMap [ '0' ] := 0
    ; GDigitCharMap [ '1' ] := 1 
    ; GDigitCharMap [ '2' ] := 2 
    ; GDigitCharMap [ '3' ] := 3 
    ; GDigitCharMap [ '4' ] := 4 
    ; GDigitCharMap [ '5' ] := 5 
    ; GDigitCharMap [ '6' ] := 6 
    ; GDigitCharMap [ '7' ] := 7 
    ; GDigitCharMap [ '8' ] := 7 
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

; BEGIN (* FM3Scanner *) 
    InitEscapeCharMap ( ) 
  ; InitDigitCharMap ( ) 
  END FM3Scanner 
. 


