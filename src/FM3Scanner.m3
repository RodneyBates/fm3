
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3canner 

; IMPORT TextIntTbl
; IMPORT TextWr 
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

; VAR RwTable : TextIntTbl . Default 
; VAR PhTable : TextIntTbl . Default 

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
         ; SsTokStringWr : Wr . T := NIL 
         ; SsPragma1stTok := FALSE          
         ; SsWCh : WIDECHAR 
         END (* ScanStateTyp *) 

; TYPE ScanStateRefTyp = REF ScanStateTyp 

; VAR GTopScanStateRef : ScanStateRefTyp := NIL 
  (* SsUniRd of every node on this stack is kept locked until popped. *) 
; VAR GScanStateCt := 0
; VAR GScanStateDepth := 0
; VAR GMaxScanStateDepth := 0   

(* These are cached copies of fields of GTopScanStateRef, for a bit 
   faster access. (Or is it?) *) 
; VAR GHash : FM3Utils . HashTyp 
; VAR GUniRd : UniRd . T 
; VAR GLineNo : INTEGER 
; VAR GCharPos : INTEGER 
; VAR GTokStringWr := Wr . T 
; VAR GWCh : WIDECHAR 
; VAR GCh : CHAR 

(* EXPORTED: *) 
; PROCEDURE PushState ( NewUniRd : UniRd . T ; UnitNo : INTEGER ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

  = VAR LScanStateRef : ScanStateRefTyp 

  ; BEGIN 
      Tread . Acquire ( NewUniRd ) 
    ; GTopScanStateRef . SsLineNo := GLineNo 
    ; GTopScanStateRef . SsCharPos := GCharPos  
    ; GTopScanStateRef . SsTokStringWr := GTokStringWr  
    ; GTopScanStateRef . SsWCh := GWCh 
    ; GTopScanStateRef . SsCh := GCh 
    ; GTopScanStateRef . SsHash := GHash  
    ; LScanStateRef := NEW ( ScanStateRefTyp ) 
    ; LScanStateRef ^ . SsLink := GTopScanStateRef 
    ; LScanStateRef ^ . SsUniRd := NewUniRd 
    ; LScanStateRef ^ . SsUnitNo := UnitNo 
    ; LScanStateRef ^ . SsLineNo := 0 
    ; LScanStateRef ^ . SsPragma1stTok := FALSE 
    ; LScanStateRef ^ . SsTokStringWr := NIL  
    ; GLineNo := 0 
    ; LScanStateRef ^ . SsCharPos : 0   
    ; GLineNo := 0 
    ; GTopScanStateRef := LScanStateRef 
    ; GUniRd := NewUniRd 
    ; GWCh := UnsafeUniRd ( GUniRd ) 
    ; GTopScanStateRef . GWCh := GWCh 
    ; GCh := MIN ( GWCh , WLastOfChar )  
    ; INC ( GScanStateCt ) 
    ; INC ( GScanStateDepth ) 
    ; GMaxScanStateDepth := MAX ( GMaxScanStateDepth , GScanStateDepth ) 
    END PushState 

(* EXPORTED: *) 
; PROCEDURE PopState ( ) : UniRd . T (* Previous reader. *)  

  = VAR LScanStateRef : ScanStateRefTyp 

  ; BEGIN 
      IF GTopScanStateRef = NIL THEN RETURN NIL END (* IF *) 
    ; LScanStateRef := GTopScanStateRef 
    ; GTopScanStateRef := LScanStateRef . SsLink 
    ; GLineNo := GTopScanStateRef . SsLineNo 
    ; GCharPos := GTopScanStateRef . SsCharPos 
    ; GTokStringWr := GTopScanStateRef . SsTokStringWr 
    ; GHash := GTopScanStateRef . SsHash 
    ; GWCh := GTopScanStateRef . SsWCh 
    ; GCh := GTopScanStateRef . SsCh 
    ; UniRd . Close ( LScanStateRef . SsUniRd )  
    ; Thread . Release ( LScanStateRef . SsUniRd ) 
    ; DEC ( GScanStateDepth ) 
    ; RETURN LScanStateRef 
    END PopState 

(* EXPORTED: *) 
; PROCEDURE CurrentUnitNo ( ) : INTEGER 

  = BEGIN 
      IF GTopScanStateRef = NIL THEN RETURN - 1 END (* IF *) 
    ; RETURN GTopScanStateRef . SsUnitNo 
    END CurrentUnitNo 

(* EXPORTED: *) 
; PROCEDURE Scan ( ) 

  = VAR Sif : ScannerIf . ScanIfTyp 
  ; VAR InString : Strings . StringTyp 
  ; VAR State : LbeStd . ScanStateTyp 
  ; VAR SaveState : LbeStd . ScanStateTyp 
  ; VAR InLength : Strings . StringSsTyp 
  ; VAR Pos : PortTypes . Int32Typ 
  ; VAR TokString : Strings . StringTyp 
  ; VAR Ch : WIDECHAR 
  ; VAR BackCh : WIDECHAR (* Meaningful only when Pos = - 1 *) 
  ; VAR LPragma1stTok := FALSE 

  ; PROCEDURE BegOfPlainTok ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfPlainTok *) 
        GCurrentTok . TrTok := Tok 
      ; GCurrentTok . TrLineNo := GLineNo 
      ; GCurrentTok . TrCharPos := GCharPos 
      ; GTokStringWr := NIL 
      ; GCurrentTok . TrText := NIL 
      ; GCurrentTok . TrHash := FM3Utils . HashNull 
      ; GCurrentTok . TrAtom := FM3Base . AtomNull 
      END BegOfPlainTok 

  ; PROCEDURE BegOfTextTok 
      ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfTextTok *) 
        GCurrentTok . TrTok := Tok 
      ; GCurrentTok . TrLineNo := GLineNo 
      ; GCurrentTok . TrCharPos := GCharPos 
      ; GTokStringWr := TextWr . T 
      ; GHash := FM3Utils . HashGround ( ) 
      END BegOfTextTok 

  ; PROCEDURE NextChar ( ) 
    RAISES { Backout } 

    = VAR LWCh : WIDECHAR 
    ; VAR LWCh2 : CHAR 

    ; BEGIN (* NextChar *) 
        IF GCh = CR 
        THEN 
          INC ( GLineNo ) 
        ; GCharPos := 0 
        END (* IF *) 
      ; TRY GWCh := UnsafeUniRd . GetWideCh ( ) 
        EXCEPT 
        | Rd . EndOfFile , Rd . Failure 
        => GWCh := WEOF 
        ;  GCh := '\H00' 
        ; RETURN 
        END (*EXCEPT*) 
      ; GCh := MIN ( GWCh , WLastOfChar )  
      ; INC ( GCharPos )
      ; IF GCh = CR 
        THEN 
          LWCh2 := UnsafeUniRd . FastGetWideCh ( GUniRd ) 
        ; IF LWCh2 = LF 
          THEN INC ( GCharPos )
          (* Leave GCh and GWCh = CR.  It'a cononical new line. *)
          ELSE UnsafeUniRd . FastUnGetWideCodePoint ( GUniRd ) 
          END (* IF *) 
        ELSIF GCh IN  SET OF CHAR { LF , FF , VT , NEL } 
              OR GWCh = WPS 
              OR GWCh = WLS
        THEN  
          GWCh := CR (* Canonical new line. *) 
        ; GCh := CR 
        END (* IF *) 
      ; IF GTokStringWr # NIL 
        THEN 
          Wr . PutWideChar ( GTokStringWr , GWCh ) 
        END (* IF *) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( Code : LbeStd . ErrCodeTyp ) 
    RAISES { Backout } 
    (* PRE: First bad char is in GWCh. *)  

    = BEGIN (* LexErrorChars *) 
        BegOfTextTok ( ) 
      ; NextChar ( ) 
      ; WHILE ( GWCh > WLastOfChar AND GWCh # WLS AND GWCh # WPS ) 
              OR NOT GCh IN M3Chars 
        DO NextChar ( ) END 
        END (*WHILE*) 
      ; GCurrentTok . TrTok := FM3Toks . TkLexErrChars 
      ; GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash (* Any use for this? *) 
      ; GCurrentTok . TrAtom := FM3Dict . AtomNull 
      END LexErrorChars 

  ; PROCEDURE IdentSuffix ( ) 
    RAISES { Backout } 
    (* PRE: The identifier is already started, possibly non-empty. *) 

    = VAR LRwTok : FM3Base . TokTyp 
    ; VAR LIsRw : BOOLEAN 

    ; BEGIN (* Ident *) 
        WHILE GCh 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO 
          GHash := ContributeToHash ( GHash , VAL ( GCh , FM3Utils . HashTyp ) 
        ; Wr . PutChar ( GTokStringWr , GCh ) 
        ; NextChar ( ) 
        END (* WHILE *) 
      ; GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash 
      ; IF GCurrentStat . Ss1stPragma 
           AND FM3TextDict . Lookup 
                 ( FM3Globals . PragmaDict , GText , GHash , (*OUT*) LRwTok ) 
        THEN GCurrentTok . TrTok := LRwTok 
        ELSIF FM3TextDict . Lookup 
                ( FM3Globals . RwDict , GText , GHash , (*OUT*) LRwTok ) 
        THEN GCurrentTok . TrTok := LRwTok 
        ELSE 
          GCurrentTok . TrAtom 
            := FM3TextDict . MakeAtom 
                 ( IdentDict , GCurrentTok . TrText , Ghash ) 
        ; GCurrentTok . TrTok := FM3Tok . TkIdent
        END (* IF *) 
      END IdentSuffix 

  ; PROCEDURE Number ( ) 
    RAISES { Backout } 

    = BEGIN (* Number *) 
        BegOfTextTok ( FM3Tok . TkNumber ) 
      ; WHILE GCh IN SET OF CHAR { '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *)
      ; CASE GCh
        OF '_' 
        => NextChar ( ) 
        ; IF GCh IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
          THEN 
            WHILE GCh IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
            DO NextChar ( ) 
            END (* WHILE *) 
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoHexDigit ) 
          END (* IF *) 

        | '.' 
        => NextChar ( ) 
        ; IF GCh = '.' 
          THEN UnsafeUniRd . FastUnGetWideCodePoint ( GUniRd ) 
          ELSE
            IF GCh IN SET OF CHAR { '0' .. '9' } 
            THEN 
              WHILE GCh IN SET OF CHAR { '0' .. '9' } 
              DO NextChar ( ) 
              END (* WHILE *) 
            ELSE 
              ScannerIf . LexErr ( Sif , LbeStd . LeNoFractionalDigit ) 
            END (* IF *) 
          END (* IF *) 
        ELSE 
        END (*CASE*)  
      ; IF GCh
           IN SET OF CHAR { 'E' , 'e' , 'D' , 'd' , 'X' , 'x' }  
        => NextChar ( ) 
        ; IF GCh IN SET OF CHAR { '+' , '-' }  
          THEN  NextChar ( ) 
          END (* IF *) 
        ; IF GCh IN SET OF CHAR { '0' .. '9' } 
          THEN 
            WHILE GCh IN SET OF CHAR { '0' .. '9' } 
            DO NextChar ( ) 
            END (*WHILE*)
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoExponentDigit ) 
          END (* IF *) 
        ELSE 
        END  
      ; GCurrentTok . TrTok := FM3Tok . Number 
      END Number 

  ; CONST LitTypeName
            = ARRAY BOOLEAN (*Wide*) , BOOLEAN (*Text*) OF TEXT
                { ARRAY BOOLEAN (*Text*) OF TEXT { "Wide TEXT" , "WIDECHAR" } 
                , ARRAY BOOLEAN (*Text*) OF TEXT { "TEXT" , "CHAR" }
                } 

; PROCEDURE LitCharExists 
      ( Msg1 : TEXT ; Wide , Text : BOOLEAN ; Msg2 : TEXT := NIL ) 
    : BOOLEAN (* Neither at end-of-file nor end-of-line. *)
    (* PRE: Msg1 # NIL *)

    = VAR LMsg : TEXT 
    = VAR LLoc : TEXT 
    ; VAR LKind : LitKindTyp 

    ; BEGIN
        IF GWCh # WEOF 
        THEN 
          IF GCh # CR 
          THEN RETURN TRUE 
          ELSE LLoc := " at end of line." 
        ELSE LLoc := " at end of file."
        END (*IF*)  
      ; LMsg := Msg & LitTypeName [ Wide , Text ] & Msg2 & LLoc 
      ; Error ( GFileName , GLineNo , GCharPos , LMsg )  
      ; RETURN FALSE 
      END LitCharExists 

  ; CONST EscapeChars = SET OF CHAR { '\t' , '\r' , '\f' , '\\' , '\'' , '\"' }
  ; CONST OctalDigits = SET OF CHAR { '0' .. '7' } 
  ; CONST HexDigits = SET OF CHAR { '0' .. '9' , 'a' .. 'f' , 'A' .. 'F' }
  ; CONST HexTagChars = SET OF CHAR { 'h' , 'H' }

  ; PROCEDURE EscapeSeq ( Wide , Text : BOOLEAN ) : BOOLEAN (* Success. *) 
      : WIDECHAR 
    (* PRE: GCh is the backslash, not yet handled. *)
    (* POST: The sequence, possibly incorectly short, has been
             handled. GWCh/GCh are next. *) 

    = VAR LCount : INTEGER
    = VAR LShift : INTEGER
    = VAR LChVal : INTEGER
    = VAR LVal : INTEGER
    ; VAR LChars : SET OF CHAR

    ; BEGIN
        GHash := ContribToHash ( GHash , VAL ( '\\' , FM3Utils . HashTyp ) 
      ; Wr . PutChar ( GTokStringWr  , '\\' ) (* The backslash. *) 
      ; NextChar ( ) 
      ; IF NOT LitCharExists
                 ( "Empty escape sequence in " , Wide , Text 
                 , Msg2 := " literal"
                 )
        THEN RETURN FALSE
        END (*IF*) 
      ; IF GCh IN  '\t' , '\r' , '\f' , '\\' , '\'' , '\"' 
        THEN
          GHash := ContribToHash ( GHash , VAL ( GCh , FM3Utils . HashTyp ) 
        ; Wr . PutChar ( GTokStringWr  , GCh  ) 
        ; NextChar ( )
        ; RETURN TRUE
        END (*IF*)

      ; IF GCh IN OctalDigits
        THEN 
          IF Wide THEN LCount := 6 ELSE LCount := 3 END (*IF*) 
        ; LChars := OctalDigits 
        ; LShift := 3  
        ELSIF GCh IN HexTagChars
        THEN  
          Wr . PutChar ( GTokStringWr , GCh ) 
        ; NextChar ( ) 
        ; IF NOT LitCharExists
                   ( "Incomplete hex escape sequence in " , Wide , Text 
                   , Msg2 := " literal"
                   )
          THEN RETURN FALSE
          END (*IF*)
        ; IF Wide THEN LCount := 4 ELSE LCount := 2 ) END (*IF*)
        ; LChars := HexDigits 
        ; LShift :=   
        ELSIF GCh = "U"
        THEN 
          Wr . PutChar ( GTokStringWr  , GCh  ) 
        ; NextChar ( ) 
        ; IF NOT LitCharExists
                   ( "Incomplete Unicode escape sequence in " , Wide , Text 
                   , Msg2 := " literal" 
                   )
          THEN RETURN FALSE
          END (*IF*)
        ; LCount := 6 
        ; LChars := HexDigits 
        ; LShift :=   
        END (*IF*) 

      ; LIntVal := 0 
      ; LOOP (* Thru' LCount chars in LChars. *)
        (* INVARIANT: GCh is a digit of the escape sequence. )  
          GHash := ContribToHash ( GHash , VAL ( GCh , FM3Utils . HashTyp ) 
        ; Wr . PutChar ( GTokStringWr  , GCh ) 
        ; IF GCh IN SET OF CHAR { '0' .. '9' } 
          THEN LChVal := ORD ( GCh ) - ORD ( '0' )  
          ELSIF GCh IN SET OF CHAR { 'A' .. 'Z' } 
          THEN LChVal := ORD ( GCh ) - ORD ( 'A' )  
        ; IF GCh IN SET OF CHAR { 'a' .. z ' } 
          THEN LChVal := ORD ( GCh ) - ORD ( 'a' )  
          END (IF*) 
        ; LVal := Word . LeftShift ( LVal , LShift ) + LChVal 
        ; NextChar ( ) 
        ; DEC ( LCount )
        ; IF LCount = 0 THEN RETURN TRUE 
        ; ELSIF NOT LitCharExists 
                      ( "Short escape sequence" , Wide , Text 
                      , Msg2 := " literal"
                      )
          THEN RETURN FALSE 
        ; ELSIF IF NOT GCh IN LChars
          THEN 
            Error
              ( GFileName , GLineNo , GCharPos 
              , "Short escape sequence in "
                & LitTypeName [ Wide , Text := FALSE ]
                & " literal."
              )
          ; RETURN FALSE
       (* ELSE Loop *) 
          END (*IF*) 
        END (*LOOP*) 
      ; RETURN LVal 
      END EscapeSeq 

  ; PROCEDURE CharLit ( Wide : BOOLEAN ) 
    RAISES { Backout }
    (* PRE: GCh is opening single quote, not yet handled. *) 

    = VAR LCount : PortTypes . Int32Typ
    ; VAR LMsg : TEXT 

    ; BEGIN (* CharLit *) 
        BegOfTextTok ( ) 
      ; GHash 
          := ContribToHash ( GHash , VAL ( W'\'' , FM3Utils . HashTyp ) 
      ; Wr . PutChar ( GTokStringWr  , '\'' ) 
      ; NextChar ( ) (* Consume the opening quote. *)
      
      ; IF NOT LitCharExists
                 ( "No character in " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN 
        ELSIF GCh = '\\' 
        => GCurrentTok . TrAtom := EscapeSeq ( Wide , Text := FALSE ) 
        ELSE
          GHash 
            := ContribToHash ( GHash , VAL ( GWCh , FM3Utils . HashTyp ) 
          Wr . PutWideChar ( GTokStringWr  , GWCh  ) 
        ; GCurrentTok . TrAtom := ORD ( GWCh ) 
        ; NextChar ( ) 
        END (*IF*) 
      ; IF NOT LitCharExists
                 ( "No closing quote on " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN
        ELSIF GCh = '\'' 
        THEN  
          GHash 
            := ContribToHash ( GHash , VAL ( W'\'' , FM3Utils . HashTyp ) 
        ; Wr . PutChar ( GTokStringWr  , GCh  ) 
        ; NextChar ( ) 
        ELSE 
          Error
            ( GFileName , GLineNo , GCharPos
            , "No closing quote on "
              & LitTypeName [ Wide , Text := FALSE ]
              & " Literal"
            )
          
        END (* IF *) 

      ; GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash 
      ; GCurrentTok . Tok := FM3Tok . CharLit  
      END CharLit 

  ; PROCEDURE TextLit ( Wide : BOOLEAN ) 
    RAISES { Backout } 
    (* PRE: GCh is opening double quote, not yet handled. *) 

    = VAR LCount : INTEGER 
    ; VAR LLoc : TEXT 
    ; VAR LMsg : TEXT 

    ; BEGIN (* TextLit *) 
        BegOfTextTok ( ) 
      ; GHash 
          := ContribToHash ( GHash , VAL ( W'\"' , FM3Utils . HashTyp ) 
      ; Wr . PutChar ( GTokStringWr , '\"' ) 
      ; NextChar ( ) (* Consume the opening quote. *) 

      ; LOOP (* Thru' chars of text literal. *) 
          IF NOT LitCharExists
                   ( "Unclosed " , Wide , Text := TRUE 
                   , Msg2 := " literal"
                   ) 
          THEN EXIT 
          ELSIF GCh = '\"' 
          THEN 
            GHash := ContribToHash ( GHash , VAL ( W'\"' , FM3Utils . HashTyp ) 
          ; Wr . PutChar ( GTokStringWr  , W'\"'  ) 
          ; NextChar ( ) 
          ; EXIT 
          ELSIF GCh = '\\' 
          THEN 
            LCharVal := EscapeSeq ( Wide , Text := TRUE ) 
          ELSE
            GHash := ContribToHash ( GHash , VAL ( GWCh , FM3Utils . HashTyp ) 
          ; Wr . PutWideChar ( GTokStringWr  , GWCh  ) 
          ; NextChar ( ) 
          END (*IF*) 
        END (*LOOP*) 

      ; GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash 
      ; GCurrentTok . TrAtom 
          := FM3TextDict . MakeAtom 
               ( TextDict , GCurrentTok . TrText , Ghash ) 
      ; GCurrentTok . TrTextVal := 
      ; GCurrentTok . Tok := FM3Tok . TkTextLit 
      END TextLit

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { Backout } 
    (* PRE: Initial "(*" has been scanned and consumed. * ) 

    = VAR LNestingDepth : INTEGER 

    ; BEGIN 
        LNestingDepthj := 1 
      ; LOOP (* Thru chars in comment *) 
          IF GWCh = WEOF  
          THEN 
            Error 
              ( GFileName , GLineNo , GCharPos 
              , "Comment unclosed at end-of-file"
              )  
          ELSIF GWCh = W'(' 
          THEN 
            NextChar ( ) 
          ; IF GWCh = W'*'
            THEN 
              INC ( LNestingDepth ) 
            ; NextChar ( )
            END (*IF*) 
          ELSIF GWCh = W'*' 
          THEN 
            NextChar ( ) 
          ; IF GWCh = W')'
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
      TRY 
        InLength := Strings . Length ( InString ) 
      ; Pos := 0 
      ; LPragma1stTok := LScanStateRef ^ . SsPragma1stTok 
      ; LScanStateRef ^ . SsPragma1stTok := FALSE 
      ; LOOP (* Through tokens, not necessarily successive ones. *) 
          IF GCh = CR 
          THEN INC ( Pos )   
          ELSIF State >= LbeStd . SsInCmnt 
          THEN 
            BegOfTextTok ( ) 
          ; IF ( ( State - LbeStd . SsInCmnt ) MOD 2 ) = 0 
            THEN CommentSuffix ( ) 
            ELSE PragmaSuffix ( ) 
            END 
          ELSIF State = LbeStd . SsInTok 
          THEN 
            CantHappen ( AFT . A_M3Scanner_Scan_StateInToken ) 
          ELSE (* State = LbeStd . SsIdle *) 
            CASE GCh 
            OF ' ' , CR 
            NextChar ( )
            
            | 'w' , 'W'
            => LCh := GCh
            ; NextChar ( )
            ; IF GCh = '\''
              THEN 
                BegOfTextTok ( )
              ; Wr . PutChar ( GTokStringWr  , LTok ) (* The double-u *) 
              ; CharLit ( Wide := TRUE ) 
              ELSIF GCh = '\"'
              THEN 
                BegOfTextTok ( )
              ; Wr . PutChar ( GTokStringWr  , LTok ) (* The double-u *)
              ; TextLit ( Wide := TRUE )
              ELSE 
                BegOfTextTok ( )
              ; Wr . PutChar ( GTokStringWr  , LTok ) (* The double-u *)
              ; NextChar ( ) 
              ; Ident ( ) 
            
            | 'a' .. 'u' , 'x .. 'z' , 'A' .. 'U' , 'X' .. 'Z'   
            => BegOfTextTok ( )
            ; Ident ( )

            | LbeStd . LeftPlaceholderDelimChar 
            => Placeholder ( )  

            | '0' .. '9' 
            => Number ( ) 

            | '\'' 
            => CharLit ( Wide := FALSE )  

            | '"' 
            => TextLit ( Wide := FALSE ) 

            | '(' 
            => BegOfPlainTok ( ) 
            ; INC ( Pos ) 
            ; NextChar ( ) 
            ; IF GCh = '*' 
              THEN 
                BegOfTextTok ( ) 
              ; Wr . PutChar ( GTokStringWr  , '(' ) 
              ; Wr . PutChar ( GTokStringWr  , '*' ) 
              ; INC ( Pos ) 
              ; NextChar ( ) 
              ; State := LbeStd . SsInCmnt 
              ; CommentSuffix ( ) 
              ELSE 
                BegOfPlainTok ( )
              ; GCurrentTok . Tok := FM3Tok . OpenParen_Tok  
              END (* IF *) 

            | '<' 
            => BegOfPlainTok ( ) 
            ; NextChar ( ) 
            ; CASE GWCh 
              OF W'=' 
              => NextChar ( ) 
              ; GCurrentTok . Tok := FM3Tok . LessEqual_Tok  
              | W':' 
              => NextChar ( ) 
              ; GCurrentTok . Tok := FM3Tok . Subtype_Tok  
              | W'*' 
              => (* Opening pragma delimiter. *)
                NextChar ( )
              ; GTopScanStateRef ^ . SsPragma1stTok := TRUE 
              ; GCurrentTok . Tok := FM3Tok . TkPragaOpem  
              ELSE 
                GCurrentTok . Tok := FM3Tok . TkLessThan 
              END (* CASE *) 

            | '*' 
            => BegOfPlainTok ( )
            ; NextChar ( )
            ; IF GWCh = W'>'
              THEN (* Closing pragma delimiter. *) 
                NextChar ( )
              ; GCurrentTok . Tok := FM3Tok . PragmaClose 
              ELSE GCurrentTok . Tok := FM3Tok . Star_Tok
              END (*IF*) 

            | ':' 
            => BegOfPlainTok ( ) 
            ; INC ( Pos ) 
            ; NextChar ( ) 
            ; IF GCh = '=' 
              THEN GCurrentTok . Tok := FM3Tok . Becomes_Tok  
              ELSE GCurrentTok . Tok := FM3Tok . Colon_Tok  
              END (* IF *) 

            | '.' 
            => BegOfPlainTok ( ) 
            ; INC ( Pos ) 
            ; NextChar ( ) 
            ; IF GCh = '.' 
              THEN GCurrentTok . Tok := FM3Tok . Ellipsis_Tok  
              ELSE GCurrentTok . Tok := FM3Tok . Dot_Tok 
              END (* IF *) 

            | '=' 
            => BegOfPlainTok ( ) 
            ; INC ( Pos ) 
            ; NextChar ( ) 
            ; IF GCh = '>' 
              THEN GCurrentTok . Tok := FM3Tok . Arrow_Tok 
              ELSE GCurrentTok . Tok := FM3Tok . Equal_Tok  
              END (* IF *) 

            | '>' 
            => BegOfPlainTok ( ) 
            ; INC ( Pos ) 
            ; NextChar ( ) 
            ; IF GCh = '=' 
              THEN GCurrentTok . Tok := FM3Tok . GreaterEqual_Tok 
              ELSE GCurrentTok . Tok := FM3Tok . Greater_Tok 
              END (* IF *) 

            | '+' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Plus_Tok  

            | '-' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Minus_Tok  

            | '^' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Deref_Tok  

            | '#' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Unequal_Tok  

            | ';' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Semicolon_Tok  

            | '[' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . OpenBracket_Tok  

            | ']' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . CloseBracket_Tok  

            | '{' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . OpenBrace_Tok  

            | '}' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . CloseBrace_Tok  

            | ')' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . CloseParen_Tok  

            | ',' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Comma_Tok  

            | '&' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Ampersand_Tok  

            | '|' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Stroke_Tok  

            | '/' 
            => BegOfPlainTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Slash_Tok  

            ELSE 
              LexErrorChars ( LbeStd . LeBadChars ) 
            END (* CASE *) 
          END (* IF *)
        END (* LOOP *) 
      EXCEPT Backout 
      => <* FATAL Backout *> 
        BEGIN 
          BegOfPlainTok ( ) 
        ; GCurrentTok . Tok := FLbeStd . Tok__Unknown  
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END Scan 

; BEGIN (* FM3Scanner *) 
    InitTables ( ) 
  ; LangUtil . RegisterScanner ( LbeStd . LangM3 , Scan ) 
  END FM3Scanner 
. 
