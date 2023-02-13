
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

(* New line characters: *) 
; CONST LF =  '\x0A' (* = '\n' *)
; CONST CR =  '\x0D' (* = '\r' *)
; CONST FF =  '\x0C' (* = '\f' *) 
; CONST VT =  '\x09' (* = '\t' *) 
; CONST NEL = '\x85'  
; CONST WLS =  W'\x2028'  
; CONST WPS =  W'\x2029'  
(* Also CR immediately followed by LF *)  

; CONST GoodChars 
    = SET OF CHAR 
        { LbeStd . CharEndOfImage , LbeStd . CharNewLine , ' ' , '.' , ':' 
        , ';' , '*' , '/' , '<' , '>' , '=' , '#' , '|' , '^' , ',' , '&' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"'
        , LbeStd . LeftPlaceholderDelimChar 
        , LbeStd . RightPlaceholderDelimChar   
        } 

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

(* EXPORTED: *) 
; PROCEDURE PushReader ( NewUniRd : UniRd . T ; UnitNo : INTEGER ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

  = VAR LScanStateRef : ScanStateRefTyp 

  ; BEGIN 
      Tread . Acquire ( NewUniRd ) 
    ; GTopScanStateRef . SsLineNo := GLineNo 
    ; GTopScanStateRef . SsCharPos := GCharPos  
    ; GTopScanStateRef . SsTokStringWr := GTokStringWr  
    ; GTopScanStateRef . SsWCh := GWCh 
    ; GTopScanStateRef . Ss := GHash  
    ; LScanStateRef := NEW ( ScanStateRefTyp ) 
    ; LScanStateRef ^ . SsLink := GTopScanStateRef 
    ; LScanStateRef ^ . SsUniRd := NewUniRd 
    ; LScanStateRef ^ . SsUnitNo := UnitNo 
    ; LScanStateRef ^ . SsLineNo := 0 
    ; LScanStateRef ^ . SsTokStringWr := NIL  
    ; GLineNo := 0 
    ; LScanStateRef ^ . SsCharPos : 0   
    ; GLineNo := 0 
    ; GTopScanStateRef := LScanStateRef 
    ; GUniRd := NewUniRd 
    ; GWCh := UnsafeUniRd ( GUniRd ) 
    ; GTopScanStateRef . GWCh := GWCh
    ; INC ( GScanStateCt ) 
    ; INC ( GScanStateDepth ) 
    ; GMaxScanStateDepth := MAX ( GMaxScanStateDepth , GScanStateDepth ) 
    END PushReader 

(* EXPORTED: *) 
; PROCEDURE PopReader ( ) : UniRd . T (* Previous reader. *)  

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
    ; UniRd . Close ( LScanStateRef . SsUniRd )  
    ; Thread . Release ( LScanStateRef . SsUniRd ) 
    ; DEC ( GScanStateDepth ) 
    ; RETURN LScanStateRef 
    END PopReader 

(* EXPORTED: *) 
; PROCEDURE CurrentUnitNo ( ) : INTEGER 

  = BEGIN 
      IF GTopScanStateRef = NIL THEN RETURN - 1 END (* IF *) 
    ; RETURN GTopScanStateRef . SsUnitNo 
    END CurrentUnitNo 

(* EXPORTED: *) 
; PROCEDURE Scan ( Cr : SchutzCoroutine . T ) 


  = VAR Sif : ScannerIf . ScanIfTyp 
  ; VAR InString : Strings . StringTyp 
  ; VAR State : LbeStd . ScanStateTyp 
  ; VAR SaveState : LbeStd . ScanStateTyp 
  ; VAR InLength : Strings . StringSsTyp 
  ; VAR Pos : PortTypes . Int32Typ 
  ; VAR TokString : Strings . StringTyp 
  ; VAR Ch : WIDECHAR 
  ; VAR BackCh : WIDECHAR (* Meaningful only when Pos = - 1 *) 
    ; AreAllBlanks : BOOLEAN 

  (* It is possible to put back at most one character, and it must 
     be the one that was gotten there. *) 
  ; PROCEDURE PutBackChar ( Ch : WIDECHAR ) 
    RAISES { Backout } 

    = BEGIN (* PutBackChar *) 
        TRY 
          DEC ( Pos ) 
        ; IF Pos < 0 
          THEN 
            <* ASSERT Pos = - 1 , "PutBackChar, LowPos" *> 
          ; BackCh := Ch 
          ELSE 
            <* ASSERT Strings . IthChar ( InString , Pos ) = Ch 
            , "PutBackChar, WrongChar" 
            ) 
          END (* IF *) 
        EXCEPT Strings . SsOutOfBounds 
        => RAISE Backout ( "Strings.SsOutOfBounds" ) 
        END (* TRY EXCEPT *) 
      END PutBackChar 

  ; PROCEDURE EnsureOneChar ( ) 
    RAISES { Backout } 

    = BEGIN (* EnsureOneChar *) 
        TRY 
          IF Pos < 0 
          THEN 
            Assert ( Pos = - 1 , AFT . A_M3Scanner_Scan_EnsureOneChar_LowPos ) 
          ; Ch := BackCh 
          ELSE 
            WHILE Pos >= InLength 
            DO ScannerIf . ConsumeChars 
                 ( Sif 
                 , (* VAR *) State 
                 , InLength 
                 , (* VAR *) InString 
                 , (* VAR *) AreAllBlanks 
                 ) 
            ; InLength := Strings . Length ( InString ) 
            ; Pos := 0 
            END (* WHILE *) 
          ; Ch := Strings . IthChar ( InString , Pos ) 
          END (* IF *) 
        EXCEPT Strings . SsOutOfBounds 
        => RAISE Backout ( "Strings.SsOutOfBounds" ) 
        END (* TRY EXCEPT *) 
      END EnsureOneChar 

  (* NOTE: The invariant on when to call EnsureOneChar is inconsistent. 
           I don't want to call it prematurely when a lookahead 
           is not required. So it is generally called right before 
           a character is to be examined.  But something earlier 
           could have already examined the same character, meaning it 
           will already have been done for this character. 
           Fortunately, it is harmless to call it multiple times 
           without incrementing Pos. *) 

  ; PROCEDURE BegOfTok 
      ( Tok : FM3Base . TokTyp := FM3Base . TokNull ; DoText := FALSE ) 
    RAISES { Backout } 

    = BEGIN (* BegOfTok *) 
        GCurrentTok . TrTok := Tok 
      ; GCurrentTok . TrLineNo := GLineNo 
      ; GCurrentTok . TrCharPos := GCharPos 
      ; IF DoText 
        THEN GTokStringWr := TextWr . T 
        ELSE GTokStringWr := NIL 
        END (*IF*)
      END BegOfTok 

  ; PROCEDURE DeliverTok 
      ( Tok : LbeStd . TokTyp ; AtomDict : FM3TextDict . T  ) 
    RAISES { Backout } 

    = BEGIN (* DeliverTok *) 
        IF GTokStringWr = NIL 
        THEN 
          GCurrentTok . TrText := NIL 
        ; GCurrentTok . TrHash := FM3Utils . HashNull 
        ; GCurrentTok . TrAtom := Fm3Base . AtomNull 
        ELSE 
          GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
        ; GCurrentTok . TrHash := GHash 
        ; GCurrentTok . TrAtom 
            := FM3TextDict . MakeAtom 
                 ( AtomDict , GCurrentTok . TrText , Ghash ) 
        END (*IF*) 
      ; RETURN 
      END DeliverTok 
(*
  ; PROCEDURE AppendAndDeliverTok ( Tok : LbeStd . TokTyp ) 
    RAISES { Backout } 

    = BEGIN (* AppendAndDeliverTok *) 
        Strings . AppendCharInPlace ( TokString , Ch ) 
      ; INC ( Pos ) 
      ; DeliverTok ( Tok ) 
      END AppendAndDeliverTok 
*)

  ; PROCEDURE NextChar ( ) 
    RAISES { Backout } 

    = VAR LWCh : WIDECHAR 

    ; BEGIN (* NextChar *) 
        IF GWCh = CR 
        THEN 
          INC ( GLineNo ) 
        ; GCharPos := 0 
        END (* IF *) 
      ; GWCh := UnsafeUniRd . GetWideCh ( ) 
      ; INC ( GCharPos )
      ; IF GWCh = CR 
        THEN 
          LCh2 := UnsafeUniRd . FastGetWideCh ( GUniRd ) 
        ; IF LCh2 = LF 
          THEN INC ( GCharPos )
          (* Leave GWCh = CR.  It'a cononical new line. *)
          ELSE UnsafeUniRd . FastUnGetWideCodePoint ( GUnird ) 
          END (* IF *) 
        ELSIF GWCh IN  SET OF CHAR { LF , FF , VT , NEL } 
              OR GWCh = WPS 
              OR GWCh = WLS
        THEN  
          GWCh := CR (* Canonical new line. *) 
        END (* IF *) 
      ; IF GTokStringWr # NIL 
        THEN 
          Wr . PutWideChar ( GTokStringWr , GWCh ) 
        END (* IF *) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( Code : LbeStd . ErrCodeTyp ) 
    RAISES { Backout } 

    = BEGIN (* LexErrorChars *) 
        State := LbeStd . SsInTok 
      ; EnsureOneChar ( ) 
      ; WHILE NOT Ch IN GoodChars DO NextChar ( ) END (* WHILE *) 
      ; ScannerIf . LexErr ( Sif , Code ) 
      ; DeliverTok ( LbeStd . Tok__LexErrChars ) 
      END LexErrorChars 

  ; PROCEDURE Ident ( ) 
    RAISES { Backout } 

    = VAR LIntTok : INTEGER 

    ; BEGIN (* Ident *) 
        BegOfTok ( ) 
      ; WHILE Ch 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *) 
      ; IF RwTable . get ( Strings . ToText ( TokString ) , LIntTok ) 
        THEN 
          DeliverTok ( LIntTok ) 
        ELSE 
          DeliverTok ( M3Tok . Id ) 
        END (* IF *) 
      END Ident 

  ; PROCEDURE Placeholder ( ) 
    RAISES { Backout } 

    = VAR LIntTok : INTEGER 

    ; BEGIN (* Ident *) 
        BegOfTok ( ) 
      ; NextChar ( ) 
      ; WHILE Ch 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *)
      ; IF Ch = LbeStd . RightPlaceholderDelimChar 
        THEN 
          NextChar ( ) 
        ELSE 
          ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedPlaceholder ) 
        ; Strings . AppendCharInPlace 
            ( TokString , LbeStd . RightPlaceholderDelimChar )  
        END (* IF *) 
      ; IF PhTable . get ( Strings . ToText ( TokString ) , LIntTok ) 
        THEN 
          DeliverTok ( LIntTok ) 
        ELSE 
          ScannerIf . LexErr ( Sif , LbeStd . LeUnknownPlaceholder ) 
        ; DeliverTok ( LbeStd . Tok__LexErrChars ) 
        END (* IF *) 
      END Placeholder  

  ; PROCEDURE Number ( ) 
    RAISES { Backout } 

    = BEGIN (* Number *) 
        BegOfTok ( ) 
      ; WHILE Ch IN SET OF CHAR { '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *)
      ; CASE Ch
        OF '_' 
        => NextChar ( ) 
        ; IF Ch IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
          THEN 
            WHILE Ch IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
            DO NextChar ( ) 
            END (* WHILE *) 
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoHexDigit ) 
          END (* IF *) 
        ; DeliverTok ( M3Tok . Number ) 
        ; RETURN 
        | '.' 
        => INC ( Pos ) 
        ; EnsureOneChar ( ) 
        ; IF Ch = '.' 
          THEN PutBackChar ( '.' ) 
          ELSE
            Strings . AppendCharInPlace ( TokString , '.' ) 
          ; IF Ch IN SET OF CHAR { '0' .. '9' } 
            THEN 
              WHILE Ch IN SET OF CHAR { '0' .. '9' } 
              DO NextChar ( ) 
              END (* WHILE *) 
            ELSE 
              ScannerIf . LexErr ( Sif , LbeStd . LeNoFractionalDigit ) 
            END (* IF *) 
          END (* IF *) 
        ELSE 
        END  
      ; CASE Ch
        OF 'E' , 'e' , 'D' , 'd' , 'X' , 'x'  
        => NextChar ( ) 
        ; CASE Ch 
          OF '+' , '-' 
          => NextChar ( ) 
          ELSE 
          END 
        ; IF  Ch IN SET OF CHAR { '0' .. '9' } 
          THEN 
            WHILE Ch IN SET OF CHAR { '0' .. '9' } 
            DO NextChar ( ) 
            END (* WHILE *)
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoExponentDigit ) 
          END (* IF *) 
        ELSE 
        END  
      ; DeliverTok ( M3Tok . Number ) 
      END Number 

  ; PROCEDURE String ( ) 
    RAISES { Backout } 

    = VAR LCount : PortTypes . Int32Typ 

    ; BEGIN (* String *) 
        BegOfTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; LOOP 
          CASE Ch 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
          ; Strings . AppendCharInPlace ( TokString , '"' ) 
          ; DeliverTok ( M3Tok . TextLit ) 
          ; EXIT 

          | '"' 
          => AppendAndDeliverTok ( M3Tok . TextLit ) 
          ; EXIT 

          | '\\' 
          => NextChar ( ) 
          ; CASE Ch 
            OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
            => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
            ; Strings . AppendCharInPlace ( TokString , '"' ) 
            ; DeliverTok ( M3Tok . TextLit ) 
            ; EXIT 

            | '0' .. '7' 
            => LCount := 3 
            ; WHILE LCount > 0 AND Ch IN SET OF CHAR { '0' .. '7' } 
              DO NextChar ( ) 
              ; DEC ( LCount ) 
              END (* WHILE *) 
            ; IF LCount > 0 
              THEN 
                ScannerIf . LexErr ( Sif , LbeStd . LeBadOctalDigit ) 
              END (* IF *) 

            | '\t' , '\r' , '\f' , '\\' , '\'' , '\"' 
            => NextChar ( ) 

            ELSE 
              NextChar ( ) (* For now, just allow anything to be escaped. *) 
            END (* CASE *) 

          ELSE 
            NextChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END String 

  ; PROCEDURE CharLit ( ) 
    RAISES { Backout } 

    = VAR LCount : PortTypes . Int32Typ 

    ; BEGIN (* CharLit *) 
        BegOfTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; CASE Ch 
        OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
        => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedChar ) 
        ; Strings . AppendCharInPlace ( TokString , '\'' ) 
        ; DeliverTok ( M3Tok . CharLit ) 
        ; RETURN 

        | '\\' 
        => NextChar ( ) 
        ; CASE Ch 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedChar ) 
          ; Strings . AppendCharInPlace ( TokString , '\'' ) 
          ; DeliverTok ( M3Tok . CharLit )
          ; RETURN  

          | '0' .. '7' 
          => LCount := 3 
          ; WHILE LCount > 0 AND Ch IN SET OF CHAR { '0' .. '7' } 
            DO NextChar ( ) 
            ; DEC ( LCount ) 
            END (* WHILE *) 
          ; IF LCount > 0 
            THEN 
              ScannerIf . LexErr ( Sif , LbeStd . LeBadOctalDigit ) 
            END (* IF *) 

          | '\t' , '\r' , '\f' , '\\' , '\'' , '\"' 
          => NextChar ( ) 

          ELSE 
            NextChar ( ) (* For now, just allow anything to be escaped. *) 
          END (* CASE *) 

        ELSE 
          NextChar ( ) 
        END (* CASE *) 
      ; IF Ch = '\'' 
        THEN  
          NextChar ( ) 
        ELSE 
          Strings . AppendCharInPlace ( TokString , '\'' ) 
        END (* IF *) 
      ; DeliverTok ( M3Tok . CharLit ) 
      END CharLit 

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { Backout } 

    = BEGIN 
        SaveState := State 
      ; State := LbeStd . SsInTok 
      ; LOOP (* Thru chars in comment *) 
          (* INVARIANT: EnsureOneChar ( ) has been done since the last 
                        INC ( Pos ) *) 
          CASE Ch 
          OF LbeStd . CharEndOfImage 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Char, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , TRUE )
          ; EXIT 
          | LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Char, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , FALSE )
          ; EXIT 
          | '(' 
          => Strings . AppendCharInPlace ( TokString , '(' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '*' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '*' ) 
            ; INC ( SaveState , 2 ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            END (* IF *) 

          | '*' 
          => Strings . AppendCharInPlace ( TokString , '*' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = ')' 
            THEN 
              Strings . AppendCharInPlace ( TokString , ')' ) 
            ; INC ( Pos ) 
            ; IF SaveState = LbeStd . SsInCmnt 
              THEN 
                State := SaveState 
              ; DeliverTok ( LbeStd . Tok__Cmnt ) 
              ; EXIT 
              ELSE 
                DEC ( SaveState , 2 ) 
              ; EnsureOneChar ( ) 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Strings . AppendCharInPlace ( TokString , Ch ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END CommentSuffix 

  ; PROCEDURE PragmaSuffix ( ) 
    RAISES { Backout } 

    = BEGIN 
        SaveState := State 
      ; State := LbeStd . SsInTok 
      ; LOOP (* Thru chars in comment *) 
          (* INVARIANT: EnsureOneChar ( ) has been done since the last 
                        INC ( Pos ) *) 
          CASE Ch 
          OF LbeStd . CharEndOfImage 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , TRUE )
          ; EXIT 
          | LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , FALSE )
          ; EXIT 
          | '<' 
          => Strings . AppendCharInPlace ( TokString , '<' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '*' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '*' ) 
            ; INC ( SaveState , 2 ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            END (* IF *) 

          | '*' 
          => Strings . AppendCharInPlace ( TokString , '*' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '>' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '>' ) 
            ; INC ( Pos ) 
            ; IF SaveState = LbeStd . SsInCmnt + 1 
              THEN 
                State := SaveState 
              ; DeliverTok ( LbeStd . Tok__Cmnt ) 
              ; EXIT 
              ELSE 
                DEC ( SaveState , 2 ) 
              ; EnsureOneChar ( ) 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Strings . AppendCharInPlace ( TokString , Ch ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END PragmaSuffix 

  ; BEGIN (* Scan *) 
      TRY 
        Sif := NARROW ( Cr , ScannerIf . ScanIfTyp ) 
      ; Strings . MakeEmpty ( TokString ) 
      ; ScannerIf . GetInitialChars 
          ( Sif 
          , (* VAR *) State 
          , (* VAR *) InString 
          , (* VAR *) AreAllBlanks 
          ) 
      ; InLength := Strings . Length ( InString ) 
      ; Pos := 0 
      ; LOOP (* Through tokens, not necessarily successive ones. *) 
          (* (Non-)INVARIANT: EnsureOneChar has not necessarily been done. *) 
          EnsureOneChar ( ) 
        ; IF Ch = LbeStd . CharNewLine 
          THEN 
            INC ( Pos )   
          ELSIF State >= LbeStd . SsInCmnt 
          THEN 
            BegOfTok ( ) 
          ; IF ( ( State - LbeStd . SsInCmnt ) MOD 2 ) = 0 
            THEN 
              CommentSuffix ( ) 
            ELSE 
              PragmaSuffix ( ) 
            END 
          ELSIF State = LbeStd . SsInTok 
          THEN 
            CantHappen ( AFT . A_M3Scanner_Scan_StateInToken ) 
          ELSE (* State = LbeStd . SsIdle *) 
            CASE Ch 
            OF ' ' , LbeStd . CharNewLine , LbeStd . CharTab 
            => INC ( Pos ) 
            | LbeStd . CharEndOfImage 
            => BegOfTok ( ) 
            ; DeliverTok ( LbeStd . Tok__EndOfImage ) 
            | 'a' .. 'z' , 'A' .. 'Z'   
            => Ident ( )

            | LbeStd . LeftPlaceholderDelimChar 
            => Placeholder ( )  

            | '0' .. '9' 
            => Number ( ) 

            | '"' 
            => String ( ) 

            | '\'' 
            => CharLit ( )  

            | '(' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '*' 
              THEN 
                Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; State := LbeStd . SsInCmnt 
              ; CommentSuffix ( ) 
              ELSE 
                DeliverTok ( M3Tok . OpenParen_Tok ) 
              END (* IF *) 

            | '<' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; CASE Ch 
              OF  '=' 
              => AppendAndDeliverTok ( M3Tok . LessEqual_Tok ) 
              |  ':' 
              => AppendAndDeliverTok ( M3Tok . Subtype_Tok ) 
              |  '*' 
              => Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; State := LbeStd . SsInCmnt + 1  
              ; PragmaSuffix ( ) 
              ELSE 
                DeliverTok ( M3Tok . Less_Tok ) 
              END (* CASE *) 

            | ':' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '=' 
              THEN 
                AppendAndDeliverTok ( M3Tok . Becomes_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Colon_Tok ) 
              END (* IF *) 

            | '.' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '.' 
              THEN 
                AppendAndDeliverTok ( M3Tok . Ellipsis_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Dot_Tok ) 
              END (* IF *) 

            | '=' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '>' 
              THEN 
                AppendAndDeliverTok ( M3Tok . Arrow_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Equal_Tok ) 
              END (* IF *) 

            | '>' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '=' 
              THEN 
                AppendAndDeliverTok ( M3Tok . GreaterEqual_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Greater_Tok ) 
              END (* IF *) 

            | '+' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Plus_Tok ) 
            | '-' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Minus_Tok ) 
            | '^' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Deref_Tok ) 
            | '#' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Unequal_Tok ) 
            | ';' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Semicolon_Tok ) 
            | '[' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . OpenBracket_Tok ) 
            | ']' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . CloseBracket_Tok ) 
            | '{' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . OpenBrace_Tok ) 
            | '}' 
            => BegOfTok ( ) 
             ; AppendAndDeliverTok ( M3Tok . CloseBrace_Tok ) 
            | ')' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . CloseParen_Tok ) 
            | ',' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Comma_Tok ) 
            | '&' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Ampersand_Tok ) 
            | '|' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Stroke_Tok ) 
            | '*' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Star_Tok ) 
            | '/' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Slash_Tok ) 

            ELSE 
              BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; LexErrorChars ( LbeStd . LeBadChars ) 
            END (* CASE *) 
          END (* IF *) 
       END (* LOOP *) 
       EXCEPT Backout 
      => <* FATAL Backout *> 
        BEGIN 
          BegOfTok ( ) 
        ; DeliverTok ( LbeStd . Tok__Unknown ) 
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END Scan 

; BEGIN (* FM3Scanner *) 
    InitTables ( ) 
  ; LangUtil . RegisterScanner ( LbeStd . LangM3 , Scan ) 
  END FM3Scanner 
. 
