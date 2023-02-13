
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

; CONST WideLastOfChar : WIDECHAR := LAST ( CHAR ) 

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
    ; GCh := MIN ( GWCh , WideLastOfChar )  
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

  ; PROCEDURE BegOfTok ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfTok *) 
        GCurrentTok . TrTok := Tok 
      ; GCurrentTok . TrLineNo := GLineNo 
      ; GCurrentTok . TrCharPos := GCharPos 
      ; GTokStringWr := NIL 
      END BegOfTok 

  ; PROCEDURE BegOfTextTok 
      ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfTextTok *) 
        GCurrentTok . TrTok := Tok 
      ; GCurrentTok . TrLineNo := GLineNo 
      ; GCurrentTok . TrCharPos := GCharPos 
      ; GTokStringWr := TextWr . T 
      ; GHash := FM3Util . HashGround ( ) 
      END BegOfTextTok 

  ; PROCEDURE FinishTok 
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
      END FinishTok 

  ; PROCEDURE FinishAtomTok ( AtomDict : FM3TextDict . T ) 
    RAISES { Backout } 

    = BEGIN (* FinishAtomTok *) 
        GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash 
      ; GCurrentTok . TrAtom 
          := FM3TextDict . MakeAtom 
               ( AtomDict , GCurrentTok . TrText , Ghash ) 
      END FinishAtomTok 

  ; PROCEDURE DeliverTok 
      ( Tok : LbeStd . TokTyp ; AtomDict : FM3TextDict . T  ) 
    RAISES { Backout } 

    = BEGIN (* DeliverTok *) 
        RETURN 
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
        IF GCh = CR 
        THEN 
          INC ( GLineNo ) 
        ; GCharPos := 0 
        END (* IF *) 
      ; GWCh := UnsafeUniRd . GetWideCh ( ) 
      ; GCh := MIN ( GWCh , WideLastOfChar )  
      ; INC ( GCharPos )
      ; IF GCh = CR 
        THEN 
          LCh2 := UnsafeUniRd . FastGetWideCh ( GUniRd ) 
        ; IF LCh2 = LF 
          THEN INC ( GCharPos )
          (* Leave GCh and GWCh = CR.  It'a cononical new line. *)
          ELSE UnsafeUniRd . FastUnGetWideCodePoint ( GUnird ) 
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

    = BEGIN (* LexErrorChars *) 
        State := LbeStd . SsInTok 
      ; EnsureOneChar ( ) 
      ; WHILE NOT Ch IN GoodChars DO NextChar ( ) END (* WHILE *) 
      ; ScannerIf . LexErr ( Sif , Code ) 
      ; DeliverTok ( LbeStd . Tok__LexErrChars ) 
      END LexErrorChars 

  ; PROCEDURE Ident ( ) 
    RAISES { Backout } 

    = VAR LRwTok : FM3Base . TokTyp 
    ; VAR LIsRw : BOOLEAN 

    ; BEGIN (* Ident *) 
        BegOfTextTok ( ) 
      ; WHILE GCh 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *) 
      ; GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash 
      ; LIsRw 
          := FM3TextDict . Lookup 
               ( FM3Globals . RwDict , GText , GHash , LRwTok ) 
      ; IF LIsRw 
        THEN GCurrentTok . TrTok := LRwTok 
        ELSE GCurrentTok . TrTok := FM3Tok . Id 
        END (* IF *) 
      END Ident 

  ; PROCEDURE Placeholder ( ) 
    RAISES { Backout } 

    = VAR LIntTok : INTEGER 

    ; BEGIN (* Placeholder *) 
        BegOfTextTok ( ) 
      ; NextChar ( ) 
      ; WHILE GWCh >= LAST ( CHAR ) 
              AND GWCh 
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
        BegOfTextTok ( FM3Tok . Number , DoText := TRUE ) 
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
        ; GCurrentTok . TrTok := FM3Tok . Number 
        ; FinishAtomTok ( M3Tok . Number ) 

        | '.' 
        => INC ( Pos ) 
        ; NextChar ( ) 
        ; IF GCh = '.' 
          THEN PutBackChar ( ) 
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
        ; IF GChIN SET OF CHAR { '+' , '-' }  
          THE  NextChar ( ) 
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
      ; GCurrentTok . TrText := TextWr . ToText ( GTokStringWr ) 
      ; GCurrentTok . TrHash := GHash 
      ; GCurrentTok . TrTok := FM3Tok . Number  
      END Number 

  ; PROCEDURE String ( ) 
    RAISES { Backout } 

    = VAR LCount : INTEGER 

    ; BEGIN (* String *) 
        BegOfTextTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; LOOP 
          CASE GCh 
          OF CR , EOI 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
          ; Wr . PutChar ( GTokStringWr  , '"' ) 
          ; GCurrentTok . Tok := FM3Tok . TextLit  
          ; EXIT 

          | '"' 
          => Wr . PutChar ( GTokStringWr , GCh ) 
          ; GCurrentTok . Tok := FM3Tok . TextLit 
          ; EXIT 

          | '\\' 
          => NextChar ( ) 
          ; CASE GCh 
            OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
            => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
            ; Wr . PutChar ( GTokStringWr  , '"' ) 
            ; GCurrentTok . Tok := FM3Tok . TextLit  
            ; EXIT 

            | '0' .. '7' 
            => LCount := 3 
            ; WHILE LCount > 0 AND GCh IN SET OF CHAR { '0' .. '7' } 
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
            END (*CASE*) 

          ELSE 
            NextChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END String 

  ; PROCEDURE CharLit ( ) 
    RAISES { Backout } 

    = VAR LCount : PortTypes . Int32Typ 

    ; BEGIN (* CharLit *) 
        BegOfTextTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; CASE GCh 
        OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
        => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedChar ) 
        ; Wr . PutChar ( GTokStringWr  , '\'' ) 
        ; GCurrentTok . Tok := FM3Tok . CharLit 
        ; RETURN 

        | '\\' 
        => NextChar ( ) 
        ; CASE GCh 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedChar ) 
          ; Wr . PutChar ( GTokStringWr  , '\'' ) 
          ; GCurrentTok . Tok := FM3Tok . CharLit 
          ; RETURN  

          | '0' .. '7' 
          => LCount := 3 
          ; WHILE LCount > 0 AND GCh IN SET OF CHAR { '0' .. '7' } 
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
      ; IF GCh = '\'' 
        THEN  
          NextChar ( ) 
        ELSE 
          Wr . PutChar ( GTokStringWr  , '\'' ) 
        END (* IF *) 
      ; GCurrentTok . Tok := FM3Tok . CharLit  
      END CharLit 

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { Backout } 

    = BEGIN 
        SaveState := State 
      ; State := LbeStd . SsInTok 
      ; LOOP (* Thru chars in comment *) 
          (* INVARIANT: EnsureOneChar ( ) has been done since the last 
                        INC ( Pos ) *) 
          CASE GCh 
          OF LbeStd . CharEndOfImage 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Char, but do not append to token. *) 
          ; GCurrentTok . Tok := FLbeStd . Tok__CmntAtEndOfLine , TRUE )
          ; EXIT 
          | LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Char, but do not append to token. *) 
          ; GCurrentTok . Tok := FLbeStd . Tok__CmntAtEndOfLine , FALSE )
          ; EXIT 
          | '(' 
          => Wr . PutChar ( GTokStringWr  , '(' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF GCh = '*' 
            THEN 
              Wr . PutChar ( GTokStringWr  , '*' ) 
            ; INC ( SaveState , 2 ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            END (* IF *) 

          | '*' 
          => Wr . PutChar ( GTokStringWr  , '*' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF GCh = ')' 
            THEN 
              Wr . PutChar ( GTokStringWr  , ')' ) 
            ; INC ( Pos ) 
            ; IF SaveState = LbeStd . SsInCmnt 
              THEN 
                State := SaveState 
              ; GCurrentTok . Tok := FLbeStd . Tok__Cmnt  
              ; EXIT 
              ELSE 
                DEC ( SaveState , 2 ) 
              ; EnsureOneChar ( ) 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Wr . PutChar ( GTokStringWr  , GCh ) 
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
          CASE GCh 
          OF LbeStd . CharEndOfImage 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; GCurrentTok . Tok := FLbeStd . Tok__CmntAtEndOfLine , TRUE 
          ; EXIT 
          | LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; GCurrentTok . Tok := FLbeStd . Tok__CmntAtEndOfLine , FALSE 
          ; EXIT 
          | '<' 
          => Wr . PutChar ( GTokStringWr  , '<' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF GCh = '*' 
            THEN 
              Wr . PutChar ( GTokStringWr  , '*' ) 
            ; INC ( SaveState , 2 ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            END (* IF *) 

          | '*' 
          => Wr . PutChar ( GTokStringWr  , '*' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF GCh = '>' 
            THEN 
              Wr . PutChar ( GTokStringWr  , '>' ) 
            ; INC ( Pos ) 
            ; IF SaveState = LbeStd . SsInCmnt + 1 
              THEN 
                State := SaveState 
              ; GCurrentTok . Tok := FLbeStd . Tok__Cmnt  
              ; EXIT 
              ELSE 
                DEC ( SaveState , 2 ) 
              ; EnsureOneChar ( ) 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Wr . PutChar ( GTokStringWr  , GCh ) 
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
        ; IF GCh = LbeStd . CharNewLine 
          THEN 
            INC ( Pos )   
          ELSIF State >= LbeStd . SsInCmnt 
          THEN 
            BegOfTextTok ( ) 
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
            CASE GCh 
            OF ' ' , LbeStd . CharNewLine , LbeStd . CharTab 
            => INC ( Pos ) 
            | LbeStd . CharEndOfImage 
            => BegOfTextTok ( ) 
            ; GCurrentTok . Tok := FLbeStd . Tok__EndOfImage  
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
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF GCh = '*' 
              THEN 
                BegOfTextTok ( ) 
              ; Wr . PutChar ( GTokStringWr  , '(' ) 
              ; Wr . PutChar ( GTokStringWr  , '*' ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; State := LbeStd . SsInCmnt 
              ; CommentSuffix ( ) 
              ELSE 
                BegOfTok ( )
              ; GCurrentTok . Tok := FM3Tok . OpenParen_Tok  
              END (* IF *) 

            | '<' 
            => BegOfTok ( ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            ; CASE GCh 
              OF  '=' 
              => BegOfTok ( ) 
              ; GCurrentTok . Tok := FM3Tok . LessEqual_Tok  
              |  ':' 
              => BegOfTok ( )
              ; GCurrentTok . Tok := FM3Tok . Subtype_Tok  
              |  '*' 
              => BegOfTextTok ( )
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; PragmaSuffix ( ) 
              ELSE 
                GCurrentTok . Tok := FM3Tok . Less_Tok  
              END (* CASE *) 

            | ':' 
            => BegOfTok ( ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            ; IF GCh = '=' 
              THEN GCurrentTok . Tok := FM3Tok . Becomes_Tok  
              ELSE GCurrentTok . Tok := FM3Tok . Colon_Tok  
              END (* IF *) 

            | '.' 
            => BegOfTok ( ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            ; IF GCh = '.' 
              THEN GCurrentTok . Tok := FM3Tok . Ellipsis_Tok  
              ELSE GCurrentTok . Tok := FM3Tok . Dot_Tok 
              END (* IF *) 

            | '=' 
            => BegOfTok ( ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            ; IF GCh = '>' 
              THEN GCurrentTok . Tok := FM3Tok . Arrow_Tok 
              ELSE GCurrentTok . Tok := FM3Tok . Equal_Tok  
              END (* IF *) 

            | '>' 
            => BegOfTok ( ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            ; IF GCh = '=' 
              THEN GCurrentTok . Tok := FM3Tok . GreaterEqual_Tok 
              ELSE GCurrentTok . Tok := FM3Tok . Greater_Tok 
              END (* IF *) 

            | '+' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Plus_Tok  

            | '-' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Minus_Tok  

            | '^' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Deref_Tok  

            | '#' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Unequal_Tok  

            | ';' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Semicolon_Tok  

            | '[' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . OpenBracket_Tok  

            | ']' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . CloseBracket_Tok  

            | '{' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . OpenBrace_Tok  

            | '}' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . CloseBrace_Tok  

            | ')' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . CloseParen_Tok  

            | ',' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Comma_Tok  

            | '&' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Ampersand_Tok  

            | '|' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Stroke_Tok  

            | '*' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Star_Tok  

            | '/' 
            => BegOfTok ( ) 
            ; GCurrentTok . Tok := FM3Tok . Slash_Tok  

            ELSE 
              BegOfTok ( ) 
            ; INC ( Pos ) 
            ; LexErrorChars ( LbeStd . LeBadChars ) 
            END (* CASE *) 
          END (* IF *) 
       END (* LOOP *) 
       EXCEPT Backout 
      => <* FATAL Backout *> 
        BEGIN 
          BegOfTok ( ) 
        ; GCurrentTok . Tok := FLbeStd . Tok__Unknown  
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END Scan 

; BEGIN (* FM3Scanner *) 
    InitTables ( ) 
  ; LangUtil . RegisterScanner ( LbeStd . LangM3 , Scan ) 
  END FM3Scanner 
. 
