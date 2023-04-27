
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3sfm3dict Modula-3 compiler.                   *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Scanner 

; IMPORT Fmt 
; IMPORT Rd
; IMPORT TextIntTbl
; IMPORT Thread 
; IMPORT UniRd 
; IMPORT UnsafeUniRd
; IMPORT Word 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_OAWideChars 
; IMPORT FM3Base
; IMPORT FM3Dict_OAChars_Int
; IMPORT FM3Errors 
; IMPORT FM3Globals
; IMPORT FM3Toks 
; IMPORT FM3Units
; IMPORT FM3Utils 

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

; CONST WLastOfChar : WIDECHAR = LAST ( CHAR ) 

; CONST M3Chars 
    = SET OF CHAR 
        { ' ' , '.' , ':' 
        , ';' , '*' , '/' , '<' , '>' , '=' , '#' , '|' , '^' , ',' , '&' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"'
        } 
      + EOLCHARS 

; VAR RwTable : TextIntTbl . T 
; VAR PgTable : TextIntTbl . T 

; PROCEDURE InitTables ( ) 

  = BEGIN 
    END InitTables 

; TYPE tScanAttribute = ScanStateTyp
; TYPE PositionTyp = RECORD Line , Column : FM3Base . Card32Typ 
; TYPE ScanStateTyp 
       = RECORD 
           Position : PositionTyp (* Accomodate lalr-generated parser. *)  
         ; SsLink : ScanStateRefTyp := NIL 
         ; SsUniRd : UniRd . T := NIL 
         ; SsUnitRef : FM3Units . UnitRefTyp 
         ; SsWCh : WIDECHAR 
         ; SsCh : CHAR 
         ; SsAtBegOfPragma := FALSE 
           (* ^The immediately-preceding token was "<*". *)
         END (* ScanStateTyp *) 

; TYPE ScanStateRefTyp = REF ScanStateTyp 

; VAR GTopSsRef : ScanStateRefTyp := NIL 
; VAR GScanStateCt := 0
; VAR GScanStateDepth := 0
; VAR GMaxScanStateDepth := 0   

(* EXPORTED: *) 
; PROCEDURE PushState 
     ( NewUniRd : UniRd . T ; UnitRef : FM3Units ; UnitRefTyp ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

  = VAR LSsRef : ScanStateRefTyp 

  ; BEGIN 
      LSsRef := NEW ( ScanStateRefTyp ) 
    ; LSsRef ^ . SsLink := GTopSsRef 
    ; LSsRef ^ . SsUnitRef := UnitRef
    ; LSsRef ^ . SsUniRd := NewUniRd 
    ; LSsRef ^ . Position . Line := 0 
    ; LSsRef ^ . Position . Column := 0   
    ; LSsRef ^ . SsAtBegOfPragma := FALSE 
    ; LSsRef ^ . SsUnitRef ^ . UntIdentAtomDict 
        := FM3Atom_OAChars . New 
             ( FM3Globals . IdentInitAtomSize , 1 , NIL ) 
    ; LSsRef ^ . SsUnitRef ^ . UntNumberAtomDict 
        := FM3Atom_OAChars . New 
             ( FM3Globals . NumberInitAtomSize , 1 , NIL ) 
    ; LSsRef ^ . SsUnitRef ^ . UntCharsAtomDict 
        := FM3Atom_OAChars . New 
             ( FM3Globals . TextInitAtomSize , 1 , NIL ) 
    ; LSsRef ^ . SsUnitRef ^ . UntWCharsAtomDict 
        := FM3Atom_OAWideChars . New 
             ( FM3Globals . WideInitTextAtomSize , 1 , NIL ) 

    ; TRY 
        LSsRef ^ . SsWCh := UnsafeUniRd . FastGetWideChar( LSsRef ^ . SsUniRd ) 
      ; IF GTopSsRef . SsWCh <= WLastOfChar 
        THEN GTopSsRef . SsCh := GTopSsRef . SsWCh 
        ELSE GTopSsRef . SsCh := NUL 
        END (*IF*) 
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

  = VAR LSsRef : ScanStateRefTyp 

  ; BEGIN 
      IF GTopSsRef = NIL THEN RETURN NIL END (* IF *) 
    ; LSsRef := GTopSsRef 
    ; GTopSsRef := LSsRef ^ . SsLink 
    ; TRY UniRd . Close ( LSsRef . SsUniRd ) 
      EXCEPT 
      | Thread . Alerted => (* Ignore *)  
      | Rd . Failure 
      => GTopSsRef . SsWCh := WEOF 
      ;  GTopSsRef . SsCh := NUL 
      END (*EXCEPT*) 
    ; DEC ( GScanStateDepth ) 
    ; RETURN LSsRef ^ . SsUniRd   
    END PopState 

(* EXPORTED: *) 
; PROCEDURE CurrentUnitNo ( ) : FM3Units . UnitNoTyp  

  = BEGIN 
      IF GTopSsRef = NIL THEN FM3Units . UnitNoNull END (* IF *) 
    ; RETURN GTopSsRef . SsUnitRef ^ . UntUnitNo  
    END CurrentUnitNo 

; PROCEDURE ErrorAtPos ( CharPos : INTEGER ; Msg : TEXT )
  (* Report at CharPos of the current line. *) 
 
  = BEGIN 
      FM3Errors . Err  
        ( GTopSsRef ^ . SsUnitRef ^ . UntSrcFileName 
        , Attribute ^ . SaLineNo 
        , CharPos 
        , Msg 
        )
    END ErrorAtPos 

; PROCEDURE ErrorAtTok ( Msg : TEXT ; Adjust := 0 )
  (* Report relative to the beginning of the current token. *) 
 
  = BEGIN 
      FM3Errors . Err 
        ( GTopSsRef ^ . SsUnitRef ^ . UntSrcFileName 
        , Attribute ^ . SaLineNo 
        , Attribute ^ . SaCharPos + Adjust  
        , Msg 
        )
    END ErrorAtTok 

; PROCEDURE ErrorAtSs ( Msg : TEXT ; Adjust := 0 ) 
  (* Report relative to the current spot in the input *) 

  = BEGIN 
      FM3Errors . Err 
        ( GTopSsRef ^ . SsUnitRef ^ . UntSrcFileName 
        , GTopSsRef ^ . Position . Line  
        , GTopSsRef ^ . Position . Column + Adjust 
        , Msg 
        ) 
    END ErrorAtSs 

; PROCEDURE AppendChar ( VarArr : VarArr_Char . T ; Ch : CHAR ) 

  = VAR LSs : INTEGER 

  ; BEGIN 
      LSs := VarArr_Char . TouchedRange ( VarArr ) . Hi + 1 
    ; VarArr_Char . Assign ( VarArr , LSs , Ch ) 
    END AppendChar 

; PROCEDURE AppendWChar ( VarArr : VarArr_WChar . T ; WCh : WIDECHAR ) 

  = VAR LSs : INTEGER 

  ; BEGIN 
      LSs := VarArr_WChar . TouchedRange ( VarArr ) . Hi + 1 
    ; VarArr_WChar . Assign ( VarArr , LSs , WCh ) 
    END AppendWChar 


(* EXPORTED: *) 
; PROCEDURE GetToken ( ) 

  = VAR ScWCharVarArr : VarArr_WChar . T
  ; VAR ScCharVarArr : VarArr_Char . T  
  ; VAR ScHash : FM3Utils . HashTyp 
  ; VAR ScCh : CHAR 
  ; VAR ScAtBegOfPragma := FALSE 

  ; PROCEDURE BegOfPlainTok ( ) 

    = BEGIN (* BegOfPlainTok *) 
        Attribute ^ . SaHash := FM3Utils . HashNull 
      ; Attribute ^ . SaAtom := FM3Base . AtomNull 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := NIL 
      END BegOfPlainTok 

  ; PROCEDURE NextChar ( ) 

    = VAR LWCh : WIDECHAR 

    ; BEGIN (* NextChar *) 
        IF GTopSsRef . SsCh = LF 
        THEN 
(* TODO: Decide how to indicate new line to client. *) 
          INC ( GTopSsRef . Position . Line ) 
        ; GTopSsRef . Position . Column := 0 
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
      ; INC ( GTopSsRef . Position . Column )
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
            INC ( GTopSsRef . Position . Column )
          ; GTopSsRef . SsWCh := WLF (* canonical new line. *)
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
      ; Attribute ^ . SaWideChars 
          := FM3Utils . WCharVarArrayToOAWChar ( ScWCharVarArr ) 
      ; LBadCharText 
          := FM3Utils . WideTextLiteral ( Attribute ^ . SaWideChars ) 
      ; ErrorAtTok 
          ( Fmt . Int ( LCharCt ) 
            & " illegal characters: " 
            & LBadCharText 
            ) 
      ; Attribute ^ . SaTok := FM3Toks . TkLexErrChars 
      END LexErrorChars 

  ; CONST IdentFollowChars 
            = SET OF CHAR { 'A' .. 'F' , 'a' .. 'f' , '0' .. '9' , '_' }

  ; PROCEDURE IdentSuffix ( ) 
    (* PRE: The identifier is already started, possibly non-empty. *) 

    = VAR LIntTok : INTEGER 

    ; BEGIN (* Ident *) 
        WHILE GTopSsRef . SsCh IN IdentFollowChars 
        DO 
          FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            ) 
        ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        END (* WHILE *) 
      ; Attribute ^ . SaHash := ScHash 
      ; Attribute ^ . SaChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; IF ScAtBegOfPragma 
           AND FM3Dict_OAChars_Int . LookupGrowable 
                 ( FM3Globals . PgRwDict 
                 , Attribute ^ . SaChars 
                 , ScHash 
                 , (*OUT*) LIntTok 
                 ) 
        THEN Attribute ^ . SaTok := LIntTok 
        ELSIF FM3Dict_OAChars_Int . LookupGrowable 
                ( FM3Globals . M3RwDict 
                , Attribute ^ . SaChars 
                , ScHash 
                , (*OUT*) LIntTok ) 
        THEN Attribute ^ . SaTok := LIntTok 
        ELSE 
          Attribute ^ . SaAtom 
            := FM3Atom_OAChars . MakeAtom 
                 ( GTopSsRef . SsUnitRef ^ . UntIdentAtomDict
                 , Attribute ^ . SaChars 
                 , ScHash 
                 ) 
        ; Attribute ^ . SaTok := FM3Toks . TkIdent
        END (* IF *) 
      END IdentSuffix 

  ; CONST DigitChars = SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' }

  ; PROCEDURE Number ( ) 
    (* PRE: GTopSsRef . SsCh is a digit.  TextTok not started. *)  

    = VAR LTok : FM3Toks . TokTyp 

    ; BEGIN (* Number *) 
        ScHash := FM3Utils . GroundHash ( ) 
      ; Attribute ^ . SaAtom := FM3Base . AtomNull (* Overlaid later? *) 
      ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 40 } ) 
      ; ScWCharVarArr := NIL 
      ; LTok := FM3Toks . TkIntLit 
      ; REPEAT 
          FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            )
        ; AppendChar ( ScCharVarArr , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
        
      ; CASE GTopSsRef . SsCh
        OF '_' 
        => LTok := FM3Toks . TkBasedLit 
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
          ELSE 
            ErrorAtSs ( "Based literal has no digit" ) 
          END (* IF *) 

        | '.' 
        => NextChar ( ) 
        ; IF GTopSsRef . SsCh = '.' 
          THEN 
            <* ASSERT UnsafeUniRd . FastUnGetCodePoint  
                        ( GTopSsRef ^ . SsUniRd ) 
            *> 
          ELSE
            LTok := FM3Toks . TkRealLit 
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
              ErrorAtSs ( "Floating point literal has no fractional digit" ) 
            END (* IF *) 
          END (* IF *) 
        ; IF GTopSsRef . SsCh
             IN SET OF CHAR { 'E' , 'e' , 'D' , 'd' , 'X' , 'x' }  
          THEN 
            IF GTopSsRef . SsCh IN SET OF CHAR { 'D' , 'd' }
            THEN LTok := FM3Toks . TkLongRealLit 
            ELSIF GTopSsRef . SsCh IN SET OF CHAR { 'X' , 'x' }
            THEN LTok := FM3Toks . TkExtendedLit 
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
              ErrorAtSs ( "Floating point literal has no exponent digit." ) 
            END (* IF *) 
          ELSE 
          END (*IF*)  
        ELSE 
        END (*CASE*)  
      ; Attribute ^ . SaHash := ScHash 
      ; Attribute ^ . SaChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
(* CHECK: Is an atom for a numeric literal character string really needed? 
          Probably only the binary version of the value. *) 
      ; Attribute ^ . SaAtom 
          := FM3Atom_OAChars . MakeAtom 
               ( GTopSsRef ^ . SsUnitRef ^ . UntNumberAtomDict 
               , Attribute ^ . SaChars 
               , ScHash 
               ) 
      ; Attribute ^ . SaTok := LTok 
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

    = VAR LMsg : TEXT 
    ; VAR LLoc : TEXT 

    ; BEGIN
        IF GTopSsRef . SsWCh # WEOF 
        THEN 
          IF GTopSsRef . SsCh # LF 
          THEN RETURN TRUE 
          ELSE LLoc := " at end of line." 
          END (*IF*)  
        ELSE LLoc := " at end of file."
        END (*IF*)  
      ; LMsg := Msg1 & LitTypeName [ Wide , Text ] & Msg2 & LLoc 
      ; ErrorAtSs ( LMsg )  
      ; RETURN FALSE 
      END LineCharExists 

  ; CONST EscapeChars 
      = SET OF CHAR { '\n' , '\t' , '\r' , '\f' , '\\' , '\'' , '\"' }
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
              ( "Unicode escape in non-wide "
                & LitTypeName [ Wide , (*Text:=*) FALSE ]
                & " literal."
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
              ( "Short escape sequence in "
                & LitTypeName [ Wide , (*Text:=*) FALSE ]
                & " literal."
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
                     ( "Short escape sequence" , Wide , Text 
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
            ( LCharPos 
            , "Out-of-range escape value in "
              & LitTypeName [ Wide , (*Text:=*) FALSE ]
              & " literal: " & LBaseText 
              & Fmt . Pad 
                  ( Fmt . Int ( LIntVal , base := LBase ) , LPadDigitCt , '0' )
              & ", max value is " & LBaseText 
              & Fmt . Pad 
                  ( Fmt . Int ( LMaxIntVal , base := LBase ) , LPadDigitCt , '0' )
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
            ( "Character literal value 16_" 
              & Fmt . Pad 
                  ( Fmt . Int ( ORD ( GTopSsRef . SsWCh ) , base := 16 ) , 6 , '0' ) 
              & " is beyond the range of CHAR."
            )
        ; LWCh := WNUL 
        ELSE
          LWCh := GTopSsRef . SsWCh 
        ; NextChar ( ) 
        END (*IF*) 
      ; Attribute ^ . SaWCh := LWCh 
      ; Attribute ^ . SaHash := VAL ( ORD ( LWCh ) , LONGINT )   
      ; Attribute ^ . SaAtom := ORD ( LWCh )  
      ; IF NOT LineCharExists
                 ( "No closing quote on " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN
        ELSIF GTopSsRef . SsCh = '\'' 
        THEN NextChar ( ) 
        ELSE 
          ErrorAtSs
            ( "No closing quote on "
              & LitTypeName [ Wide , (*Text:=*) FALSE ]
              & " Literal."
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
              ( "Text literal has " 
                & Fmt . Int ( LBadCharCt ) 
                & " characters beyond the range of CHAR."
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

      ; Attribute ^ . SaHash := ScHash 
      ; Attribute ^ . SaChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; Attribute ^ . SaAtom 
          := FM3Atom_OAChars . MakeAtom 
               ( GTopSsRef ^ . SsUnitRef ^ . UntCharsAtomDict 
               , Attribute ^ . SaChars 
               , ScHash 
               ) 
      ; ScCharVarArr := NIL 
      ; Attribute ^ . SaTok := FM3Toks . TkTextLit 
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

      ; Attribute ^ . SaHash := ScHash 
      ; Attribute ^ . SaWideChars 
          := FM3Utils . WCharVarArrayToOAWChar ( ScWCharVarArr ) 
      ; Attribute ^ . SaAtom 
          := FM3Atom_OAWideChars . MakeAtom 
               ( GTopSsRef ^ . SsUnitRef ^ . UntWCharsAtomDict 
               , Attribute ^ . SaWideChars 
               , ScHash 
               ) 
      ; ScWCharVarArr := NIL 
      ; Attribute ^ . SaTok := FM3Toks . TkWideTextLit 
      END WideTextLit 

  ; PROCEDURE CommentSuffix ( ) 
    (* PRE: Initial opening delimiter has been scanned and consumed. *) 

    = VAR LNestingDepth : INTEGER 

    ; BEGIN 
        LNestingDepth := 1 
      ; LOOP (* Thru chars in comment *) 
          IF GTopSsRef . SsWCh = WEOF  
          THEN 
            ErrorAtTok ( "Comment unclosed at end-of-file" )  
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
      ScAtBegOfPragma := GTopSsRef ^ . SsAtBegOfPragma 
    ; GTopSsRef ^ . SsAtBegOfPragma := FALSE 
    ; Attribute ^ . SaWCh := WNUL  
    ; Attribute ^ . SaAtom := FM3Base . AtomNull 
    ; LOOP (* Skip non-token chars. *) 
        IF GTopSsRef . SsWCh = WEOF 
        THEN 
          Attribute ^ . SaTok := FM3Toks . TkEOF 
        ; RETURN 
        ELSIF GTopSsRef . SsWCh > WLastOfChar 
        THEN 
          IF GTopSsRef . SsWCh = WLS OR GTopSsRef . SsWCh = WPS 
             (* ^Unicode end-of-line code points, *)  
          THEN NextChar ( ) 
          ELSE 
            LexErrorChars ( ) 
          ; EXIT 
          END (*IF*) 
        ELSIF GTopSsRef . SsCh 
              IN SET OF CHAR { ' ' , CR , LF , FF , VT , TAB }  
        THEN NextChar ( ) 
        ELSE EXIT  
        END (*IF*) 
      END (*LOOP*) 

    ; Attribute ^ . SaLineNo := GTopSsRef ^ . Position . Line 
    ; Attribute ^ . SaCharPos := GTopSsRef ^ . Position . Column 

    ; CASE GTopSsRef . SsCh 
      OF 'w' , 'W'
      => ScCh := GTopSsRef . SsCh
      ; NextChar ( )
      ; IF GTopSsRef . SsCh = '\''
        THEN (* WIDECHAR literal. *) 
          CharLit ( Wide := TRUE ) 
        ; Attribute ^ . SaTok := FM3Toks . TkWideCharLit  
        ELSIF GTopSsRef . SsCh = '\"'
        THEN (* Wide TEXT literal. *)
          WideTextLit ( )
        ELSE (* An identifier starting with w or W. *)
          Attribute ^ . SaWCh := WNUL  
        ; ScHash := FM3Utils . GroundHash ( ) 
        ; ScCharVarArr 
            := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 160 } ) 
        ; ScWCharVarArr := NIL 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            ) (* The 'w' or 'W'. *) 
        ; VarArr_Char . Assign ( ScCharVarArr , 0 , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        ; IdentSuffix ( ) 
        END (*IF*)

      | 'a' .. 'v' , 'x' .. 'z' , 'A' .. 'V' , 'X' .. 'Z' 
        (* Other identifier. *) 
      => Attribute ^ . SaWCh := WNUL  
      ; ScHash := FM3Utils . GroundHash ( ) 
      ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 160 } ) 
      ; ScWCharVarArr := NIL 
      ; IdentSuffix ( )

      | '0' .. '9' 
      => Number ( ) 

      | '\'' (* CHAR literal. *)
      => CharLit ( Wide := FALSE )  
      ; Attribute ^ . SaTok := FM3Toks . TkCharLit  

      | '"' (* TEXT literal. *)  
      => TextLit ( ) 

      | '(' 
      => BegOfPlainTok ( ) 
      ; NextChar ( ) 
      ; IF GTopSsRef . SsCh = '*' (* Opening comment delimiter. *)  
        THEN 
          NextChar ( ) 
        ; CommentSuffix ( ) 
        ELSE 
          Attribute ^ . SaTok := FM3Toks . TkOpenParen
        END (* IF *) 

      | '<' 
      => BegOfPlainTok ( ) 
      ; NextChar ( ) 
      ; CASE GTopSsRef . SsCh 
        OF '=' 
        => NextChar ( ) 
        ; Attribute ^ . SaTok := FM3Toks . TkLessEqual  
        | ':' 
        => NextChar ( ) 
        ; Attribute ^ . SaTok := FM3Toks . TkSubtype  
        | '*' 
        => (* Opening pragma delimiter. *)
          NextChar ( )
        ; GTopSsRef ^ . SsAtBegOfPragma := TRUE 
        ; Attribute ^ . SaTok := FM3Toks . TkOpenPragma 
        ELSE 
          Attribute ^ . SaTok := FM3Toks . TkLess
        END (* CASE *) 

      | '*' 
      => BegOfPlainTok ( )
      ; NextChar ( )
      ; IF GTopSsRef . SsCh = '>'
        THEN (* Closing pragma delimiter. *) 
          NextChar ( )
        ; Attribute ^ . SaTok := FM3Toks . TkClosePragma
        ELSE Attribute ^ . SaTok := FM3Toks . TkStar
        END (*IF*) 

      | ':' 
      => BegOfPlainTok ( ) 
      ; NextChar ( ) 
      ; IF GTopSsRef . SsCh = '=' 
        THEN
          NextChar ( )
        ; Attribute ^ . SaTok := FM3Toks . TkBecomes  
        ELSE Attribute ^ . SaTok := FM3Toks . TkColon  
        END (* IF *) 

      | '.' 
      => BegOfPlainTok ( ) 
      ; NextChar ( ) 
      ; IF GTopSsRef . SsCh = '.' 
        THEN
          NextChar ( )
        ; Attribute ^ . SaTok := FM3Toks . TkEllipsis  
        ELSE Attribute ^ . SaTok := FM3Toks . TkDot 
        END (* IF *) 

      | '=' 
      => BegOfPlainTok ( ) 
      ; NextChar ( ) 
      ; IF GTopSsRef . SsCh = '>' 
        THEN
          NextChar ( )
        ; Attribute ^ . SaTok := FM3Toks . TkArrow 
        ELSE Attribute ^ . SaTok := FM3Toks . TkEqual  
        END (* IF *) 

      | '>' 
      => BegOfPlainTok ( ) 
      ; NextChar ( ) 
      ; IF GTopSsRef . SsCh = '=' 
        THEN
          NextChar ( )
        ; Attribute ^ . SaTok := FM3Toks . TkGreaterEqual 
        ELSE Attribute ^ . SaTok := FM3Toks . TkGreater 
        END (* IF *) 

      | '+' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkPlus  

      | '-' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkMinus  

      | '^' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkDeref  

      | '#' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkUnequal  

      | ';' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkSemicolon  

      | '[' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkOpenBracket  

      | ']' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkCloseBracket  

      | '{' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkOpenBrace  

      | '}' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkCloseBrace  

      | ')' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkCloseParen  

      | ',' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkComma  

      | '&' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkAmpersand  

      | '|' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkStroke  

      | '/' 
      => BegOfPlainTok ( ) 
      ; NextChar ( )
      ; Attribute ^ . SaTok := FM3Toks . TkSlash  

      ELSE (* Other values in CHAR *) 
        LexErrorChars ( ) 
      END (* CASE *) 
    END GetToken 

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

; BEGIN (* FM3Scanner *) 
    InitEscapeCharMap ( ) 
  ; InitDigitCharMap ( ) 
  END FM3Scanner 
. 


