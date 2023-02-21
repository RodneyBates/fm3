
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
; IMPORT FM3Utils 
; IMPORT IntRanges 
; IMPORT IntCharVarArray AS VarArr_Char 
; IMPORT IntWideCharVarArray AS VarArr_WChar

(*
; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT LangUtil  
; IMPORT LbeStd 
; IMPORT M3InitTokStrings 
; IMPORT M3Tok 
; IMPORT PortTypes 
; IMPORT Strings
*)
; EXCEPTION Backout 

; TYPE IntRangeTyp = IntRanges . RangeTyp

(* New line and end-of-file characters: *) 
; CONST LF =  '\x0A' (* = '\n' *)
; CONST CR =  '\x0D' (* = '\r' *)
; CONST FF =  '\x0C' (* = '\f' *) 
; CONST VT =  '\x09' (* = '\t' *) 
; CONST NEL = '\x85'  
; CONST WLS =  W'\x2028'  
; CONST WPS =  W'\x2029'  
(* Also CR immediately followed by LF *)  
; CONST WEOF = W'\X7000' (* Use a unicode app-specific value. *) 
; CONST WNUL = W'\X7001' (* Use a unicode app-specific value. *) 
; CONST NUL = '\X00' 

; CONST WLastOfChar : WIDECHAR = LAST ( CHAR ) 

; CONST EOLCHARS = SET OF CHAR { LF , CR , FF , VT , NEL } 
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

; TYPE ScanStateTyp 
       = RECORD 
           SsHash : FM3Utils . HashTyp 
         ; SsLink : ScanStateRefTyp := NIL 
         ; SsUniRd : UniRd . T := NIL 
         ; SsUnitNo : INTEGER 
         ; SsLineNo : INTEGER 
         ; SsCharPos : INTEGER 
         ; SsIdentAtom : FM3Atom_OAChars . T (* Identifiers. *)  
         ; SsNumberAtom : FM3Atom_OAChars . T (* Numeric literals. *)  
         ; SsCharsAtom : FM3Atom_OAChars . T (* TEXT literals. *) 
         ; SsWideCharsAtom : FM3Atom_OAWideChars . T (* ^Wide TEXT literals. *)
         ; SsChars : VarArr_Char . T  
         ; SsWideChars : VarArr_WChar . T  
         ; SsFileName : TEXT 
         ; SsWCh : WIDECHAR 
         ; SsCh : CHAR 
         ; SsAtBegOfPragma := FALSE 
           (* ^The immediately preceding token was "<*". *)
         END (* ScanStateTyp *) 

; TYPE ScanStateRefTyp = REF ScanStateTyp 

; VAR GTopSsRef : ScanStateRefTyp := NIL 
  (* SsUniRd of every node on this stack is kept locked until popped. *) 
; VAR GScanStateCt := 0
; VAR GScanStateDepth := 0
; VAR GMaxScanStateDepth := 0   

(* EXPORTED: *) 
; PROCEDURE PushState 
     ( NewUniRd : UniRd . T ; FileName : TEXT ; UnitNo : INTEGER ) 
  (* PRE: NewUniRd is open and ready to be read. but not locked. *) 

  = VAR LSsRef : ScanStateRefTyp 

  ; BEGIN 
(* Thread . Acquire ( NewUniRd ) 
   Apparently, UniRd.T is NOT a MUTEX! 
    ; *) 
      LSsRef := NEW ( ScanStateRefTyp ) 
    ; LSsRef ^ . SsLink := GTopSsRef 
    ; LSsRef ^ . SsUnitNo := UnitNo 
    ; LSsRef ^ . SsUniRd := NewUniRd 
    ; LSsRef ^ . SsFileName := FileName 
    ; LSsRef ^ . SsLineNo := 0 
    ; LSsRef ^ . SsCharPos := 0   
    ; LSsRef ^ . SsAtBegOfPragma := FALSE 
    ; LSsRef ^ . SsIdentAtom 
        := FM3Atom_OAChars . New ( FM3Globals . IdentInitAtomSize ) 
    ; LSsRef ^ . SsNumberAtom 
        := FM3Atom_OAChars . New ( FM3Globals . NumberInitAtomSize ) 
    ; LSsRef ^ . SsCharsAtom 
        := FM3Atom_OAChars . New ( FM3Globals . TextInitAtomSize ) 
    ; LSsRef ^ . SsWideCharsAtom 
        := FM3Atom_OAWideChars . New ( FM3Globals . WideInitTextAtomSize ) 

    ; TRY 
        LSsRef ^ . SsWCh := UnsafeUniRd . FastGetWideChar( LSsRef ^ . SsUniRd ) 
      ; LSsRef ^ . SsCh := MIN ( GTopSsRef . SsWCh , WLastOfChar )  
      EXCEPT 
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
    ; UniRd . Close ( LSsRef . SsUniRd )  
(*  ; Thread . Release ( LSsRef . SsUniRd ) 
      Apparently, UniRd.T is NOT a MUTEX! *) 
    ; DEC ( GScanStateDepth ) 
    ; RETURN LSsRef ^ . SsUniRd   
    END PopState 

(* EXPORTED: *) 
; PROCEDURE CurrentUnitNo ( ) : INTEGER 

  = BEGIN 
      IF GTopSsRef = NIL THEN RETURN - 1 END (* IF *) 
    ; RETURN GTopSsRef . SsUnitNo 
    END CurrentUnitNo 

; PROCEDURE ErrorPos ( CharPos : INTEGER ; Msg : TEXT )
  (* Report at the beginning of the current token. *) 
 
  = BEGIN 
      FM3Errors . Err  
        ( GTopSsRef ^ . SsFileName 
        , GCurTokRef ^ . TrLineNo 
        , CharPos 
        , Msg 
        )
    END ErrorPos 

; PROCEDURE ErrorTok ( Msg : TEXT ; Adjust := 0 )
  (* Report relative to the beginning of the current token. *) 
 
  = BEGIN 
      FM3Errors . Err 
        ( GTopSsRef ^ . SsFileName 
        , GCurTokRef ^ . TrLineNo 
        , GCurTokRef ^ . TrCharPos + Adjust  
        , Msg 
        )
    END ErrorTok 

; PROCEDURE ErrorSs ( Msg : TEXT ; Adjust := 0 ) 
  (* Report at the current spot in the input *) 

  = BEGIN 
      FM3Errors . Err 
        ( GTopSsRef ^ . SsFileName 
        , GTopSsRef ^ . SsLineNo  
        , GTopSsRef ^ . SsCharPos + Adjust 
        , Msg 
        ) 
    END ErrorSs 

(* EXPORTED: *) 
; PROCEDURE NextTok ( ) 

  = VAR ScWCharVarArr : VarArr_WChar . T
  ; VAR ScCharVarArr : VarArr_Char . T  
  ; VAR ScHash : FM3Utils . HashTyp 
  ; VAR ScCh : CHAR 
  ; VAR ScAtBegOfPragma := FALSE 

  ; PROCEDURE BegOfPlainTok ( Tok : FM3Base . TokTyp := FM3Base . TokNull ) 
    RAISES { Backout } 

    = BEGIN (* BegOfPlainTok *) 
        GCurTokRef ^ . TrTok := Tok 
      ; GCurTokRef ^ . TrHash := FM3Utils . HashNull 
      ; GCurTokRef ^ . TrAtom := FM3Base . AtomNull 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := NIL 
      END BegOfPlainTok 

  ; PROCEDURE BegOfTokWInfo ( ) 
    RAISES { Backout } 

    = BEGIN (* BegOfTokWInfo *) 
(* TODO: INLINE-ME *) 
        GCurTokRef ^ . TrWCh := WNUL  
      ; GCurTokRef . TrHash := FM3Utils . GroundHash ( ) 
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
      ; TRY GTopSsRef . SsWCh 
              := UnsafeUniRd . FastGetWideChar ( GTopSsRef ^ . SsUniRd ) 
        EXCEPT 
        | Rd . EndOfFile , Rd . Failure 
        => GTopSsRef . SsWCh := WEOF 
        ;  GTopSsRef . SsCh := NUL 
        ; RETURN 
        END (*EXCEPT*) 
      ; GTopSsRef . SsCh := MIN ( GTopSsRef . SsWCh , WLastOfChar )  
      ; INC ( GTopSsRef . SsCharPos )
      ; IF GTopSsRef . SsCh = CR 
        THEN 
          LWCh2 := UnsafeUniRd . FastGetWideChar ( GTopSsRef ^ . SsUniRd ) 
        ; IF LWCh2 = LF 
          THEN INC ( GTopSsRef . SsCharPos )
          (* Leave GTopSsRef.SsCh and .SsWCh = CR: canonical new line. *)
          ELSE 
            <* ASSERT UnsafeUniRd . FastUnGetCodePoint 
                        ( GTopSsRef ^ . SsUniRd ) 
            *> 
          END (* IF *) 
        ELSIF GTopSsRef . SsCh IN  SET OF CHAR { LF , FF , VT , NEL } 
              OR GTopSsRef . SsWCh = WPS 
              OR GTopSsRef . SsWCh = WLS
        THEN  
          GTopSsRef . SsWCh := CR (* Canonical new line. *) 
        ; GTopSsRef . SsCh := CR 
        END (* IF *) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( ) 
    RAISES { Backout } 
    (* PRE: First bad char is in GTopSsRef . SsWCh. *)  

    = VAR LSs : INTEGER 
    ; VAR LCharCt : INTEGER 
    ; VAR LBadCharText : TEXT 


    ; BEGIN (* LexErrorChars *)  
        BegOfTokWInfo ( ) 
      ; GCurTokRef ^ . TrAtom := FM3Base . AtomNull 
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr := VarArr_WChar . New ( NUL , IntRangeTyp { 0 , 120 } ) 
      ; LCharCt := 0 
      ; WHILE ( GTopSsRef . SsWCh > WLastOfChar 
                AND GTopSsRef . SsWCh # WLS 
                AND GTopSsRef . SsWCh # WPS 
              ) OR NOT GTopSsRef . SsCh IN M3Chars 
        DO 
          LSs := VarArr_WChar . TouchedRange ( ScWCharVarArr ) . Hi + 1 
        ; VarArr_WChar . Assign ( ScWCharVarArr , LSs , GTopSsRef . SsWCh ) 
        ; INC ( LCharCt ) 
        ; NextChar ( )
        END (*WHILE*) 
      ; GCurTokRef ^ . TrWideChars 
          := FM3Utils . WCharVarArrayToOAWChar ( ScWCharVarArr ) 
      ; LBadCharText 
          := FM3Utils . WideTextLiteral ( GCurTokRef ^ . TrWideChars ) 
      ; ErrorTok 
          ( Fmt . Int ( LCharCt ) 
            & " illegal characters: " 
            & LBadCharText 
            ) 
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
          FM3Utils . ContribToHash 
            ( ScHash , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) ) 
        ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; VarArr_Char . Assign 
            ( ScCharVarArr , LSs , VAL ( ORD ( GTopSsRef . SsWCh ) , CHAR ) ) 
        ; NextChar ( ) 
        END (* WHILE *) 
      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef . TrChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; IF ScAtBegOfPragma 
           AND FM3Globals . PgRwDict . lookup  
                 ( GCurTokRef . TrChars , ScHash , (*OUT*) LRwTok ) 
        THEN GCurTokRef ^ . TrTok := LRwTok 
        ELSIF FM3Globals . M3RwDict . lookup  
                ( GCurTokRef . TrChars , ScHash , (*OUT*) LRwTok ) 
        THEN GCurTokRef ^ . TrTok := LRwTok 
        ELSE 
          GCurTokRef ^ . TrAtom 
            := FM3Atom_OAChars . MakeAtom 
                 ( GTopSsRef . SsIdentAtom
                 , GCurTokRef ^ . TrChars 
                 , ScHash 
                 ) 
        ; GCurTokRef ^ . TrTok := FM3Toks . TkIdent
        END (* IF *) 
      END IdentSuffix 

  ; PROCEDURE Number ( ) 
    RAISES { Backout } 
    (* PRE: GTopSsRef . SsCh is a digit.  TextTok not started. *)  

    = VAR LSs : INTEGER 
    ; VAR LTok : FM3Toks . TokTyp 

    ; BEGIN (* Number *) 
        BegOfTokWInfo ( ) 
      ; GCurTokRef . TrAtom := FM3Base . AtomNull (* Overlaid later? *) 
      ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 40 } ) 
      ; ScWCharVarArr := NIL 
      ; ScHash := FM3Utils. GroundHash ( ) 
      ; LTok := FM3Toks . TkIntLit 
      ; REPEAT 
          FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            )
        ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
        
      ; CASE GTopSsRef . SsCh
        OF '_' 
        => LTok := FM3Toks . TkBasedLit 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
            )
        ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh 
             IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
          THEN 
            REPEAT 
              FM3Utils . ContribToHash 
                ( (*IN OUT*) ScHash 
                , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                )
            ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
            ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
            ; NextChar ( )  
            UNTIL NOT GTopSsRef . SsCh 
                      IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 

          ELSE 
            ErrorSs ( "Based literal has no digit" ) 
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
              ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
              ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
              ; NextChar ( ) 
              UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            ELSE 
              ErrorSs ( "Floating point literal has no fractional digit" ) 
            END (* IF *) 
          END (* IF *) 
        ELSE 
          IF GTopSsRef . SsCh
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
          ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
          ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
          ; NextChar ( ) 
          ; IF GTopSsRef . SsCh IN SET OF CHAR { '+' , '-' }  
            THEN 
               FM3Utils . ContribToHash 
                 ( (*IN OUT*) ScHash 
                 , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                 )
            ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
            ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
            ; NextChar ( ) 
            END (* IF *) 
          ; IF GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            THEN 
              REPEAT 
                FM3Utils . ContribToHash 
                  ( (*IN OUT*) ScHash 
                  , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
                  )
              ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
              ; VarArr_Char . Assign ( ScCharVarArr , LSs , GTopSsRef . SsCh ) 
              ; NextChar ( ) 
              UNTIL NOT GTopSsRef . SsCh IN SET OF CHAR { '0' .. '9' } 
            ELSE 
              ErrorSs ( "Floating point literal has no exponent digit." ) 
            END (* IF *) 
          ELSE 
          END (*IF*)  
        END (*CASE*)  
      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef ^ . TrChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
(* CHECK: Is an atom for a numeric literal character string really needed? *) 
      ; GCurTokRef ^ . TrAtom 
          := FM3Atom_OAChars . MakeAtom 
               ( GTopSsRef ^ . SsNumberAtom 
               , GCurTokRef ^ . TrChars 
               , ScHash 
               ) 
      ; GCurTokRef ^ . TrTok := LTok 
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
    ; VAR LLoc : TEXT 

    ; BEGIN
        IF GTopSsRef . SsWCh # WEOF 
        THEN 
          IF GTopSsRef . SsCh # CR 
          THEN RETURN TRUE 
          ELSE LLoc := " at end of line." 
          END (*IF*)  
        ELSE LLoc := " at end of file."
        END (*IF*)  
      ; LMsg := Msg1 & LitTypeName [ Wide , Text ] & Msg2 & LLoc 
      ; ErrorSs ( LMsg )  
      ; RETURN FALSE 
      END LineCharExists 

  ; CONST EscapeChars = SET OF CHAR { '\t' , '\r' , '\f' , '\\' , '\'' , '\"' }
  ; CONST OctalDigits = SET OF CHAR { '0' .. '7' } 
  ; CONST HexDigits = SET OF CHAR { '0' .. '9' , 'a' .. 'f' , 'A' .. 'F' }
  ; CONST HexTagChars = SET OF CHAR { 'x' , 'X' } 
  ; CONST UnicodeTagChars = SET OF CHAR { 'u' , 'U' } 

  ; PROCEDURE IncludeChar ( WCh : WIDECHAR ; Wide : BOOLEAN ) 
    (* It might be in CHAR, or only in WIDECHAR. *) 

    = VAR LSs : INTEGER 

    ; BEGIN 
        FM3Utils . ContribToHash 
          ( (*IN OUT*) ScHash , VAL ( ORD ( WCh ) , FM3Utils . HashTyp ) )
      ; IF Wide 
        THEN 
          LSs := VarArr_WChar . TouchedRange ( ScWCharVarArr ) . Hi + 1 
        ; VarArr_WChar . Assign ( ScWCharVarArr , LSs , GTopSsRef . SsWCh ) 
        ELSE 
          LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; VarArr_Char . Assign 
            ( ScCharVarArr , LSs , VAL ( ORD  ( GTopSsRef . SsWCh ) , CHAR ) ) 
        END (* IF *) 
      ; NextChar ( ) 
      END IncludeChar  

  ; PROCEDURE EscapeSeq ( Wide , Text : BOOLEAN ) : WIDECHAR 
    (* PRE: GTopSsRef . SsCh is the backslash, not yet handled. *)
    (* POST: The sequence, possibly incorectly short, has been
             scanned. GTopSsRef . SsWCh/SsCh are the next char to scan. *) 

    = VAR LCharPos : INTEGER 
    ; VAR LIntVal : INTEGER 
    ; VAR LMaxIntVal : INTEGER 
    ; VAR LDigitChars : SET OF CHAR
    ; VAR LWCh : WIDECHAR 
    ; VAR LDigitCount : FM3Base . Card8Typ
    ; VAR LShift : FM3Base . Card8Typ
    ; VAR LChIntVal : FM3Base . Card8Typ
    ; VAR LPadDigitCt : FM3Base . Card8Typ 
    ; VAR LCh : CHAR 

    ; BEGIN 
        IF Wide THEN LMaxIntVal := 16_10FFFF ELSE LMaxIntVal := 16_FF END (*IF*)
      ; LCharPos := GTopSsRef . SsCharPos  
      ; NextChar ( ) (* Consume backslash.*) 
      ; IF NOT LineCharExists
                 ( "Empty escape sequence in " , Wide , Text 
                 , Msg2 := " literal."
                 )
        THEN RETURN WNUL
        END (*IF*) 
      ; IF GTopSsRef . SsCh 
           IN SET OF CHAR { '\n' , '\t' , '\r' , '\f' , '\\' , '\'' , '\"' } 
        THEN 
          LCh := GEscapeCharMap [ GTopSsRef . SsCh ] 
        ; IncludeChar ( LCh , Wide ) 
        ; RETURN LCh 
        END (*IF*)

      ; IF GTopSsRef . SsCh IN OctalDigits
        THEN 
          IF Wide THEN LDigitCount := 6 ELSE LDigitCount := 3 END (*IF*) 
        ; LDigitChars := OctalDigits 
        ; LShift := 3 
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
        ; LShift := 4   
        ELSIF GTopSsRef . SsCh IN UnicodeTagChars 
        THEN 
          IF NOT Wide 
          THEN  
            ErrorSs
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
        ; LShift := 4
        END (*IF*) 

      ; LIntVal := 0 
      ; LOOP (* Thru' LDigitCount chars in LDigitChars. *)
          IF NOT LineCharExists 
                   ( "Short escape sequence" , Wide , Text 
                   , Msg2 := " literal"
                   )
          THEN RETURN WNUL  
        ; ELSIF NOT GTopSsRef . SsCh IN LDigitChars
          THEN 
            ErrorSs
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
          END (*IF*) 
        END (*LOOP*) 
      ; IF LIntVal > LMaxIntVal 
        THEN 
          IF Wide THEN LPadDigitCt := 6 ELSE LPadDigitCt := 2 END (*IF*)
        ; ErrorPos 
            ( LCharPos 
            , "Out-of-range escape value in "
              & LitTypeName [ Wide , (*Text:=*) FALSE ]
              & " literal: 16_"
              & Fmt . Pad 
                  ( Fmt . Int ( LIntVal , base := 16 ) , LPadDigitCt , '0' )
              & ", max value is 16_"
              & Fmt . Pad 
                  ( Fmt . Int ( LMaxIntVal , base := 16 ) , LPadDigitCt , '0' )
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
        BegOfPlainTok ( )  
      ; NextChar ( ) (* Opening quote. *)
      
      ; IF NOT LineCharExists
                 ( "No character in " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN 
        ELSIF GTopSsRef . SsCh = '\\' 
        THEN LWCh := EscapeSeq ( Wide , Text := FALSE ) 
        ELSIF NOT Wide AND GTopSsRef . SsWCh > WLastOfChar 
        THEN 
          ErrorSs ( "Character literal is beyond the range of CHAR." ) 
        ELSE
          LWCh := GTopSsRef . SsWCh 
        ; NextChar ( ) 
        END (*IF*) 
      ; GCurTokRef ^ . TrWCh := LWCh 
      ; GCurTokRef ^ . TrHash := VAL ( ORD ( LWCh ) , LONGINT )   
      ; GCurTokRef ^ . TrAtom := ORD ( LWCh )  
      ; IF NOT LineCharExists
                 ( "No closing quote on " , Wide , Text := FALSE
                 , Msg2 := " literal"
                 )
        THEN
        ELSIF GTopSsRef . SsCh = '\'' 
        THEN NextChar ( ) 
        ELSE 
          ErrorSs
            ( "No closing quote on "
              & LitTypeName [ Wide , (*Text:=*) FALSE ]
              & " Literal"
            )
        END (* IF *) 
      END CharLit 

  ; PROCEDURE TextLit ( ) 
    RAISES { Backout } 
    (* PRE: GTopSsRef . SsCh is the opening double quote. 
            NO Tr fields have been initialized. *) 

    = VAR LSs : INTEGER 
    ; VAR LBadCharCt : INTEGER 
    ; VAR LCharVal : CHAR 
    ; VAR LLoc : TEXT 
    ; VAR LMsg : TEXT 

    ; BEGIN (* WideTextLit *) 
        BegOfTokWInfo ( )
      ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 100 } ) 
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
            UNTIL GTopSsRef . SsWCh <= WLastOfChar 
                  OR GTopSsRef . SsWCh = WEOF 
          ; ErrorSs 
              ( "Text literal has " 
                & Fmt . Int ( LBadCharCt ) 
                & " characters beyond the range of CHAR."
              , - LBadCharCt
              ) 
          ELSE (* Ordinary character. *) 
            LCharVal := GTopSsRef . SsWCh 
          ; NextChar ( ) 
          END (*IF*) 
        ; FM3Utils . ContribToHash 
            ( (*IN OUT*) ScHash 
            , VAL ( ORD ( LCharVal ) , FM3Utils . HashTyp ) 
            )
        ; LSs := VarArr_Char . TouchedRange ( ScCharVarArr ) . Hi + 1 
        ; VarArr_Char . Assign ( ScCharVarArr , LSs , LCharVal ) 
        END (*LOOP*) 

      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef ^ . TrChars 
          := FM3Utils . CharVarArrayToOAChar ( ScCharVarArr ) 
      ; GCurTokRef ^ . TrAtom 
          := FM3Atom_OAChars . MakeAtom 
               ( GTopSsRef ^ . SsCharsAtom 
               , GCurTokRef ^ . TrChars 
               , ScHash 
               ) 
      ; GCurTokRef ^ . TrTok := FM3Toks . TkTextLit 
      END TextLit 

  ; PROCEDURE WideTextLit ( ) 
    RAISES { Backout } 
    (* PRE: 'w' or 'W' has been consumed. 
            GTopSsRef . SsCh is the opening double quote. 
            NO Tr fields have been initialized. *) 

    = VAR LSs : INTEGER 
    ; VAR LWCharVal : WIDECHAR 
    ; VAR LLoc : TEXT 
    ; VAR LMsg : TEXT 

    ; BEGIN (* WideTextLit *) 
        BegOfTokWInfo ( )
      ; ScCharVarArr := NIL 
      ; ScWCharVarArr 
          := VarArr_WChar . New ( WNUL , IntRangeTyp { 0 , 100 } ) 
      ; NextChar ( ) (* Consume the opening double quote. *) 

      ; LOOP (* Thru' chars of text literal. *) 
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
        ; LSs := VarArr_WChar . TouchedRange ( ScWCharVarArr ) . Hi + 1 
        ; VarArr_WChar . Assign ( ScWCharVarArr , LSs , LWCharVal ) 
        END (*LOOP*) 

      ; GCurTokRef ^ . TrHash := ScHash 
      ; GCurTokRef ^ . TrWideChars 
          := FM3Utils . WCharVarArrayToOAWChar ( ScWCharVarArr ) 
      ; GCurTokRef ^ . TrAtom 
          := FM3Atom_OAWideChars . MakeAtom 
               ( GTopSsRef ^ . SsWideCharsAtom 
               , GCurTokRef ^ . TrWideChars 
               , ScHash 
               ) 
      ; GCurTokRef ^ . TrTok := FM3Toks . TkWideTextLit 
      END WideTextLit 

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { Backout } 
    (* PRE: Initial opening delimiter has been scanned and consumed. *) 

    = VAR LNestingDepth : INTEGER 

    ; BEGIN 
        LNestingDepth := 1 
      ; LOOP (* Thru chars in comment *) 
          IF GTopSsRef . SsWCh = WEOF  
          THEN 
            ErrorTok ( "Comment unclosed at end-of-file" )  
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
      ScAtBegOfPragma := GTopSsRef ^ . SsAtBegOfPragma 
    ; GTopSsRef ^ . SsAtBegOfPragma := FALSE 

    ; TRY 
        LOOP (* Skip uninteresting chars. *) 
          IF GTopSsRef . SsWCh = WEOF 
          THEN 
            GCurTokRef ^ . TrTok := FM3Toks . TkEOF 
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
          ELSIF GTopSsRef . SsCh IN SET OF CHAR { ' ' , CR , LF , FF , VT }  
          THEN NextChar ( ) 
          ELSE EXIT  
          END (*IF*) 
        END (*LOOP*) 

      ; GCurTokRef ^ . TrLineNo := GTopSsRef ^ . SsLineNo 
      ; GCurTokRef ^ . TrCharPos := GTopSsRef ^ . SsCharPos 
          
      ; CASE GTopSsRef . SsCh 
        OF 'w' , 'W'
        => ScCh := GTopSsRef . SsCh
        ; NextChar ( )
        ; IF GTopSsRef . SsCh = '\''
          THEN (* WIDECHAR literal. *) 
            CharLit ( Wide := TRUE ) 
          ; GCurTokRef ^ . TrTok := FM3Toks . TkWideCharLit  
          ELSIF GTopSsRef . SsCh = '\"'
          THEN (* Wide TEXT literal. *)
            WideTextLit ( )
          ELSE (* An identifier starting with w or W. *)
            BegOfTokWInfo ( )
          ; ScCharVarArr 
              := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 160 } ) 
          ; ScWCharVarArr := NIL 
          ; ScHash := FM3Utils. GroundHash ( ) 
          ; FM3Utils . ContribToHash 
              ( (*IN OUT*) ScHash 
              , VAL ( ORD ( GTopSsRef . SsCh ) , FM3Utils . HashTyp ) 
              )
          ; VarArr_Char . Assign ( ScCharVarArr , 0 , GTopSsRef . SsCh ) 
          ; NextChar ( ) 
          ; IdentSuffix ( ) 
          END (*IF*)

        | 'a' .. 'v' , 'x' .. 'z' , 'A' .. 'V' , 'X' .. 'Z' , '_'    
          (* Other identifier. *) 
        => BegOfTokWInfo ( )
        ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 80 } ) 
        ; ScWCharVarArr := NIL 
        ; ScHash := FM3Utils. GroundHash ( ) 
        ; IdentSuffix ( )

        | '0' .. '9' 
        => Number ( ) 

        | '\'' (* CHAR literal. *)
        => CharLit ( Wide := FALSE )  
        ; GCurTokRef ^ . TrTok := FM3Toks . TkCharLit  

        | '"' (* TEXT literal. *)  
        => BegOfTokWInfo ( )
        ; ScCharVarArr := VarArr_Char . New ( NUL , IntRangeTyp { 0 , 160 } ) 
        ; ScWCharVarArr := NIL
        ; TextLit ( ) 

        | '(' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '*' (* Opening comment delimiter. *)  
          THEN 
            NextChar ( ) 
          ; CommentSuffix ( ) 
          ELSE 
            GCurTokRef ^ . TrTok := FM3Toks . TkOpenParen
          END (* IF *) 

        | '<' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; CASE GTopSsRef . SsWCh 
          OF W'=' 
          => NextChar ( ) 
          ; GCurTokRef ^ . TrTok := FM3Toks . TkLessEqual  
          | W':' 
          => NextChar ( ) 
          ; GCurTokRef ^ . TrTok := FM3Toks . TkSubtype  
          | W'*' 
          => (* Opening pragma delimiter. *)
            NextChar ( )
          ; GTopSsRef ^ . SsAtBegOfPragma := TRUE 
          ; GCurTokRef ^ . TrTok := FM3Toks . TkOpenPragma 
          ELSE 
            GCurTokRef ^ . TrTok := FM3Toks . TkLess
          END (* CASE *) 

        | '*' 
        => BegOfPlainTok ( )
        ; NextChar ( )
        ; IF GTopSsRef . SsWCh = W'>'
          THEN (* Closing pragma delimiter. *) 
            NextChar ( )
          ; GCurTokRef ^ . TrTok := FM3Toks . TkClosePragma
          ELSE GCurTokRef ^ . TrTok := FM3Toks . TkStar
          END (*IF*) 

        | ':' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '=' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . TrTok := FM3Toks . TkBecomes  
          ELSE GCurTokRef ^ . TrTok := FM3Toks . TkColon  
          END (* IF *) 

        | '.' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '.' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . TrTok := FM3Toks . TkEllipsis  
          ELSE GCurTokRef ^ . TrTok := FM3Toks . TkDot 
          END (* IF *) 

        | '=' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '>' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . TrTok := FM3Toks . TkArrow 
          ELSE GCurTokRef ^ . TrTok := FM3Toks . TkEqual  
          END (* IF *) 

        | '>' 
        => BegOfPlainTok ( ) 
        ; NextChar ( ) 
        ; IF GTopSsRef . SsCh = '=' 
          THEN
            NextChar ( )
          ; GCurTokRef ^ . TrTok := FM3Toks . TkGreaterEqual 
          ELSE GCurTokRef ^ . TrTok := FM3Toks . TkGreater 
          END (* IF *) 

        | '+' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkPlus  

        | '-' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkMinus  

        | '^' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkDeref  

        | '#' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkUnequal  

        | ';' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkSemicolon  

        | '[' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkOpenBracket  

        | ']' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkCloseBracket  

        | '{' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkOpenBrace  

        | '}' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkCloseBrace  

        | ')' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkCloseParen  

        | ',' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkComma  

        | '&' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkAmpersand  

        | '|' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkStroke  

        | '/' 
        => BegOfPlainTok ( ) 
        ; NextChar ( )
        ; GCurTokRef ^ . TrTok := FM3Toks . TkSlash  

        ELSE (* Other values in CHAR *) 
          LexErrorChars ( ) 
        END (* CASE *) 

      EXCEPT Backout 
      => <* FATAL Backout *> 
        BEGIN 
          BegOfPlainTok ( ) 
        ; GCurTokRef ^ . TrTok := FM3Toks . TkUnknown  
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END NextTok 

; VAR GEscapeCharMap := ARRAY CHAR OF CHAR { '\X00' , .. } 

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


