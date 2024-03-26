        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3CLArgs

; IMPORT Atom 
; IMPORT AtomList 
; IMPORT FileWr
; IMPORT OSError
; IMPORT Params
; IMPORT Pathname 
; IMPORT Rd 
; IMPORT Stdio
; IMPORT Text 
; IMPORT Wr

; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3CLToks 
; IMPORT FM3CLToks AS Clt
; IMPORT FM3Files 
; IMPORT FM3Globals
; IMPORT FM3LexTable 
; IMPORT FM3Messages 
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3TextColors 
; IMPORT FM3Version  

; EXCEPTION TerminateCL ( TEXT (* Message. *) )

; PROCEDURE AppendTextToList ( VAR List : AtomList . T ; Txt : TEXT )

  = BEGIN (*AppendTextToList*) 
      List := AtomList . Cons ( Atom . FromText ( Txt ) , List )
    END AppendTextToList 


; PROCEDURE AssignOptionSetElem
    ( VAR Set : FM3CLOptions . OptionTokSetTyp
    ; Elem : FM3CLOptions . OptionTokTyp
    ; Value : BOOLEAN
    )

  = BEGIN (*AssignOptionSetElem*)
      IF Value
      THEN FM3CLOptions . InclOptionTok ( (*IN OUT*) Set , Elem ) 
      ELSE FM3CLOptions . ExclOptionTok ( (*IN OUT*) Set , Elem ) 
      END (*IF*) 
    END AssignOptionSetElem  

; PROCEDURE AlterOptionSet
    ( VAR Set : FM3CLOptions . OptionTokSetTyp
    ; Changes : FM3CLOptions . OptionTokSetTyp
    ; Include : BOOLEAN (* Otherwise, exclude. *) 
    )

  = BEGIN (*AlterOptionSet*)
      IF Include
      THEN FM3CLOptions . OptionTokSetUnion ( (*IN OUT*) Set , Changes ) 
      ELSE FM3CLOptions . OptionTokSetDiff ( (*IN OUT*) Set , Changes ) 
      END (*IF*) 
    END AlterOptionSet 

; PROCEDURE AlterPassNos
    ( VAR Set : FM3CLOptions . PassNoSetTyp
    ; Changes : FM3CLOptions . PassNoSetTyp
    ; Include : BOOLEAN (* Otherwise, exclude. *) 
    )

  = BEGIN (*AlterPassNos*)
      IF Include
      THEN FM3CLOptions . PassNoSetUnion ( (*IN OUT*) Set , Changes ) 
      ELSE FM3CLOptions . PassNoSetDiff ( (*IN OUT*) Set , Changes ) 
      END (*IF*) 
    END AlterPassNos 

; PROCEDURE SingleDigitParam ( Param : TEXT ) : INTEGER
  RAISES { TerminateCL } 

  = VAR LResult : INTEGER

  ; BEGIN (*SingleDigitParam*)
      IF Text . Length ( Param ) # 1
      THEN RAISE TerminateCL ( "Parameter must be a single digit." )
      END (*IF*)
    ; LResult := ORD ( Text . GetChar ( Param , 0 ) ) - ORD ( '0' )
    ; RETURN LResult  
    END SingleDigitParam

; PROCEDURE ParseArgs ( )
  RAISES { FM3SharedUtils . Terminate } 

  = VAR PaArgCt : INTEGER
  ; VAR PaArgNo : INTEGER
  ; VAR PaArgLen : INTEGER
  ; VAR PaArgSs : INTEGER 
  ; VAR PaArgText : TEXT

  ; PROCEDURE PaFetchArg ( MinLen : INTEGER := 1 ) RAISES { TerminateCL } 

    = BEGIN
        PaArgText := Params . Get ( PaArgNo )
      ; IF PaArgText = NIL THEN PaArgText := "" END (*IF*)
      ; PaArgLen := Text . Length ( PaArgText )
      ; PaArgSs := 0 
      ; IF PaArgLen < MinLen
        THEN RAISE TerminateCL ( "Arg too short.")
        END (*IF*)
      END PaFetchArg

  ; PROCEDURE PaFindParam ( ) RAISES { TerminateCL }

    = VAR LResult : TEXT

    ; BEGIN
        IF PaArgSs < PaArgLen 
        THEN (* We're already here. *) 
        ELSE
          IF PaArgNo >= PaArgCt
          THEN (* No value. *)
            RAISE TerminateCL ( "Arg requires a param."  ) 
          END (*IF*) 
        ; INC ( PaArgNo )
        ; IF PaArgNo >= PaArgCt
          THEN
            RAISE TerminateCL ( "Arg requires a param."  ) 
          END (*IF*) 
        ; PaFetchArg ( MinLen := 1 ) 
        END (*IF*)
      END PaFindParam

  ; PROCEDURE PaNoNo ( No : BOOLEAN ) RAISES { TerminateCL } 

    = BEGIN (*PaNoNo*)
        IF No
        THEN
          PaArgSs := 2 
        ; RAISE TerminateCL ( "Option does not support the \"no-\" prefix." )
        END (*IF*) 
      END PaNoNo 

  ; CONST SingleDigits = SET OF CHAR { '1' .. '9' }

  ; PROCEDURE PaPassNoSet
      ( VAR Set : FM3CLOptions . PassNoSetTyp
      ; Include : BOOLEAN (* Otherwise, exclude. *) 
      )
    RAISES { TerminateCL } 
  
    = VAR LPassNo : INTEGER
    ; VAR LChar : CHAR

    ; BEGIN (*PaPassNoSet*)
        WHILE PaArgSs < PaArgLen
        DO
          LChar := Text . GetChar ( PaArgText , PaArgSs )
        ; IF NOT LChar IN SingleDigits 
          THEN RAISE TerminateCL  ( "Pass number must be a digit." ) 
          END (*IF*)
        ; LPassNo := ORD ( LChar ) - ORD ( '0' )
        ; IF NOT LPassNo IN FM3CLOptions . PassNoSetValid
          THEN RAISE TerminateCL ( "Invalid pass number." ) 
          END (*IF*) 
        ; IF Include
          THEN FM3CLOptions . InclPassNo ( Set , LPassNo )
          ELSE FM3CLOptions . ExclPassNo ( Set , LPassNo )
          END (*IF*) 
        ; INC ( PaArgSs ) 
        END (*WHILE*)
      END PaPassNoSet

  ; PROCEDURE PaTwoHyphenArg ( ) RAISES { TerminateCL } 
    (* PRE: Arg starts with two hyphens, PaArgSs = 2. *) 

    = VAR LParam : TEXT
    ; VAR LLexState : FM3LexTable . StateNoTyp 
    ; VAR LLexValue : FM3LexTable . ValueTyp
    ; VAR LChar : CHAR 
    ; VAR LNo : BOOLEAN
    ; VAR LHasEquals : BOOLEAN
(*TODO: Check that arg allows '='. *) 

    ; BEGIN
        IF PaArgLen >= 5
           AND Text . Equal ( Text . Sub ( PaArgText , 2 , 3 ) , "no-" )
        THEN
          LNo := TRUE (* Confusing? *)
        ; INC ( PaArgSs , 3 )  
        ELSE LNo := FALSE 
        END (*IF*)
      ; IF PaArgSs >= PaArgLen
        THEN RAISE TerminateCL  ( "Incomplete arg." )
        END (*IF*)
        
      ; IF FM3CLOptions . OptionsLexTable = NIL
        THEN
          FM3CLOptions . OptionsLexTable
            := FM3Files . ReadFsm
                 ( "Clt" , FM3SharedGlobals . FM3FileKindCltPkl )
        END (*IF*)

      ; LHasEquals := FALSE 
      ; LLexState := FM3LexTable . IncrInit ( FM3CLOptions . OptionsLexTable )
      ; LOOP
          (* INVARIANT: Lex machine needs a(nother) character.
             The next one is at PaArgSs, but not fetched.
          *) 
          IF PaArgSs >= PaArgLen
          THEN LChar := FM3LexTable . NullChar
          ELSE
            LChar := Text . GetChar ( PaArgText , PaArgSs )
          ; IF LChar = '='
            THEN
              LChar := FM3LexTable . NullChar
            ; LHasEquals := TRUE 
            ; INC ( PaArgSs )
            END (*IF*) 
          ; INC ( PaArgSs )
          END (*IF*) 
        ; LLexValue
            := FM3LexTable . IncrNext
                 ( FM3CLOptions . OptionsLexTable
                 , LChar
                 , (*IN OUT*) LLexState
                 )
        ; IF LLexValue = FM3LexTable . ValueUnrecognized 
          THEN RAISE TerminateCL  ( "Unrecognized arg." )
          ELSIF LLexValue = FM3LexTable . ValueNull
          THEN (* LexTable wants more characters. *)
            (* And loop. *)
          ELSE (* A recognized option, but it might be only a proper prefix. *)
            (* This is made very messy by virtue that LexTable sometimes can
               and will recognize a string without seeing its successor, if
               any following character would make for an unrecognizable string.
               Otherwise, LexTable will need to receive a trailing NullChar
               before it recognizes the string.
            *) 
            IF LChar # FM3LexTable . NullChar
            THEN (* LexTable recognized the option from its last character. *)
              IF PaArgSs < PaArgLen
                 AND Text . GetChar ( PaArgText , PaArgSs ) = '='
              THEN (* Consume and note the equal sign. *) 
                LHasEquals := TRUE 
              ; INC ( PaArgSs )
              END (*IF*) 
            END (*IF*) 
          ; IF LHasEquals OR PaArgSs >= PaArgLen  
            THEN (* No more chars in PaArgText. *)
              EXIT
            ELSE (* There is an extra trailing character, which will make it
                    urecognizable after all.
                 *) 
              RAISE TerminateCL  ( "Unrecognized arg." )
            END (*IF*) 
          END (*IF*) 
        END (*LOOP*) 

      ; CASE LLexValue 
        OF
        | Clt . CltVersion
        => IF NOT LNo
           THEN (* OK, so this is silly.  But as much on the user's part. *) 
             DisplayVersion ( )
           ; RAISE FM3SharedUtils . Terminate ( NIL )
           END (*IF*) 
        
        | Clt . CltHelp 
        => IF NOT LNo
           THEN (* OK, so this is silly.  But as much on the user's part. *) 
             DisplayVersion ( )
           ; DisplayHelp ( ) 
           ; RAISE FM3SharedUtils . Terminate ( NIL ) 
           END (*IF*) 
        
        | Clt . CltSrcFile  
        => PaNoNo ( LNo )
        ; PaFindParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; AppendTextToList ( FM3CLOptions . SourceFileNames , LParam )
        ; FM3CLOptions . SrcFileName := LParam  
        
        | Clt . CltSrcDir  
        => PaNoNo ( LNo )
        ; PaFindParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; AppendTextToList ( FM3CLOptions . SourceDirNames , LParam ) 
        
        | Clt . CltImportDir  
        => PaNoNo ( LNo )
        ; PaFindParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; AppendTextToList ( FM3CLOptions . ImportDirNames , LParam ) 
        
        | Clt . CltResourceDir  
        => PaNoNo ( LNo )
        ; PaFindParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; FM3CLOptions . ResourceDirName := LParam
        
        | Clt . CltBuildDir 
        => PaNoNo ( LNo )
        ; PaFindParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; FM3CLOptions . BuildDir := LParam
        
        | Clt . CltKeepPasses 
        => PaFindParam ( )
        ; PaPassNoSet ( FM3CLOptions . PassNosToKeep , NOT LNo ) 
        
        | Clt . CltKeep 
        => AlterPassNos
             ( FM3CLOptions . PassNosToKeep
             , FM3CLOptions . PassNoSetValid
             , NOT LNo
             ) 

        | Clt . CltDisAsmPasses 
        => PaFindParam ( )
        ; PaPassNoSet ( FM3CLOptions . PassNosToDisAsm , NOT LNo ) 
        
        | Clt . CltDisAsm 
        => AlterPassNos
             ( FM3CLOptions . PassNosToDisAsm
             , FM3CLOptions . PassNoSetValid
             , NOT LNo
             ) 
        
        (* Binary, parameterless options: *) 
        | Clt . CltStdErr  
        , Clt . CltStdOut  
        , Clt . CltFM3Log
        , Clt . CltUnitLog 
        => AssignOptionSetElem 
             ( FM3CLOptions . OptionTokSet , LLexValue , Value := NOT LNo ) 
        
        ELSE RAISE TerminateCL ( "Unknown arg." ) 
        END (*CASE*)
      ; INC ( PaArgNo )
      END PaTwoHyphenArg 

  ; PROCEDURE PaHyphenArg ( ArgString : TEXT )
    RAISES { TerminateCL , FM3SharedUtils . Terminate } 
    (* PRE: Arg starts with one hyphen only. *) 

    = VAR LParam : TEXT
    ; VAR LArgLength : INTEGER 
    ; VAR LPassNo : INTEGER 
    ; VAR LArgChar : CHAR 

    ; BEGIN
        IF PaArgLen <= 1 THEN RAISE TerminateCL ( "Missing arg." ) END (*IF*)
      ; PaArgSs := 1
      ; LOOP 
          LArgChar := Text . GetChar ( ArgString , PaArgSs )
        ; INC ( PaArgSs )
        ; CASE LArgChar OF 
          | 'v'
          =>  DisplayVersion ( )
            ; RAISE FM3SharedUtils . Terminate ( NIL )
         
          | 'h'
          =>  DisplayVersion ( )
            ; DisplayHelp ( )
            ; RAISE FM3SharedUtils . Terminate ( NIL )
           
          | 's' (* Source file. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; AppendTextToList ( FM3CLOptions . SourceFileNames , LParam )
            ; FM3CLOptions . SrcFileName := LParam 

          | 'S' (* Source directory. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; AppendTextToList ( FM3CLOptions . SourceDirNames , LParam ) 
        
          | 'B'
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; FM3CLOptions . BuildDir := LParam

          | 'k' (* Keep one pass. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
            ; LPassNo := SingleDigitParam ( LParam ) 
            ; IF NOT LPassNo IN FM3CLOptions . PassNoSetValid
              THEN RAISE TerminateCL ( "Invalid pass number." ) 
              END (*IF*) 
            ; FM3CLOptions . InclPassNo
               ( FM3CLOptions . PassNosToKeep , LPassNo )

          | 'K' (* Keep, all passes. *) 
          => AlterPassNos
               ( FM3CLOptions . PassNosToKeep
               , FM3CLOptions . PassNoSetValid
               , Include := TRUE 
               ) 
                
          | 'd' (* Disassemble one pass. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
            ; LPassNo := SingleDigitParam ( LParam )
            ; IF NOT LPassNo IN FM3CLOptions . PassNoSetValid
              THEN RAISE TerminateCL ( "Invalid pass number." ) 
              END (*IF*) 
            ; FM3CLOptions . InclPassNo
               ( FM3CLOptions . PassNosToDisAsm , LPassNo )
               
          | 'D' (* Disassemble , all passes. *) 
          => AlterPassNos
               ( FM3CLOptions . PassNosToDisAsm
               , FM3CLOptions . PassNoSetValid
               , Include := TRUE 
               ) 
                
          | 'I'
          =>  PaFindParam ( ) 
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; AppendTextToList ( FM3CLOptions . ImportDirNames , LParam ) 

          ELSE RAISE TerminateCL  ( "Unknown arg." )
          END (*CASE*)
        ; INC ( PaArgSs )
        ; IF PaArgSs >= PaArgLen THEN EXIT END (*IF*) 
        END (*LOOP*) 
      END PaHyphenArg 

  ; BEGIN (* ParseArgs *) 
      PaArgCt := Params . Count 
    ; PaArgNo := 1

    ; TRY 
        WHILE PaArgNo < PaArgCt DO
          PaFetchArg ( MinLen := 1 )
        ; IF PaArgLen >= 1 AND Text . GetChar ( PaArgText , 0 ) = '-'
          THEN
            IF PaArgLen >= 2 AND Text . GetChar ( PaArgText , 1 ) = '-' 
            THEN (* Two hyphens.*) 
              INC ( PaArgSs , 2 )
            ; PaTwoHyphenArg ( )
            ELSE (* One hyphen. *) 
              INC ( PaArgSs )
            ; PaHyphenArg ( PaArgText )
            END (*IF*) 
          ELSE (* No hyphens. *) 
            FM3CLOptions . SourceFileNames
               := AtomList . Cons
                    ( Atom . FromText ( PaArgText )
                    , FM3CLOptions . SourceFileNames
                    )
          ; FM3CLOptions . SrcFileName := PaArgText 
          END (*IF*) 
        ; INC ( PaArgNo )
        END (*WHILE*)
      EXCEPT TerminateCL  ( EMsg )
      =>  Wr . PutText ( Stdio . stderr , FM3TextColors . FGDkRed ) 
        ; Wr . PutText ( Stdio . stderr , "Command line error: " ) 
        ; Wr . PutText ( Stdio . stderr , FM3TextColors . Reset) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText ( Stdio . stderr , "    " ) 
        ; Wr . PutText ( Stdio . stderr , PaArgText ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText
            ( Stdio . stderr , FM3SharedUtils . Blanks ( 4 + PaArgSs ) ) 
        ; Wr . PutChar ( Stdio . stderr , '^' ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText ( Stdio . stderr , "    " ) 
        ; Wr . PutText ( Stdio . stderr , EMsg ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . Flush ( Stdio . stderr )  
        ; DisplayHelp ( ) 
        ; RAISE FM3SharedUtils . Terminate ( "FM3CLArgs" )  
      END (*EXCEPT*)
    END ParseArgs

; PROCEDURE DisplayVersion ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . PutText ( Stdio . stderr , FM3TextColors . FGDkGreen ) 
    ; Wr . PutText ( Stdio . stderr , "Running " ) 
    ; Wr . PutText ( Stdio . stderr , FM3TextColors . Reset) 
    ; Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . PutText
        ( Stdio . stderr , "    FM3 Modula-3 compiler, version " ) 
    ; Wr . PutText ( Stdio . stderr , FM3Version . VersionString ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . Flush ( Stdio . stderr ) 
    END DisplayVersion

; CONST HelpTextSimpleName = "FM3HelpText"
    
; PROCEDURE DisplayHelp ( )

  = VAR LHelpRdT : Rd . T
  ; VAR LLine : TEXT
  ; VAR LReason : TEXT
  ; VAR LLength : INTEGER 
  ; VAR LOpenFailed : BOOLEAN 

  ; BEGIN
      LOpenFailed := FALSE 
    ; TRY (*EXCEPT*)
        LHelpRdT
          := FM3SharedUtils . OpenRd
               ( FM3CLOptions . ResourceDirName
               , HelpTextSimpleName
               , "help text"
               )
      EXCEPT
      | FM3SharedUtils . FatalError ( EMsg ) 
      => LReason 
           := FM3SharedUtils . CatArrT 
                ( ARRAY OF REFANY { "    (" , EMsg , ")" , Wr . EOL } ) 
      ; LOpenFailed := TRUE 
      ELSE 
        LReason := NIL  
      ; LOpenFailed := TRUE 
      END (*EXCEPT*) 
    ; IF LOpenFailed 
      THEN 
        Wr . PutText ( Stdio . stderr , "Unable to open help text file " )
      ; Wr . PutText 
          ( Stdio . stderr
          , Pathname . Join 
              ( FM3CLOptions . ResourceDirName ,  HelpTextSimpleName , NIL ) 
          ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL )  
      ; Wr . PutText ( Stdio . stderr , LReason )
      ; Wr . PutText ( Stdio . stderr , "    Try supplying " ) 
      ; Wr . PutText 
          ( Stdio . stderr , FM3CLToks . Image ( FM3CLToks . CltResourceDir ) )
      ; Wr . PutText ( Stdio . stderr , " argument first." ) 
      ; Wr . Flush ( Stdio . stderr ) 
      ELSE      
        WHILE NOT Rd . EOF ( LHelpRdT )
        DO
          LLine := Rd . GetLine ( LHelpRdT )
        ; LLength := Text . Length ( LLine )
        ; IF LLength >= 2
             AND Text . GetChar ( LLine , 0 ) = '$' 
             AND Text . GetChar ( LLine , 1 ) = 'Z'
          THEN (* Don't copy lines beginning with "$Z". *) 
          ELSE
            Wr . PutText ( Stdio . stderr , LLine )
          ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
          END (*IF*) 
        END (*WHILE*)
      ; Wr .Flush (Stdio . stderr )
      ; Rd . Close ( LHelpRdT )
      END (*IF*) 
    END DisplayHelp

; CONST OptionTokSetDefault 
          = FM3CLOptions . OptionTokSetTyp
             { Clt . CltStdErr 
             , Clt . CltFM3Log 
             , Clt . CltStdErr 
             , Clt . CltUnitLog
             }

; PROCEDURE SetDefaults ( )

  = VAR LExeName : TEXT
    
  ; BEGIN (*SetDefaults*) 
      FM3CLOptions . SourceDirNames := NIL 
    ; FM3CLOptions . SourceFileNames := NIL
    ; FM3CLOptions . ImportDirNames := NIL
    ; LExeName := Params . Get ( 0 )
    ; FM3CLOptions . ResourceDirName
        := FM3SharedUtils  . DefaultResourceDirName ( ) 

    ; FM3CLOptions . BuildDirRelPath := "../build"
    
    ; FM3CLOptions . SrcFileName := "" 

    ; FM3CLOptions . OptionTokSet := OptionTokSetDefault 
             
    ; FM3CLOptions . PassNosToKeep := FM3CLOptions . PassNoSetEmpty 
    ; FM3CLOptions . PassNosToDisAsm := FM3CLOptions . PassNoSetEmpty 

  (* TEMPORARY: during development: *)

    ; FM3CLOptions . SrcFileName := "Main.m3" 

    (* Keep intermediate files. *) 
    ; AlterPassNos
        ( FM3CLOptions . PassNosToKeep
        , FM3CLOptions . PassNoSetValid
        , Include := TRUE 
        ) 

    (* Disassemble intermediate files. *)
    ; AlterPassNos
        ( FM3CLOptions . PassNosToDisAsm
        , FM3CLOptions . PassNoSetValid
        , Include := TRUE 
        ) 

    END SetDefaults

; PROCEDURE ComputeDerivedInfo ( ) 

  = BEGIN
      FM3CLOptions . PassNosToKeep
        := FM3CLOptions . PassNosToKeep + FM3CLOptions . PassNosToDisAsm 
      
    ; IF Clt . CltFM3Log IN FM3CLOptions . OptionTokSet 
      THEN 
        TRY FM3Messages . FM3LogFileWrT
              := FileWr . Open ( FM3Messages . FM3LogFileName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => FM3Messages . FM3LogArr
             ( ARRAY OF REFANY
               { "Unable to open FM3 log file "
               , FM3Messages . FM3LogFileName 
               , ": OSError.E(" 
               , EAtoms
               , ")"
               , FM3Messages . NLIndent 
               , "Will proceed without it." 
               }
             ) 
        ; FM3CLOptions . ExclOptionTok
            ( FM3CLOptions . OptionTokSet , Clt . CltFM3Log ) 
        END (*EXCEPT*)
      END (*IF*)
    ; FM3SharedUtils . ResourceDirName := FM3CLOptions . ResourceDirName
      (* Push this out so FM3SharedUtils need not import FM3CLOptions and thus
         can be used in other main programs that get their options other ways.
      *)

    END ComputeDerivedInfo

(*EXPORTED*)
; PROCEDURE Process ( ) 
  RAISES { FM3SharedUtils . Terminate } 

  = BEGIN
      SetDefaults ( )
    ; ParseArgs ( )
    ; ComputeDerivedInfo ( ) 
    END Process 

(*EXPORTED*)
; PROCEDURE Cleanup ( )

  = BEGIN
      IF FM3Messages . FM3LogFileWrT # NIL
      THEN Wr . Close ( FM3Messages . FM3LogFileWrT )
      END (*IF*) 
    END Cleanup

; BEGIN
  END FM3CLArgs
.

