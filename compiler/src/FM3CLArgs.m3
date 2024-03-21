        
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
; IMPORT Wr
; IMPORT OSError
; IMPORT Params
; IMPORT Rd 
; IMPORT Stdio
; IMPORT Text 

; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3CLToks AS Clt
; IMPORT FM3Files 
; IMPORT FM3Globals
; IMPORT FM3LexTable 
; IMPORT FM3Messages 
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3Version  

; VAR xGSourceDirNames : AtomList . T 
; VAR xGFileNames : AtomList . T 

; EXCEPTION HelpExcept
    ( BOOLEAN (* Display help text, in addition to version. *) )

; PROCEDURE AppendTextToList ( VAR List : AtomList . T ; Txt : TEXT )

  = BEGIN (*AppendTextToList*) 
      List := AtomList . Cons ( Atom . FromText ( Txt ) , List )
    END AppendTextToList 


; PROCEDURE AssignTokSetElem
    ( VAR Set : FM3CLOptions . OptionTokSetTyp
    ; Elem : FM3CLOptions . OptionTokTyp
    ; Value : BOOLEAN
    )

  = BEGIN (*AssignTokSetElem*)
      IF Value
      THEN FM3CLOptions . InclOptionTok ( (*IN OUT*) Set , Elem ) 
      ELSE FM3CLOptions . ExclOptionTok ( (*IN OUT*) Set , Elem ) 
      END (*IF*) 
    END AssignTokSetElem  

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

; PROCEDURE ParseArgs ( )
  RAISES { FM3SharedUtils . Terminate } 

  = VAR PaArgCt : INTEGER
  ; VAR PaArgNo : INTEGER
  ; VAR PaArgLen : INTEGER
  ; VAR PaArgSs : INTEGER 
  ; VAR PaArgText : TEXT

  ; PROCEDURE PaFetchArg ( MinLen : INTEGER := 1 ) RAISES { HelpExcept } 

    = BEGIN
        PaArgText := Params . Get ( PaArgNo )
      ; IF PaArgText = NIL THEN PaArgText := "" END (*IF*)
      ; PaArgLen := Text . Length ( PaArgText )
      ; IF PaArgLen < MinLen THEN RAISE HelpExcept ( TRUE ) END (*IF*)
      END PaFetchArg

  ; CONST SingleDigits = SET OF CHAR { '1' .. '9' }

  ; PROCEDURE PaPassNoSet
      ( VAR Set : FM3CLOptions . PassNoSetTyp
      ; Include : BOOLEAN (* Otherwise, exclude. *) 
      ) 
  
    = VAR LPassNo : INTEGER
    ; VAR LChar : CHAR

    ; BEGIN (*PaPassNoSet*)
        WHILE PaArgSs < PaArgLen
        DO
          LChar := Text . GetChar ( PaArgText , PaArgSs )
        ; IF NOT LChar IN SingleDigits 
          THEN RAISE HelpExcept  ( TRUE ) 
          END (*IF*)
        ; LPassNo := ORD ( LChar ) - ORD ( '1' )
        ; CASE LPassNo OF
          | FM3CLOptions . PassNo1 .. FM3CLOptions . PassNoMax - 1
          => IF Include
             THEN FM3CLOptions . InclPassNo ( Set , LPassNo )
             ELSE FM3CLOptions . ExclPassNo ( Set , LPassNo )
             END (*IF*) 
          ELSE RAISE HelpExcept  ( TRUE )
          END (*CASE*)
        ; INC ( PaArgSs ) 
        END (*WHILE*)
      END PaPassNoSet

  ; PROCEDURE PaFindTwoHyphenParam ( ) RAISES { HelpExcept }
    (* Full arg started with two hyphens, thus has a multi-letter option tag.
       It calls for parameter, which can be the following argument or a
       suffix of this argument, attached by '='.
    *) 

    = VAR LResult : TEXT

    ; BEGIN (*PaFindTwoHyphenParam*) 
        IF PaArgSs < PaArgLen
        THEN
          IF Text . GetChar ( PaArgText , PaArgSs ) = '='
          THEN (* Parameter is part of this CL argument. *) 
            INC ( PaArgSs ) 
          ELSE (* No equal sign. *) RAISE HelpExcept ( TRUE )
          END (*IF*)
        ELSE 
          INC ( PaArgNo )
        ; IF PaArgNo >= PaArgCt
          THEN (* No value. *)
            PaArgText := "<beyond end of command>"
          ; PaArgLen := Text . Length ( PaArgText )
          ; PaArgSs := 0 
          ; RAISE HelpExcept ( TRUE ) 
          END (*IF*) 
        ; PaFetchArg ( MinLen := 1 )
        END (*IF*)
      END PaFindTwoHyphenParam

  ; PROCEDURE PaTwoHyphenParam ( ) : TEXT RAISES { HelpExcept }
    (* Just find it and return it.  Is this too trivial? *) 

    = VAR LResult : TEXT

    ; BEGIN (*PaTwoHyphenParam*)
        PaFindTwoHyphenParam ( )
      ; RETURN Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
      END PaTwoHyphenParam

  ; PROCEDURE PaTwoHyphenArg ( ) RAISES { HelpExcept } 
    (* PRE: Arg starts with two hyphens. *) 

    = VAR LParam : TEXT
    ; VAR LOptTok : FM3LexTable . ValueTyp 
    ; VAR LNo : BOOLEAN

    ; BEGIN
        IF PaArgLen >= 5
           AND Text . Equal ( Text . Sub ( PaArgText , 2 , 3 ) , "no-" )
        THEN
          LNo := TRUE (* Confusing? *)
        ; INC ( PaArgSs , 3 )  
        ELSE LNo := FALSE 
        END (*IF*)
      ; IF PaArgSs >= PaArgLen THEN RAISE HelpExcept  ( TRUE ) END (*IF*)
      ; IF FM3CLOptions . OptionsLexTable = NIL
        THEN 
          FM3CLOptions . OptionsLexTable
            := FM3Files . ReadFsm
                 ( "Clt" , FM3SharedGlobals . FM3FileKindCltPkl )
(*TODO: ^Catch an exception and emit a helpful message if this fails to load. *)
        END (*IF*) 
      ; LOptTok
          := FM3LexTable . ValueFromText
               ( FM3CLOptions . OptionsLexTable
               , Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
               )
      ; IF LOptTok = FM3LexTable . ValueNull
        THEN RAISE HelpExcept  ( TRUE ) 
        END (*IF*) 
      ; CASE LOptTok
        OF
        | Clt . CltVersion
        => RAISE HelpExcept  ( FALSE )
        
        | Clt . CltHelp 
        => RAISE HelpExcept  ( TRUE )
        
        | Clt . CltSrcFile  
        => PaFindTwoHyphenParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; AppendTextToList ( FM3CLOptions . SourceFileNames , LParam )
        
        | Clt . CltSrcDir  
        => PaFindTwoHyphenParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; AppendTextToList ( FM3CLOptions . SourceDirNames , LParam ) 
        
        | Clt . CltIncludeDir  
        => PaFindTwoHyphenParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; AppendTextToList ( FM3CLOptions . IncludeDirNames , LParam ) 
        
        | Clt . CltDisAsmPasses 
        => PaFindTwoHyphenParam ( )
        ; PaPassNoSet ( FM3CLOptions . PassNosToDisAsm , NOT LNo) 
        
        | Clt . CltDisAsm 
        => FM3CLOptions . PassNosToDisAsm := FM3CLOptions . PassNoSetAll
        
        | Clt . CltKeepPasses 
        => PaFindTwoHyphenParam ( )
        ; PaPassNoSet ( FM3CLOptions . PassNosToKeep , NOT LNo ) 
        
        | Clt . CltKeep 
        => FM3CLOptions . PassNosToKeep := FM3CLOptions . PassNoSetAll
        
        | Clt . CltBuildDir 
        => PaFindTwoHyphenParam ( )
        ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
        ; FM3CLOptions . BuildDir := LParam
        
        | Clt . CltStdErr  
        , Clt . CltStdOut  
        , Clt . CltFM3Log
        , Clt . CltUnitLog 
        => AssignTokSetElem 
             ( FM3CLOptions . OptionTokSet , LOptTok , Value := NOT LNo ) 
        
        ELSE RAISE HelpExcept  ( TRUE ) 
        END (*CASE*)
      ; INC ( PaArgNo ) 
      END PaTwoHyphenArg 

  ; PROCEDURE PaHyphenArgWMore ( ArgChar : CHAR ) : TEXT RAISES { HelpExcept }
    (* Full arg started with one hyphen, thus has single letter option
       tags.  The current one calls for parameter, which can be 
       the following argument or a suffix of this argument.
    *) 

    = VAR LResult : TEXT

    ; BEGIN
        IF PaArgSs < PaArgCt 
        THEN
          LResult := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
        ; INC ( PaArgNo ) 
        ELSE
          INC ( PaArgNo )
        ; IF PaArgNo >= PaArgCt
          THEN (* No value. *)
            PaArgText := "<beyond end of command>"
          ; PaArgLen := Text . Length ( PaArgText )
          ; PaArgSs := 0 
          ; RAISE HelpExcept ( TRUE ) 
          END (*IF*) 
        ; PaFetchArg ( MinLen := 1 ) 
        ; LResult := PaArgText 
        END (*IF*)
      ; RETURN LResult 
      END PaHyphenArgWMore

  ; PROCEDURE PaHyphenArg ( ArgString : TEXT )
    RAISES { HelpExcept , FM3SharedUtils . Terminate } 
    (* PRE: Arg starts with one hyphen only. *) 

    = VAR LMore : TEXT
    ; VAR LArgLength : INTEGER 
    ; VAR LArgChar : CHAR 

    ; BEGIN
        IF PaArgLen <= 1 THEN RAISE HelpExcept ( TRUE ) END (*IF*)
      ; PaArgSs := 1
      ; LOOP 
          LArgChar := Text . GetChar ( ArgString , PaArgSs )
        ; CASE LArgChar
          OF 'v' => RAISE HelpExcept  ( FALSE )  
          | 'h' => RAISE HelpExcept  ( TRUE )
          | 's'
            => FM3CLOptions . SrcFileName := PaHyphenArgWMore ( LArgChar )
            ; EXIT 
          | 'd'
          => FM3CLOptions . InclPassNo
               ( FM3CLOptions . PassNosToDisAsm , FM3CLOptions . PassNo1 )
          
          | 'e'
          => FM3CLOptions . InclPassNo
                ( FM3CLOptions . PassNosToDisAsm , FM3CLOptions . PassNo2 )  
          | 'k'
           => FM3CLOptions . InclPassNo
                 ( FM3CLOptions . PassNosToKeep , FM3CLOptions . PassNo2 ) 
          | 'I'
            => LMore := PaHyphenArgWMore ( LArgChar )
            ; FM3CLOptions . SourceDirNames
                := AtomList . Cons
                     ( Atom . FromText ( LMore )
                     , FM3CLOptions . SourceDirNames
                     )
            ; EXIT 
          | 'B'
            => FM3CLOptions . BuildDirRelPath := PaHyphenArgWMore ( LArgChar )
            ; EXIT 
          ELSE RAISE HelpExcept  ( TRUE )
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
        ; IF PaArgLen >= 2
             AND Text . Equal ( Text . Sub ( PaArgText , 0 , 2 ) , "--" ) 
          THEN
            INC ( PaArgSs , 2 )
          ; PaTwoHyphenArg ( )
          ELSIF PaArgLen >= 1
                AND Text . Equal ( Text . Sub ( PaArgText , 0 , 1 ) , "-" ) 
          THEN PaHyphenArg ( PaArgText ) 
          ELSE (* No hyphens. *) 
            FM3CLOptions . SourceFileNames
               := AtomList . Cons
                    ( Atom . FromText ( PaArgText ) , FM3CLOptions . SourceFileNames )
          ; FM3CLOptions . SrcFileName := PaArgText 
          END (*IF*) 
        ; INC ( PaArgNo )
        END (*WHILE*)
      EXCEPT HelpExcept  ( DoHelp )
      => DisplayVersion ( )
      ; IF DoHelp
        THEN
          Wr . PutText ( Stdio . stderr , "Error in command-line argument:" ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText ( Stdio . stderr , PaArgText ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . PutText ( Stdio . stderr , FM3SharedUtils . Blanks ( PaArgSs ) ) 
        ; Wr . PutChar ( Stdio . stderr , '^' ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        ; Wr . Flush ( Stdio . stderr )  
       ; DisplayHelp ( ) END (*IF*)
      ; RAISE FM3SharedUtils . Terminate ( "FM3CLArgs" )  
      END (*EXCEPT*)
    END ParseArgs

; PROCEDURE DisplayVersion ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText
        ( Stdio . stderr , ": FM3 Modula-3 compiler, version " ) 
    ; Wr . PutText ( Stdio . stderr , FM3Version . VersionString ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    END DisplayVersion

; VAR xGOptionsLexTable : FM3LexTable . T

; CONST HelpTextSimpleName = "FM3HelpText" 
    
; PROCEDURE DisplayHelp ( )

  = VAR LHelpRdT : Rd . T
  ; VAR LLine : TEXT
  ; VAR LLength : INTEGER 

  ; BEGIN
      TRY (*EXCEPT*)
        LHelpRdT
          := FM3SharedUtils . OpenRd
               ( HelpTextSimpleName
               , FM3CLOptions . ResourcePathName
               , "help text"
               )
      EXCEPT
      | FM3SharedUtils . FatalError ( EMsg )
      => FM3SharedUtils . StandaloneFatalError ( EMsg )
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
(* TODO^v Better integrate failure message into the compiler. *) 
      ELSE 
        Wr . PutText ( Stdio . stderr , "Unable to open help text." )
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      END (*EXCEPT*) 
    ; WHILE NOT Rd . EOF ( LHelpRdT )
      DO
        LLine := Rd . GetLine ( LHelpRdT )
      ; LLength := Text . Length ( LLine )
        (* Don't copy lines beginning with "$Z". *) 
      ; IF LLength >= 2
           AND Text . GetChar ( LLine , 0 ) = '$' 
           AND Text . GetChar ( LLine , 1 ) = 'Z'
        THEN
        ELSE
          Wr . PutText ( Stdio . stderr , LLine )
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        END (*IF*) 
      END (*WHILE*)
    ; Wr .Flush (Stdio . stderr ) 
    END DisplayHelp

; CONST OptionTokSetDefault 
          = FM3CLOptions . OptionTokSetTyp
             { Clt . CltStdErr 
             , Clt . CltFM3Log 
             , Clt . CltStdErr 
             , Clt . CltUnitLog
(* Add as desired: 
             , Clt . Clt 
             , Clt . Clt 
             , Clt . Clt 
             , Clt . Clt
             , Clt . Clt 
             , Clt . Clt 
             , Clt . Clt 
             , Clt . Clt
*) 
             }

; PROCEDURE SetDefaults ( )

    = VAR LExeName : TEXT
    
    ; BEGIN
      FM3CLOptions . SourceDirNames := NIL 
    ; FM3CLOptions . SourceFileNames := NIL
    ; FM3CLOptions . OptionTokSet := OptionTokSetDefault 
    ; LExeName := Params . Get ( 0 )
    ; FM3CLOptions . ResourcePathName
        := FM3SharedUtils . SibDirectoryPath ( LExeName , "lib" )

    ; FM3CLOptions . BuildDirRelPath := "../build"
    
    ; FM3CLOptions . SrcFileName
        := "Main.m3" (* Temporary default, during development *)

    ; FM3CLOptions . OptionTokSet := OptionTokSetDefault 
             
    ; FM3CLOptions . PassNosToKeep := FM3CLOptions . PassNoSetEmpty 
    ; FM3CLOptions . PassNosToDisAsm := FM3CLOptions . PassNoSetEmpty 

    ; FM3CLOptions . InclOptionTok
        ( FM3CLOptions . OptionTokSet , Clt . CltKeep  ) 
        (* Keep intermediate files. *)

    (* Disassemble intermediate files. *)
    (* TEMPORARY: during development: *)
    ; FM3CLOptions . InclPassNo
        ( FM3CLOptions . PassNosToDisAsm , FM3CLOptions . PassNo2 )
    ; FM3CLOptions . PassNoSetUnion
        ( FM3CLOptions . PassNosToKeep , FM3CLOptions . PassNoSetAll ) 

    END SetDefaults

; PROCEDURE HandleOptions ( ) 

  = BEGIN
(* COMPLETEME *)
    END HandleOptions 

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
    ; FM3SharedUtils . ResourcePathName := FM3CLOptions . ResourcePathName
      (* Push this out so FM3SharedUtils need not import FM3CLOptions and can
         be used in other man programs that get their options other ways.
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

(* ------------------------------- Maybe use: --------------------------- *) 
(* To incorporate, maybe: 

; PROCEDURE SetArgDefaults ( )

  = BEGIN
      GDoHelp := FALSE 
    ; GDoGenInterface := FALSE
    ; GDoGenModule := FALSE
    ; GDoImageFunc := FALSE
    ; GDoCountIntToks := FALSE
    ; GDoGenIntToks := FALSE
    ; GDoGenSets := FALSE
    ; GDoGenIntFsm := FALSE
    ; GDoCountSrcToks := FALSE
    ; GDoGenSrcToks := FALSE
    ; GDoGenSrcFsm := FALSE

    ; GInputFileName := "FM3Toks.gentok"
    ; GOutputFilePrefix := "Tok"
    ; GOutputDirName := "."
    ; SetOutputFileNames ( )
    ; GDoOverrideUNITNAME := FALSE 
    END SetArgDefaults 

; PROCEDURE MessageLine ( Msg : TEXT )

  = BEGIN
      Wr . PutText ( Stdio . stderr , "In Line " ) 
    ; Wr . PutText ( Stdio . stderr , Fmt . Int ( GInputLineNo ) ) 
    ; Wr . PutText ( Stdio . stderr , ", " ) 
    ; Wr . PutText ( Stdio . stderr , Msg  ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . Flush ( Stdio . stderr )
    END MessageLine

; PROCEDURE OpenInput ( FileName : TEXT ) : Rd . T 
  RAISES { FM3SharedUtils . Terminate } 

  = VAR LResult : Rd . T
  
  ; BEGIN
      IF FileName = NIL
         OR Text . Equal ( FileName , "" )
         OR Text . Equal ( FileName , "-" )
      THEN LResult := Stdio . stdin 
      ELSE
        TRY 
          LResult := FileRd . Open ( FileName )
        EXCEPT OSError . E ( <*UNUSED*> Code )
        => MessageLine ( "Unable to open input file " & FileName )
        ; LResult := NIL 
        ; RAISE FM3SharedUtils . Terminate 
        END (*EXCEPT*)
      END (*IF*)
    ; GInputLineNo := 1 
    ; GNextInChar := '\X00'
    ; GInputRdT := LResult 
    ; ConsumeChar ( )
    ; RETURN LResult 
    END OpenInput



*) 

; BEGIN
  END FM3CLArgs
.

