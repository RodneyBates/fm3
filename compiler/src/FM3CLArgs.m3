        
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
; IMPORT Stdio
; IMPORT Text 

; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3SharedUtils 
; IMPORT FM3Version  

; VAR GSourceDirNames : AtomList . T 
; VAR GFileNames : AtomList . T 

; EXCEPTION HelpExc ( BOOLEAN (* Display version AND help *) )

; PROCEDURE ParseArgs ( )
  RAISES { FM3SharedUtils . Terminate } 

  = VAR PaArgCt : INTEGER
  ; VAR PaArgNo : INTEGER
  ; VAR PaArgLen : INTEGER
  ; VAR PaArgText : TEXT 

  ; PROCEDURE PaFetchArg ( MinLen : INTEGER := 1 ) RAISES { HelpExc } 

    = BEGIN
        PaArgText := Params . Get ( PaArgNo )
      ; IF PaArgText = NIL THEN PaArgText := "" END (*IF*)
      ; PaArgLen := Text . Length ( PaArgText )
      ; IF PaArgLen < MinLen THEN RAISE HelpExc ( TRUE ) END (*IF*)
      END PaFetchArg 

  ; PROCEDURE PaTwoHyphenArg ( ArgString : TEXT )
    (* PRE: Arg started with two hyphens, and they have been
            removed from ArgString.
    *) 

    = BEGIN
      END PaTwoHyphenArg 

  ; PROCEDURE PaHyphenArgWMore ( ) : TEXT RAISES { HelpExc }
    (* Full arg starts with one hyphen and a single letter that calls
       for more informationkl which can be a suffix of this argument,
       or the following argument.
    *) 

    = VAR LResult : TEXT

    ; BEGIN
        IF PaArgLen > 2
        THEN LResult := Text . Sub ( PaArgText , 2 , PaArgLen - 2 )
        ELSE
          INC ( PaArgNo )
        ; IF PaArgNo >= PaArgCt
          THEN (* No value. *) RAISE HelpExc ( TRUE ) END (*IF*) 
        ; PaFetchArg ( MinLen := 1 ) 
        ; LResult := PaArgText 
        END (*IF*)
      ; RETURN LResult 
      END PaHyphenArgWMore

  ; PROCEDURE PaHyphenArg ( ArgString : TEXT )
    RAISES { HelpExc , FM3SharedUtils . Terminate } 
    (* PRE: Arg started with one hyphen only, and it has been
            removed from ArgString.
    *) 

    = VAR LMore : TEXT
    ; VAR LArgChar : CHAR 

    ; BEGIN
        IF PaArgLen <= 1 THEN RAISE HelpExc ( TRUE ) END (*IF*) 
      ; LArgChar := Text . GetChar ( ArgString , 0 )
      ; CASE LArgChar
        OF 'v' => RAISE HelpExc  ( FALSE )  
        | 'h' => RAISE HelpExc  ( TRUE )
        | 's' => SrcFileName := PaHyphenArgWMore ( )
        | 'd' => DoDisass := TRUE
        | 'k' => DoKeep := TRUE 
        | 'I'
          => LMore := PaHyphenArgWMore ( )
          ; GSourceDirNames
              := AtomList . Cons
                   ( Atom . FromText ( LMore ) , GSourceDirNames )
        | 'B'
          => FM3Globals . BuildDirRelPath := PaHyphenArgWMore ( ) 
        ELSE RAISE HelpExc  ( TRUE )
        END (*CASE*)
      END PaHyphenArg 

  ; BEGIN (* ParseArgs *) 
      PaArgCt := Params . Count 
    ; PaArgNo := 1

    ; TRY 
        WHILE PaArgNo < PaArgCt DO
          PaFetchArg ( MinLen := 1 )
        ; IF PaArgLen >= 2
             AND Text . Equal ( Text . Sub ( PaArgText , 0 , 2 ) , "--" ) 
          THEN PaTwoHyphenArg ( Text . Sub ( PaArgText , 2 , PaArgLen - 2 ) )
          ELSIF PaArgLen >= 1
                AND Text . Equal ( Text . Sub ( PaArgText , 0 , 1 ) , "-" ) 
          THEN PaHyphenArg ( Text . Sub ( PaArgText , 1 , PaArgLen - 1 ) ) 
          ELSE (* No hyphens. *) 
            GFileNames
               := AtomList . Cons ( Atom . FromText ( PaArgText ) , GFileNames )
          ; SrcFileName := PaArgText 
          END (*IF*) 
        ; INC ( PaArgNo )
        END (*WHILE*)
      EXCEPT HelpExc  ( DoHelp )
      => DisplayVersion ( )
      ; IF DoHelp THEN DisplayHelp ( ) END (*IF*)
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
    
; PROCEDURE DisplayHelp ( )

  = BEGIN

(* FIXME: This is from the gentok program.  Put the right stuff in here. *) 


      Wr . PutText ( Stdio . stderr , "Usage: ") 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText ( Stdio . stderr , " Options infile (default stdin)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    
    ; Wr . PutText ( Stdio . stderr , "  -v Display version and exit." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText ( Stdio . stderr , "  -h Display this help and exit." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText ( Stdio . stderr , "  -s Count and number source tokens." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -S Declare source tokens (implies -s)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE.)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -t Count and number intermediate tokens." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -T Declare intermediate tokens (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE.)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -c Emit intermediate token property sets." ) 
    ; Wr . PutText ( Stdio . stderr , " (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE.)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    
    ; Wr . PutText
        ( Stdio . stderr , "  -l Generate intermediate token lex table." ) 
    ; Wr . PutText ( Stdio . stderr , " (implies -t)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -L Generate source token spelling lex table." ) 
    ; Wr . PutText ( Stdio . stderr , " (implies -s)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -l and -L are mutually exclusive." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -n Generate Image function (tokNo-to-string map)." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText
        ( Stdio . stderr , "      (implies generate an INTERFACE and MODULE.)" )
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -F <prefix> of generated file names." ) 
    ; Wr . PutText
        ( Stdio . stderr , " (default: \"Toks\".)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  -D <directory> to put output files into." ) 
    ; Wr . PutText
        ( Stdio . stderr , " (default: \".\".)" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    END DisplayHelp

; PROCEDURE SetDefaults ( )

    = VAR LExeName : TEXT
    
    ; BEGIN
      GSourceDirNames := NIL 
    ; GFileNames := NIL 
    ; FM3Globals . BuildDirRelPath := "../build"
    ; SrcFileName := "test1.m3" (* Temporary, during development *)

    ; DoKeep := TRUE (* Temporary, during development *)
        (* Keep intermediate files. *)

    ; DoDisass := TRUE (* Temporary, during development *) 
        (* Disassemble intermediate files. *)

    ; FM3Messages . DoStdErr := TRUE
        (* Write compilation process messages to stderr. *)

    ; FM3Messages . DoLog := TRUE
        (* Write compilation process messages to compiler log file. *)
    ; FM3Messages . LogFileName := FM3Globals . BuildDirRelPath & "/FM3Log"

    ; FM3Messages . DoStdOut := TRUE
        (* Write compiled code messages to stdout. *)

    ; FM3Messages . DoCompLog := TRUE
        (* Write compiled code messages to unit-specific log file. *)

    ; LExeName := Params . Get ( 0 )
    ; FM3Globals . ResourcePathName
        := FM3SharedUtils . SibDirectoryPath ( LExeName , "lib" ) 

    END SetDefaults

; PROCEDURE HandleOptions ( ) 

  = BEGIN
(* COMPLETEME *)
    END HandleOptions 

; PROCEDURE ComputeDerivedInfo ( ) 

  = BEGIN

      DoKeep := DoKeep OR DoDisass
      
    ; IF FM3Messages . DoLog
      THEN 
        TRY FM3Messages . LogFileWrT
              := FileWr . Open ( FM3Messages . LogFileName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => FM3Messages . LogArr
             ( ARRAY OF REFANY
               { "Unable to open log file "
               , FM3Messages . LogFileName 
               , ": OSError.E(" 
               , EAtoms
               , ")"
               , FM3Messages . NLIndent 
               , "Will proceed without it." 
               }
             ) 
        ; FM3Messages . DoLog := FALSE  
        END (*EXCEPT*)
      END (*IF*)
    ; FM3SharedUtils . ResourcePathName := FM3Globals . ResourcePathName
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
      IF FM3Messages . LogFileWrT # NIL
      THEN Wr . Close ( FM3Messages . LogFileWrT )
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

