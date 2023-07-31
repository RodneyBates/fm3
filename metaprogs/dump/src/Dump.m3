 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Program to display contents of an FM3 stream. *) 

MODULE Dump EXPORTS Main 

; IMPORT Params
; IMPORT Stdio
; IMPORT Text 
; IMPORT Wr 

; IMPORT DumpWork
; IMPORT FM3SharedUtils 
; IMPORT RdBackFile 

; VAR GDoHelp := FALSE 
; VAR GDoToks := FALSE
; VAR GInputFileName : TEXT := NIL 

; CONST VersionString = "0.1"

; CONST VersionOpt = "-v" 
; CONST HelpOpt = "-h" 
; CONST TokOpt = "-t" 

; <*IMPLICIT*>
  EXCEPTION Terminate

; PROCEDURE SetArgDefaults ( )

  = BEGIN
      GDoHelp := FALSE 
    ; GDoToks := FALSE
    ; GInputFileName := NIL 
    END SetArgDefaults 

; EXCEPTION HelpExc 

; PROCEDURE ParseArgs ( )

  = VAR PaArgCt : INTEGER
  ; VAR PaArgNo : INTEGER
  ; VAR PaArgLen : INTEGER
  ; VAR PaArgText : TEXT 

  ; BEGIN

      PaArgCt := Params . Count 
    ; PaArgNo := 1

    ; PROCEDURE FetchArg ( MinLen : INTEGER := 1 ) RAISES { HelpExc } 

      = BEGIN
          PaArgText := Params . Get ( PaArgNo )
        ; IF PaArgText = NIL THEN PaArgText := "" END (*IF*)
        ; PaArgLen := Text . Length ( PaArgText )
        ; IF PaArgLen < MinLen  
          THEN
            GDoHelp := TRUE
          ; RAISE HelpExc 
          END (*IF*)
        END FetchArg 

    ; PROCEDURE ArgWMore ( ) : TEXT RAISES { HelpExc }  

      = VAR LResult : TEXT

      ; BEGIN
          IF PaArgLen > 2
          THEN LResult := Text . Sub ( PaArgText , 2 , PaArgLen - 2 )
          ELSE
            INC ( PaArgNo )
          ; IF PaArgNo >= PaArgCt
            THEN (* No value. *)
              GDoHelp := TRUE 
            ; RAISE HelpExc
            END (*IF*) 
          ; FetchArg ( MinLen := 1 ) 
          ; LResult := PaArgText 
          END (*IF*)
        ; RETURN LResult 
        END ArgWMore
        
    ; VAR LArgPrefix : TEXT 
    ; BEGIN
        TRY 
          WHILE PaArgNo < PaArgCt DO
            FetchArg ( MinLen := 2 ) 
          ; LArgPrefix := Text . Sub ( PaArgText , 0 , 2 )
          ; IF Text . Equal ( LArgPrefix , HelpOpt )
            THEN
              GDoHelp:= TRUE 
            ; RAISE HelpExc 
            ELSIF Text . Equal ( LArgPrefix , VersionOpt )
            THEN 
              DisplayVersion ( )
            ; RAISE Terminate 
            ELSIF Text . Equal ( LArgPrefix , TokOpt )
            THEN GDoToks := TRUE 
(*          ELSIF Text . Equal ( LArgPrefix , "-F" )
            THEN
              GOutputFilePrefix := ArgWMore ( ) 
            ; SetOutputFileNames ( )
*)            
            ELSIF Text . GetChar ( PaArgText , 0 ) = '-'
            THEN 
              GDoHelp:= TRUE 
            ; RAISE HelpExc 
            ELSE GInputFileName := PaArgText  
            END (*IF*)
          ; INC ( PaArgNo )
          END (*WHILE*)
        EXCEPT HelpExc
        => DisplayVersion ( )
        ; IF GDoHelp THEN DisplayHelp ( ) END (*IF*)
        ; RAISE Terminate 
        END (*EXCEPT*)
      END (*BEGIN*)
    END ParseArgs

; PROCEDURE DeriveOptions ( ) RAISES { HelpExc } 

  = BEGIN
      IF GInputFileName = NIL
      THEN
        DisplayVersion ( )
      ; DisplayHelp ( ) 
      ; RAISE Terminate 
      END (*IF*) 
    ; IF GDoToks 
      THEN
        Wr . PutText
          ( Stdio . stderr
          , "Command-line option " & TokOpt
            & " is not yet implemented, ignored."
          ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
      END (*IF*)
    END DeriveOptions

; PROCEDURE DisplayVersion ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText
        ( Stdio . stderr , ": Program to dump FM3 files, version. " ) 
    ; Wr . PutText ( Stdio . stderr , VersionString ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    END DisplayVersion 

; PROCEDURE DisplayHelp ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , "Usage: ") 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    ; Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText ( Stdio . stderr , " Options infile" ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    
    ; Wr . PutText
        ( Stdio . stderr , "  " & VersionOpt & " Display version and exit." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  " & HelpOpt & " Display this help and exit." ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )

    ; Wr . PutText
        ( Stdio . stderr , "  " & TokOpt & " Interpret token codes." )
    ; Wr . PutText ( Stdio . stderr , Wr . EOL  )
    END DisplayHelp

; PROCEDURE OpenFiles ( VAR RBT : RdBackFile . T  ; VAR WrT : Wr . T )

  = BEGIN
      RBT := RdBackFile . Open ( GInputFileName ) 
    ; WrT := Stdio . stdout 
    END OpenFiles 

; PROCEDURE DumpNumericBwd ( )

  = VAR LRBT : RdBackFile . T 
  ; VAR LWrT : Wr . T

  ; BEGIN
      OpenFiles ( (*OUT*) LRBT , (*OUT*) LWrT )  
    ; DumpWork . DumpNumericBwd ( LRBT , LWrT ) 
    END DumpNumericBwd

; PROCEDURE DumpInterpretBwd ( )

  = VAR LRBT : RdBackFile . T 
  ; VAR LWrT : Wr . T

  ; BEGIN
      OpenFiles ( (*OUT*) LRBT , (*OUT*) LWrT )  
    ; DumpWork . DumpInterpretBwd ( LRBT , LWrT ) 
    END DumpInterpretBwd 

; PROCEDURE Work ( )

  = BEGIN
      SetArgDefaults ( )
    ; ParseArgs ( )
    ; DeriveOptions ( ) 

    ; IF NOT GDoToks
      THEN DumpNumericBwd ( )
      ELSE DumpInterpretBwd ( ) 
      END (*IF*) 
    END Work 

; BEGIN
    TRY 
      Work ( )
     EXCEPT
     | Terminate =>
     | FM3SharedUtils . FatalError ( EMsg )
       => Wr . PutText ( Stdio . stderr , EMsg )
       ; Wr . PutText ( Stdio . stderr , Wr . EOL )
       ; Wr . Flush ( Stdio . stderr ) 
     END (*EXCEPT*) 
  END Dump
.

