
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Files

; IMPORT FileRd
; IMPORT FS 
; IMPORT OSError
; IMPORT Pathname 
; IMPORT Rd
; IMPORT Text 
; IMPORT Thread 
; IMPORT UniEncoding 
; IMPORT UniRd 

; IMPORT FM3LexTable 
; IMPORT FM3Messages 
; IMPORT FM3SharedUtils 

; VAR SrcEnc : UniEncoding . Encoding := UniEncoding . Encoding . ISO8859_1
      (* UTF8 is a reasonable alternative. *)   

(*EXPORTED*) 
; PROCEDURE FileSuffix ( FileName : TEXT ) : SuffixTyp 

  = VAR LSuffixText : TEXT
  ; VAR LLength : INTEGER

  ; BEGIN (* FileSuffix *)
      IF FileName = NIL THEN RETURN SuffixTyp . SfxNull END (*IF*) 
    ; LLength := Text . Length ( FileName )
    ; IF LLength < 3 THEN RETURN SuffixTyp . SfxNull END (*IF*)
    ; LSuffixText := Text . Sub ( FileName , LLength - 3 , 3 )
    ; IF Text . Equal ( LSuffixText , ".i3" ) THEN RETURN SuffixTyp . Sfxi3
      ELSIF Text . Equal ( LSuffixText , ".ig" ) THEN RETURN SuffixTyp . Sfxig
      ELSIF Text . Equal ( LSuffixText , ".m3" ) THEN RETURN SuffixTyp . Sfxm3
      ELSIF Text . Equal ( LSuffixText , ".mg" ) THEN RETURN SuffixTyp . Sfxmg
      ELSE RETURN SuffixTyp . SfxNull 
      END (*IF*) 
    END FileSuffix 

(*EXPORTED*) 
; PROCEDURE RemoveSuffix ( FileName : TEXT ) : TEXT  

  = VAR LSuffix : SuffixTyp 
  ; VAR LLength : INTEGER

  ; BEGIN (* RemoveSuffix *)
      LSuffix := FileSuffix ( FileName )
    ; IF LSuffix = SuffixTyp . SfxNull THEN RETURN FileName
      ELSE
        LLength := Text . Length ( FileName )
      ; RETURN Text . Sub ( FileName , 0 , LLength - 3 ) 
      END (*IF*) 
    END RemoveSuffix 

(*EXPORTED*) 
; PROCEDURE OpenUniRd
    ( DirName , FileName , Note1 , Note2 : TEXT := "" ) : UniRd . T
  RAISES { } 

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LResult : UniRd . T 
  
  ; BEGIN
      LFullFileName := Pathname . Join ( DirName , FileName ) 
    ; TRY 
       LRdT := FileRd . Open ( LFullFileName ) 
      EXCEPT
      | OSError . E ( EMsg ) 
      => FM3Messages . FatalArr
           ( ARRAY OF REFANY
               { "Unable to open "
               , Note1
               , Note2
               , " \""
               , LFullFileName
               , "\": OSError.E("
               , EMsg 
               , ")."
               } 
           ) 
      END (*EXCEPT*)

(*TODO: Use FM3SharedUtils.OpenRd for above. *)
    ; LResult := UniRd . New ( LRdT , SrcEnc )
    ; RETURN LResult 
    END OpenUniRd

(*EXPORTED*) 
; PROCEDURE ReadFsm ( NamePrefix : TEXT ; Kind : FM3SharedUtils . FileKindTyp )
  : FM3LexTable . T
  RAISES { Thread . Alerted } 

  = VAR LFileName : TEXT
  ; VAR LResult : FM3LexTable . T

  ; BEGIN
      LFileName := NamePrefix & "SrcFsm.pkl"
    ; LResult := FM3SharedUtils . ReadFsm ( LFileName , Kind )
    ; RETURN LResult 
    END ReadFsm

; BEGIN
  END FM3Files 
. 

