
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Files

; IMPORT File
; IMPORT FileRd
; IMPORT FS 
; IMPORT OSError
; IMPORT Pathname 
; IMPORT Rd
; IMPORT Text 
; IMPORT Thread
; IMPORT Time 
; IMPORT UniEncoding 
; IMPORT UniRd 

; IMPORT FM3LexTable 
; IMPORT FM3SharedGlobals  
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
    ( DirName : TEXT
    ; FileName : TEXT
    ; VAR (*OUT*) UniRdT : UniRd . T
    ; VAR (*OUT*) Time : Time . T
    ) 
  RAISES { OSError . E (* Which means not found. *) }
  
  = VAR LFullFileName : TEXT
  ; VAR LFile : File . T 
  ; VAR LRdT : Rd . T
  
  ; BEGIN
      LFullFileName := Pathname . Join ( DirName , FileName )
    ; UniRdT := NIL
    ; Time := 0.0D0
    
    (* Any of the following could raise OSError.E.  Let it propate out. *)
    ; LFile := FS . OpenFileReadonly ( LFullFileName ) (*M3If79*) 
    ; LRdT := NEW ( FileRd . T ) . init (*M3Ifp60*) ( LFile )  
    ; Time := LFile . status (*M3Ifp64*) ( ) . modificationTime
    
    (* Phew. no exceptions raised. Success. *) 
    END OpenUniRd

(*EXPORTED*) 
; PROCEDURE ReadFsm
    ( NamePrefix : TEXT ; Kind : FM3SharedGlobals . FileKindTyp )
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

