
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

; IMPORT FM3CLOptions 
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

(*EXPORTED.*)
; PROCEDURE OpenUniRd ( SrcFileT : File . T ) : UniRd . T
  RAISES { OSError . E }
  (* PRE: We already have an open File . T for the source file we want. *)
  (* Create an Rd.T on it and then a UniRd.T for that. *) 

  = VAR LRdT : Rd . T
  ; VAR LResult : UniRd . T 

  ; BEGIN (*OpenUniRd*)
      TRY 
        LRdT := NEW ( FileRd . T ) . init (*M3Ifp60*) ( SrcFileT )
      ; LResult := UniRd . New ( LRdT , UniEncoding . Encoding . ISO8859_1 )
      EXCEPT ELSE
        LResult := NIL 
      END (*EXCEPT*) 
    ; RETURN LResult 
    END OpenUniRd

(*EXPORTED*) 
; PROCEDURE CloseUniRd ( UniRdT : UniRd . T ) 

  = VAR LRdT : Rd . T

  ; BEGIN (*CloseUniRd*) 
      IF UniRdT = NIL THEN RETURN END (*IF*)
    ; LRdT := UniRd . Source ( UniRdT )  
    ; UniRd . Close ( UniRdT )
    ; IF LRdT = NIL THEN RETURN END (*IF*)
    ; Rd . Close ( LRdT )
    END CloseUniRd 

(*EXPORTED*) 
; PROCEDURE FindAndOpenRdFile
    ( READONLY DirNameList : ARRAY OF TEXT 
    ; FileSimpleName : TEXT
    ; SeekSrcFile : BOOLEAN
      (* Look for a source file in <somepkgdir>/src.
         Otherwise a unit file in <somepkgdir>/<unitfile>.
      *)
    ; VAR (*OUT*) FoundInDirName : TEXT 
    ; VAR (*OUT*) ResultFile : File . T
    )
    : BOOLEAN (* Found one. *) 

  = VAR LDirNumber : INTEGER
  ; VAR LDirSs : INTEGER
  ; VAR LPkgChildDir : TEXT 
  ; VAR LPkgDir : TEXT 
  ; VAR LSearchDir : TEXT 
  ; VAR LAbsSearchDir : TEXT
  ; VAR LOpenFileSimpleName : TEXT 
  ; VAR LFullFilePath : Pathname . T 

  ; BEGIN (* FindAndOpenRdFile *) 
      FoundInDirName := NIL
    ; ResultFile := NIL 
    ; LDirNumber := NUMBER ( DirNameList ) 
    ; IF LDirNumber = 0 THEN RETURN FALSE END (*IF*)
    ; IF FileSimpleName = NIL OR Text . Equal ( FileSimpleName , "" )
      THEN RETURN FALSE 
      END (*IF*)
    ; IF SeekSrcFile
      THEN
        LPkgChildDir := SrcDirName 
      ; LOpenFileSimpleName := FileSimpleName 
      ELSE
        LPkgChildDir := FM3CLOptions . BuildDirRelPath 
      ; LOpenFileSimpleName := Pathname . Join ( NIL , FileSimpleName , "unit" )
      END (*IF*) 
    ; LDirSs := 0
    ; LOOP
        IF LDirSs >= LDirNumber THEN RETURN FALSE END (*IF*) 
      ; LPkgDir := DirNameList [ LDirSs ]
      ; LSearchDir := Pathname . Join ( LPkgDir , LPkgChildDir , NIL )  
      ; LAbsSearchDir := FM3SharedUtils . AbsFileName ( LSearchDir )
      ; LFullFilePath
          := Pathname . Join ( LAbsSearchDir , LOpenFileSimpleName , NIL ) 
      ; TRY ResultFile := FS . OpenFileReadonly (*M3If79*) ( LFullFilePath )
        EXCEPT OSError . E ( EMsg )
        =>  ResultFile := NIL 
        END (*EXCEPT*)
      ; IF ResultFile = NIL 
        THEN INC ( LDirSs )
        ELSE 
          FoundInDirName := LPkgDir
        ; RETURN TRUE 
        END (*IF*) 
      END (*LOOP*) 
    END FindAndOpenRdFile 

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

