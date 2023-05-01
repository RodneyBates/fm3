
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Files

; IMPORT FileRd 
; IMPORT OSError
; IMPORT Pathname AS Libm3Pathname 
; IMPORT Rd
; IMPORT UniEncoding 
; IMPORT UniRd 

; FROM FM3Messages IMPORT Fatal
; IMPORT FM3Utils 

; CONST ALT = FM3Utils . AtomListToText

; VAR SrcEnc : UniEncoding . Encoding := UniEncoding . Encoding . ISO8859_1
      (* UTF8 is a reasonable alternative. *)   

(*EXPORTED*) 
; PROCEDURE OpenUniRd
    ( FileName , PathName , Note1 , Note2 : TEXT := "" ) : UniRd . T

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LResult : UniRd . T 
  
  ; BEGIN
      LFullFileName := Libm3Pathname . Join ( PathName , FileName ) 
    ; TRY 
       LRdT := FileRd . Open ( LFullFileName ) 
      EXCEPT
      | OSError . E ( EMsg ) 
      => Fatal
           ( "Unable to open " , Note1 , Note2 , LFullFileName
           , ": OSError.E(" , ALT ( EMsg ) , ")."
           ) 
      END (*EXCEPT*)
    ; LResult := UniRd . New ( LRdT , SrcEnc )
    ; RETURN LResult 
    END OpenUniRd

; BEGIN
  END FM3Files 
. 

