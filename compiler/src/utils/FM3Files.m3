
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Files

; IMPORT FileRd 
; IMPORT OSError
; IMPORT PathName 
; IMPORT Rd
; IMPORT UniRd 

; IMPORT FM3Globals 
; FROM FM3Messages IMPORT Fatal

(*EXPORTED*) 
; PROCEDURE OpenUniRd
    ( FileName , PathHame , Note1 , Note2 : TEXT := "" ) : UniRd . T

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LResult UniRd . T 
  
  ; BEGIN
      LFullFileName := PathName & FMGlobals . PathSep & FileName 
    ; TRY 
       LRdT := FileRd . Open ( LFullFileName ) 
      EXCEPT
      | OSError . E ( EMsg ) 
      => Fatal
           ( "Unable to open " , Note1 , Note2 , LFullFileName
           , ": OSError.E(" , EMsg , ")."
           ) 
      END (*EXCEPT*)
    ; LResult := UniRd . New ( LRdT , Enc )
    ; RETURN LResult 
    END OpenUniRd

; BEGIN
  END FM3Files 
. 

