
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Files

; IMPORT FileRd 
; FROM Messages IMPORT FATAL
; IMPORT OSError 
; IMPORT Rd
; IMPORT UniRd 

(*EXPORTED*) 
; PROCEDURE OpenUniRd
    ( FileName , PathHame , Note1 , NOte2 : TEXT := "" ) : UniRd . T

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T 
  
  ; BEGIN
      LFullFileName := PathName & FMGlobals . PathSep & FileName 
    ; TRY 
       LRdT := FileRd . Open ( LFullFileName ) 
      EXCEPT
      | OSError . E ( EMsg ) 
      => FATAL
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

