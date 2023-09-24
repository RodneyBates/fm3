
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

; IMPORT FM3Messages 
; IMPORT FM3SharedUtils 

; VAR SrcEnc : UniEncoding . Encoding := UniEncoding . Encoding . ISO8859_1
      (* UTF8 is a reasonable alternative. *)   

(*EXPORTED*) 
; PROCEDURE OpenUniRd
    ( FileName , PathName , Note1 , Note2 : TEXT := "" ) : UniRd . T
  RAISES { } 

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LResult : UniRd . T 
  
  ; BEGIN
      LFullFileName := Libm3Pathname . Join ( PathName , FileName ) 
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

(*TODO: Use FM3SharedUtils.OpenRd for all above. *)
    ; LResult := UniRd . New ( LRdT , SrcEnc )
    ; RETURN LResult 
    END OpenUniRd

; BEGIN
  END FM3Files 
. 

