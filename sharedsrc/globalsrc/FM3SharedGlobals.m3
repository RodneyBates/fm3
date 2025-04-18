        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3SharedGlobals

; IMPORT Text

; PROCEDURE StaticCheck (  )

  = VAR LText : TEXT
  ; VAR LOK : BOOLEAN 

  ; BEGIN (*StaticCheck*)
      LText := Text . FromChars ( FM3FileTagA )
    ; LOK := Text . Equal ( LText , FM3FileTagT )
    ; <* ASSERT LOK *>  
      LText := Text . FromChars ( FM3MagicA )
    ; LOK := Text . Equal ( LText , FM3MagicT )
    ; <* ASSERT LOK *>  
    END StaticCheck

; BEGIN
    StaticCheck ( ) 
  END FM3SharedGlobals
.

