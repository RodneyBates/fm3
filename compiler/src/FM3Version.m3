
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Version

; IMPORT Fmt
; IMPORT TextWr
; IMPORT Wr 

(*EXPORTED:*)
; PROCEDURE VersionImage ( Version : VersionTyp ) : TEXT
 
  = VAR LResult : TEXT 
  ; VAR LWrT : TextWr . T 

  ; BEGIN (* VersionImage *)
      LWrT := TextWr . New ( ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Version . Incompatible ) )
    ; Wr . PutChar ( LWrT , '.' )
    ; Wr . PutText ( LWrT , Fmt . Int ( Version . UpwardCompatible ) )
    ; Wr . PutChar ( LWrT , '.' )
    ; Wr . PutText ( LWrT , Fmt . Int ( Version . Compatible ) )
    ; Wr . PutChar ( LWrT , '.' )
    ; Wr . PutText ( LWrT , Fmt . Int ( Version . Minor ) )
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END VersionImage

; BEGIN
  END FM3Version
.

