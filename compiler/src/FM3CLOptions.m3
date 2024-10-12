
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Internal representation of command-line options. *) 

MODULE FM3CLOptions

; IMPORT FM3Atom_Text
; IMPORT FM3Base 
; IMPORT FM3Globals

(* ---------------------------- Option token sets ---------------------------*) 

(*EXPORTED*) 
; PROCEDURE OptionTokSetUnion
   ( VAR Left : OptionTokSetTyp ; Right : OptionTokSetTyp )

  = BEGIN (*OptionTokSetUnion*)
      Left := Left + Right
    END OptionTokSetUnion 

(*EXPORTED*) 
; PROCEDURE OptionTokSetDiff
   ( VAR Left : OptionTokSetTyp ; Right : OptionTokSetTyp )

  = BEGIN (*OptionTokSetDiff*)
      Left := Left - Right
    END OptionTokSetDiff 

(*EXPORTED*) 
; PROCEDURE InclOptionTok ( VAR Set : OptionTokSetTyp ; Tok : OptionTokTyp )

  = BEGIN (*InclOptionTok*) 
      Set := Set + OptionTokSetTyp { Tok } 
    END InclOptionTok 

(*EXPORTED*) 
; PROCEDURE ExclOptionTok ( VAR Set : OptionTokSetTyp ; Tok : OptionTokTyp )

  = BEGIN (*ExclOptionTok*) 
      Set := Set - OptionTokSetTyp { Tok } 
    END ExclOptionTok 

(* ---------------------------- Pass number sets ----------------------------*) 

(*EXPORTED*) 
; PROCEDURE PassNoSetUnion ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

  = BEGIN (*PassNoSetUnion*)
      Left := Left + Right
    END PassNoSetUnion 

(*EXPORTED*) 
; PROCEDURE PassNoSetDiff ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

  = BEGIN (*PassNoSetDiff*)
      Left := Left - Right
    END PassNoSetDiff 

(*EXPORTED*) 
; PROCEDURE InclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoTyp )

  = BEGIN (*InclPassNo*) 
      Set := Set + PassNoSetTyp { No } 
    END InclPassNo 

(*EXPORTED*) 
; PROCEDURE ExclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoTyp )

  = BEGIN (*ExclPassNo*) 
      Set := Set - PassNoSetTyp { No } 
    END ExclPassNo

; BEGIN
  END FM3CLOptions 
.

