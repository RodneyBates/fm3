
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE FM3Atom ( KeyTyp , ValTyp )

(* KeyTyp declares:
     . T
     . Hash
     . Brand.

*) 

; IMPORT FM3Base 

; TYPE T <:
    BRANDED "FM3(" & KeyTyp . Brand & ")Atom"_0.1"
    REFANY

(* Sizes are count of Key-value pairs.  Any extra space needed by
   the internal data structure will be added internally. *)

; PROCEDURE NewFixed ( MaxSize : INTEGER ; TwoPhase : BOOLEAN ) T
  (* Will not cater to growth beyond MaxSize Key-value pairs. *)
  (* If TwoPhase, all calls on either Insert or MakeAtom must precede
     a single call on Finalize, before any calls on Lookup. *)
  
; PROCEDURE NewGrowable ( InitSize : INTEGER ) T
  (* InitSize is an initial Key-value pair estimate.  Will auto-expand
     beyond this, if necessary. *) 

(* You can use a hash function of your choice, but all Hash values 
   passed in below must be computed from the adjacent Key value
   by the same function. *)

; PROCEDURE MakeAtom
    ( Dict : T
    ; Key : KeyTyp
    ; Hash : FM3Base . HashTyp
    )
  : ValTyp
  (* If Key is absent from Dict, assign a new atom value, post-incrementing
     NextAtom, and add the pair.  Either way, return the atom now associated
     with Key. *)

; END FM3Dict 
.

