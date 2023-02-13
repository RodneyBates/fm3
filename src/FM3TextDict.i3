
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3TextDict

; IMPORT FM3Base 

; TYPE T <: REFANY

; TYPE KeyTyp = TEXT
; TYPE ValTyp = INTEGER 

(* Sizes are count of Key-value pairs.  Any extra space needed by
   the internal data structure will be added internally. *)

; PROCEDURE NewFixed ( MaxSize : INTEGER ; TwoPhase : BOOLEAN ) T
  (* Will not cater to growth beyond MaxSize Key-value pairs. *)
  (* If TwoPhase, all calls on either Insert or MakeAtom must precede
     a single call on Finalize, before any calls on Lookup. *)
  
; PROCEDURE PhaseTwo ( Dict : T ) 

; PROCEDURE NewGrowable ( InitSize : INTEGER ) T
  (* InitSize is an initial Key-value pair estimate.  Will auto-expand
     beyond this, if necessary. *) 

(* All Key-Hash pairs below must match, as as computed by FM3Utils.TextHash *)

; PROCEDURE Insert
    ( Dict : T
    ; Key : KeyTyp
    ; Hash : FM3Base . HashTyp
    ; Value : ValTyp
    )
  (* Using the value given.  Change the value, if already present. *) 

; PROCEDURE MakeAtom
    ( Dict : T
    ; Key : KeyTyp
    ; Hash : FM3Base . HashTyp
    ; VAR NextAtom : ValType
    )
  : ValTyp
  (* If Key is absent, assign a new atom value, post-incrementing NextAtom,
     and add the pair.  Either way, return the atom now associated with Key. *)

; PROCEDURE Lookup
    ( Dict : T
    ; Key : KeyTyp
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val ValTyp
    )
  : BOOLEAN (* Was found. *)

; END FM3TextDict 
.

