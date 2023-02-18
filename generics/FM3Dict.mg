
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MOODLE FM3Dict ( KeyTyp , ValTyp )

(* KeyTyp declares:
     . T
     . Hash
     . Brand.

   ValTyp declares:
     . T
     . Default, a member of T
     . Brand 
*) 

; IMPORT FM3Base 

; REVEAL T
    = BRANDED"FM3(" & KeyTyp . Brand & "," & ValTyp , Brand & ")Dict_0.1"
      REF RECORD
          END (*RECORD*) 

(* Sizes are count of Key-value pairs.  Any extra space needed by
   the internal data structure will be added internally. *)

; PROCEDURE NewFixed ( MaxSize : INTEGER ; TwoPhase : BOOLEAN ) T
  (* Will not support growth beyond MaxSize Key-value pairs. *)
  (* If TwoPhase, all calls on Insert must precede a single call
     on Finalize, before any calls on Lookup.  There may be
     efficiency benefits to these restrictions. *)

  = BEGIN
      RETURN NEW ( T ) 
    END NewFixed 
  
; PROCEDURE NewGrowable ( InitSize : INTEGER ) T
  (* InitSize is an initial Key-value pair estimate.
     Will auto-expand beyond this, if necessary. *) 

  = BEGIN
      RETURN NEW ( T ) 
    END NewGrowable 

(* You can use a hash function of your choice, but all Hash values 
   passed in below must be computed from the adjacent Key value
   by the same function. *)

; PROCEDURE Insert
    ( Dict : T
    ; Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    ; Value : ValTyp
    )
  (* Using the value given.  Change the value, if already present. *) 

  = BEGIN
    END insert 

; PROCEDURE PhaseTwo ( Dict : T ) 

; PROCEDURE Lookup
    ( Dict : T
    ; Key : KeyTyp
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValTyp
    )
  : BOOLEAN (* Was found. *)

  = BEGIN
      RETURN FALSE 
    END Lookup 

; END FM3Dict 
.

