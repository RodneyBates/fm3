
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE FM3Dict ( Key , Value )

(* Key declares:
     . T
     . Hash
     . Brand.

   Value declares:
     . T
     . Default, a member of T
     . Brand 
*) 

; IMPORT FM3Base 

; TYPE T <: Public

; TYPE Public
    = OBJECT METHODS
        insert
          ( Key : Key . T  
          ; Hash : FM3Base . HashTyp
          ; Value : Value . T
          ; VAR OldValue : Value . T (* Meaningful IFF returns TRUE. *) 
          ; DoUpdate : BOOLEAN := FALSE
            (* ^If Key is already present, update its Velue.
               Otherwise, leave the value unchanged. *) 
          )
        : BOOLEAN (* Key was already present. *) 

      ; enterPhaseTwo ( ) 

      ; lookup
          ( Key : Key . T 
          ; Hash : FM3Base . HashTyp
          ; VAR (*OUT*) Val : Value . T 
          )
        : BOOLEAN (* Was found. *)
      END (*Public*) 

(* Sizes are count of Key-value pairs.  Any extra space needed by
   the internal data structure will be added internally. *)

; PROCEDURE NewFixed ( MaxSize : INTEGER ; TwoPhase : BOOLEAN ) : T
  (* Will not support growth beyond MaxSize Key-value pairs. *)
  (* If TwoPhase, all calls on Insert must precede a single call
     on Finalize, before any calls on Lookup.  There may be
     efficiency benefits to these restrictions. *)
  
; PROCEDURE NewGrowable ( InitSize : INTEGER ) : T
  (* InitSize is an initial Key-value pair estimate.
     Will auto-expand beyond this, if necessary. *) 

(* You can use a hash function of your choice, but all Hash values 
   passed in below must be computed from the adjacent Key value
   by the same function. *)

; END FM3Dict 
.

