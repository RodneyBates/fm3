
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC INTERFACE FM3Dict ( KeyGenformal , ValueGenformal )

(* KeyGenformal declares:
     . T
     . Hash
     . Brand.

   ValueGenformal declares:
     . T
     . Default, a member of T
     . Brand 
*) 

; IMPORT FM3Base 

; CONST
    Brand = "FM3Dict0.1_" & KeyGenformal . Brand & "_" & ValueGenformal . Brand 

; EXCEPTION Error ( TEXT )

(* For Hash parameters, You can use a hash function of your choice, as
   long as It is always the same for a given Key value, consistent for all
   calls involving a single dictionary, never produces FM3Utils.HashNull,
   and is always the same for a given Key value. 

   If you don't already have it for a Key you pass in, you can leave
   parameter Hash as FM3Utils.HashNull and the procedure will compute it,
   using the function passed to HashFunction.
*)

; TYPE HashFuncTyp = PROCEDURE ( Key : KeyGenformal . T ) : FM3Base . HashTyp 
   
(* Fixed dictionaries. *) 

(* Fixed dictionaries have some restrictions, but may be more compact
   and possibly faster, if you can with them and if MaxKeyCt is smallish. 
   They do not support growth beyond MaxKeyCt Keys. 
   All calls on InsertFixed must precede a call on FinalizeFixedFixed,
   before any calls on LookupFixed.  Also, duplicate keys will result
   in undetected duplicate entries, with different values, and
   nondeterministic results from LookupFixed.
*) 

; TYPE Private <: REFANY

; TYPE FixedTyp <: Private 
; TYPE GrowableTyp <: Private 

; VAR GMaxFixedSize := 15  
  (* More keys than this, and a hash table implementation will be
     used for a fixed dictionary. *) 

; PROCEDURE NewFixed
    ( MaxKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : FixedTyp 

; PROCEDURE InsertFixed 
    ( Dict : FixedTyp  
    ; Key : KeyGenformal . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueGenformal . T
    )
  RAISES { Error }  

; PROCEDURE FinalizeFixed ( Dict : FixedTyp ) 

; PROCEDURE LookupFixed  
    ( Dict : FixedTyp
    ; Key : KeyGenformal . T 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueGenformal . T 
    )
  : BOOLEAN (* Was found. *)
  RAISES { Error } 

(* Growable dictionaries: *)

(* Growable dictionaries will be auto-expanded as needed.
   Duplicate Key insertions leave only one entry.  Insertions
   and Lookups can be interspersed arbitrarily
*) 

; PROCEDURE NewGrowable
    ( InitKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : GrowableTyp 
      (* InitKeyCt is an initial  estimate.  *) 

; PROCEDURE InsertGrowable 
    ( Dict : GrowableTyp  
    ; Key : KeyGenformal . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueGenformal . T
    ; VAR (*OUT*) OldValue : ValueGenformal . T
      (* ^Meaningful IFF InsertGrowable returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Value.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *) 

; PROCEDURE LookupGrowable 
    ( Dict : GrowableTyp
    ; Key : KeyGenformal . T 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueGenformal . T 
    )
  : BOOLEAN (* Was found. *)

; END FM3Dict 
.

