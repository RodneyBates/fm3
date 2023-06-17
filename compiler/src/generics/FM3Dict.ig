
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

; CONST InstantiationBrand
        = "Fm3Dict0.1_" & KeyGenformal . Brand & "_" & ValueGenformal . Brand
; CONST BaseTypBrand = InstantiationBrand & "_BaseTyp"
; CONST GrowableTypBrand = InstantiationBrand & "_GrowableTyp"
; CONST FixedTypBrand = InstantiationBrand & "_FixedTyp"

; CONST Brand = InstantiationBrand 

; EXCEPTION Error ( TEXT )

(* For Hash parameters, You can use a hash function of your choice, as
   long as it is always the same for a given Key value, consistent for all
   calls involving a single dictionary, and never produces FM3Utils.HashNull,

   If you don't already have it for a Key you pass in, you can leave
   parameter Hash as FM3Utils.HashNull and the procedure will compute it,
   using the function passed to HashFunction.
*)

; TYPE KeyTyp = KeyGenformal . T 

; TYPE HashFuncTyp = PROCEDURE ( Key : KeyTyp ) : FM3Base . HashTyp 
   
; TYPE Private <: REFANY

(* Fixed dictionaries. *) 

(* Fixed dictionaries have some restrictions, but may be more compact
   and possibly faster, if you can live with them and if MaxKeyCt is
   smallish.  All calls on InsertFixed must happen before one call on
   FinalizeFixedFixed, before any calls on LookupFixed.  Also, duplicate
   keys will result Error's being raised, or possibly in undetected
   duplicate entries, with different values, and nondeterministic
   results from LookupFixed.
*) 

; TYPE FixedTyp <: Private 

; VAR GMaxFixedSize := 15  
  (* More keys than this, and a hash table implementation will be
     used for a fixed dictionary. *) 

; PROCEDURE NewFixed
    ( MaxKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : FixedTyp 

; PROCEDURE InsertFixed 
    ( Dict : FixedTyp  
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueGenformal . T
    )
  RAISES { Error }  

; PROCEDURE FinalizeFixed ( Dict : FixedTyp ) RAISES { Error }

; PROCEDURE LookupFixed  
    ( Dict : FixedTyp
    ; Key : KeyTyp 
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

; TYPE GrowableTyp <: Private 

; PROCEDURE NewGrowable
    ( InitKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : GrowableTyp 
      (* InitKeyCt is an initial  estimate.  *) 

; PROCEDURE InsertGrowable 
    ( Dict : GrowableTyp  
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueGenformal . T
    ; VAR (*OUT*) OldValue : ValueGenformal . T
      (* ^Meaningful IFF InsertGrowable returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Value.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *) 
  RAISES { Error } 

; PROCEDURE LookupGrowable 
    ( Dict : GrowableTyp
    ; Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueGenformal . T 
    )
  : BOOLEAN (* Was found. *)
  RAISES { Error } 

; END FM3Dict 
.

