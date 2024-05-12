
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Map keys of a type to compactly-numbered, internally-generated
   integer values called "atoms".  Repeated adding of the same key
   value, as decided by function "Equal" will get the same atom value.
*) 

GENERIC INTERFACE FM3Atom ( DictGenformal )

(* DictGenFormal must provide declarations for:
     TYPE KeyTyp:  Type of Keys.  Not an open array type
                   (But could be REF thereto) .
     TYPE HashFuncTyp: PROCEDURE (Key : KeyTyp) : FM3Base.HashTyp
     CONST Brand = whatever you like.
   These can be provided by an instantiation of FM3Dict. 
*) 

; IMPORT FM3Base 

; CONST Brand = "FM3Atom0.1_" & DictGenformal . Brand 

; TYPE T <: REFANY

; TYPE KeyTyp = DictGenformal . KeyTyp

; TYPE HashFuncTyp = DictGenformal . HashFuncTyp 

; TYPE CompareProcTyp
    = PROCEDURE ( Left , Right : KeyTyp ) : FM3Base . CompareTyp 

; PROCEDURE New
    ( InitSize : CARDINAL
    ; StartAtom : INTEGER
    ; HashFunc : HashFuncTyp
    ; DoReverseMap : BOOLEAN 
    )
  : T 
  (* A new, empty table of Key-value/atom pairs. *)
  (* InitSize is an initial estimate of the eventual number of Keys. 
     New will expand internal allocations if and when needed.  *)
  (* You can use a hash function of your choice, but all Hash values 
     passed to MakeAtom for a given table T must be computed 
     consistently from the adjacent Key value by the same function.
     If NOT DoReverseMap, procedure Key will always return FALSE. *) 
  
; PROCEDURE MakeAtom
    ( Dict : T
    ; READONLY Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* If Key is absent from Dict, assign a new atom value and 
     add a Key-to-atom entry to Dict.  Either way, return the 
     atom now associated with Key.  MakeAtom  will not return
     FM3Base.AtomNull.  
   *)

; PROCEDURE LookupKey
    ( AtomDict : T 
    ; READONLY Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* The atom associated with Key. FM3Base . AtomNull if not present *)

; PROCEDURE Key
    ( AtomDict : T ; Atom : FM3Base . AtomTyp ; VAR Value : KeyTyp )
  : BOOLEAN (*Found*)

  (* Size is an initial estimate of the eventual number of Keys. 
     Internal allocations will be expanded if and when needed.

     You can use a hash function of your choice, but all Hash values 
     passed to MakeAtom for a given table T must be computed 
     consistently from the adjacent Key value by the same function.

     If you don't already have it for a Key you pass in, you can leave
     parameter Hash as FM3Utils.HashNull and the procedure will compute it,
     using the function passed to HashFunction.
  *)


; END FM3Atom
.

