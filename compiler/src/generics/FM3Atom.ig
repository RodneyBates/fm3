
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Map keys of a type to compactly-numbered, internally-generated
   integer values called "atoms".  Repeated adding of the same key
   value, as decided by function "Equal" will get the same atom value.
*) 

GENERIC INTERFACE FM3Atom ( DictGenformal )

(* DictGenFormal must provide declarations for:
     TYPE KeyTyp:  Type of Keys.  Not an open array type.
     TYPE HashFuncTyp: PROCEDURE (Key : KeyTyp) : FM3Base.HashTyp
     CONST Brand = whatever you like.
*) 

(*  declares:
     TYPE T Type of Keys
     VAR Compare : CompareProcTyp 
     CONST Brand = whatever you like.
*) 

; IMPORT FM3Base 

; CONST Brand = "FM3Atom0.1_" & DictGenformal . Brand 

; TYPE T <: REFANY

; TYPE KeyTyp = DictGenformal . KeyTyp

; TYPE HashFuncTyp = DictGenformal . HashFuncTyp 

; TYPE CompareProcTyp
    = PROCEDURE ( Left , Right : KeyTyp ) : FM3Base . CompareTyp 

; PROCEDURE New
    ( InitSize : CARDINAL ; StartAtom : INTEGER ; HashFunc : HashFuncTyp ) : T 
  (* A new, empty table of Key-value/atom pairs. *) 
  
; PROCEDURE MakeAtom
    ( Dict : T
    ; READONLY Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* If Key is absent from Dict, assign a new atom value and 
     add a Key-to-atom entry to Dict.  Either way, return the 
     atom now associated with Key. *)

  (* Size is an initial estimate of the eventual number of Keys. 
     Internal allocations will be expanded if and when needed.  *)

  (* FM3Base . AtomNull will not be assigned by MakeAtom. *) 

  (* You can use a hash function of your choice, but all Hash values 
     passed to MakeAtom for a given table T must be computed 
     consistently from the adjacent Key value by the same function. *)

; END FM3Atom
.

