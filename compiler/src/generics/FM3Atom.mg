
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Map keys of an type to compactly-numbered, internally-generated
   integer values called "atom".  Repeated adding of the same keyu
   valud, as decided by function "Equal" will get the same atom value.
*) 

GENERIC MODULE FM3Atom ( DictGenformal )

(* KeyTyp declares:
     TYPE T Type of Keys
     VAR Compare : CompareProcTyp 
     CONST Brand = whatever you like.
*) 

; IMPORT FM3Base 

; REVEAL T =
    BRANDED "FM3Atom_" & DictGenformal . Brand & "_0.1"
    REF RECORD
      AtGrowableDict : DictGenformal  . GrowableTyp 
    ; AtNextAtom : FM3Base . AtomTyp := 1 
    END (*RECORD*)

(*EXPORTED:*)
; PROCEDURE New
    ( InitSize : CARDINAL ; StartAtom : INTEGER ; HashFunc : HashFuncTyp ) : T 
  (* A new, empty table of Key-value/atom pairs. *) 

  = VAR LResult : T

  ; BEGIN
      LResult
        := NEW ( T ) 
    ; LResult . AtGrowableDict
        := DictGenformal . NewGrowable ( InitSize , HashFunc )
    ; LResult . AtNextAtom := StartAtom 
    ; RETURN LResult 
    END New 
  
(*EXPORTED:*)
; PROCEDURE MakeAtom
    ( AtomDict : T 
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* If Key is absent from AtomDict, assign a new atom value and 
     add a Key-to-atom entry to AtomDict.  Either way, return the 
     atom now associated with Key. *)

  (* Size is an initial estimate of the eventual number of Keys. 
     Internal allocations will be expanded if and when needed.  *)

  (* You can use a hash function of your choice, but all Hash values 
     passed to MakeAtom for a given table T must be computed 
     consistently from the adjacent Key value by the same function. *) 

  = VAR LNewValue , LOldValue: INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      LNewValue := AtomDict . AtNextAtom 
    ; LFound 
        := DictGenformal. InsertGrowable 
             ( AtomDict . AtGrowableDict
             , Key , Hash , LNewValue , (*OUT*) LOldValue
             , DoUpdate := FALSE 
             )
    ; IF LFound
      THEN RETURN LOldValue
      ELSE
        INC ( AtomDict . AtNextAtom )
      ; RETURN LNewValue 
      END (*IF*) 
    END MakeAtom 

; BEGIN
  END FM3Atom 
.

