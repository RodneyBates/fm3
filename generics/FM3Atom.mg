
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3Atom ( KeyTyp , ValTyp )

(* KeyTyp dec(* Map keys of an type to compactly-numbered, internally-generated
   integer values called "atom".  Repeated adding of the same keyu
   valud, as decided by function "Equal" will get the same atom value. 

GENERIC INTERFACE FM3Atom ( KeyTyp ) 

(* KeyTyp declares:
     TYPE T Type of Keys
     CONST AreEqual : EqProcTyp
     CONST Brand = whatever you like.
*) 

; IMPORT FM3Base 

; REVEAL T =
    BRANDED "FM3(" & KeyTyp . Brand & ")Atom"_0.1"
    REF RECORD 
    EMD (*RECORD*)


; PROCEDURE New ( Equal : EqProc ; InitSize : CARDINAL ) T 
  (* A new, empty table of Key-value/atom pairs. *) 

  = BEGIN 
      RETURN NEW ( T ) 
    END New 
  
; PROCEDURE MakeAtom
    ( Dict : T
    ; Key : KeyTyp
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* If Key is absent from Dict, assign a new atom value and 
     add a Key-to-atom entry to Dict.  Either way, return the 
     atom now associated with Key. *)

  (* Size is an initial estimate of the eventual number of Keys. 
     Internal allocations will be expanded if and when needed.  *)

  (* You can use a hash function of your choice, but all Hash values 
     passed to MakeAtom for a given table T must be computed 
     consistently from the adjacent Key value by the same function. *) 

  = BEGIN 
      RETURN FM3Base . AtomNull 
    END MakeAtom 

; END FM3Dict 
.

