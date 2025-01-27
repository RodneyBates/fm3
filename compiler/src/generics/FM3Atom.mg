
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Map keys of a type to compactly-numbered, internally-generated
   integer values called "atoms".  Repeated adding of the same key
   value, as decided by function "Equal" will get the same atom value.
*) 

GENERIC MODULE FM3Atom ( DictGenformal )

(* DictGenFormal must provide declarations for these things that
     an instantiation of FM3Dict will provide: 
     GrowableTyp , NewGrowable, InsetGrowable, LookupGrowable, Brand
*) 

; IMPORT FM3Base 

; REVEAL T =
    BRANDED Brand 
    REF RECORD
      AtGrowableDict : DictGenformal  . GrowableTyp 
    ; AtNextAtom : FM3Base . AtomTyp := 1
    ; AtReverseMap : REF ARRAY OF KeyTyp 
    END (*RECORD*)

(*EXPORTED:*)
; PROCEDURE New
    ( InitSize : CARDINAL
    ; StartAtom : INTEGER
    ; HashFunc : HashFuncTyp
    ; DoReverseMap : BOOLEAN 
    )
  : T 
  (* A new, empty table of Key-value/atom pairs. *) 
  (* InitSize is an initial estimate of the eventual number of Keys. 
     New will overallocate and expand internal allocations as needed.  *)
  (* You can use a hash function of your choice, but all Hash values 
     passed to MakeAtom for a given table T must be computed 
     consistently from the adjacent Key value by the same function.
     If NOT DoReverseMap, procedure Key will always return FALSE. *) 

  = VAR LResult : T

  ; BEGIN
      LResult := NEW ( T ) 
    ; LResult . AtGrowableDict
        := DictGenformal . NewGrowable ( InitSize , HashFunc )
    ; LResult . AtNextAtom := StartAtom
    ; IF DoReverseMap 
      THEN  
        LResult . AtReverseMap := NEW ( REF ARRAY OF KeyTyp , InitSize ) 
      ELSE LResult . AtReverseMap := NIL
      END (*IF*) 
    ; RETURN LResult 
    END New 
  
(*EXPORTED:*)
; PROCEDURE MakeAtom
    ( AtomDict : T 
    ; READONLY Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* If Key is absent from AtomDict, assign a new atom value and 
     add a Key-to-atom entry to AtomDict.  Either way, return the 
     atom now associated with Key. *)

  = VAR LNewValue , LOldValue: INTEGER
  ; VAR LNumber , LNewSize: INTEGER
  ; VAR LNewMapRef : REF ARRAY OF KeyTyp 
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
      ELSE (* Create a new atom. *) 
        INC ( AtomDict . AtNextAtom )
      ; IF AtomDict  . AtReverseMap # NIL
        THEN
          LNumber := NUMBER ( AtomDict . AtReverseMap ^ )
        ; IF LNewValue >= LNumber
          THEN
            LNewSize := MIN ( LNumber * 2 , LNewValue + 10 ) 
          ; LNewMapRef := NEW ( REF ARRAY OF KeyTyp , LNewSize )
          ; SUBARRAY ( LNewMapRef ^ , 0 , LNumber ) 
              := SUBARRAY ( AtomDict . AtReverseMap ^ , 0 , LNumber ) 
          ; AtomDict . AtReverseMap := LNewMapRef 
          END (*IF*) 
        ; AtomDict . AtReverseMap ^ [ LNewValue ] := Key 
        END (*IF*) 
      ; RETURN LNewValue 
      END (*IF*) 
    END MakeAtom 

(*EXPORTED:*)
; PROCEDURE LookupKey
    ( AtomDict : T 
    ; READONLY Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    )
  : FM3Base . AtomTyp 
  (* The atom associated with Key. FM3Base . AtomNull if not present *)

  = VAR LValue : INTEGER
  ; VAR LFound : BOOLEAN 

  ; BEGIN
      LFound 
        := DictGenformal. LookupGrowable 
             ( AtomDict . AtGrowableDict , Key , Hash , (*OUT*) LValue )
    ; IF LFound
      THEN RETURN LValue
      ELSE RETURN FM3Base . AtomNull
      END (*IF*) 
    END LookupKey 
      
(*EXPORTED.*)
; PROCEDURE Key
    ( AtomDict : T ; Atom : FM3Base . AtomTyp ; VAR Value : KeyTyp )
    : BOOLEAN (*Found*)

  = BEGIN (*Key*)
      IF AtomDict . AtReverseMap = NIL THEN RETURN FALSE END (*IF*) 
    ; IF Atom < FIRST ( AtomDict . AtReverseMap ^ ) THEN RETURN FALSE END (*IF*) 
    ; IF Atom >= AtomDict . AtNextAtom THEN RETURN FALSE END (*IF*)
    ; Value := AtomDict . AtReverseMap ^ [ Atom ] 
    ; RETURN TRUE 
    END Key
      
; BEGIN
  END FM3Atom 
.

