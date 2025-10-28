
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Eliminate duplicate same-values copies of things accessed indirectly.
   Works on any type, but FM3Unique is useful only if HandleTyp is some kind
   of handle that indirectly denotes a value <value>(Handle) of a type unknown
   to FM3Unique. 
*) 

GENERIC MODULE FM3Unique ( UniquGenformal )

(* UniqueGenFormal must provide declarations for:
     TYPE HandleTyp:  Type of Handles. 
     PROCEDURE EqualValues : covered by EqualProcTyp.
       Such that EqualValues(H1,H2) actually returns <value>(H1)=<value>(H2).
     PROCEDURE HashFunc : covered by HashFuncTyp.  
       which depends only on <value>(Handle) (but not on Handle itself),
       and respects EqualValues, i.e. EqualValues(H1,H2)
       IMPLIES HashFunc(<value>(H1)) = HashFunc(<value>(H2)).
       The converse should hold only with high probability.
       Must never return FM3Utils.HashNull. 
     CONST Brand : TEXT = whatever you like.
*) 

; IMPORT FM3Base 
; IMPORT FM3Utils 

; CONST Brand = "FM3Unique0.1_" & UniqueGenformal . Brand 

; TYPE T <: REFANY

; TYPE HashFuncTyp: PROCEDURE (Handle : HandleTyp) : FM3Base.HashTyp

; TYPE EqualProcTyp = PROCEDURE ( Left , Right : HandleTyp ) : BOOLEAN

(*EXPORTED:*)
; PROCEDURE New ( InitSize : CARDINAL ) : T 
  (* A new, empty table of values. *)
  (* InitSize is an initial estimate of the eventual number of distinct values. 
     New will overallocate and expand internal allocations as needed.
  *)

  = BEGIN
      RETURN NIL
    END New 
  
(*EXPORTED:*)
; PROCEDURE Make
    ( Table : T
    ; READONLY Handle : HandleTyp
    ; Hash : FM3Base . HashTyp
        := FM3Utils.HashNull (* := HashFunc(<value>(Handle) *) 
    )
  : HandleTyp 
  (* If <value>(Handle) is absent from Table, insert it.
     Return the first equal-valued handle to have been inserted
     If you don't already have the hash for Handle you pass in, you can leave
     parameter Hash as FM3Utils.HashNull and the procedure will compute it,
     using the function passed to HashFunc.
  *)

  = BEGIN
      RETURN Handle
    END Make 

; BEGIN (*FM3Unique*)

  END FM3Unique
.

