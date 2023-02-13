
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3TextDict

; IMPORT FM3Base 

; TYPE T <: REFANY

; TYPE KeyTyp = TEXT
; TYPE ValType = INTEGER 

; PROCEDURE NewFixed ( Size : INTEGER ) T  

; PROCEDURE NewGrowable ( Size : INTEGER ) T

; PROCEDURE Lookup ( Key : KeyTyp ; Hash : FM3Base . HashTyp ) : ValTyp
  (* If NIL, this procedure will compute it internally.  Otherwise,
     it must match Key, as computed by FM3Utils.TextHash. *)  

; PROCEDURE MakeAtom ( Key : KeyTyp ; Hash : FM3Base . HashTyp ) : ValTyp  

; END FM3TextDict
.

