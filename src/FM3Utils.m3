
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Utils

; IMPORT Long AS BitArith
; IMPORT Text 

; VAR GoundHashVar := 17345
; VAR ShiftFactor := 7 
  (* Don't be changing this while hash computations and their uses
     are going on! )

(*EXPORTED:*)
; PROCEDURE GroundHash ( ) : HashTyp

  = BEGIN
      RETURN GroundHashVar 
    END GroundHash 

(*EXPORTED:*)
; PROCEDURE ContributeToHash ( OldHash , Contribution : HashTyp ) : HashTyp
  (* A value of GroundHash(), altered by a series of ContributeToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *)

  = VAR LResult : HashTyp

  ; BEGIN
      LResult
        := BitArith . Xor
             ( BitArith . Shift ( OldHash , ShiftFactor ) , Contribution )
    ; RETURN LResult 
    END ContributeToHash

(*EXPORTED:*)
; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp

  = VAR LResult : HashTyp
  ; VAR LLength : CARDINAL 

  ; BEGIN
      LResult := GroundHash ( )
    ; LLength := Text . Length ( Key )
    ; FOR RI := 0 TO LLength - 1
      DO
        LResult
          := ContributeToHash
               ( LResult , VAL ( Text . GetWideChar ( Key , RI ) , HashTyp ) )  
      END (*FOR*) 
    ; RETURN LResult 
    END HashOfText 

; END FM3Utils
.
