
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Utils

; TYPE HashTyp = LONGINT 

; PROCEDURE HashOfText ( Key : TEXT ) : HashTyp 

; PROCEDURE GroundHash ( ) : HashTyp  

; PROCEDURE ContributeToHash ( OldHash , Contribution : HashTyp ) : HashTyp
  (* A value of GroundHash(), altered by a series of ContributeToHash
     calls is a hash of the contributions.  Assume the order of the
     contributions affects the hash value. *) 

; END FM3Utils
.
