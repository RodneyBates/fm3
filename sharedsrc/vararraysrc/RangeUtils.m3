
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE RangeUtils

; IMPORT Ranges_Int

(* TODO: move this into Ranges.[im]g. *)

; PROCEDURE TrimHi ( Range : Ranges_Int . RangeTyp ) : Ranges_Int . RangeTyp
  (* Shorten Range by on the high end. *) 

  = BEGIN (*TrimHi*)
      IF Ranges_Int . RangeIsEmpty ( Range )
      THEN RETURN Ranges_Int . EmptyRange
      ELSIF Range . Hi = FIRST ( Ranges_Int . BaseTyp )  
      THEN RETURN Ranges_Int . EmptyRange
      ELSE RETURN Ranges_Int . RangeTyp { Range . Lo , Range . Hi - 1 } 
      END (*IF*) 
    END TrimHi 

; BEGIN
  END RangeUtils
.
