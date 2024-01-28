
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE RangeUtils

; IMPORT Ranges_Int

; PROCEDURE TrimHi ( Range : Ranges_Int . RangeTyp ) : Ranges_Int . RangeTyp
  (* Shorten Range on the high end by one. *) 

; END RangeUtils
.


