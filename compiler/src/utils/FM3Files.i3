
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Files

; IMPORT UniRd 

; PROCEDURE OpenUniRd
    ( FileName , PathHame , Note1 , Note2 : TEXT := "" ) : UniRd . T

; END FM3Files
.
