        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3CLArgs

; IMPORT Wr 

; VAR SrcFileName : TEXT := NIL 

; VAR DoKeep : BOOLEAN := FALSE
      (* Keep intermediate files. *)

; VAR DoStdErr : BOOLEAN := TRUE
      (* Write compilation process messages to stderr. *)
      
; VAR DoLog : BOOLEAN := TRUE
      (* Write compilation process messages to compiler log file. *)
; VAR LogFileName := "FM3Log"
; VAR LogFileWrT : Wr . T (* DoLog => Non-NIL and open. *) 

; VAR DoStdOut : BOOLEAN := TRUE
      (* Write compiled code messages to stdout. *)

; VAR DoCompLog : BOOLEAN := TRUE
      (* Write compiled code messages to unit-specific log file. *)

; PROCEDURE Process ( )

; PROCEDURE Cleanup ( )

; END FM3CLArgs
.

