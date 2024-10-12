        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Parsing the command line, its options, arguments, etc. *)
(* Store decoded values in FM3CLOptions. *)
(* See compiler/lib/FM3HelpText. *) 
(* See metaprogs/gentok/gen/FM3CLToks.gentok and generated
   files FM3CLToks.[im]3 in the same directory.
*) 

INTERFACE FM3CLArgs

; IMPORT Wr

; IMPORT FM3Base
; IMPORT FM3CLToks 
; IMPORT FM3SharedUtils

; VAR DoKeep : BOOLEAN := FALSE
      (* Keep intermediate files. *)
      
(*
; VAR DoStdErr : BOOLEAN := TRUE
      (* Write compilation process messages to stderr. *)
      
; VAR DoLog : BOOLEAN := TRUE
      (* Write compilation process messages to compiler log file. *)

; VAR LogFileWrT : Wr . T (* DoLog => Non-NIL and open. *) 

; VAR DoStdOut : BOOLEAN := TRUE
      (* Write compiled code messages to stdout. *)

; VAR DoCompLog : BOOLEAN := TRUE
      (* Write compiled code messages to unit-specific log file. *)
*)

; PROCEDURE Process ( )
  RAISES { FM3SharedUtils . Terminate } 

; PROCEDURE Cleanup ( )

; PROCEDURE ArgListAsText ( ) : TEXT

(* Compiler completion codes. *) 

; CONST CcPatchStackNotEmpty = 1 
; CONST CcPass1OutNotEmpty = 2 

; END FM3CLArgs
.

