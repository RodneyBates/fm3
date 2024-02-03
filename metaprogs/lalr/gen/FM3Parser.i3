
(* This file was generated by metaprogram lalr,
   with command line:
   ../lib/lalr -M -g -i -t -d -D -k FM3Parser.lalr
*)




  INTERFACE FM3Parser;

(* line 22 "FM3Parser.lalr" *)

(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024 Rodney M. Bates.                                     *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

    IMPORT FM3IntToks;
    IMPORT FM3ParsePass;
  

VAR ParsTabName	: TEXT; 

PROCEDURE TokenName (Token: INTEGER; VAR Name: TEXT);

  PROCEDURE FM3Parser (): CARDINAL;
  PROCEDURE CloseFM3Parser ();

  END FM3Parser.
