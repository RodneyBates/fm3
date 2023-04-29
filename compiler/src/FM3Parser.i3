(* Derived from: ** $Id: Parser.md,v 2.1 1992/08/07 15:28:42 grosch rel $ *)
(* Parser.m30 *)
(* Modified Rodney M. Bates. Various times. rodney.m.bates@acm.org*)
(* Beginning 3-2023: 
    3-2023 Add generation of Module-3 code. 
    Further changes in https://github.com/RodneyBates/cocktail
*) 

INTERFACE FM3Parser;

(* line 16 "FM3Parser.lalr" *)
 

VAR ParsTabName	: ARRAY [0..128] OF CHAR;

PROCEDURE TokenName (Token: INTEGER; VAR Name: TEXT);

PROCEDURE FM3Parser (): CARDINAL;
PROCEDURE CloseFM3Parser () ;

END FM3Parser.
