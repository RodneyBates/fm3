
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Things to help looking at iternal operations.
   Suitable for calling within a debigger.
*)

INTERFACE FM3Introspection

; IMPORT FM3Globals

; PROCEDURE CurrentSrcFileName ( ) : TEXT 

; PROCEDURE DeclNoImage ( DeclNo : FM3Globals . DeclNoTyp ) : TEXT 
      
; END FM3Introspection
  .


  