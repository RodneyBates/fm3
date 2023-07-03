
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Sets

(* The original purpose of this is for calling from a debugger. *) 

; IMPORT Fmt

; IMPORT IntSets 

(*EXPORTED*)
; PROCEDURE IntImage ( Val : INTEGER ) : TEXT

  = BEGIN
      RETURN Fmt . Int ( Val ) 
    END IntImage
    
(*EXPORTED*)
; PROCEDURE IntSetImage ( Val : IntSets . T ) : TEXT  

  = BEGIN
      RETURN IntSets . Image ( Val , IntImage , "" , 80 )  
    END IntSetImage
    
; BEGIN
  END FM3Sets
.

