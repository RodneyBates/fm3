
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Base

; IMPORT Fmt 
; IMPORT Text

(*EXPORTED*) 
; PROCEDURE IntImage ( Value : INTEGER ) : TEXT
  (* Result never has blanks. *) 

  = VAR LImage : TEXT := Fmt . Int ( Value ) 
  ; VAR LLength := Text . Length ( LImage ) 

  ; BEGIN (* IntImage *) 
      IF LLength > 0 AND Text . GetChar ( LImage , 0 ) = ' ' 
      THEN 
        RETURN Text . Sub ( LImage , 1 ) 
      ELSE 
        RETURN LImage 
      END (* IF *) 
    END IntImage 

(*EXPORTED*) 
; PROCEDURE Int64Image ( Value : LONGINT ) : TEXT 
  (* Result never has blanks. *)
  
  = VAR LImage : TEXT := Fmt . LongInt ( Value ) 
  ; VAR LLength := Text . Length ( LImage ) 

  ; BEGIN (* IntImage *) 
      IF LLength > 0 AND Text . GetChar ( LImage , 0 ) = ' ' 
      THEN 
        RETURN Text . Sub ( LImage , 1 ) 
      ELSE 
        RETURN LImage 
      END (* IF *) 
    END Int64Image 

(*EXPORTED*) 
; PROCEDURE PassNoSetUnion ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

  = BEGIN (*PassNoSetUnion*)
      Left := Left + Right
    END PassNoSetUnion 

(*EXPORTED*) 
; PROCEDURE PassNoSetDiff ( VAR Left : PassNoSetTyp ; Right : PassNoSetTyp )

  = BEGIN (*PassNoSetDiff*)
      Left := Left - Right
    END PassNoSetDiff 

(*EXPORTED*) 
; PROCEDURE InclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoRangeTyp )

  = BEGIN (*InclPassNo*) 
      Set := Set + PassNoSetTyp { No } 
    END InclPassNo 

(*EXPORTED*) 
; PROCEDURE ExclPassNo ( VAR Set : PassNoSetTyp ; No : PassNoRangeTyp )

  = BEGIN (*ExclPassNo*) 
      Set := Set - PassNoSetTyp { No } 
    END ExclPassNo 

; BEGIN
  END FM3Base
.

