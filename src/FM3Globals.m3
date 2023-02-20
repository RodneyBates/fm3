
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Globals

; IMPORT TextIntTbl 

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Toks 

; CONST M3RwCt = 61
; CONST PgRwCt = 20 

; PROCEDURE Init ( )

  = BEGIN
      IdentAtom := FM3Atom_OAChars . NewGrowable ( IdentAtomSize ) 
    ; TextAtom := FM3Atom_OAChars . NewGrowable ( TextAtomSize ) 
    ; WideTextAtom := FM3Atom_OAWideChars . NewGrowable ( WideTextAtomSize )
(* TODO: Populate RwDict *) 
    END Init

; PROCEDURE InitM3RwTbl ( )
  = BEGIN
(*
      M3RwTbl := TextIntTbl . init ( M3RwCt + M3RwCt DIV 2 )
    ; M3RwTbl . put ( "BEGIN" , FM3Toks . TkRwBegin )
(* Finish me. *)
*) 
    END InitM3RwTbl 

; PROCEDURE InitPgRwTbl ( )

  = BEGIN
(*  
      PragmeRwTbl := TextIntTbl . init ( PgRwCt + PgRwCt DIV 2 )
    ; PgRwTbl . put ( "INLINE" , FPragmaToks . TkPgInline )
(* Finish me. *)
*)
    END InitPgRwTbl 

; BEGIN
    InitM3RwTbl ( )
  ; InitPgRwTbl ( ) 
  ; Init ( ) 
  END FM3Globals
.
