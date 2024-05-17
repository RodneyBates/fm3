
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Globals

; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *)

; IMPORT Ranges_Int 

; CONST M3RwCt = 61
; CONST PgRwCt = 20 

(*EXPORTED*) 
; PROCEDURE Init ( )

  = BEGIN
      InitM3RwTbl ( )
    ; InitPgRwTbl ( ) 
    ; SkipNoStack 
        := VarArray_Int_Int . New
             ( FIRST ( INTEGER )
             , Ranges_Int . RangeTyp {  0 , InitSkipStackCt - 1 }
             )
    ; VarArray_Int_Int . Touch (* It needs a lower bound. *) 
        ( SkipNoStack , Ranges_Int . RangeTyp { 0 , 0 } )   
    ; NextSkipNo := 1 (* But don't use element 0. *) 
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
      PragmaRwTbl := TextIntTbl . init ( PgRwCt + PgRwCt DIV 2 )
    ; PgRwTbl . put ( "INLINE" , FPragmaToks . TkPgInline )
(* Finish me. *)
*)
    END InitPgRwTbl

(*EXPORTED*) 
; PROCEDURE Finalize ( )

  = BEGIN 
      <* ASSERT
           VarArray_Int_Int . TouchedRange ( SkipNoStack ) 
             = Ranges_Int . RangeTyp { 0 , 0 }
      *> 
    END Finalize 

; BEGIN
  END FM3Globals
.
