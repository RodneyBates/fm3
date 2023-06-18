
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedUtils

; IMPORT AtomList 
; IMPORT Rd

; IMPORT FM3Base 

; <*IMPLICIT*>
  EXCEPTION Terminate ( TEXT ) 

; EXCEPTION FatalError ( TEXT )

; PROCEDURE StandaloneFatalError ( Msg : TEXT )
  (* Convenience procedure for catchers of FatalError.
     Just write to stderror.
  *) 

; PROCEDURE AtomListToText ( List : AtomList . T ): TEXT

; PROCEDURE CatStrings
    ( T0 , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : REFANY := NIL ) : TEXT
  (* Each Tn can be TEXT, Atom.T, or AtomList.T. *) 
    
; TYPE FileKindTyp = CHAR

; PROCEDURE FileKindImage ( Kind : FileKindTyp ) : TEXT

; CONST FM3FileKindIntPkl = 'A'
; CONST FM3FileKindM3RwPkl = 'B'
; CONST FM3FileKindPgRwPkl = 'C'

; PROCEDURE FilePrefix ( Kind : CHAR ) : TEXT

; PROCEDURE OpenRd
    ( FileName , PathName , Note1 , Note2 : TEXT := "" )
  : Rd . T
  RAISES { FatalError } 

; PROCEDURE OpenResourceRd
    ( FileName : TEXT := "" ; ExpectedFileKind : FileKindTyp )
  : Rd . T
  RAISES { FatalError } 

; PROCEDURE IntHash ( Val : INTEGER ) : FM3Base . HashTyp

; VAR ResourcePathName := "." 
  (* ^Since this is a shared compiler/metaprogs interface, clients
     need to set it as desired. *) 
; END FM3SharedUtils 
.
