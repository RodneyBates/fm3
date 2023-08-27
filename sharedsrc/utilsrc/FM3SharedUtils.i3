
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedUtils

; IMPORT AtomList 
; IMPORT Rd
; IMPORT Thread 

; IMPORT IntSets 

; IMPORT FM3Base 
; IMPORT FM3LexTable 

; <*IMPLICIT*>
  EXCEPTION Terminate ( TEXT ) 

; <*IMPLICIT*>
  EXCEPTION FatalError ( TEXT )

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

; PROCEDURE FilePrefix ( Kind : CHAR ) : TEXT

; PROCEDURE OpenRd
    ( FileName , PathName , Note1 , Note2 : TEXT := "" )
  : Rd . T
  RAISES { FatalError } 

; PROCEDURE OpenResourceRd
    ( FileName : TEXT := "" ; ExpectedFileKind : FileKindTyp )
  : Rd . T
  RAISES { FatalError , Thread . Alerted } 

; PROCEDURE ReadPickle
    ( FileName : TEXT ; ExpectedKind : FileKindTyp )
  : REFANY
  RAISES { FatalError , Thread . Alerted } 

; PROCEDURE ReadFsm ( FileName : TEXT ; Kind : FileKindTyp )
    : FM3LexTable . T
    RAISES { Thread . Alerted } 

; PROCEDURE ReadSets
    ( FileName : TEXT
    ; Kind : FileKindTyp
    ; VAR Temp : IntSets . T 
    ; VAR Patch : IntSets . T 
    ; VAR Arg1 : IntSets . T 
    ; VAR Arg2 : IntSets . T 
    ; VAR Arg3 : IntSets . T 
    ; VAR Arg4 : IntSets . T 
    ; VAR Arg5 : IntSets . T 
    ; VAR Arg6 : IntSets . T 
    )
  RAISES { FatalError , Thread . Alerted } 

; PROCEDURE LoadSets ( )
  RAISES { FatalError , Thread . Alerted } 

; PROCEDURE IntHash ( Val : INTEGER ) : FM3Base . HashTyp

; VAR ResourcePathName := "../lib" 
  (* ^Since this is a shared compiler/metaprogs interface, clients
     need to set it as desired. *) 
; END FM3SharedUtils 
.
