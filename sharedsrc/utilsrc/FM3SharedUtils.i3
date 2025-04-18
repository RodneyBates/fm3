
(*---------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3SharedUtils

; IMPORT AtomList 
; IMPORT Rd
; IMPORT Thread
; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Base 
; IMPORT FM3LexTable
; IMPORT FM3SharedGlobals

; <*IMPLICIT*>
  EXCEPTION Terminate ( TEXT ) 

; <*IMPLICIT*>
  EXCEPTION AllocationFailure ( TEXT )

; <*IMPLICIT*>
  EXCEPTION FatalError ( TEXT )

; PROCEDURE LongHexImage ( Value : LONGINT ) : TEXT 

; <*INLINE*> PROCEDURE RefanyImage ( Value : REFANY ) : TEXT

; PROCEDURE PluralSuffix ( Value : INTEGER ) : TEXT 

; PROCEDURE StandaloneFatalError ( Msg : TEXT )
  (* Convenience procedure for catchers of FatalError.
     Just write to stderror.
  *) 

; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT 

; PROCEDURE SibDirectoryPath ( FileName : TEXT ; SibDirName : TEXT ) : TEXT
(* Absolute path name of "SibDir" directory beside parent directory of
   FileName.
*) 

; PROCEDURE AtomListToText ( List : AtomList . T ): TEXT

; PROCEDURE CatStrings
    ( T0 , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : REFANY := NIL ) : TEXT
  (* Each Tn can be TEXT, Atom.T, or AtomList.T. *) 
    
; PROCEDURE CatArrT
    ( READONLY Arr : ARRAY OF REFANY ; T0 : TEXT := NIL ) : TEXT
  (* T0 and each Arr [ I ] can be TEXT, Atom.T, or AtomList.T. *)
  (* T0 will appear *left* of elements of Arr.  
     Although this signature order seems very peculiar, it allows message
     procedures to prepend a tag such as "error", in color, while
     allowing the rest to be passed multiple levels as one parameter.
  *) 
    
; PROCEDURE PutPosImage ( WrT : Wr . T ; Position : FM3Base . tPosition )

; PROCEDURE PutTextish ( WrT : Wr . T ; Textish : REFANY )
  (* Textish can be NIL, TEXT, Atom.T, AtomList.T, REF ARRAY OF CHAR,
     or REF ARRAY OF REFANY.
  *) 

; PROCEDURE PutTextishArr ( WrT : Wr . T ; READONLY Arr : ARRAY OF REFANY )

; PROCEDURE FileKindImage ( Kind : FM3SharedGlobals . FileKindTyp ) : TEXT

; PROCEDURE FilePrefixT
    ( Kind : FM3SharedGlobals . FileKindTyp
    ; Version : FM3SharedGlobals . FileVersionTyp
    )
  : TEXT 

; PROCEDURE FilePrefixA
    ( Kind : FM3SharedGlobals . FileKindTyp
    ; Version : FM3SharedGlobals . FileVersionTyp
    )
  : ARRAY [ 0 .. 7 ] OF CHAR  

; PROCEDURE ReadPrefix
    ( RdT : Rd . T
    ; VAR Kind : FM3SharedGlobals . FileKindTyp
    ; VAR Version : FM3SharedGlobals . FileVersionTyp
    ; VAR IsOK : BOOLEAN
    )

; PROCEDURE OpenRd
    ( DirName , FileName , Note1 , Note2 : TEXT := "" )
  : Rd . T
  RAISES { FatalError } 

; PROCEDURE OpenResourceRd
    ( FileName : TEXT := ""
    ; ExpectedFileKind : FM3SharedGlobals . FileKindTyp
    )
  : Rd . T
  RAISES { FatalError , Thread . Alerted } 

; PROCEDURE ReadPickle
    ( FileName : TEXT ; ExpectedKind : FM3SharedGlobals . FileKindTyp )
  : REFANY
  RAISES { FatalError , Thread . Alerted } 

; PROCEDURE ReadFsm ( FileName : TEXT ; Kind : FM3SharedGlobals . FileKindTyp )
    : FM3LexTable . T
    RAISES { Thread . Alerted } 

; PROCEDURE ReadSets
    ( FileName : TEXT
    ; Kind : FM3SharedGlobals . FileKindTyp
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

; VAR ResourceDirName := "../lib" 
  (* ^Since this is a shared compiler/metaprogs interface, clients
     can set it as desired. *)

; PROCEDURE DefaultResourceDirName ( ) : TEXT 

; PROCEDURE Blanks ( Length : INTEGER ) : TEXT 
  (* A TEXT of length Length, all blanks. *) 

; PROCEDURE DeleteFile ( FileName : TEXT )
(* PRE-noncondition: File does not necessarily exist. *) 

; PROCEDURE IntSetsImage
    ( Set : IntSets . T ; LinePrefix : TEXT ; MaxLine : CARDINAL ) : TEXT 

; END FM3SharedUtils 
.
