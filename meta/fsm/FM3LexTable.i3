
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3LexTable  

(* A fast two-way mapping between integers and strings.  For string-to-int,
   uses a lexical scanner whose transition table is prebuilt by 
   FM3BuildLexMachine.  For the reverse, uses a directly subscripted
   array.
*)    

; IMPORT FM3Base 

; TYPE ValueTyp = FM3Base. Int32Typ

; CONST ValueNull = LAST ( ValueTyp )
; CONST ValueUnrecognized = ValueNull - 1 

; TYPE StateTyp : FM3Base . Int32Typ 

; TYPE T <: REFANY

; TYPE ValidCharTyp = [ ' ' .. '~' ] (* Printable chars. *)   
; CONST NullChar 
    = VAL ( ORD ( FIRST ( FM3LexTable . ValidCharTyp ) ) - 1 , CHAR ) 

; PROCEDURE ToText ( Table : T ; Value : ValueTyp ) : TEXT 
  (* NIL if Value not in Table. *) 

; PROCEDURE ValueFromChars ( Table : T ; READONLY Name : ARRAY OF CHAR ) 
  : ValueTyp 
  (* ValueNull if Name is not in Table. *) 

; PROCEDURE ValueFromText ( Table : T ; Name : TEXT ) : ValueTyp 
  (* ValueNull if Name is not in Table. *) 

; PROCEDURE IncrInit ( Table : T ) : StateTyp
  (* Initialize for char-at-a-time lookup *)

  = BEGIN
      RETURN 0 
    END IncrInit
    
; PROCEURE IncrNext
    ( Table : T ; Char : CHAR ; VAR (*IN OUT*) State : StateTyp ) 
  : Value : ValueTyp
  (* Supply one character to an incremental lookup.  State must be what was
     returned by the last IncrInit or IncrNext, and using the same Table.
     Supply NullChar as and only-as the last in the string. 
     ValueNull means not enough information, commonly more charasters needed.
     Caller must call with 
     ValueUnrecognized means, well, unrecognized? *)
     
; END FM3LexTable 
. 
