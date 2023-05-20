
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3LexTableRep

; IMPORT FM3Base 
; IMPORT FM3LexTable   

; CONST NoTransition = LAST ( FM3LexTable . StateNoTyp ) 
; CONST LastRealTransition = NoTransition - 1 
; CONST FirstNegResultValue = FIRST ( FM3LexTable . StateNoTyp ) 
(* Negative "transitions" are mapped-to values, biased by FirstNegResultValue. 
   NoTransition means there is none.  
   Nonnegative transitions < NoTransition are unbiased states to go to. 
*) 

; CONST NullChar 
    = VAL ( ORD ( FIRST ( FM3LexTable . ValidCharTyp ) ) - 1 , CHAR ) 

; TYPE SpaceSsTyp = FM3Base . Card32Typ 
; TYPE SpaceTyp = ARRAY (* SpaceSsTyp *) OF FM3LexTable . StateNoTyp  
  (* A single array of transitions with concatenated transition subranges for
     the various states, in no particular order. *) 
; TYPE SpaceRefTyp = REF SpaceTyp 

; TYPE StateTyp 
    = RECORD 
        Min : CHAR := LAST ( CHAR ) 
      ; Max : CHAR := FIRST ( CHAR ) 
      ; SpaceBias : FM3Base . Int32Typ := FIRST ( SpaceSsTyp ) 
        (* ^Add this to ORD of a character to get a Space subscript. *)    
      END 

; TYPE StatesTyp = ARRAY (* StateNoTyp *) OF StateTyp 
; TYPE StatesRefTyp = REF StatesTyp 

; TYPE NamesTyp 
    = ARRAY (* FM3LexTable . ValueTyp, relative to MinValue *) OF TEXT 
; TYPE NamesRefTyp = REF NamesTyp 

; TYPE TableTyp 
    = RECORD 
        SpaceRef : SpaceRefTyp 
      ; StatesRef : StatesRefTyp
      ; MinValue : FM3LexTable . ValueTyp 
      ; MaxValue : FM3LexTable . ValueTyp 
      ; NamesRef : NamesRefTyp 
      END 

; TYPE TableRefTyp = REF TableTyp 

; REVEAL FM3LexTable . T = BRANDED "LexTable.T" REF TableTyp 

; END FM3LexTableRep  
. 
