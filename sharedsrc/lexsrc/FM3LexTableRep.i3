
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3LexTableRep

; IMPORT FM3Base 
; IMPORT FM3LexTable   

; CONST NullChar 
    = VAL ( ORD ( FIRST ( FM3LexTable . ValidCharTyp ) ) - 1 , CHAR ) 
; TYPE TransitionTyp = FM3Base . Int16Typ

(* A _Transition_ denotes a scanning action.  It is either NoTransition,
   a state number, or a biased value.  State numbers are generated,
   starting with LowestState and going upward.  Value transitions are
   biased from the actual mapped-to values, with the highest at
   HighestBiasedValue.  It all fails if these two ranges collide in the middle.
*)

; CONST NoTransition = LAST ( FM3LexTable . TransitionTyp )
; CONST HighestBiasedValue = NoTransition - 1
; CONST LowestStateNo = FIRST ( FM3LexTable . TransitionTyp ) 

; TYPE SpaceTyp = ARRAY OF TransitionTyp  
; TYPE SpaceRefTyp = REF SpaceTyp 

; TYPE SpaceBiasTyp = BITS 24 FOR [ 0 .. 16_FFFFFF ] 
; TYPE StateTyp 
    = RECORD 
        SpaceBias : SpaceBiasTyp 
        (* ^Add this to ORD of a character to get a Space subscript. *)
      ; Min : CHAR := LAST ( CHAR ) 
      ; Max : CHAR := FIRST ( CHAR ) 
      END 

; TYPE StatesTyp = ARRAY OF StateTyp 
; TYPE StatesRefTyp = REF StatesTyp 

; TYPE NamesTyp 
    = ARRAY (* FM3LexTable . ValueTyp, relative to MinValue *) OF TEXT 
; TYPE NamesRefTyp = REF NamesTyp 

; TYPE TableTyp 
    = RECORD 
        SpaceRef : SpaceRefTyp 
      ; StatesRef : StatesRefTyp
      ; NamesRef : NamesRefTyp 
      ; MinValue : FM3LexTable . ValueTyp 
      ; MaxValue : FM3LexTable . ValueTyp 
      ; ValueBias : INTEGER 
        (* ^Add this to a real value to get biased value. *)
        (* NOTE: [un]biasing can reqire wraparound if TransitionTyp is as big
                 as ValueBias, which it is not as of 11-13-2024.  But it is
                 being done with Word.Plus/Minus, as a hedge for the future.
        *) 
      ; HighestStateNo : TransitionTyp 
      END 

; TYPE TableRefTyp = REF TableTyp 

; REVEAL FM3LexTable . T = BRANDED "LexTable.T" REF TableTyp 

; END FM3LexTableRep  
. 
