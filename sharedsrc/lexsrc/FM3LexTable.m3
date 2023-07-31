
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3LexTable 

(* A fast two-way mapping between integers and strings.  For string-to-int,
   uses a lexical scanner whose transition table is prebuilt by 
   FM3BuildLexMachine.  For the reverse, uses a directly subscripted
   array.
*)    

; IMPORT Text 

; IMPORT FM3LexTableRep 
; IMPORT FM3Base 

(*EXPORTED*) 
; PROCEDURE ToText ( Table : T ; Value : ValueTyp ) : TEXT 
  (* NIL if Value not in Table. *) 

  = VAR LSs : FM3Base . Int32Typ 

  ; BEGIN 
      IF Table = NIL OR Table ^ . NamesRef = NIL  
      THEN RETURN NIL 
      ELSE 
        LSs := Value - Table ^ . MinValue 
      ; IF LSs < 0 OR Value > Table ^ . MaxValue 
        THEN RETURN NIL 
        ELSE
          RETURN Table ^ . NamesRef ^ [ LSs ] 
        END (* IF *) 
      END (* IF *) 
    END ToText 

(*EXPORTED*) 
; PROCEDURE ValueFromChars ( Table : T ; READONLY Name : ARRAY OF CHAR ) 
  : ValueTyp 
  (* ValueNull if Name is not in Table. *) 

  = VAR LNameSs : INTEGER 
  ; VAR LNameLast : INTEGER 
  ; VAR LStateNo : StateNoTyp 
  ; VAR LTransition : StateNoTyp 
  ; VAR LChar : CHAR 

  ; BEGIN 
      IF Table = NIL 
         OR Table ^ . StatesRef = NIL 
         OR NUMBER ( Table ^ . StatesRef ^ ) = 0  
      THEN RETURN ValueNull 
      ELSE 
        LNameSs := 0 
      ; LNameLast := LAST ( Name ) 
      ; LStateNo := 0 
      ; LOOP 
          IF LNameSs > LNameLast 
          THEN LChar := NullChar 
          ELSE LChar := Name [ LNameSs ] 
          END (* IF *) 
        ; WITH WState = Table ^ . StatesRef ^ [ LStateNo ] 
          DO IF LChar < WState . Min OR LChar > WState . Max 
            THEN RETURN ValueUnrecognized 
            ELSE 
              LTransition 
                := Table ^ . SpaceRef [ ORD ( LChar ) + WState . SpaceBias ] 
            ; IF LTransition < 0 
              THEN RETURN LTransition - FM3LexTableRep . FirstNegResultValue 
              ELSIF LTransition = FM3LexTableRep . NoTransition 
              THEN RETURN ValueUnrecognized 
              ELSE 
                LStateNo := LTransition 
              ; INC ( LNameSs ) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 
        END (* LOOP *) 
      END (* IF *) 
    END ValueFromChars

(*EXPORTED*) 
; PROCEDURE ValueFromText ( Table : T ; Name : TEXT ) : ValueTyp 
  (* ValueNull if Name is not in Table. *) 

  = VAR LChars : ARRAY [ 0 .. MaxStringLength - 1 ] OF CHAR 
  ; VAR LLength : INTEGER 

  ; BEGIN
      <* ASSERT Text . Length ( Name ) <= MaxStringLength 
         , "StringTooLong." 
      *> 
      Text . SetChars ( LChars , Name ) 
    ; LLength := MIN ( Text . Length ( Name ) , MaxStringLength ) 
    ; RETURN ValueFromChars ( Table , SUBARRAY ( LChars , 0 , LLength ) ) 
    END ValueFromText 

(*EXPORTED*)
; PROCEDURE IncrInit ( <* UNUSED *> Table : T ) : StateNoTyp
  (* Initialize for char-at-a-time lookup *)

  = BEGIN
      RETURN 0 
    END IncrInit
    
(*EXPORTED*)
; PROCEDURE IncrNext
    ( Table : T ; Char : CHAR ; VAR (*IN OUT*) State : StateNoTyp ) 
  : ValueTyp
  (* Supply one character to an incremental lookup.  State must be what was
     returned by the last IncrInit or IncrNext, and using the same Table.
     Supply NullChar as and only-as the last in the string. 
     ValueNull means not enough information, commonly more characters needed.
     Caller must call with 
     ValueUnrecognized means, well, unrecognized? *)
     
  = VAR LTransition : StateNoTyp
  
  ; BEGIN
      WITH WState = Table ^ . StatesRef ^ [ State ] 
      DO 
        IF Char < WState . Min OR Char > WState . Max 
        THEN RETURN ValueUnrecognized 
        ELSE 
          LTransition 
            := Table ^ . SpaceRef [ ORD ( Char ) + WState . SpaceBias ] 
        ; IF LTransition < 0 
          THEN RETURN LTransition - FM3LexTableRep . FirstNegResultValue 
          ELSIF LTransition = FM3LexTableRep . NoTransition 
          THEN RETURN ValueUnrecognized 
          ELSE
            State := LTransition
          ; RETURN ValueNull 
          END (* IF *) 
        END (* IF *) 
      END (* WITH *) 
    END IncrNext 

; CONST MaxStringLength = 512 

; BEGIN 
  END FM3LexTable 
. 
