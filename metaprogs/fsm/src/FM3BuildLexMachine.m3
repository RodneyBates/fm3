
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3BuildLexMachine 

(* Build a table for FM3LexTable to use.  Entire table must be built
   before it can be used.  Calls must follow the protocol given by\
   the regular expression:
     MakeEmpty AddPair* Build
*) 

; IMPORT Text 

; IMPORT FM3LexTable 
; IMPORT FM3LexTableRep  
; IMPORT FM3Base 

; TYPE MapSsTyp = CHAR  

; TYPE MapTyp = ARRAY MapSsTyp OF TempStateTyp 
; TYPE MapRefTyp = REF MapTyp 

; TYPE TempStateTyp = (* ABSTRACT *) OBJECT END 

; TYPE SingletonTempStateTyp 
       = TempStateTyp OBJECT 
           Value : FM3LexTable . ValueTyp 
         ; String : TEXT 
         ; ReverseMap : BOOLEAN 
         END 

; TYPE MultTempStateTyp 
       = TempStateTyp OBJECT 
           MapRef : MapRefTyp := NIL 
         ; MinOccupied : MapSsTyp := LAST ( MapSsTyp ) 
         ; MaxOccupied : MapSsTyp := FIRST ( MapSsTyp ) 
         ; StateNo : FM3LexTableRep . StateNoTyp := 0 
         END 

; VAR GMachine : TempStateTyp := NIL 
; VAR GNextStateNo : FM3LexTableRep . StateNoTyp := 0 
; VAR GRuleCt : FM3Base . Int32Typ := 0 
; VAR GMinValue : FM3LexTable . ValueTyp := LAST ( FM3LexTable . ValueTyp ) 
; VAR GMaxValue : FM3LexTable . ValueTyp := FIRST ( FM3LexTable . ValueTyp ) 

(* VISIBLE *) 
; PROCEDURE MakeEmpty ( ) 

  = BEGIN 
      GMachine := NIL 
    ; GNextStateNo := 0 
    ; GRuleCt := 0 
    ; GMinValue := LAST ( FM3LexTable . ValueTyp ) 
    ; GMaxValue := FIRST ( FM3LexTable . ValueTyp ) 
    END MakeEmpty 

; PROCEDURE NextStateNo ( VAR Next : FM3LexTableRep . StateNoTyp ) 
  : FM3LexTableRep . StateNoTyp  
  (* Has SIDE EFFECTS! *) 

  = VAR LResult : FM3LexTableRep . StateNoTyp 

  ; BEGIN 
      LResult := Next
    ; INC ( Next ) 
    ; RETURN LResult 
    END NextStateNo 

; PROCEDURE EmptyMapRef ( ) : MapRefTyp 

  = VAR LResult : MapRefTyp 

  ; BEGIN 
      LResult := NEW ( MapRefTyp ) 
    ; FOR RI := FIRST ( MapSsTyp ) TO LAST ( MapSsTyp ) 
      DO LResult ^ [ RI ] := NIL 
      END (* FOR *) 
    ; RETURN LResult 
    END EmptyMapRef 

; PROCEDURE EmptyMultState ( ) : MultTempStateTyp 

  = VAR LResult : MultTempStateTyp 

  ; BEGIN 
      LResult := NEW ( MultTempStateTyp ) 
    ; LResult . MapRef := EmptyMapRef ( ) 
    ; LResult . MinOccupied := LAST ( MapSsTyp ) 
    ; LResult . MaxOccupied := FIRST ( MapSsTyp ) 
    ; LResult . StateNo := NextStateNo ( GNextStateNo ) 
    ; RETURN LResult 
    END EmptyMultState 

(* VISIBLE *) 
; PROCEDURE AddPair 
    ( AddString : TEXT 
    ; Value : FM3LexTable . ValueTyp 
    ; ReverseMap : BOOLEAN := TRUE  
    ) 

  = PROCEDURE ApMapElem 
      ( MultState : MultTempStateTyp 
      ; String : TEXT 
      ; CharSs : INTEGER 
      ; Value : FM3LexTable . ValueTyp 
      ; ReverseMap : BOOLEAN 
      ) 
    (* First char of Tail selects within this MultState. *) 

    = VAR LLength : INTEGER 
    ; VAR LChar : CHAR 
    ; VAR LCharSs : INTEGER 

    ; BEGIN 
        LLength := Text . Length ( String ) 
      ; IF CharSs = LLength  
        THEN 
          LChar := FM3LexTableRep . NullChar 
        ; LCharSs := CharSs 
        ELSE 
          LChar := Text . GetChar ( String , CharSs ) 
        ; LCharSs := CharSs + 1  
        END (* IF *) 
      ; ApRecurse 
          ( (* VAR *) MultState . MapRef ^ [ LChar ] 
          , String 
          , LCharSs 
          , Value 
          , ReverseMap 
          ) 
      ; MultState . MinOccupied := MIN ( MultState . MinOccupied , LChar ) 
      ; MultState . MaxOccupied := MAX ( MultState . MaxOccupied , LChar ) 
      END ApMapElem 

  ; PROCEDURE ApRecurse 
      ( VAR State : TempStateTyp 
      ; String : TEXT 
      ; CharSs : INTEGER 
      ; Value : FM3LexTable . ValueTyp 
      ; ReverseMap : BOOLEAN 
      ) 

    = VAR LNewSingletonState : SingletonTempStateTyp 
    ; VAR LNewMultState : MultTempStateTyp 

    ; BEGIN 
        TYPECASE State <* NOWARN *> 
        OF NULL 
        => IF CharSs = Text . Length ( String )  
           THEN 
             LNewSingletonState := NEW ( SingletonTempStateTyp ) 
           ; LNewSingletonState . Value := Value 
           ; LNewSingletonState . String := String 
           ; LNewSingletonState . ReverseMap := ReverseMap  
           ; State := LNewSingletonState 
           ELSE
             LNewMultState := EmptyMultState ( ) 
           ; ApMapElem 
               ( LNewMultState , String , CharSs , Value ,  ReverseMap ) 
           ; State := LNewMultState 
           END (* IF *) 

        | MultTempStateTyp ( TMultState )  
        => ApMapElem ( TMultState , String , CharSs , Value , ReverseMap )  

        | SingletonTempStateTyp ( TSingle ) 
        => IF Text . Equal ( String , TSingle . String )  
           THEN (* Reinserting this string. *)  
             IF TSingle . Value # Value 
             THEN (* This string has > 1 value. *) 
               TSingle . Value := Value
             END (* IF *) 
           ELSE 
             LNewMultState := EmptyMultState ( ) 
           ; ApMapElem 
               ( LNewMultState 
               , TSingle . String 
               , CharSs 
               , TSingle . Value 
               , TSingle . ReverseMap  
               ) 
           ; ApMapElem 
               ( LNewMultState , String , CharSs , Value , ReverseMap ) 
           ; State := LNewMultState 
           END (* IF *) 
        END (* TYPECASE *) 
      END ApRecurse 

  ; BEGIN (* AddPair *)  
      ApRecurse 
        ( (* VAR *) GMachine 
        , AddString 
        , CharSs := 0 
        , Value := Value 
        , ReverseMap := ReverseMap 
        ) 
    ; INC ( GRuleCt ) 
    ; GMinValue := MIN ( GMinValue , Value ) 
    ; GMaxValue := MAX ( GMaxValue , Value ) 
    END AddPair 

; PROCEDURE BuildPass1 
    ( VAR States : FM3LexTableRep . StatesTyp 
    ; VAR TotalTransitionCt : FM3LexTableRep . TransitionTyp 
    )  
  (* Renumber states in preorder.
     Fill in fields of States. 
     Count Total Transitions
  *) 

  = VAR P1NextStateNo : FM3LexTableRep . StateNoTyp := 0

  ; PROCEDURE P1Recurse ( TempState : TempStateTyp ) 

    = BEGIN 
        TYPECASE TempState <* NOWARN *> 
        OF NULL => 

        | MultTempStateTyp ( TMult ) 
        => TMult . StateNo := NextStateNo ( P1NextStateNo ) 
        ; WITH WState = States [ TMult . StateNo ] 
          DO
            WState . Min := TMult . MinOccupied  
          ; WState . Max:= TMult . MaxOccupied 
          ; WState . SpaceBias 
              := TotalTransitionCt - ORD ( TMult . MinOccupied )    
          ; INC ( TotalTransitionCt 
                , ORD ( TMult . MaxOccupied ) 
                  - ORD ( TMult . MinOccupied ) + 1 
                ) 
          ; FOR RI := TMult . MinOccupied TO TMult . MaxOccupied   
            DO P1Recurse ( TMult . MapRef ^ [ RI ] ) 
            END (* FOR *) 
          END (* WITH *)  

        | SingletonTempStateTyp =>
        END (* TYPECASE *) 
      END P1Recurse 

  ; BEGIN (* BuildPass1 *) 
      TotalTransitionCt := 0 
    ; P1NextStateNo := 0 
    ; P1Recurse ( GMachine ) 
    END BuildPass1 

; PROCEDURE BuildPass2 ( Table : FM3LexTable . T )  
  (* Fill in Space and Names. *) 

  = PROCEDURE P2Recurse 
      ( TempState : TempStateTyp 
      ; VAR ParentTransition : FM3LexTableRep . TransitionTyp 
      ) 

    = BEGIN 
        TYPECASE TempState <* NOWARN *>
        OF NULL 
        => ParentTransition := FM3LexTableRep . NoTransition 

        | MultTempStateTyp ( TMult ) 
        => ParentTransition := TMult . StateNo 
        ; WITH WState = Table . StatesRef ^ [ TMult . StateNo ] 
          DO 
            FOR RI := TMult . MinOccupied TO TMult . MaxOccupied   
            DO P2Recurse 
                 ( TMult . MapRef ^ [ RI ] 
                 , (* VAR *) Table . SpaceRef  
                               ^ [ WState . SpaceBias + ORD ( RI ) ] 
                 ) 
            END (* FOR *) 
          END (* WITH *)  

        | SingletonTempStateTyp ( TSingle ) 
        => ParentTransition := TSingle . Value + FM3LexTableRep . FirstRealValue 
        ; IF TSingle . ReverseMap 
          THEN 
            WITH WRef 
                 = Table . NamesRef ^  [ TSingle . Value - Table . MinValue ] 
            DO IF WRef = NIL 
               THEN WRef := TSingle . String 
               ELSE (* This value has > 1 reverse map string. *)  
               END (* IF *) 
            END (* WITH *) 
          END (* IF *) 
        END (* TYPECASE *) 
      END P2Recurse 

  ; VAR LDontCareTransition : FM3LexTableRep . TransitionTyp 

  ; BEGIN (* BuildPass2 *) 
      P2Recurse ( GMachine , (* VAR *) LDontCareTransition ) 
    END BuildPass2 

(* VISIBLE *) 
; PROCEDURE Build ( ) : FM3LexTable . T 

  = VAR LResult : FM3LexTable . T
  ; VAR LTotalTransitionCt : FM3LexTableRep . TransitionTyp 

  ; BEGIN 
      LResult := NEW ( FM3LexTable . T ) 
    ; LResult . StatesRef := NEW ( FM3LexTableRep . StatesRefTyp , GNextStateNo ) 
    ; IF GNextStateNo > 0 
      THEN 
        BuildPass1 ( LResult . StatesRef ^ , (* VAR *) LTotalTransitionCt ) 
      ; LResult . SpaceRef 
         := NEW ( FM3LexTableRep . SpaceRefTyp , LTotalTransitionCt ) 
      ; LResult . MinValue := GMinValue 
      ; LResult . MaxValue := GMaxValue 
      ; LResult . NamesRef 
          := NEW ( FM3LexTableRep . NamesRefTyp 
                 , LResult . MaxValue - LResult . MinValue + 1 
                 ) 
      ; BuildPass2 ( LResult ) 
      END (* IF*) 
    ; RETURN LResult 
    END Build 

; BEGIN 
  END FM3BuildLexMachine 
. 
