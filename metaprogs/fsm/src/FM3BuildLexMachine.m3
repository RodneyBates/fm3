
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Flint Hills Modula-3 compiler, FM3.              *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
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
; IMPORT Word 

; IMPORT FM3LexTable 
; IMPORT FM3LexTableRep  

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
         ; StateNo : FM3LexTable . TransitionTyp := 0 
         END 

; VAR GMachine : TempStateTyp := NIL
; VAR GPrelimStateCt : INTEGER 
; VAR GNextStateNo : FM3LexTable . TransitionTyp
; VAR GMinValue : FM3LexTable . ValueTyp := LAST ( FM3LexTable . ValueTyp ) 
; VAR GMaxValue : FM3LexTable . ValueTyp := FIRST ( FM3LexTable . ValueTyp ) 
; VAR GHighestStateNo : FM3LexTableRep . TransitionTyp

(*EXPORTED:*) 
; PROCEDURE MakeEmpty ( ) 

  = BEGIN 
      GMachine := NIL
    ; GPrelimStateCt := 0 
    ; GNextStateNo := FM3LexTableRep . LowestStateNo  
    ; GMinValue := LAST ( FM3LexTable . ValueTyp ) 
    ; GMaxValue := FIRST ( FM3LexTable . ValueTyp ) 
    END MakeEmpty 

; PROCEDURE NextStateNo ( ) : FM3LexTable . TransitionTyp  
  (* Has SIDE EFFECTS! *) 

  = VAR LResult : FM3LexTable . TransitionTyp 

  ; BEGIN 
      LResult := GNextStateNo
    ; GHighestStateNo := GNextStateNo  
    ; INC ( GNextStateNo ) 
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
    ; INC ( GPrelimStateCt ) 
    ; RETURN LResult 
    END EmptyMultState 

(*EXPORTED:*) 
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
    ; GMinValue := MIN ( GMinValue , Value ) 
    ; GMaxValue := MAX ( GMaxValue , Value ) 
    END AddPair 

; PROCEDURE BuildPass1 
    ( VAR States : FM3LexTableRep . StatesTyp 
    ; VAR TotalTransitionCt : FM3LexTable . TransitionTyp 
    )
  RAISES { Error } 
  (* Number states in preorder.
     Fill in fields of States. 
     Count Total Transitions
  *) 

  = PROCEDURE P1Recurse ( TempState : TempStateTyp ) RAISES { Error } 

    = VAR LStateTransitionCt : INTEGER

    ; BEGIN 
        TYPECASE TempState <* NOWARN *> 
        OF NULL => 

        | MultTempStateTyp ( TMult ) 
        => TMult . StateNo := NextStateNo ( ) 
        ; WITH WState
               = States [ TMult . StateNo - FM3LexTableRep . LowestStateNo ] 
          DO
            WState . Min := TMult . MinOccupied  
          ; WState . Max:= TMult . MaxOccupied 
          ; WState . SpaceBias := TotalTransitionCt 
          ; LStateTransitionCt
              := ORD ( TMult . MaxOccupied ) - ORD ( TMult . MinOccupied ) + 1
          ; IF FM3LexTableRep . HighestBiasedValue - LStateTransitionCt
               <= TotalTransitionCt (* Test won't overflow. *) 
            THEN
              RAISE Error ( "Excessive space size." ) 
            END (*IF*) 
          ; INC ( TotalTransitionCt , LStateTransitionCt ) 
          ; FOR RI := TMult . MinOccupied TO TMult . MaxOccupied   
            DO P1Recurse ( TMult . MapRef ^ [ RI ] ) 
            END (* FOR *) 
          END (* WITH *)  

        | SingletonTempStateTyp =>
        END (* TYPECASE *) 
      END P1Recurse 

  ; BEGIN (* BuildPass1 *) 
      TotalTransitionCt := 0 
    ; GNextStateNo := FM3LexTableRep . LowestStateNo  
    ; P1Recurse ( GMachine )
    END BuildPass1 

; PROCEDURE BuildPass2 ( Table : FM3LexTable . T )  
  (* Fill in Space and Names. *) 

  = PROCEDURE P2Recurse 
      ( TempState : TempStateTyp 
      ; VAR ParentTransition : FM3LexTable . TransitionTyp 
      ) 

    = BEGIN 
        TYPECASE TempState <* NOWARN *>
        OF NULL 
        => ParentTransition := FM3LexTableRep . NoTransition 

        | MultTempStateTyp ( TMult ) 
        => ParentTransition := TMult . StateNo 
        ; WITH WState
            = Table . StatesRef
                ^ [ TMult . StateNo - FM3LexTableRep . LowestStateNo ] 
          DO 
            FOR RI := TMult . MinOccupied TO TMult . MaxOccupied   
            DO P2Recurse 
                 ( TMult . MapRef ^ [ RI ] 
                 , (*OUT*) Table . SpaceRef
                    ^ [ WState . SpaceBias + ORD ( RI ) - ORD ( WState . Min ) ]
                 ) 
            END (* FOR *) 
          END (* WITH *)  

        | SingletonTempStateTyp ( TSingle ) 
        => ParentTransition
             := Word . Plus ( TSingle . Value , Table . ValueBias ) 
        ; IF TSingle . ReverseMap 
          THEN 
            WITH WRef = Table . NamesRef ^ [ TSingle . Value - GMinValue ] 
            DO IF WRef = NIL 
               THEN WRef := TSingle . String 
               ELSE (* This value has > 1 reverse map string. *)  
               END (* IF *) 
            END (* WITH *) 
          END (* IF *) 
        END (* TYPECASE *) 
      END P2Recurse 

  ; VAR LDontCareTransition : FM3LexTable . TransitionTyp 

  ; BEGIN (* BuildPass2 *) 
      P2Recurse ( GMachine , (*OUT*) LDontCareTransition (*Unused.*) ) 
    END BuildPass2 

(*EXPORTED:*) 
; PROCEDURE Build ( ) : FM3LexTable . T RAISES { Error } 

  = VAR LResult : FM3LexTable . T
  ; VAR LTotalTransitionCt : FM3LexTable . TransitionTyp 

  ; BEGIN 
      LResult := NEW ( FM3LexTable . T )
    ; LResult ^ . StatesRef
        := NEW ( FM3LexTableRep . StatesRefTyp , GPrelimStateCt )
    ; BuildPass1 ( LResult . StatesRef ^ , (*OUT*) LTotalTransitionCt )
    ; IF GNextStateNo > FM3LexTableRep . LowestStateNo  
      THEN
        <* ASSERT
             GNextStateNo - FM3LexTableRep . LowestStateNo = GPrelimStateCt
        *>  
        LResult ^ . SpaceRef 
         := NEW ( FM3LexTableRep . SpaceRefTyp , LTotalTransitionCt ) 
      ; LResult ^ . MinValue := GMinValue 
      ; LResult ^ . MaxValue := GMaxValue
      ; LResult ^ . ValueBias
          := Word . Minus ( FM3LexTableRep . HighestBiasedValue , GMaxValue )   
      ; LResult ^ . NamesRef 
          := NEW ( FM3LexTableRep . NamesRefTyp 
                 , LResult . MaxValue - LResult . MinValue + 1 
                 )
      ; LResult ^ . HighestStateNo := GHighestStateNo 
      ; IF GHighestStateNo
             >= Word . Plus ( LResult . MinValue , LResult . ValueBias ) 
        THEN RAISE Error ( "States and values collide." ) 
        END (*IF*) 
      ; BuildPass2 ( LResult ) 
      END (* IF*) 
    ; RETURN LResult 
    END Build 

; BEGIN 
  END FM3BuildLexMachine 
. 
