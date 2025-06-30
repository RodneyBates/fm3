
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Graph

; IMPORT Word 

; IMPORT IntSets 

; CONST Null = LAST ( INTEGER )  

; TYPE NodeInfoTyp
    = RECORD
        NiPreorder : INTEGER := Null  
      ; NiIsInAnSCC : BOOLEAN := FALSE  
      END
; TYPE NodesInfoTyp = ARRAY OF NodeInfoTyp
; TYPE NodesInfoRefTyp = REF NodesInfoTyp

; REVEAL GraphTyp = BRANDED "FM3Graph-1.0" REF GraphRec
; TYPE GraphRec
    = RECORD
        GrArcSet : IntSets . T 
      ; GrNodeCt : INTEGER := - 1
      ; GrNodeInfo : NodesInfoRefTyp := NIL 
      END
      
(*EXPORTED:*)
; <*INLINE*>
  PROCEDURE NewEmpty ( NodeCt : INTEGER ) : GraphTyp 

  = VAR LResult : GraphTyp

  ; BEGIN
      LResult := NEW ( GraphTyp )
    ; LResult . GrNodeCt := NodeCt 
    ; LResult . GrArcSet := IntSets . Empty ( )
    ; LResult . GrNodeInfo := NIL 
    ; RETURN LResult 
    END NewEmpty

(*EXPORTED:*)
; <*INLINE*>
  PROCEDURE MakeEmpty ( VAR (*IN OUT*) Graph : GraphTyp )

  = BEGIN
      Graph ^ . GrArcSet := IntSets . Empty ( ) 
    ; Graph ^ . GrNodeInfo := NIL 
    END MakeEmpty 

(*EXPORTED:*)
; <*INLINE*>
  PROCEDURE MakeArc ( READONLY Graph : GraphTyp ; Pred , Succ : INTEGER )
    : ArcTyp
    RAISES { BadNodeNo } 

  = VAR LArc : INTEGER

  ; BEGIN
      IF Pred < 0 OR Pred >= Graph ^ . GrNodeCt
      THEN RAISE BadNodeNo ( Pred )
      END (*IF*) 
    ; IF Succ < 0 OR Succ >= Graph ^ . GrNodeCt
      THEN RAISE BadNodeNo ( Succ )
      END (*IF*) 
    ; LArc := Pred * Graph ^ . GrNodeCt + Succ
    ; IF Word . GT ( LArc , LAST ( IntSets . ValidElemT ) )   
      THEN RAISE BadNodeNo ( LArc )
        (* ^This could be peculiar in strange edge cases. *) 
      END (*IF*) 
    ; RETURN LArc 
    END MakeArc 

(*EXPORTED:*)
; <*INLINE*>
  PROCEDURE PredNodeNo ( Graph : GraphTyp ; Arc : ArcTyp )
    : INTEGER  

  = BEGIN
      RETURN Arc DIV Graph ^ . GrNodeCt 
    END PredNodeNo 

(*EXPORTED:*)
; <*INLINE*> PROCEDURE SuccNodeNo ( Graph : GraphTyp ; Arc : ArcTyp ) : INTEGER  

  = BEGIN
      RETURN Arc MOD Graph ^ . GrNodeCt 
    END SuccNodeNo 

(*EXPORTED:*)
; PROCEDURE AddArc
    ( VAR (*IN OUT *) Graph : GraphTyp ; Pred , Succ : INTEGER ) 
    RAISES { BadNodeNo } 

  = BEGIN
      Graph ^ . GrArcSet
        := IntSets . Include
             ( Graph ^ . GrArcSet , MakeArc ( Graph , Pred , Succ ) ) 
    END AddArc

(*EXPORTED:*)
; PROCEDURE ArcCt ( Graph : GraphTyp ) : INTEGER

  = BEGIN
      RETURN IntSets . Card ( Graph ^ . GrArcSet ) 
    END ArcCt 



; TYPE StackTyp = RECORD
    StkTopSs : INTEGER:= - 1 
  ; StkElemsRef : REF ARRAY OF INTEGER  
  END

; PROCEDURE InitStack ( VAR Stack : StackTyp ; ElemCt : INTEGER  ) 
  = BEGIN
      Stack . StkTopSs := - 1 
    ; Stack . StkElemsRef := NEW ( REF ARRAY OF INTEGER , ElemCt ) 
    END InitStack 

; PROCEDURE Push ( VAR Stack : StackTyp ; Elem : INTEGER )

  = BEGIN
      INC ( Stack . StkTopSs )
    ; Stack . StkElemsRef ^ [ Stack . StkTopSs ] := Elem 
    END Push 

; PROCEDURE Top ( READONLY Stack : StackTyp ) : INTEGER

  = BEGIN
      IF Stack . StkTopSs < 0 THEN RETURN Null END (*IF*)
    ; RETURN Stack . StkElemsRef ^ [ Stack . StkTopSs ] 
    END Top 

; PROCEDURE Pop ( VAR Stack : StackTyp ) : INTEGER

  = VAR LResult : INTEGER

  ; BEGIN
      IF Stack . StkTopSs < 0 THEN RETURN Null END (*IF*)
    ; LResult := Stack . StkElemsRef ^ [ Stack . StkTopSs ]
    ; DEC ( Stack . StkTopSs )
    ; RETURN LResult 
    END Pop

; TYPE ProcOfValidElem = PROCEDURE ( Elem : IntSets . ValidElemT ) RAISES ANY

; PROCEDURE ForAllInRangeDo
    ( ArcSet : IntSets . T
    ; Visiter : IntSets . ProcOfElem
    ; Lo , Hi : INTEGER 
    )
(*TODO: Put this into OrdSets by small modification of OrdSets.ForAllDo,
        which could do it in place, avoiding allocation and copying.
*) 

  = VAR LProjectedSet : IntSets . T

  ; BEGIN
      LProjectedSet := IntSets . Project ( ArcSet , Lo , Hi )
    ; IntSets . ForAllDo ( LProjectedSet , Visiter ) 
    END ForAllInRangeDo 

(*EXPORTED:*)
; PROCEDURE ForAllArcsDo ( Graph : GraphTyp ; VisitArc : ArcVisitorProc )
  (* Call back VisitArc for each arc in Graph. *)

  = BEGIN
      IntSets . ForAllDo ( Graph ^ . GrArcSet , VisitArc )
    END ForAllArcsDo 

(*EXPORTED:*)
; PROCEDURE SCCs ( Graph : GraphTyp ; VisitSCC : SCCVisitorProc )

  = VAR SCCPreorder : INTEGER
  ; VAR SCCPStack : StackTyp 
  ; VAR SCCSStack : StackTyp 

  ; PROCEDURE Search ( SearchNodeNo (*V*) : INTEGER )
  
    = PROCEDURE SchVisitArc ( Arc : IntSets . ElemT ) RAISES ANY 

      = VAR LSuccNodeNo (*W*) : INTEGER

      ; BEGIN (* SchVisitArc *) 
          LSuccNodeNo := SuccNodeNo ( Graph , Arc ) 
        ; WITH WWInfo = Graph ^ . GrNodeInfo ^ [ LSuccNodeNo ]
          DO 
            IF WWInfo. NiPreorder = Null
            THEN Search ( LSuccNodeNo )
            ELSIF NOT WWInfo . NiIsInAnSCC 
            THEN
              WHILE
                Graph ^ . GrNodeInfo ^ [ Top ( SCCPStack ) ] . NiPreorder
                > WWInfo . NiPreorder
              DO
                EVAL Pop ( SCCPStack ) 
              END (*WHILE*) 
            END (*IF*)
          END (*WITH*) 
        END SchVisitArc

    ; VAR LPoppedNodeNo : INTEGER
    ; VAR LSCCMemberCt : INTEGER 
    ; VAR LSCCLoSs : INTEGER 

    ; BEGIN (* Search *)
        Graph ^ . GrNodeInfo ^ [ SearchNodeNo ] . NiPreorder := SCCPreorder
      ; INC ( SCCPreorder )
      ; Push ( SCCSStack , SearchNodeNo ) 
      ; Push ( SCCPStack , SearchNodeNo )
      ; ForAllInRangeDo (* All outgoing arcs of SearchNodeNo. *) 
          ( Graph ^ . GrArcSet
          , SchVisitArc 
          , SearchNodeNo * Graph ^ . GrNodeCt
          , ( SearchNodeNo + 1 ) * Graph ^ . GrNodeCt - 1
          )
      ; IF Top ( SCCPStack ) = SearchNodeNo
        THEN (* Found an SCC. *) 
          LSCCMemberCt := 0 
        ; LOOP
            IF SCCSStack . StkTopSs < 0 THEN EXIT END (*IF*) 
          ; LSCCLoSs := SCCSStack . StkTopSs 
          ; LPoppedNodeNo := Pop ( SCCSStack )
          ; Graph ^ . GrNodeInfo ^ [ LPoppedNodeNo ] . NiIsInAnSCC := TRUE 
          ; INC ( LSCCMemberCt )
          ; IF LPoppedNodeNo = SearchNodeNo THEN EXIT END (*IF*) 
          END (*LOOP*)
        ; IF LSCCMemberCt = 1 (* Singleton SCC. *) 
          AND NOT IntSets . IsElement
                    ( ( Graph . GrNodeCt + 1 ) * LPoppedNodeNo 
                    , Graph ^ . GrArcSet
                    )
          THEN (* There is no self-to-self arc, we don't want such an SCC. *)
          ELSE 
            VisitSCC
              ( SUBARRAY ( SCCSStack . StkElemsRef ^ , LSCCLoSs , LSCCMemberCt ) )
          END (*IF*) 
        ; <* ASSERT Pop ( SCCPStack) = SearchNodeNo *> 
        END (*IF*)
      END Search

  ; BEGIN (* SCCs *) 
      Graph ^ . GrNodeInfo := NEW ( NodesInfoRefTyp , Graph ^ . GrNodeCt )
    ; InitStack ( SCCSStack , Graph ^ . GrNodeCt ) 
    ; InitStack ( SCCPStack , Graph ^ . GrNodeCt )
    ; SCCPreorder := 1
    ; FOR RI := 0 TO Graph ^ . GrNodeCt - 1
      DO
         IF Graph ^ . GrNodeInfo ^ [ RI ] . NiPreorder = Null
         THEN Search ( RI )
         END (*IF*)
      END (*FOR*)
    END SCCs 
    
; BEGIN  
  END FM3Graph 
. 

