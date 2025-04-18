
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Graph

; TYPE NodeNoTyp = CARDINAL 
  (* Numbers of real nodes start at zero and must be < LAST(NodeNoTyp *)
  (* Space used goes up as O(maxNodeNumber^2), albeit with a low constant
     factor, so if your node number space has unused low end, it might be
     nice to bias your numbers down to starting with zero.  This could also
     give some constant O(N) savings in time and space.
  *) 

; EXCEPTION BadNodeNo ( INTEGER ) 

; TYPE ArcTyp = INTEGER (* Treat as opaque. *) 

; TYPE GraphTyp <: REFANY 

; PROCEDURE PredNodeNo ( Graph : GraphTyp ; Arc : ArcTyp ) : INTEGER  

; PROCEDURE SuccNodeNo ( Graph : GraphTyp ; Arc : ArcTyp ; ) : INTEGER

; PROCEDURE NewEmpty ( MaxNodeCt : INTEGER ) : GraphTyp 

; PROCEDURE MakeEmpty ( VAR (*IN OUT*) Graph : GraphTyp )

; PROCEDURE MakeArc ( READONLY Graph : GraphTyp ; Pred , Succ : INTEGER )
    : ArcTyp
    RAISES { BadNodeNo } 

; PROCEDURE AddArc
    ( VAR (*IN OUT *) Graph : GraphTyp ; Pred , Succ : INTEGER ) 
    RAISES { BadNodeNo }

; PROCEDURE ArcCt ( Graph : GraphTyp ) : INTEGER 

; TYPE ArcVisitorProc = PROCEDURE ( Arc : ArcTyp (* Must = IntSets.ElemT. *))

; PROCEDURE ForAllArcsDo ( Graph : GraphTyp ; VisitArc : ArcVisitorProc )
  (* Call back VisitArc for each arc in Graph. *) 

; TYPE SCCVisitorProc
         = PROCEDURE ( READONLY SCC : ARRAY OF INTEGER )
  (* The node Nos of nodes in one SCC ^. *) 

; PROCEDURE SCCs ( Graph : GraphTyp ; VisitSCC : SCCVisitorProc )
  (* Call back VisitSCC for each SCC subgraph. *) 

; END FM3Graph 
. 

