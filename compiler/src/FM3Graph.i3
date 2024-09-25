
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Graph

; TYPE NodeNoTyp = CARDINAL 
  (* Numbers of real nodes start at zero and must be < LAST(NodeNoTuyp *)
  (* Space used goes up as O(maxNodeNumber^2), albeit with a low constant
     factor, so if your node number space has unused low end, it might be
     nice to bias your numbers down to starting with zero.  This could also
     give some constant O(N) savings in time and space.
  *) 

; EXCEPTION BadNodeNo ( NodeNoTyp ) 

; TYPE ArcTyp = INTEGER (* Treat as opaque. *) 

; TYPE GraphTyp <: REFANY 

; PROCEDURE PredNode ( Graph : GraphTyp ; Arc : ArcTyp ) : NodeNoTyp  

; PROCEDURE SuccNodeNo ( Graph : GraphTyp ; Arc : ArcTyp ; ) : NodeNoTyp

; PROCEDURE NewEmpty ( MaxNodeCt : INTEGER ) : GraphTyp 

; PROCEDURE MakeEmpty ( VAR (*IN OUT*) Graph : GraphTyp )

; PROCEDURE AddArc
    ( VAR (*IN OUT *) Graph : GraphTyp ; Pred , Succ : NodeNoTyp ) 
    RAISES { BadNodeNo } 

; TYPE SCCVisitorProc = PROCEDURE ( READONLY SCC : ARRAY OF NodeNoTyp ) 

; PROCEDURE SCCs ( Graph : GraphTyp ; VisitSCC : SCCVisitorProc )

; END FM3Graph 
. 

