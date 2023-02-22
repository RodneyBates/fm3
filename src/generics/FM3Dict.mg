
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3Dict ( KeyInterface , ValueInterface )

(* KeyInterface declares:
     T: A type.
     Compare: a compare procedure on T. 
     Brand 

   ValueInterface declares:
     T: A type 
     Brand 
*) 

; IMPORT FM3Base
; FROM FM3Base IMPORT CmpLT , CmpEQ , CmpGT 
; IMPORT FM3Primes 

; TYPE RowTyp
  = RECORD
      RowHash : FM3Base . HashTyp := 0L (* Which means this row is empty. *)  
    ; RowKey : KeyInterface . T
    ; RowValue : ValueInterface . T 
    END (*RECORD*)
; TYPE KindTyp = { DkHashed , DkUnsorted , DkSorted , DkFSM } 

; REVEAL T
    = Public
      BRANDED Brand 
      OBJECT
        DctTableRef : REF ARRAY OF RowTyp
      ; DctOccupiedCt : INTEGER := 0 
      ; DctKind : KindTyp
      ; DctGrowable : BOOLEAN := FALSE 
      END (*T*)

; TYPE THashTyp
    = T OBJECT OVERRIDES
          insert := HashInsert
        ; enterPhaseTwo := NullPhaseTwo
        ; lookup := HashLookup
        END (*THashTyp*) 

; TYPE TBinSchTyp
    = T OBJECT OVERRIDES
          insert := BinSchInsert
        ; enterPhaseTwo := BinSchPhaseTwo
        ; lookup := BinSchLookup
        END (*TBinSchTyp*) 

; VAR GMaxTwoPhaseSize := 15  

(* Sizes are count of rows.  Any extra space needed by
   the internal data structure will be added internally. *)

(*EXPORTED:*)
; PROCEDURE NewFixed ( MaxSize : INTEGER ; TwoPhase : BOOLEAN ) : T
  (* Will not support growth beyond MaxSize rows. *)
  (* If TwoPhase, all calls on Insert must precede a single call
     on Finalize, before any calls on Lookup.  There may be
     efficiency benefits to these restrictions. *)

  = VAR LNew : T
  ; VAR LTableNumber : INTEGER  

  ; BEGIN
      IF TwoPhase AND MaxSize <= GMaxTwoPhaseSize
      THEN
        LNew := NEW ( TBinSchTyp )  
      ; LNew . DctOccupiedCt := 0 
      ; LTableNumber := MaxSize 
      ; LNew . DctGrowable := FALSE  
      ; LNew . DctKind := KindTyp . DkUnsorted  
      ELSE
        LNew := NEW ( THashTyp )  
      ; LNew . DctKind := KindTyp . DkHashed
      ; LTableNumber := MaxSize + MaxSize DIV 2
      ; LTableNumber := FM3Primes . NextLargerOrEqualPrime ( LTableNumber )
      ; LNew . DctGrowable := FALSE  
      ; LNew . DctOccupiedCt := 0
      ; LNew . DctKind := KindTyp . DkHashed 
      END (*IF*)
    ; LNew . DctTableRef := NEW ( REF ARRAY OF RowTyp , LTableNumber )
    ; RETURN LNew 
    END NewFixed 
  
(*EXPORTED:*)
; PROCEDURE NewGrowable ( InitSize : INTEGER ) : T
  (* InitSize is an initial Key-value pair estimate.
     Will auto-expand beyond this, if necessary. *) 

  = VAR LNew : T
  ; VAR LTableNumber : INTEGER 

  ; BEGIN
      LNew := NEW ( THashTyp )  
    ; LNew . DctKind := KindTyp . DkHashed
    ; LTableNumber := InitSize + InitSize DIV 2
    ; LTableNumber := FM3Primes . NextLargerOrEqualPrime ( LTableNumber )
    ; LNew . DctTableRef := NEW ( REF ARRAY OF RowTyp , LTableNumber )
    ; LNew . DctOccupiedCt := 0  (* Not used. *) 
    ; LNew . DctGrowable := TRUE  
    ; LNew . DctKind := KindTyp . DkHashed 
    ; RETURN LNew 
    END NewGrowable 

(* You can use a hash function of your choice, but all Hash values 
   passed in below must be computed from the adjacent value of Key
   by the same function. *)

; CONST GDefaultHash = 13L (* Any random prime. *)  

; PROCEDURE HashInsert
    ( DictHash : THashTyp 
    ; Key : KeyInterface . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueInterface . T
    ; VAR OldValue : ValueInterface . T (* Meaningful IFF returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Velue.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *) 

  = VAR LTableNumber : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER
  ; VAR LResult : BOOLEAN 

  ; BEGIN
      IF Hash = 0L THEN Hash := GDefaultHash END (*IF*)
      (* Hash = 0L is reserved to mean row is unoccupied. *) 
    ; LTableNumber := NUMBER ( DictHash . DctTableRef ^ ) 
    ; LOrigProbe := VAL ( Hash MOD VAL ( LTableNumber , LONGINT ) , INTEGER )  
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD VAL ( LTableNumber - 1 , LONGINT ) , INTEGER ) + 1
      (* 0 < LSkip < LTableNumber. *) 
    ; LOOP
        WITH WRow = DictHash . DctTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = 0L (* A free slot. *)
          THEN (* Fill it. *)
            WRow . RowHash := Hash
          ; WRow . RowKey := Key
          ; WRow . RowValue := Value 
          ; INC ( DictHash . DctOccupiedCt ) 
          ; GrowHash ( DictHash ) 
          ; LResult := FALSE
          ; EXIT 
          ELSIF Hash = WRow . RowHash
                AND KeyInterface . Compare ( WRow . RowKey , Key ) = CmpEQ
          THEN (* Found it. *)
            OldValue := WRow . RowValue 
          ; IF DoUpdate THEN WRow . RowValue := Value END (*IF*)
          ; LResult := TRUE 
          ; EXIT 
          ELSE
            LProbe := ( LProbe + LSkip ) MOD LTableNumber
          ; IF LProbe = LOrigProbe
            THEN <* ASSERT FALSE , "Hash table overflow." *>
         (* ELSE loop *) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*)
      END (*LOOP*)
    ; INC ( DictHash . DctOccupiedCt )
    ; RETURN LResult 
    END HashInsert

; PROCEDURE NullPhaseTwo ( Dict : T ) 

  = BEGIN
    END NullPhaseTwo

; PROCEDURE GrowHash ( DictHash : THashTyp )

  = VAR LOldDictCopy : THashTyp
  ; VAR LOldSize : INTEGER
  ; VAR LOldOccupiedCt : INTEGER
  ; VAR LNewSize : INTEGER
  ; VAR LJunkValue : ValueInterface . T
  ; VAR LDuplicate : BOOLEAN 

  ; BEGIN
      LOldSize := NUMBER ( DictHash . DctTableRef ^ )
    ; LOldOccupiedCt := DictHash . DctOccupiedCt
    ; IF LOldOccupiedCt + LOldOccupiedCt DIV 3 < LOldSize (* < 75% full *)
      THEN RETURN
      END (*IF*) 
    ; LNewSize := LOldOccupiedCt * 2
    ; LNewSize := FM3Primes . NextLargerOrEqualPrime ( LNewSize ) 
    ; LOldDictCopy := NEW ( THashTyp )
    ; CopyDictFields ( LOldDictCopy , DictHash )
    ; DictHash . DctTableRef := NEW ( REF ARRAY OF RowTyp , LNewSize )
      (* Change the original in place to have a bgger table. *)
    ; FOR RI := 0 TO LAST ( LOldDictCopy . DctTableRef ^ )
      DO WITH WOldRow = LOldDictCopy . DctTableRef ^ [ RI ] 
        DO IF WOldRow . RowHash # 0L (* Row is occupied. *) 
          THEN 
            LDuplicate
              := HashInsert
                   ( DictHash
                   , WOldRow . RowKey
                   , WOldRow . RowHash
                   , WOldRow . RowValue
                   , (*OUT*) LJunkValue
                   , DoUpdate := FALSE
                   )
          ; <* ASSERT NOT LDuplicate *>
          END (*IF*) 
        END (*WITH*) 
      END (*FOR*) 
    ; LOldDictCopy := NIL 
    END GrowHash 

; PROCEDURE CopyDictFields ( From , To  : T ) 

  = BEGIN
      To . DctTableRef := From . DctTableRef 
    ; To . DctOccupiedCt := From . DctOccupiedCt
    ; To . DctKind := From . DctKind  
    ; To . DctGrowable := From . DctGrowable
    END CopyDictFields 

; PROCEDURE HashLookup
    ( DictHash : THashTyp 
    ; Key : KeyInterface . T 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueInterface . T 
    )
  : BOOLEAN (* Was found. *)

  = VAR LTableNumber : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER 

  ; BEGIN
      IF Hash = 0L THEN Hash := GDefaultHash END (*IF*)
      (* 0 < LSkip < LTableNumber. *) 
    ; LTableNumber := NUMBER ( DictHash . DctTableRef ^ ) 
    ; LOrigProbe := VAL ( Hash MOD VAL ( LTableNumber , LONGINT ) , INTEGER ) 
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD VAL ( LTableNumber - 1 , LONGINT ) , INTEGER ) + 1  
      (* 0 < LSkip < LTableNumber. *) 
    ; LOOP
        WITH WRow = DictHash . DctTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = 0L (* A free slot, key not present. *)
          THEN RETURN FALSE 
          ELSIF Hash = WRow . RowHash
                AND KeyInterface . Compare ( WRow . RowKey , Key ) = CmpEQ
          THEN (* Found it. *)
            Val := WRow . RowValue 
          ; RETURN TRUE 
          ELSE
            LProbe := ( LProbe + LSkip ) MOD LTableNumber
          ; IF LProbe = LOrigProbe
            THEN <* ASSERT FALSE , "Hash table overfull." *>
         (* ELSE loop *) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*)
      END (*LOOP*)
    END HashLookup

; PROCEDURE BinSchInsert
    ( Dict : TBinSchTyp 
    ; Key : KeyInterface . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueInterface . T
    ; VAR OldValue : ValueInterface . T (* Meaningful IFF returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Velue.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *)

  = VAR LTableNumber : INTEGER
  ; VAR LEmptySs , LParentSs : INTEGER (* Place waiting to be filled. *)  
  ; VAR LOrphanRow : RowTyp (* Row waiting for a place to fit in.*) 

  ; BEGIN
      IF Hash = 0L THEN Hash := GDefaultHash END (*IF*)
      (* 0 < LSkip < LTableNumber. *) 
    ; LTableNumber := NUMBER ( Dict . DctTableRef ^ )
    ; IF Dict . DctOccupiedCt >= LTableNumber
      THEN (* Convert to hash form.*) 
      ELSE 
        LEmptySs := Dict . DctOccupiedCt
      ; LOrphanRow . RowHash := Hash
      ; LOrphanRow . RowKey := Key
      ; LOrphanRow . RowValue := Value
      ; LOOP (* Bubble up. *)
          IF LEmptySs = 0 
          THEN (* Done bubbling at the top. *) 
            Dict . DctTableRef ^ [ 0 ] := LOrphanRow 
          ; EXIT
          ELSE
            LParentSs := ( LEmptySs - 1 ) DIV 2
          ; IF KeyInterface . Compare
                 ( LOrphanRow . RowKey
                 , Dict . DctTableRef ^ [ LParentSs ] . RowKey
                 )
               # CmpGT
            THEN (* Can store the orphan here and be done. *)
              Dict . DctTableRef ^ [ LEmptySs ] := LOrphanRow
            ; EXIT
            ELSE (* Must bubble a level. *)
              Dict . DctTableRef ^ [ LEmptySs ]
                := Dict . DctTableRef ^ [ LParentSs ]
            ; LEmptySs := LParentSs
            (* And loop. *) 
            END (*IF*)
          END (*IF*)
        END (*LOOP*)
      ; INC ( Dict . DctOccupiedCt )
      END (*IF*) 
    END BinSchInsert 

; PROCEDURE BinSchPhaseTwo ( Dict : T ) 

  = VAR LTableNumber : INTEGER
  ; VAR LSs , LGreaterSs : INTEGER 
  ; VAR LLeftChildSs , LRightChildSs : INTEGER 
  ; VAR LReinsertRow , LLeftRow , LRightRow , LGreaterRow : RowTyp 
  ; VAR LCompare : [ -1 .. 1 ] 
  
  ; BEGIN
      IF Dict . DctOccupiedCt <= 1 (* Aleady trivially sorted.. *)
      THEN RETURN
      END (*IF*) 
    ; LTableNumber := NUMBER ( Dict . DctTableRef ^ )
    ; FOR RSs := Dict . DctOccupiedCt - 1 TO 1 BY - 1 
      DO 
        LReinsertRow := Dict . DctTableRef ^ [ RSs ] (* Make space. *) 
      ; Dict . DctTableRef ^ [ RSs ] := Dict . DctTableRef ^ [ 0 ]
      ; LSs := 0 
      ; LOOP 
          LLeftChildSs := ( LSs * 2 ) + 1 
        ; LRightChildSs := LLeftChildSs + 1 
        ; IF LLeftChildSs >= RSs 
          THEN (* No children. We are done. *) 
            Dict . DctTableRef ^ [ LSs ] := LReinsertRow 
          ; EXIT
          ELSE (* We at least have a left child. *)  
            LLeftRow := Dict . DctTableRef ^ [ LLeftChildSs ] 
          ; IF LRightChildSs >= RSs 
            THEN (* No right child. Treat left as greater child. *)  
              LGreaterSs := LLeftChildSs 
            ; LGreaterRow := LLeftRow 
            ELSE (* Both a left and right child. *) 
              LRightRow := Dict . DctTableRef ^ [ LRightChildSs ] 
            ; LCompare 
                := KeyInterface . Compare ( LLeftRow . RowKey , LRightRow . RowKey)
            ; IF LCompare = CmpGT 
              THEN (* Left is greater child. *) 
                LGreaterSs := LLeftChildSs 
              ; LGreaterRow := LLeftRow 
              ELSE 
                LGreaterSs := LRightChildSs 
              ; LGreaterRow := LRightRow 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        ; LCompare 
            := KeyInterface . Compare ( LReinsertRow . RowKey , LGreaterRow . RowKey ) 
        ; IF LCompare = CmpGT  
          THEN (* Parent is greatest of 3.  We are done. *) 
            Dict . DctTableRef ^ [ LSs ] := LReinsertRow 
          ; EXIT 
          ELSE (* Move the greater child up to the current slot, creating
                  space to push the current item down to. *) 
            Dict . DctTableRef ^ [ LSs ] := LGreaterRow 
          ; LSs := LGreaterSs 
          END (* IF *) 
        END (* LOOP *)  
      END (* FOR *)
    ; Dict . DctKind := KindTyp . DkSorted 
    END BinSchPhaseTwo

; PROCEDURE BinSchLookup
    ( Dict : TBinSchTyp 
    ; Key : KeyInterface . T 
    ; <*UNUSED*> Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueInterface . T 
    )
  : BOOLEAN (* Was found. *)

  = VAR LLo , LHi , LProbe : INTEGER
  ; VAR LCompare : [ -1 .. 1 ] 

  ; BEGIN
      LLo := 0
    ; LHi := Dict . DctOccupiedCt
    (* INVARIANT: Sought entry, if present, is at subscript S,
                  where Lo <= S < LHi *) 
    ; LOOP
        IF LLo = LHi (* Empty range. *) THEN RETURN FALSE END (*IF*) 
      ; LProbe := ( LLo + LHi ) DIV 2
      ; LCompare
          := KeyInterface . Compare
               ( Key , Dict . DctTableRef ^ [ LProbe ] . RowKey )
      ; IF LCompare = CmpEQ
        THEN (* Lucky early find. *) 
          Val := Dict . DctTableRef ^ [ LProbe ] . RowValue 
        ; RETURN TRUE
        ELSIF LCompare = CmpLT THEN LHi := LProbe 
        ELSE LLo := LProbe + 1
        END (*IF*) 
      END (*LOOP*) 
    END BinSchLookup

; BEGIN
  END FM3Dict 
.

