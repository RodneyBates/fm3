
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3Dict ( Key , Value )

(* Key declares:
     T: A type.
     Compare: a compare procedure on T. 
     Brand 

   Value declares:
     T: A type 
     Brand 
*) 

; IMPORT FM3Base
; IMPORT FM3Primes 

; TYPE RowTyp
  = RECORD
      RowHash : FM3Base . HashTyp := 0L (* Which means this row is empty. *)  
    ; RowKey : Key . T
    ; RowValue : Value . T 
    END (*RECORD*)

; TYPE DkKindTyp = { DkkHash , DkkFSM , DkkBinSch } 

; REVEAL T
    = Public
      BRANDED "Fm3Dict_" & Key . Brand & "_" & Value . Brand 
      OBJECT
        DkTableRef : REF ARRAY OF RowTyp
      ; DkOccupiedCt : INTEGER := 0 
      ; DkKind : DkKindTyp
      ; DkGrowable : BOOLEAN := FALSE 
      END (*T*)

; TYPE THashTyp
    = T OBJECT OVERRIDES
          insert := HashInsert
        ; enterPhaseTwo := NullPhaseTwo
        ; lookup := HashLookup
        END (*THashTyp*) 

; TYPE TBinSchTyp
    = T OBJECT OVERRIDES
          insert := HashInsert
        ; enterPhaseTwo := NullPhaseTwo
        ; lookup := HashLookup
        END (*THashTyp*) 

; VAR GMaxTwoPhaseSize := 15  

(* Sizes are count of Key-value pairs.  Any extra space needed by
   the internal data structure will be added internally. *)

; PROCEDURE NewFixed ( MaxSize : INTEGER ; TwoPhase : BOOLEAN ) : T
  (* Will not support growth beyond MaxSize Key-value pairs. *)
  (* If TwoPhase, all calls on Insert must precede a single call
     on Finalize, before any calls on Lookup.  There may be
     efficiency benefits to these restrictions. *)

  = VAR LNew : T
  ; VAR LTableSize : INTEGER  

  ; BEGIN
      IF TwoPhase AND MaxSize <= GMaxTwoPhaseSize
      THEN
        LNew := NEW ( TBinSchTyp )  
      ; LNew . DkKind := DkKindTyp . DkkBinSch
      ; LTableSize := MaxSize 
      ELSE
        LNew := NEW ( THashTyp )  
      ; LNew . DkKind := DkKindTyp . DkkHash
      ; LTableSize := MaxSize + MaxSize DIV 2
      ; LTableSize := FM3Primes . NextLargerOrEqualPrime ( LTableSize ) 
      END (*IF*)
    
    ; LNew . DkTableRef := NEW ( REF ARRAY OF RowTyp , MaxSize )
    ; LNew . DkOccupiedCt := 0
    ; LNew . DkGrowable := FALSE 
    ; RETURN LNew 
    END NewFixed 
  
; PROCEDURE NewGrowable ( InitSize : INTEGER ) : T
  (* InitSize is an initial Key-value pair estimate.
     Will auto-expand beyond this, if necessary. *) 

  = VAR LNew : T

  ; BEGIN
      LNew := NEW ( THashTyp )  
    ; LNew . DkKind := DkKindTyp . DkkHash
    ; LNew . DkTableRef := NEW ( REF ARRAY OF RowTyp , InitSize )
    ; LNew . DkOccupiedCt := InitSize (* Not used. *) 
    ; LNew . DkGrowable := TRUE  
    ; RETURN LNew 
    END NewGrowable 

(* You can use a hash function of your choice, but all Hash values 
   passed in below must be computed from the adjacent Key value
   by the same function. *)

; CONST GDefaultHash = 13L (* Any random prime. *)  

; PROCEDURE HashInsert
    ( DictHash : THashTyp 
    ; Key : Key . T  
    ; Hash : FM3Base . HashTyp
    ; Value : Value . T
    ; VAR OldValue : Value . T (* Meaningful IFF returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Velue.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *) 

  = VAR LTableSize : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER 

  ; BEGIN
      IF Hash = 0L THEN Hash := GDefaultHash END (*IF*)
    ; LTableSize := NUMBER ( DictHash . DkTableRef ^ ) 
    ; LOrigProbe := VAL ( Hash MOD VAL ( LTableSize , LONGINT ) , INTEGER )  
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD ( VAL ( LTableSize - 1 , LONGINT ) ) , INTEGER ) + 1  
    ; LOOP
        WITH WRow = DictHash . DkTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = 0L (* A free slot. *)
          THEN (* Fill it. *)
            WRow . RowHash := Hash
          ; WRow . RowKey := Key
          ; WRow . RowValue := Value 
          ; INC ( DictHash . DkOccupiedCt ) 
          ; GrowHash ( DictHash ) 
          ; RETURN FALSE 
          ELSIF Hash = WRow . RowHash
                AND Key . Compare ( WRow . RowKey , Key ) = FM3Base . CmpEQ
          THEN (* Found it. *)
            OldValue := WRow . RowValue 
          ; IF DoUpdate THEN WRow . RowValue := Value END (*IF*)
          ; RETURN TRUE 
          ELSE
            LProbe := ( LProbe + LSkip ) MOD LTableSize
          ; IF LProbe = LOrigProbe
            THEN <* ASSERT FALSE , "Hash table overflow." *>
         (* ELSE loop *) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*)
      END (*LOOP*)
    END HashInsert

; PROCEDURE NullPhaseTwo ( Dict : T ) 

  = BEGIN
    END NullPhaseTwo

; PROCEDURE GrowHash ( DictHash : THashTyp )

  = VAR LOldDictCopy : THashTyp
  ; VAR LOldSize : INTEGER
  ; VAR LOldOccupiedCt : INTEGER
  ; VAR LNewSize : INTEGER
  ; VAR LJunkValue : Value . T
  ; VAR LDuplicate : BOOLEAN 

  ; BEGIN
      LOldSize := NUMBER ( DictHash . DkTableRef ^ )
    ; LOldOccupiedCt := DictHash . DkOccupiedCt
    ; IF LOldOccupiedCt + LOldOccupiedCt DIV 3 < LOldSize (* 75% full *)
      THEN RETURN
      END (*IF*) 
    ; LNewSize := LOldOccupiedCt * 2
    ; LNewSize := FM3Primes . NextLargerOrEqualPrime ( LNewSize ) 
(* FIXME: and increase to a prime number. *) 
    ; LOldDictCopy := NEW ( THashTyp )
    ; CopyDictFields ( LOldDictCopy , DictHash )
    ; DictHash . DkTableRef := NEW ( REF ARRAY OF RowTyp , LNewSize )
      (* Change the original in place to have a bgger table. *)
    ; FOR RI := 0 TO LAST ( LOldDictCopy . DkTableRef ^ )
      DO WITH WOldRow = LOldDictCopy . DkTableRef ^ [ RI ] 
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
      To . DkTableRef := From . DkTableRef 
    ; To . DkOccupiedCt := From . DkOccupiedCt
    ; To . DkKind := From . DkKind  
    ; To . DkGrowable := From . DkGrowable
    END CopyDictFields 

; PROCEDURE HashLookup
    ( DictHash : THashTyp 
    ; Key : Key . T 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : Value . T 
    )
  : BOOLEAN (* Was found. *)

  = VAR LTableSize : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER 

  ; BEGIN
      IF Hash = 0L THEN Hash := GDefaultHash END (*IF*)
    ; LTableSize := NUMBER ( DictHash . DkTableRef ^ ) 
    ; LOrigProbe := VAL ( Hash MOD VAL ( LTableSize , LONGINT ) , INTEGER ) 
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD ( VAL ( LTableSize - 1 , LONGINT ) ) , INTEGER ) + 1  
    ; LOOP
        WITH WRow = DictHash . DkTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = 0L (* A free slot, key not present. *)
          THEN RETURN FALSE 
          ELSIF Hash = WRow . RowHash
                AND Key . Compare ( WRow . RowKey , Key ) = FM3Base . CmpEQ
          THEN (* Found it. *)
            Val := WRow . RowValue 
          ; RETURN TRUE 
          ELSE
            LProbe := ( LProbe + LSkip ) MOD LTableSize
          ; IF LProbe = LOrigProbe
            THEN <* ASSERT FALSE , "Hash table overflow." *>
         (* ELSE loop *) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*)
      END (*LOOP*)
    END HashLookup


(*
      IF Dict . DkOccupiedCt = 0
      THEN
        WITH WRow = Dict . DkTableRef ^ [ 0 ]
        DO WRow . RowHash := Hash
        ; WRow . RowKey := Key
        ; WRow . RowValue := Value
        END (*WITH*) 
      END (*IF*)
*)

; BEGIN
  END FM3Dict 
.

