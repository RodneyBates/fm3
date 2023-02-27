
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
; IMPORT FM3Utils 

; TYPE RowTyp
  = RECORD
      RowHash : FM3Base . HashTyp := FM3Utils.HashNull
      (* Which means this row is empty. *)  
    ; RowKey : KeyInterface . T
    ; RowValue : ValueInterface . T 
    END (*RECORD*)
    
; TYPE StateTyp = { DsHashed , DsUnsorted , DsSorted } 

; REVEAL Private 
    = BRANDED "Fm3Dict_BaseTyp"
      OBJECT
        DbTableRef : REF ARRAY OF RowTyp := NIL 
      ; DbHashFunc : HashFuncTyp := NIL 
      ; DbOccupiedCt : INTEGER := 0 
      ; DbState := StateTyp . DsUnsorted 
      ; DbHashed : BOOLEAN := FALSE 
      END (*Private*)

; TYPE DictBaseTyp = Private

; REVEAL GrowableTyp
    = DictBaseTyp
      BRANDED "Fm3Dict_GrowableTyp"
      OBJECT 
      END (*GrowableTyp*) 

; REVEAL FixedTyp 
    = DictBaseTyp 
      BRANDED "Fm3Dict_FixedTyp"
      OBJECT 
      END (*FixedTyp*) 

; VAR GMaxFixedSize := 15  

(* Growable dictionaries: *)

(* Growable dictionaries will be auto-expanded as needed.
   Duplicate Key insertions leave only one entry.  Insertions
   and Lookups can be interspersed arbitrarily
*) 

(* Sizes are count of rows.  Any extra space needed by
   the internal data structure will be added internally. *)

(* You can use a hash function of your choice, but all Hash values passed
   to a given dictionary instance must be computed from the adjacent value
   of Key by the HashFunc supplied to New* when creating the dictionary. *)

(* If the hash pseudo-value FM3Utils.HashNull is provided along with a Key,
   the hash value will be computed internally using HashFunc. *) 

; CONST GHashDefault = 13L (* Any random prime. *)
  (* More keys than this, and a hash table implementation will be used. *) 

(*EXPORTED:*)
; PROCEDURE NewGrowable
    ( InitKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : GrowableTyp 
      (* InitKeyCt is an initial  estimate.  The dictionary will be
         auto-expanded as needed. *) 

  = VAR LNew : GrowableTyp 
  ; VAR LTableNumber : INTEGER 

  ; BEGIN
      LNew := NEW ( GrowableTyp )  
    ; LTableNumber := InitKeyCt + InitKeyCt DIV 2
    ; LTableNumber := FM3Primes . NextLargerOrEqualPrime ( LTableNumber )
    ; LNew . DbTableRef := NEW ( REF ARRAY OF RowTyp , LTableNumber )
    ; LNew . DbHashFunc := HashFunc 
    ; LNew . DbOccupiedCt := 0  (* Not used. *) 
    ; LNew . DbHashed := TRUE  
    ; LNew . DbState := StateTyp . DsHashed 
    ; RETURN LNew 
    END NewGrowable 

(*EXPORTED:*)
; PROCEDURE InsertGrowable 
    ( DictHash : GrowableTyp  
    ; Key : KeyInterface . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueInterface . T
    ; VAR (*OUT*) OldValue : ValueInterface . T
      (* ^Meaningful IFF InsertGrowable returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Value.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *) 
  (* All this does is ensure what was created as a fixed dictionary,
     and could still be using the fixed data structure, is not passed
     in here.  *) 

  = BEGIN
      RETURN HashInsert ( DictHash , Key , Hash , Value , OldValue , DoUpdate ) 
    END InsertGrowable 

; PROCEDURE HashInsert
    ( Dict : DictBaseTyp  
    ; Key : KeyInterface . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueInterface . T
    ; VAR (*OUT*) OldValue : ValueInterface . T
      (* ^Meaningful IFF InsertGrowable returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Value.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *) 

  = VAR LTableNumber : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER
  ; VAR LResult : BOOLEAN 

  ; BEGIN
      IF Hash = FM3Utils.HashNull
      THEN Hash := Dict . DbHashFunc ( Key )
        (* ^FM3Utils.HashNull is reserved to mean row is unoccupied. *) 
      END (*IF*)
    ; LTableNumber := NUMBER ( Dict . DbTableRef ^ ) 
    ; LOrigProbe := VAL ( Hash MOD VAL ( LTableNumber , LONGINT ) , INTEGER )  
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD VAL ( LTableNumber - 1 , LONGINT ) , INTEGER ) + 1
      (* 0 < LSkip < LTableNumber. *) 
    ; LOOP
        WITH WRow = Dict . DbTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = FM3Utils.HashNull (* A free slot. *)
          THEN (* Fill it. *)
            WRow . RowHash := Hash
          ; WRow . RowKey := Key
          ; WRow . RowValue := Value 
          ; INC ( Dict . DbOccupiedCt ) 
          ; GrowHash ( Dict ) 
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
    ; RETURN LResult 
    END HashInsert

; PROCEDURE RebuildHash
    ( Dict : DictBaseTyp
    ; FromTableRef : REF ARRAY OF RowTyp
    ; NewNumber : INTEGER
    )

  = VAR LJunkValue : ValueInterface . T
  ; VAR LDuplicate : BOOLEAN 

  ; BEGIN
      Dict . DbTableRef := NEW ( REF ARRAY OF RowTyp , NewNumber )

    ; FOR RI := 0 TO LAST ( FromTableRef ^ )
      DO WITH WOldRow = FromTableRef ^ [ RI ] 
        DO IF WOldRow . RowHash # FM3Utils.HashNull (* Row is occupied. *) 
          THEN 
            LDuplicate 
              := HashInsert
                   ( Dict
                   , WOldRow . RowKey
                   , WOldRow . RowHash
                   , WOldRow . RowValue
                   , (*OUT*) LJunkValue
                   , DoUpdate := FALSE
                   )
          END (*IF*) 
        END (*WITH*) 
      END (*FOR*) 
    END RebuildHash 
 
; PROCEDURE GrowHash ( Dict : DictBaseTyp )

  = VAR LOldNumber : INTEGER
  ; VAR LOldOccupiedCt : INTEGER
  ; VAR LOldTableRef : REF ARRAY OF RowTyp 
  ; VAR LNewNumber : INTEGER

  ; BEGIN
      LOldNumber := NUMBER ( Dict . DbTableRef ^ )
    ; LOldOccupiedCt := Dict . DbOccupiedCt
    ; IF LOldOccupiedCt + LOldOccupiedCt DIV 3 < LOldNumber (* < 75% full *)
      THEN RETURN
      END (*IF*) 
    ; LNewNumber := LOldOccupiedCt * 2
    ; LNewNumber := FM3Primes . NextLargerOrEqualPrime ( LNewNumber )
    ; LOldTableRef := Dict . DbTableRef
    ; RebuildHash ( Dict , LOldTableRef , LNewNumber )  
    END GrowHash 

(*EXPORTED:*)
; PROCEDURE LookupGrowable 
    ( DictHash : GrowableTyp
    ; Key : KeyInterface . T 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Value : ValueInterface . T 
    )
  : BOOLEAN (* Was found. *)
  (* All this does is ensure what was created as a fixed dictionary,
     and could still be using the fixed data structure, is not passed
     in here.  *) 

  = BEGIN
      RETURN HashLookup ( DictHash , Key , Hash , Value ) 
    END LookupGrowable 

; PROCEDURE HashLookup
    ( DictBase : DictBaseTyp 
    ; Key : KeyInterface . T 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueInterface . T 
    )
  : BOOLEAN (* Was found. *)
  (* This requires that DictBase actually point to a hashed dictionary, even
     though it might have been created as Fixed.  Callers must insure this. *) 

  = VAR LTableNumber : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER 

  ; BEGIN
      IF Hash = FM3Utils.HashNull THEN Hash := GHashDefault END (*IF*)
      (* 0 < LSkip < LTableNumber. *) 
    ; LTableNumber := NUMBER ( DictBase . DbTableRef ^ ) 
    ; LOrigProbe := VAL ( Hash MOD VAL ( LTableNumber , LONGINT ) , INTEGER ) 
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD VAL ( LTableNumber - 1 , LONGINT ) , INTEGER ) + 1  
      (* 0 < LSkip < LTableNumber. *) 
    ; LOOP
        WITH WRow = DictBase . DbTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = FM3Utils.HashNull
              (* A free slot, key not present. *)
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

(* Fixed dictionaries have some restrictions, but may be more compact
   and possibly faster, if you can with them and if MaxKeyCt is smallish. 
   They do not support growth beyond MaxKeyCt Keys. 
   All calls on InsertFixed must precede a call on FinalizeFixed,
   before any calls on LookupFixed.  Also, duplicate keys will result
   in undetected duplicate entries, with different values, and
   nondeterministic results from LookupFixed.
*) 

(*EXPORTED:*)
; PROCEDURE NewFixed
    ( MaxKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : FixedTyp 

  = VAR LNew : FixedTyp
  ; VAR LTableNumber : INTEGER  

  ; BEGIN
      IF MaxKeyCt <= GMaxFixedSize
      THEN (* Really do use the sorted data structure. *)
        LNew := NEW ( FixedTyp )  
      ; LNew . DbOccupiedCt := 0 
      ; LTableNumber := MaxKeyCt 
      ; LNew . DbHashed := FALSE  
      ; LNew . DbState := StateTyp . DsUnsorted  
      ELSE (* Use the hash table anyway. *) 
        LNew := NEW ( FixedTyp )  
      ; LTableNumber := MaxKeyCt + MaxKeyCt DIV 2
      ; LTableNumber := FM3Primes . NextLargerOrEqualPrime ( LTableNumber )
      ; LNew . DbOccupiedCt := 0
      ; LNew . DbHashed := TRUE 
      END (*IF*)
    ; LNew . DbHashFunc := HashFunc 
    ; LNew . DbTableRef := NEW ( REF ARRAY OF RowTyp , LTableNumber )
    ; RETURN LNew 
    END NewFixed 
  
(*EXPORTED:*)
; PROCEDURE InsertFixed 
    ( DictFixed : FixedTyp  
    ; Key : KeyInterface . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueInterface . T
    )
  RAISES { Error } 

  = VAR LOldValue : ValueInterface . T
  ; VAR LFound : BOOLEAN

  ; BEGIN
      IF DictFixed . DbHashed
      THEN
        LFound := HashInsert
          ( DictFixed , Key , Hash , Value , LOldValue , DoUpdate := FALSE ) 
      ELSIF DictFixed . DbState # StateTyp . DsUnsorted 
      THEN RAISE Error ( "Lookup in fixed dictionary after Finalize.")
      ELSE BinSchInsert ( DictFixed , Key , Hash , Value ) 
      END (*IF*) 
    END InsertFixed

; PROCEDURE BinSchInsert
    ( DictFixed : FixedTyp 
    ; Key : KeyInterface . T  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueInterface . T
    )
  RAISES { Error } 

  = VAR LTableNumber : INTEGER
  ; VAR LEmptySs , LParentSs : INTEGER (* Place waiting to be filled. *)  
  ; VAR LOrphanRow : RowTyp (* Row waiting for a place to fit in.*) 

  ; BEGIN
      IF DictFixed . DbState = StateTyp . DsSorted 
      THEN RAISE Error ( "Insert into fixed dictionary after Finalize." ) 
      END (*IF*) 
    ; IF Hash = FM3Utils.HashNull THEN Hash := GHashDefault END (*IF*)
      (* 0 < LSkip < LTableNumber. *) 
    ; LTableNumber := NUMBER ( DictFixed . DbTableRef ^ )
    ; IF DictFixed . DbOccupiedCt >= LTableNumber
      THEN (* Convert to hash form.*) 
      ELSE 
        LEmptySs := DictFixed . DbOccupiedCt
      ; LOrphanRow . RowHash := Hash
      ; LOrphanRow . RowKey := Key
      ; LOrphanRow . RowValue := Value
      ; LOOP (* Bubble up. *)
          IF LEmptySs = 0 
          THEN (* Done bubbling at the top. *) 
            DictFixed . DbTableRef ^ [ 0 ] := LOrphanRow 
          ; EXIT
          ELSE
            LParentSs := ( LEmptySs - 1 ) DIV 2
          ; IF KeyInterface . Compare
                 ( LOrphanRow . RowKey
                 , DictFixed . DbTableRef ^ [ LParentSs ] . RowKey
                 )
               # CmpGT
            THEN (* Can store the orphan here and be done. *)
              DictFixed . DbTableRef ^ [ LEmptySs ] := LOrphanRow
            ; EXIT
            ELSE (* Must bubble a level. *)
              DictFixed . DbTableRef ^ [ LEmptySs ]
                := DictFixed . DbTableRef ^ [ LParentSs ]
            ; LEmptySs := LParentSs
            (* And loop. *) 
            END (*IF*)
          END (*IF*)
        END (*LOOP*)
      ; INC ( DictFixed . DbOccupiedCt )
      END (*IF*)
    END BinSchInsert 

(*EXPORTED:*)
; PROCEDURE Finalize ( DictFixed : FixedTyp ) 

  = VAR LTableNumber : INTEGER
  ; VAR LSs , LGreaterSs : INTEGER 
  ; VAR LLeftChildSs , LRightChildSs : INTEGER 
  ; VAR LReinsertRow , LLeftRow , LRightRow , LGreaterRow : RowTyp 
  ; VAR LCompare : [ -1 .. 1 ] 
  
  ; BEGIN
      IF DictFixed . DbState # StateTyp . DsUnsorted THEN RETURN END (*IF*) 
         (* ^Let's give a pass to duplicate calls here. *)  
    ; IF DictFixed . DbOccupiedCt <= 1 (* Aleady trivially sorted.. *)
      THEN
        DictFixed . DbState := StateTyp . DsSorted
      ; RETURN
      END (*IF*) 
    ; LTableNumber := NUMBER ( DictFixed . DbTableRef ^ )
    ; FOR RSs := DictFixed . DbOccupiedCt - 1 TO 1 BY - 1 
      DO 
        LReinsertRow := DictFixed . DbTableRef ^ [ RSs ] (* Make space. *) 
      ; DictFixed . DbTableRef ^ [ RSs ] := DictFixed . DbTableRef ^ [ 0 ]
      ; LSs := 0 
      ; LOOP 
          LLeftChildSs := ( LSs * 2 ) + 1 
        ; LRightChildSs := LLeftChildSs + 1 
        ; IF LLeftChildSs >= RSs 
          THEN (* No children. We are done. *) 
            DictFixed . DbTableRef ^ [ LSs ] := LReinsertRow 
          ; EXIT
          ELSE (* We at least have a left child. *)  
            LLeftRow := DictFixed . DbTableRef ^ [ LLeftChildSs ] 
          ; IF LRightChildSs >= RSs 
            THEN (* No right child. Treat left as greater child. *)  
              LGreaterSs := LLeftChildSs 
            ; LGreaterRow := LLeftRow 
            ELSE (* Both a left and right child. *) 
              LRightRow := DictFixed . DbTableRef ^ [ LRightChildSs ] 
            ; LCompare 
                := KeyInterface . Compare
                     ( LLeftRow . RowKey , LRightRow . RowKey)
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
            := KeyInterface . Compare
                 ( LReinsertRow . RowKey , LGreaterRow . RowKey ) 
        ; IF LCompare = CmpGT  
          THEN (* Parent is greatest of 3.  We are done. *) 
            DictFixed . DbTableRef ^ [ LSs ] := LReinsertRow 
          ; EXIT 
          ELSE (* Move the greater child up to the current slot, creating
                  space to push the current item down to. *) 
            DictFixed . DbTableRef ^ [ LSs ] := LGreaterRow 
          ; LSs := LGreaterSs 
          END (* IF *) 
        END (* LOOP *)  
      END (* FOR *)
    ; DictFixed . DbState := StateTyp . DsSorted 
    END Finalize 

(*EXPORTED:*)
; PROCEDURE LookupFixed  
    ( DictFixed : FixedTyp
    ; Key : KeyInterface . T 
    ; <*UNUSED*> Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueInterface . T 
    )
  : BOOLEAN (* Was found. *)
  RAISES { Error } 

  = VAR LLo , LHi , LProbe : INTEGER
  ; VAR LCompare : [ -1 .. 1 ] 

  ; BEGIN
      IF DictFixed . DbState = StateTyp . DsUnsorted 
      THEN RAISE Error ( "Lookup in fixed dictionary before Finalize." ) 
      END (*IF*)
      
    ; LLo := 0
    ; LHi := DictFixed . DbOccupiedCt
    (* INVARIANT: Sought entry, if present, is at subscript S,
                  where Lo <= S < LHi *) 
    ; LOOP
        IF LLo = LHi (* Empty range. *) THEN RETURN FALSE END (*IF*) 
      ; LProbe := ( LLo + LHi ) DIV 2
      ; LCompare
          := KeyInterface . Compare
               ( Key , DictFixed . DbTableRef ^ [ LProbe ] . RowKey )
      ; IF LCompare = CmpEQ
        THEN (* Lucky early find. *) 
          Val := DictFixed . DbTableRef ^ [ LProbe ] . RowValue 
        ; RETURN TRUE
        ELSIF LCompare = CmpLT THEN LHi := LProbe 
        ELSE LLo := LProbe + 1
        END (*IF*) 
      END (*LOOP*) 
    END LookupFixed

; BEGIN
  END FM3Dict 
.
