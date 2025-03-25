
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

GENERIC MODULE FM3Dict ( KeyGenformal , ValueGenformal )

(* KeyGenformal declares:
     Compare: a compare procedure on T, of type FM3Base . CompareTyp. 

   ValueGenformal declares nothing.

   The other things needed from the generic formals are renamed in
   exported interface FM3Dict.i3, and used here by those names.
   This could be done with Compare too, even though it is unused in
   the interface.
   
   But the generic formals are needed just to give an instatiation
   a unique module name. 

*) 

; IMPORT FM3Base
; FROM FM3Base IMPORT CmpLT , CmpEQ , CmpGT 
; IMPORT FM3Primes 
; IMPORT FM3Utils

; TYPE RowTyp
  = RECORD
      RowHash : FM3Base . HashTyp := FM3Utils.HashNull
      (* Which means this row is empty. *)  
    ; RowKey : KeyTyp
    ; RowValue : ValueTyp 
    END (*RECORD*)
    
; TYPE StateTyp = { DsHashed , DsUnsorted , DsSorted } 

; REVEAL Private 
    = BRANDED BaseTypBrand 
      OBJECT
        DbTableRef : REF ARRAY OF RowTyp := NIL 
      ; DbHashFunc : HashFuncTyp := NIL 
      ; DbOccupiedCt : INTEGER := 0 
      ; DbState := StateTyp . DsUnsorted
      END (*Private*)

; TYPE DictBaseTyp = Private

(* Although the two subtypes below have no fields, methods or overrides,
   their purpose is to make sure clients don't make mixed calls
   on the two kinds of dictionaries. *)
   
; REVEAL GrowableTyp
    = DictBaseTyp
      BRANDED GrowableTypBrand
      OBJECT 
      END (*GrowableTyp*) 

; REVEAL FixedTyp 
    = DictBaseTyp 
      BRANDED FixedTypBrand
      OBJECT 
      END (*FixedTyp*) 

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

(*EXPORTED:*)
; PROCEDURE NewGrowable
    ( InitKeyCt : INTEGER ; HashFunc : HashFuncTyp ) : GrowableTyp 
      (* InitKeyCt is an initial estimate of actual keys.  The dictionary
         will be oversized and auto-expanded as needed. *) 

  = VAR LNew : GrowableTyp 
  ; VAR LTableNumber : INTEGER 

  ; BEGIN
      LNew := NEW ( GrowableTyp )  
    ; LTableNumber := InitKeyCt + InitKeyCt DIV 2 (* 2/3 full. *) 
    ; LTableNumber := FM3Primes . NextLargerOrEqualPrime ( LTableNumber )
    ; LNew . DbTableRef := NEW ( REF ARRAY OF RowTyp , LTableNumber )
    ; LNew . DbHashFunc := HashFunc 
    ; LNew . DbOccupiedCt := 0
    ; LNew . DbState := StateTyp . DsHashed
    ; RETURN LNew 
    END NewGrowable 

(*EXPORTED:*)
; PROCEDURE InsertGrowable 
    ( DictHash : GrowableTyp  
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueTyp
    ; VAR (*OUT*) OldValue : ValueTyp
      (* ^Meaningful IFF InsertGrowable returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Value.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *)
  RAISES { Error } 
  (* About all this does is ensure what was created as a fixed dictionary,
     and could still be using the fixed data structure, is not passed
     in here.  *) 

  = BEGIN
      IF Hash = FM3Utils.HashNull (* Not supplied by caller. *) 
      THEN (* But internally, HashNull is reserved to denote unoccupied row . *)
        IF DictHash . DbHashFunc # NIL
        THEN Hash := DictHash . DbHashFunc ( Key )
        ELSE Hash := GHashDefault
        END (*IF*) 
      END (*IF*)
    ; RETURN
        HashInsert
          ( NARROW ( DictHash , DictBaseTyp )
          , Key
          , Hash
          , Value
          , (*OUT*) OldValue
          , DoUpdate
          ) 
    END InsertGrowable 

; PROCEDURE HashInsert
    ( DictBase : DictBaseTyp  
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueTyp
    ; VAR (*OUT*) OldValue : ValueTyp
      (* ^Meaningful IFF InsertGrowable returns TRUE. *) 
    ; DoUpdate : BOOLEAN := FALSE
      (* ^If Key is already present, update its Value.
         Otherwise, leave the value unchanged. *) 
    )
  : BOOLEAN (* Key was already present. *)
  RAISES { Error } 

  = VAR LTableNumber : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER
  ; VAR LResult : BOOLEAN 

  ; BEGIN
      LTableNumber := NUMBER ( DictBase . DbTableRef ^ ) 
    ; LOrigProbe
        := VAL ( Hash MOD VAL ( LTableNumber , FM3Base . HashTyp ) , INTEGER )  
      (* 0 <= LOrigProbe < LTableNumber. *) 
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD VAL ( LTableNumber - 1 , FM3Base . HashTyp )
               , INTEGER
               ) + 1
      (* 0 < LSkip < LTableNumber - 1,
         so LSkip MOD LTableNumber  (Which is prime) # 0 *) 
    ; LOOP
        WITH WRow = DictBase . DbTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = FM3Utils.HashNull (* A free slot. *)
          THEN (* Fill it. *)
            WRow . RowHash := Hash
          ; WRow . RowKey := Key
          ; WRow . RowValue := Value 
          ; INC ( DictBase . DbOccupiedCt ) 
          ; GrowHash ( DictBase ) 
          ; LResult := FALSE
          ; EXIT 
          ELSIF Hash = WRow . RowHash
                AND KeyGenformal . Compare ( WRow . RowKey , Key ) = CmpEQ
          THEN (* Found it. *)
            OldValue := WRow . RowValue 
          ; IF DoUpdate THEN WRow . RowValue := Value END (*IF*)
          ; LResult := TRUE 
          ; EXIT 
          ELSE
            LProbe := ( LProbe + LSkip ) MOD LTableNumber
          ; IF LProbe = LOrigProbe
            THEN RAISE Error ( "Hash table overflow." ) 
            ELSE (* Loop. *) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*)
      END (*LOOP*)
    ; RETURN LResult 
    END HashInsert

; PROCEDURE RebuildHash
    ( DictBase : DictBaseTyp
    ; FromTableRef : REF ARRAY OF RowTyp
    ; NewNumber : INTEGER 
    )
  RAISES { Error } 

  = VAR LJunkValue : ValueTyp
  ; VAR LDuplicate : BOOLEAN
  ; VAR LDuplicatesAreOK : BOOLEAN 

  ; BEGIN
      LDuplicatesAreOK := ISTYPE ( DictBase , FixedTyp ) 
    ; DictBase . DbTableRef := NEW ( REF ARRAY OF RowTyp , NewNumber )

    ; FOR RI := 0 TO LAST ( FromTableRef ^ )
      DO WITH WOldRow = FromTableRef ^ [ RI ] 
        DO IF WOldRow . RowHash # FM3Utils.HashNull (* Row is occupied. *) 
          THEN 
            LDuplicate 
              := HashInsert
                   ( DictBase 
                   , WOldRow . RowKey
                   , WOldRow . RowHash
                   , WOldRow . RowValue
                   , (*OUT*) LJunkValue
                   , DoUpdate := FALSE
                   )
          ; IF LDuplicate AND NOT LDuplicatesAreOK
            THEN RAISE Error ( "Duplicate key while hashing a fixed table." ) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*) 
      END (*FOR*) 
    END RebuildHash 
 
; PROCEDURE GrowHash ( DictBase : DictBaseTyp ) RAISES { Error } 

  = VAR LOldNumber : INTEGER
  ; VAR LOldOccupiedCt : INTEGER
  ; VAR LOldTableRef : REF ARRAY OF RowTyp 
  ; VAR LNewNumber : INTEGER

  ; BEGIN
      LOldNumber := NUMBER ( DictBase . DbTableRef ^ )
    ; LOldOccupiedCt := DictBase . DbOccupiedCt
    ; IF LOldOccupiedCt + LOldOccupiedCt DIV 3 < LOldNumber (* < 75% full *)
      THEN RETURN
      END (*IF*) 
    ; LNewNumber := LOldOccupiedCt * 2
    ; LNewNumber := FM3Primes . NextLargerOrEqualPrime ( LNewNumber )
    ; LOldTableRef := DictBase . DbTableRef
    ; RebuildHash ( DictBase , LOldTableRef , LNewNumber )
    END GrowHash 

(*EXPORTED:*)
; PROCEDURE LookupGrowable 
    ( DictHash : GrowableTyp
    ; Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Value : ValueTyp 
    )
  : BOOLEAN (* Was found. *)
  RAISES { Error } 
  (* About all this does is ensure what was created as a fixed dictionary,
     and could still be using the fixed data structure, is not passed
     in here.  *) 

  = BEGIN
      IF Hash = FM3Utils.HashNull (* Not supplied by caller. *) 
      THEN (* But internally, HashNull is reserved to denote unoccupied row . *)
        IF DictHash . DbHashFunc # NIL
        THEN Hash := DictHash . DbHashFunc ( Key )
        ELSE Hash := GHashDefault
        END (*IF*) 
      END (*IF*)
    ; RETURN HashLookup
        ( NARROW ( DictHash , DictBaseTyp ) , Key , Hash , Value ) 
    END LookupGrowable 

; PROCEDURE HashLookup
    ( DictBase : DictBaseTyp 
    ; Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueTyp 
    )
  : BOOLEAN (* Was found. *)
  RAISES { Error } 
  (* PRE: Hash # FM3Utils.HashNull. *) 
  (* This requires that DictBase actually point to a hashed dictionary, even
     though it might have been created as Fixed.  Callers must insure this. *) 

  = VAR LTableNumber : INTEGER
  ; VAR LProbe , LOrigProbe : INTEGER
  ; VAR LSkip : INTEGER 

  ; BEGIN
      LTableNumber := NUMBER ( DictBase . DbTableRef ^ ) 
    ; LOrigProbe
        := VAL ( Hash MOD VAL ( LTableNumber , FM3Base . HashTyp ) , INTEGER ) 
      (* 0 <= LOrigProbe < LTableNumber. *) 
    ; LProbe := LOrigProbe 
    ; LSkip
        := VAL ( Hash MOD VAL ( LTableNumber - 1 , FM3Base . HashTyp )
               , INTEGER
               ) + 1  
      (* 0 < LSkip < LTableNumber - 1,
         so LSkip MOD LTableNumber (Which is prime) # 0 *) 
    ; LOOP
        WITH WRow = DictBase . DbTableRef ^ [ LProbe ]
        DO IF WRow . RowHash = FM3Utils.HashNull
              (* A free slot, key not present. *)
          THEN RETURN FALSE 
          ELSIF Hash = WRow . RowHash
                AND KeyGenformal . Compare ( WRow . RowKey , Key ) = CmpEQ
          THEN (* Found it. *)
            Val := WRow . RowValue 
          ; RETURN TRUE 
          ELSE
            LProbe := ( LProbe + LSkip ) MOD LTableNumber
          ; IF LProbe = LOrigProbe
            THEN RAISE Error ( "Hash table overfull." ) 
            ELSE (* Loop. *) 
            END (*IF*) 
          END (*IF*) 
        END (*WITH*)
      END (*LOOP*)
    END HashLookup

(* Fixed dictionaries have some restrictions, but may be more compact
   and possibly faster, if you can live with them and if MaxKeyCt is smallish. 
   All calls on InsertFixed must precede a call on FinalizeFixedFixed,
   before any calls on LookupFixed.  Also, duplicate keys will result
   Error's being raised, or possibly in undetected duplicate entries,
   with different values, and nondeterministic results from LookupFixed.
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
      ; LNew . DbState := StateTyp . DsUnsorted  
      ELSE (* Use the hash table anyway. *) 
        LNew := NEW ( FixedTyp )  
      ; LTableNumber := MaxKeyCt + MaxKeyCt DIV 3 (* keep <= 75% full. *) 
      ; LTableNumber := FM3Primes . NextLargerOrEqualPrime ( LTableNumber )
      ; LNew . DbOccupiedCt := 0
      ; LNew . DbState := StateTyp . DsHashed  
      END (*IF*)
    ; LNew . DbHashFunc := HashFunc 
    ; LNew . DbTableRef := NEW ( REF ARRAY OF RowTyp , LTableNumber )
    ; RETURN LNew 
    END NewFixed 
  
(*EXPORTED:*)
; PROCEDURE InsertFixed 
    ( DictFixed : FixedTyp  
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueTyp
    )
  RAISES { Error } 
  (* PRE: Hash # FM3Utils.HashNull. *) 

  = VAR LOldValue : ValueTyp
  ; VAR LFound : BOOLEAN

  ; BEGIN
      IF Hash = FM3Utils.HashNull (* Not supplied by caller. *) 
      THEN
        (* But internally, HashNull is reserved to denote unoccupied row . *)
        IF DictFixed . DbHashFunc # NIL
        THEN Hash := DictFixed . DbHashFunc ( Key )
        ELSE Hash := GHashDefault
        END (*IF*) 
      END (*IF*)
    ; IF DictFixed . DbState = StateTyp . DsHashed
      THEN
        LFound := HashInsert
          ( DictFixed
          , Key
          , Hash
          , Value
          , (*OUT*) LOldValue
          , DoUpdate := FALSE
          )
      ; IF LFound
        THEN (* For a consistent interface, Enforce the semantic rules of
                a fixed dictionary, even when it is using hashing.
             *) 
          RAISE Error ( "Duplicate Key inserted in sorted dictionary." ) 
        END (*IF*) 
      ELSIF DictFixed . DbState # StateTyp . DsUnsorted 
      THEN RAISE Error ( "Lookup in fixed dictionary after FinalizeFixed.")
      ELSE SortedTableInsert ( DictFixed , Key , Hash , Value ) 
      END (*IF*) 
    END InsertFixed

; PROCEDURE SortedTableInsert
    ( DictFixed : FixedTyp 
    ; Key : KeyTyp  
    ; Hash : FM3Base . HashTyp
    ; Value : ValueTyp
    )
  RAISES { Error }
  (* Does only phase 1 of heapsort. *) 

  = VAR LTableNumber : INTEGER
  ; VAR LEmptySs : INTEGER (* Place waiting to be filled. *)  
  ; VAR LParentSs : INTEGER 
  ; VAR LOrphanRow : RowTyp (* Row waiting for a place to fit in.*)
  ; VAR LKeyCmp : FM3Base . CompareTyp 

  ; BEGIN
      IF DictFixed . DbState = StateTyp . DsSorted 
      THEN RAISE Error ( "Insert into fixed dictionary after FinalizeFixed." ) 
      END (*IF*) 
    ; LTableNumber := NUMBER ( DictFixed . DbTableRef ^ )
    ; IF DictFixed . DbOccupiedCt >= LTableNumber
      THEN 
       RAISE Error ( "Fixed dictionary size exceeded." )   
      ELSE 
      (* Usually, this algorithm is described as doing a series of swaps of 
         table rows.  But, also usually, there is a row that gets swapped in 
         somewhere in the table only to be swapped out again in the next step. 
         To cut down on in-and-out row assignments, we keep one row value 
         on the side in LOrphanRow, that would otherwise be in the table at 
         LEmptySs, which in turn is empty.  This also means a swap needs 
         only 2 instead of 3 assignments.
      *)  
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
              (* ^ Using a zero-origin table. *)  
          ; LKeyCmp
              := KeyGenformal . Compare
                   ( LOrphanRow . RowKey
                   , DictFixed . DbTableRef ^ [ LParentSs ] . RowKey
                   )
          ; CASE LKeyCmp
            OF CmpEQ
            => RAISE Error ( "Duplicate Key inserted into fixed table." ) 
            | CmpLT (* Can store the orphan here and be done. *)
            => DictFixed . DbTableRef ^ [ LEmptySs ] := LOrphanRow
              ; EXIT
            | CmpGT (* Must bubble a level. *)
            => DictFixed . DbTableRef ^ [ LEmptySs ]
                 := DictFixed . DbTableRef ^ [ LParentSs ]
              ; LEmptySs := LParentSs
              (* And loop. *)
            ELSE 
            END (*CASE*)
          END (*IF*)
        END (*LOOP*)
      ; INC ( DictFixed . DbOccupiedCt )
      END (*IF*)
    END SortedTableInsert 

(*EXPORTED:*)
; PROCEDURE FinalizeFixed ( DictFixed : FixedTyp )
  RAISES { Error }
  (* Do phase 2 of heapsort. *) 

  = VAR LTableNumber : INTEGER
  ; VAR LEmptySs , LGreaterChildSs : INTEGER 
  ; VAR LLeftChildSs , LRightChildSs : INTEGER 
  ; VAR LOrphanRow , LLeftChildRow , LRightChildRow , LGreaterChildRow : RowTyp 
  ; VAR LCompare : FM3Base . CompareTyp 
  
  ; BEGIN
      IF DictFixed . DbState # StateTyp . DsUnsorted THEN RETURN END (*IF*) 
         (* ^Let's give a pass to duplicate finalize calls here. *)  
    ; IF DictFixed . DbOccupiedCt <= 1 (* Aleady trivially sorted.. *)
      THEN
        DictFixed . DbState := StateTyp . DsSorted
      ; RETURN
      END (*IF*) 
    ; LTableNumber := NUMBER ( DictFixed . DbTableRef ^ )
    ; FOR RRemoveSs := DictFixed . DbOccupiedCt - 1 TO 1 BY - 1 
      DO (* All rows. *)
        IF RRemoveSs + 1 < DictFixed . DbOccupiedCt 
           AND KeyGenformal . Compare
                 ( DictFixed . DbTableRef ^ [ RRemoveSs ] . RowKey
                 , DictFixed . DbTableRef ^ [ RRemoveSs + 1 ] . RowKey
                 )
               = CmpEQ 
        THEN RAISE Error ( "Duplicate Key found sorting fixed table." )
        END (*IF*) 
      ; LOrphanRow := DictFixed . DbTableRef ^ [ RRemoveSs ] (* Make space. *) 
      ; DictFixed . DbTableRef ^ [ RRemoveSs ] := DictFixed . DbTableRef ^ [ 0 ]
      ; LEmptySs := 0 (* Start at top. *) 
      ; LOOP (* Sink orphan toward bottom. *) 
          LLeftChildSs := ( LEmptySs * 2 ) + 1 
        ; LRightChildSs := LLeftChildSs + 1 
        ; IF LLeftChildSs >= RRemoveSs 
          THEN (* No children. We are done. *) 
            DictFixed . DbTableRef ^ [ LEmptySs ] := LOrphanRow 
          ; EXIT
          ELSE (* We at least have a left child. *)  
            LLeftChildRow := DictFixed . DbTableRef ^ [ LLeftChildSs ] 
          ; IF LRightChildSs >= RRemoveSs 
            THEN (* No right child. Treat left as greater child. *)  
              LGreaterChildSs := LLeftChildSs 
            ; LGreaterChildRow := LLeftChildRow 
            ELSE (* Both a left and right child. *) 
              LRightChildRow := DictFixed . DbTableRef ^ [ LRightChildSs ] 
            ; LCompare 
                := KeyGenformal . Compare
                     ( LLeftChildRow . RowKey , LRightChildRow . RowKey)
            ; IF LCompare = CmpGT 
              THEN (* Left is greater child. *) 
                LGreaterChildSs := LLeftChildSs 
              ; LGreaterChildRow := LLeftChildRow 
              ELSE 
                LGreaterChildSs := LRightChildSs 
              ; LGreaterChildRow := LRightChildRow 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        ; LCompare 
            := KeyGenformal . Compare
                 ( LOrphanRow . RowKey , LGreaterChildRow . RowKey ) 
        ; IF LCompare = CmpGT  
          THEN (* Parent is greatest of 3.  We are done. *) 
            DictFixed . DbTableRef ^ [ LEmptySs ] := LOrphanRow 
          ; EXIT 
          ELSE (* Move the greater child up to the current slot, creating
                  space to push the current item down to. *) 
            DictFixed . DbTableRef ^ [ LEmptySs ] := LGreaterChildRow 
          ; LEmptySs := LGreaterChildSs 
          END (* IF *) 
        END (* LOOP Sink orphan. *)  
      END (* FOR All rows. *)
    ; DictFixed . DbState := StateTyp . DsSorted 
    END FinalizeFixed 

(*EXPORTED:*)
; PROCEDURE LookupFixed  
    ( DictFixed : FixedTyp
    ; Key : KeyTyp 
    ; Hash : FM3Base . HashTyp
    ; VAR (*OUT*) Val : ValueTyp 
    )
  : BOOLEAN (* Was found. *)
  RAISES { Error } 

  = VAR LLo , LHi , LProbe : INTEGER
  ; VAR LCompare : [ CmpLT .. CmpGT ] 

  ; BEGIN
      IF DictFixed . DbState = StateTyp . DsUnsorted 
      THEN RAISE Error ( "Lookup in fixed dictionary before FinalizeFixed." ) 
      END (*IF*)
    ; IF Hash = FM3Utils.HashNull (* Not supplied by caller. *) 
      THEN (* But internally, HashNull is reserved to denote unoccupied row . *)
        IF DictFixed . DbHashFunc # NIL
        THEN Hash := DictFixed . DbHashFunc ( Key )
        ELSE Hash := GHashDefault
        END (*IF*) 
      END (*IF*) 

    ; IF DictFixed . DbState = StateTyp . DsHashed 
      THEN  
        RETURN HashLookup
          ( NARROW ( DictFixed , DictBaseTyp ) 
          , Key 
          , Hash 
          , (*OUT*) Val 
          ) 

      ELSE (* Binary search. *)       
        LLo := 0
      ; LHi := DictFixed . DbOccupiedCt
      (* INVARIANT: Sought entry, if present, is at subscript S,
                    where Lo <= S < LHi *) 
      ; LOOP
          IF LLo = LHi (* Empty range. *) THEN RETURN FALSE END (*IF*) 
        ; LProbe := ( LLo + LHi ) DIV 2
        ; LCompare
            := KeyGenformal . Compare
                 ( Key , DictFixed . DbTableRef ^ [ LProbe ] . RowKey )
        ; IF LCompare = CmpEQ
          THEN (* Lucky early find. *) 
            Val := DictFixed . DbTableRef ^ [ LProbe ] . RowValue 
          ; RETURN TRUE
          ELSIF LCompare = CmpLT THEN LHi := LProbe 
          ELSE LLo := LProbe + 1
          END (*IF*) 
        END (*LOOP*) 
      END (*IF*) 
    END LookupFixed

; BEGIN
  END FM3Dict 
.
