
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Primes 

; IMPORT IntIntVarArray
; FROM IntIntVarArray IMPORT Assign , Fetch
; IMPORT IntRanges 

; CONST EstimatedPrimesDensity = 5 

; TYPE PrimesSsTyp = INTEGER 
; TYPE PrimesPtrTyp = IntIntVarArray . T 
; TYPE DynArrayIntTyp = INTEGER 

; VAR PrimesPtr : PrimesPtrTyp 
; VAR PrimesSize : PrimesSsTyp 
; VAR PrimesCt : PrimesSsTyp

; PROCEDURE MakeTableNonempty ( Arg : PrimeTyp )

  = VAR LElemCt : DynArrayIntTyp 

  ; BEGIN 
      IF PrimesPtr = NIL  
      THEN
        LElemCt := Arg DIV EstimatedPrimesDensity
      ; LElemCt := MAX ( LElemCt , 11 ) 
      ; PrimesPtr
          := IntIntVarArray . New ( 0 , IntRanges . RangeTyp { 0 , LElemCt } ) 
      ; PrimesSize := LElemCt 
      ; PrimesCt := 11 
      ; Assign ( PrimesPtr , 0 , 3 ) 
      ; Assign ( PrimesPtr , 1 , 5 ) 
      ; Assign ( PrimesPtr , 2 , 7 ) 
      ; Assign ( PrimesPtr , 3 , 11 ) 
      ; Assign ( PrimesPtr , 4 , 13 ) 
      ; Assign ( PrimesPtr , 5 , 17 ) 
      ; Assign ( PrimesPtr , 6 , 19 ) 
      ; Assign ( PrimesPtr , 7 , 23 ) 
      ; Assign ( PrimesPtr , 8 , 29 ) 
      ; Assign ( PrimesPtr , 9 , 31 ) 
      ; Assign ( PrimesPtr , 10 , 37 ) 
      END (* IF *) 
    END MakeTableNonempty 

; PROCEDURE BinarySearch ( Arg : PrimeTyp ) : PrimeTyp 
  (* PRE Arg <= Fetch ( PrimesPtr , PrimeCt - 1 ) *)

  = VAR LLow , LHigh , LProbe : PrimesSsTyp 

  ; BEGIN
      LLow := 0 
    ; LHigh := PrimesCt - 1 
    (* INVARIANT:
         Fetch ( PrimesPtr , LLow - 1 ) < Arg <= Fetch ( PrimesPtr , LHigh )
         Where Fetch ( PrimesPtr , - 1 )( which doesn't actually exist) is
         considered less than any Arg. *)
    ; LOOP
        IF LLow = LHigh 
        THEN (* Found it *) 
          RETURN Fetch ( PrimesPtr , LHigh )
        ELSE
          LProbe := ( LLow + LHigh ) DIV 2 
        ; <* ASSERT LProbe < LHigh *>  
          IF Arg <= Fetch ( PrimesPtr , LProbe ) 
          THEN LHigh := LProbe 
          ELSE LLow := LProbe + 1 
          END (* IF *)
        END (* IF *) 
      END (* LOOP *)
    END BinarySearch 

; PROCEDURE IsPrime ( Trial : PrimeTyp ) : BOOLEAN 

  = VAR LDivisorSs : PrimesSsTyp 
  ; VAR LDivisor : PrimeTyp

  ; BEGIN
      LDivisorSs := 0 
    ; LOOP 
        IF LDivisorSs >= PrimesCt
        THEN (* Mo more divisors. Every prime < Trial is in the table. *)
          RETURN TRUE 
        ELSE 
          LDivisor := Fetch ( PrimesPtr , LDivisorSs )
        ; IF (LDivisor * LDivisor) > Trial 
          THEN (* We've tried all divisors <= sqrt ( Trial ). *) 
            RETURN TRUE 
          ELSIF ( ( Trial DIV LDivisor ) * LDivisor ) = Trial 
          THEN (* Found a divisor. *) 
            RETURN FALSE 
          ELSE INC ( LDivisorSs ) 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    END IsPrime 

; PROCEDURE FillTableUpTo ( Arg : PrimeTyp ) 
  (* PRE Arg > Fetch ( PrimesPtr , PrimeCt - 1 ) *)

  = VAR LTrial : PrimeTyp 

  ; BEGIN 
      LTrial := Fetch ( PrimesPtr , PrimesCt - 1 )
    ; LOOP 
        <* ASSERT 
             ( LAST ( PrimeTyp ) - 2 ) >= LTrial
           , "Primes.FillTableUpTo, integer overflow searching for a prime."
        *> 
        INC ( LTrial , 2 )
      ; IF IsPrime ( LTrial ) 
        THEN
          Assign ( PrimesPtr , PrimesCt , LTrial ) 
        ; INC ( PrimesCt ) 
        ; IF LTrial >= Arg 
          THEN EXIT 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    END FillTableUpTo  

(*EXPORTED:*)
; PROCEDURE NextLargerOrEqualPrime ( Arg : PrimeTyp ) : PrimeTyp

  = BEGIN
      MakeTableNonempty ( Arg ) 
    ; IF Arg <= Fetch ( PrimesPtr , PrimesCt - 1 )
      THEN (* We already have enough primes in the table. *)
        RETURN BinarySearch ( Arg ) 
      ELSE 
        FillTableUpTo ( Arg ) 
      ; RETURN Fetch ( PrimesPtr , PrimesCt - 1 )
      END (* IF *) 
    END NextLargerOrEqualPrime 

; BEGIN (* Primes *) 
    PrimesPtr := NIL 
  ; PrimesSize := 0 
  ; PrimesCt := 0 
  END FM3Primes
. 

