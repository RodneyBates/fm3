(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Dec 16 11:02:01 PST 1991 by kalsow     *)
(*      modified on Sat May 19 07:28:29 1990 by muller         *)

<* FM3_PREDEFINED *> (* Special handling by FM3 compiler. *)  

MODULE Word

(* This is confusing.  It imports its own interface as Self,
   which makes each of these procedures appear, at face value, to be 
   direct, trivial, unconditional, infinite self-recursion.

   In fact, the names of these procedures are predefined in the compiler, 
   and, for *calls* on these, the compiler generates inline code instead
   of actual calls.  So, when a call on one of these appears in ordinary
   user code, this library is not involved at all.  

   The self-recursive-appearing calls herein are also replaced by the 
   compiler by the same inline code.  Thus, the library code here is
   just trivial wrappers around that.  

   However, the compiler handles non-call occurrences of their names
   as usual, using the address of the procedure in this library.    
   So the one way the procedures herein can be used is if user code 
   assigns one of them to a variable of procedure type, or passes one 
   to a parameter of procedure type, then makes a call through the 
   variable/parameter.  
*) 

; PROCEDURE Plus (x, y: T): T     = BEGIN RETURN Self.Plus (x, y)   END Plus 
; PROCEDURE Times (x, y: T): T    = BEGIN RETURN Self.Times (x, y)  END Times 
; PROCEDURE Minus (x, y: T): T    = BEGIN RETURN Self.Minus (x, y)  END Minus 
; PROCEDURE Divide (x, y: T): T   = BEGIN RETURN Self.Divide (x, y) END Divide 
; PROCEDURE Mod (x, y: T): T      = BEGIN RETURN Self.Mod (x, y)    END Mod 
; PROCEDURE LT (x, y: T): BOOLEAN = BEGIN RETURN Self.LT (x, y)     END LT 
; PROCEDURE LE (x, y: T): BOOLEAN = BEGIN RETURN Self.LE (x, y)     END LE 
; PROCEDURE GT (x, y: T): BOOLEAN = BEGIN RETURN Self.GT (x, y)     END GT 
; PROCEDURE GE (x, y: T): BOOLEAN = BEGIN RETURN Self.GE (x, y)     END GE 
; PROCEDURE And (x, y: T): T      = BEGIN RETURN Self.And (x, y)    END And 
; PROCEDURE Or (x, y: T): T       = BEGIN RETURN Self.Or (x, y)     END Or 
; PROCEDURE Xor (x, y: T): T      = BEGIN RETURN Self.Xor (x, y)    END Xor 
; PROCEDURE Not (x: T): T         = BEGIN RETURN Self.Not (x)       END Not 

; PROCEDURE Shift (x: T; n: INTEGER): T
  = BEGIN RETURN Self.Shift (x, n) END Shift 

; PROCEDURE LeftShift (x: T; n: [0..Size-1]): T
  = BEGIN RETURN Self.LeftShift (x, n) END LeftShift 

; PROCEDURE RightShift (x: T; n: [0..Size-1]): T
  = BEGIN RETURN Self.RightShift (x, n) END RightShift 

; PROCEDURE Rotate (x: T; n: INTEGER): T
  = BEGIN RETURN Self.Rotate (x, n) END Rotate 

; PROCEDURE LeftRotate (x: T; n: [0..Size-1]): T
  = BEGIN RETURN Self.LeftRotate (x, n) END LeftRotate 

; PROCEDURE RightRotate (x: T; n: [0..Size-1]): T
  = BEGIN RETURN Self.RightRotate (x, n) END RightRotate 
 
; PROCEDURE Extract (x: T; i, n: CARDINAL): T
  = BEGIN RETURN Self.Extract (x, i, n) END Extract 

; PROCEDURE Insert (x, y: T; i, n: CARDINAL): T
  = BEGIN RETURN Self.Insert (x, y, i, n) END Insert 

BEGIN
END Word
.
