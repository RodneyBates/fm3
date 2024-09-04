
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A Wr.T subtype that forwards its output to a RdBackFile . T. *) 

MODULE RdBackWr

; FROM Atom IMPORT FromText 
; IMPORT AtomList
; FROM AtomList IMPORT Cons
; IMPORT Fmt 
; IMPORT OSError
; IMPORT Thread 
; IMPORT Wr
; IMPORT WrClass 

; IMPORT RdBackFile
      
; REVEAL
    T = Wr . T  BRANDED OBJECT
          RbT : RdBackFile . T 
        OVERRIDES
          close  := Close
        ; length := Length
        ; putString := PutString
        END

(*EXPORTED:*)
; PROCEDURE New ( RbT : RdBackFile . T ) : T

  = VAR LResult : T

  ; BEGIN
      LResult := NEW ( T ) 
    ; LResult . closed := FALSE
    ; LResult . seekable := FALSE
    ; LResult . buffered := FALSE
    ; LResult . RbT := RbT
    ; RETURN LResult 
    END New
    
; PROCEDURE MakeAtomList
    ( T1 , T2 , T3 , T4 , T5 : TEXT := NIL ) : AtomList . T 
(*TODO Put this somewhere more general. *)
  = VAR LResult : AtomList . T

  ; BEGIN
      LResult := NIL
    ; IF T1 # NIL THEN LResult := Cons ( FromText ( T1 ) , LResult ) END (*IF*)
    ; IF T2 # NIL THEN LResult := Cons ( FromText ( T2 ) , LResult ) END (*IF*)
    ; IF T3 # NIL THEN LResult := Cons ( FromText ( T3 ) , LResult ) END (*IF*)
    ; IF T4 # NIL THEN LResult := Cons ( FromText ( T4 ) , LResult ) END (*IF*)
    ; IF T5 # NIL THEN LResult := Cons ( FromText ( T5 ) , LResult ) END (*IF*)
    ; RETURN LResult 
    END MakeAtomList  

; PROCEDURE Length ( RbWrT : T ) : CARDINAL
  RAISES { Wr . Failure }
  
  = VAR LLengthL : LONGINT
  
  ; BEGIN
      TRY (* Can't raise OSError.E because Length overrides a method
             that does not.  So change the exception to Wr.Failure.
          *) 
        LLengthL := RdBackFile . LengthL ( RbWrT . RbT  )
      EXCEPT OSError . E ( Msg )
      => RAISE Wr . Failure ( Msg )
      END (*EXCEPT*) 
    ; IF LLengthL > VAL ( LAST ( CARDINAL ) , LONGINT )
      THEN
        RAISE
          Wr . Failure 
            ( MakeAtomList
                ( "RdBackWr LengthL = "
                , Fmt . LongInt ( LLengthL )
                , ", out of INTEGER range." 
                )
            )
      END (*IF*)
    ; RETURN VAL ( LLengthL , INTEGER ) 
    END Length

; PROCEDURE PutString ( RbWrT : T ; READONLY Chars : ARRAY OF CHAR )
  RAISES { Wr . Failure }
  
  = BEGIN
      TRY 
        FOR RI := FIRST ( Chars ) TO LAST ( Chars )
        DO RdBackFile . Put ( RbWrT . RbT , ORD ( Chars [ RI ] ) ) 
        END (*FOR*)
      EXCEPT OSError . E ( Msg )
      => RAISE Wr . Failure ( Msg )
      END (*EXCEPT*) 
    END PutString 

; PROCEDURE Close ( RbWrT : T )  
  RAISES { Thread . Alerted , Wr . Failure } 

  = BEGIN
      Wr . Flush ( RbWrT ) (* Might result in a call on PutString. *) 
    ; RbWrT. closed := TRUE 
      (* Let's just leave the target RdBackFile open. *) 
    END Close

; BEGIN
  END RdBackWr
.

