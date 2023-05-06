        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3Messages

; IMPORT Atom
; IMPORT AtomList
; IMPORT Stdio 
; IMPORT TextWr
; IMPORT Wr

; IMPORT FM3CLArgs 
; IMPORT FM3Globals 
; IMPORT FM3Utils

  (* Fatal amd Log go immediatly to stderr and optionally to a log file. *)

; PROCEDURE PutStdErr ( Msg : TEXT )

  = BEGIN
      IF FM3CLArgs . DoStdErr
      THEN
        Wr . PutText ( Stdio . stderr , Msg )
      ; Wr . PutText ( Stdio . stderr , Wr . EOL )
      ; Wr . Flush ( Stdio . stderr ) 
      END (*IF*) 
    END PutStdErr 

; PROCEDURE PutStdOut ( Msg : TEXT )

  = BEGIN
      IF FM3CLArgs . DoStdOut
      THEN
        Wr . PutText ( Stdio . stdout , Msg )
      ; Wr . PutText ( Stdio . stdout , Wr . EOL )
      ; Wr . Flush ( Stdio . stdout ) 
      END (*IF*) 
    END PutStdOut 

; PROCEDURE PutCompileLog ( Msg : TEXT )

  = BEGIN
      IF FM3CLArgs . DoCompLog
         (* => FM3Globals . CurrentUnitRef . UntCompLogWrT # NIL and is open. *)
      THEN
        Wr . PutText ( FM3Globals . CurrentUnitRef . UntCompLogWrT , Msg )
      ; Wr . PutText ( FM3Globals . CurrentUnitRef . UntCompLogWrT , Wr . EOL )
      END (*IF*) 
    END PutCompileLog

; PROCEDURE PutLog ( Msg : TEXT )

  = BEGIN
      IF FM3CLArgs . DoLog (* => FM3CLArgs . LogWrT # NIL and is open. *) 
      THEN
        Wr . PutText ( FM3CLArgs . LogFileWrT , Msg )
      ; Wr . PutText ( FM3CLArgs . LogFileWrT , Wr . EOL )
      END (*IF*) 
    END PutLog 

; PROCEDURE CatText ( WrT : Wr . T ; Txt : REFANY )

  = BEGIN
      TYPECASE Txt OF
      | NULL =>
      | TEXT ( TTxt )
        => Wr . PutText ( WrT , TTxt )
      | Atom . T ( TAtom )
        => Wr . PutText ( WrT , Atom . ToText ( TAtom ) )
      | AtomList . T ( TAtom )
        => Wr . PutText ( WrT , FM3Utils . AtomListToText ( TAtom ) )
      ELSE
      END (*TYPECASE*)
    END CatText

; PROCEDURE Contents
    ( T0 , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) : TEXT
    
  = VAR LWrT : TextWr . T
  ; VAR LMsg : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( ) 
    ; CatText ( LWrT , T0 ) 
    ; CatText ( LWrT , T1 ) 
    ; CatText ( LWrT , T2 ) 
    ; CatText ( LWrT , T3 ) 
    ; CatText ( LWrT , T4 ) 
    ; CatText ( LWrT , T5 ) 
    ; CatText ( LWrT , T6 ) 
    ; CatText ( LWrT , T7 ) 
    ; CatText ( LWrT , T8 )
    ; LMsg := TextWr . ToText ( LWrT )
    ; IF LMsg = NIL THEN LMsg := "" END (*IF*) (* Can this happen? *) 
    ; RETURN LMsg 
    END Contents  

(*EXPORTED*)
; PROCEDURE Fatal ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 
  RAISES { Terminate }
  (* Also terminates the program. *)

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := Contents
             ( "FM3 FATAL:"
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             )
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    ; RAISE Terminate 
    END Fatal  

(*EXPORTED*)
; PROCEDURE Log ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL ) 

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := Contents
             ( "FM3: "
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    END Log

(* Within a unit, Info, Warning, and Error are collected, sorted by
   line/column, and written to stdout at the end of the unit. *)

(*EXPORTED*)
; PROCEDURE Info ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := Contents
             ( "Info: "
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg )
    ; PutCompileLog ( LMsg ) 
    END Info 

(*EXPORTED*)
; PROCEDURE Warning ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := Contents
             ( "Warning: "
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg ) 
    ; PutCompileLog ( LMsg ) 
    END Warning

(*EXPORTED*)
; PROCEDURE Error ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL )

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg
        := Contents
             ( "Error " 
             , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8
             ) 
    ; PutStdOut ( LMsg ) 
    ; PutCompileLog ( LMsg ) 
    END Error

(*EXPORTED*)
; PROCEDURE StartUnit ( UnitName : TEXT )

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := Contents ( "Start unit " , UnitName ) 
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    END StartUnit 

(*EXPORTED*)
; PROCEDURE EndUnit ( UnitName : TEXT )

  = VAR LMsg : TEXT 

  ; BEGIN
      LMsg := Contents ( "End unit " , UnitName ) 
    ; PutStdErr ( LMsg ) 
    ; PutLog ( LMsg ) 
    END EndUnit 

(*EXPORTED*)
; PROCEDURE AtomListToOSError ( AL : AtomList . T ): TEXT

  = BEGIN
      RETURN "OSError.E("
             & FM3Utils . AtomListToText ( AL )
             & ")" 
    END AtomListToOSError  

; BEGIN
  END FM3Messages
.

