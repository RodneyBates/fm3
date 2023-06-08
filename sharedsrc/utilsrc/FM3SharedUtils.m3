
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023,       Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3SharedUtils

; IMPORT Atom 
; IMPORT AtomList 
; IMPORT FileRd 
; IMPORT OSError 
; IMPORT Pathname AS Libm3Pathname 
; IMPORT Rd
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextWr 
; IMPORT Thread  
; IMPORT Wr  

; CONST FM3FileTag = "FM3"
; VAR TagLength := Text . Length ( FM3FileTag ) 

; CONST FM3Magic
    = ARRAY [ 0 .. 3 ] OF CHAR 
        { VAL ( 16_A2 , CHAR ) , VAL ( 16_0B , CHAR )
        , VAL ( 16_9F , CHAR ) , VAL ( 16_D9 , CHAR )
        }

; PROCEDURE RaiseFatal ( Msg : TEXT ) RAISES { FatalError } 
  (* Code herein needs to note fatal errors, but they need to be
     reported differently in standalone metaprograms and the compiler.
     So we raise an exception and let the whichever program catch and
     report it.  Sharing code like this can get pretty entangled. 
  *) 

  = BEGIN
      RAISE FatalError( Msg ) 
    END RaiseFatal 

(*EXPORTED*) 
; PROCEDURE StandaloneFatalError ( Msg : TEXT )

  = BEGIN 
      Wr . PutText ( Stdio . stderr , Msg )
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . Flush ( Stdio . stderr )
    END StandaloneFatalError 

(*EXPORTED*) 
; PROCEDURE AtomListToText ( List : AtomList . T ): TEXT

  = VAR LWrT : Wr . T
  ; VAR LAtomList : AtomList . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; LAtomList := List
    ; LOOP
        IF LAtomList = NIL THEN EXIT END (*IF*)
      ; IF LAtomList . head # NIL
        THEN Wr . PutText ( LWrT , Atom . ToText ( LAtomList . head ) ) 
        END (* IF *)
      ; LAtomList := LAtomList . tail 
      END (*LOOP*)
    ; LResult := TextWr . ToText ( LWrT )
    ; IF LResult = NIL THEN LResult := "" END (*IF*) (* Paranoia. *)
    ; RETURN LResult 
    END AtomListToText 

(*EXPORTED*) 
; PROCEDURE CatStrings
    ( T0 , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : REFANY := NIL ) : TEXT
  (* Each Tn can be TEXT, Atom.T, or AtomList.T. *) 
    
  = VAR LWrT : TextWr . T
  ; VAR LMsg : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( ) 
    ; CatOneString ( LWrT , T0 ) 
    ; CatOneString ( LWrT , T1 ) 
    ; CatOneString ( LWrT , T2 ) 
    ; CatOneString ( LWrT , T3 ) 
    ; CatOneString ( LWrT , T4 ) 
    ; CatOneString ( LWrT , T5 ) 
    ; CatOneString ( LWrT , T6 ) 
    ; CatOneString ( LWrT , T7 ) 
    ; CatOneString ( LWrT , T8 )
    ; LMsg := TextWr . ToText ( LWrT )
    ; IF LMsg = NIL THEN LMsg := "" END (*IF*) (* Can this happen? *) 
    ; RETURN LMsg 
    END CatStrings  

(*EXPORTED*) 
; PROCEDURE FileKindImage ( Kind : FileKindTyp ) : TEXT

  = BEGIN
      RETURN "\'" & Text . FromChar ( Kind ) & "\'" 
    END FileKindImage

; PROCEDURE CatOneString ( WrT : Wr . T ; Txt : REFANY )
  (* Txt can be TEXT, Atom.T, or AtomList.T. *) 

  = BEGIN
      TYPECASE Txt OF
      | NULL =>
      | TEXT ( TTxt )
        => Wr . PutText ( WrT , TTxt )
      | Atom . T ( TAtom )
        => Wr . PutText ( WrT , Atom . ToText ( TAtom ) )
      | AtomList . T ( TAtom )
        => Wr . PutText ( WrT , AtomListToText ( TAtom ) )
      ELSE
      END (*TYPECASE*)
    END CatOneString

(*EXPORTED*) 
; PROCEDURE FilePrefix ( Kind : FileKindTyp ) : TEXT 

  = VAR LResult : TEXT
  
  ; BEGIN
      LResult
        := FM3FileTag & Text . FromChar ( Kind ) & Text . FromChars ( FM3Magic )
    ; RETURN LResult 
    END FilePrefix

; PROCEDURE ReadPrefix
    ( RdT : Rd . T ; VAR Kind : FileKindTyp ; VAR IsOK : BOOLEAN )

  = VAR LCharPos : CARDINAL
  ; VAR LChar : CHAR

  ; <*FATAL Rd . EndOfFile *>
    BEGIN
      IsOK := FALSE
    ; IF Rd . Closed ( RdT ) THEN RETURN END (*IF*)

    ; TRY (*EXCEPT*) 
    
      (* Verify tag: *) 
        LCharPos := 0
      ; LOOP
          IF Rd . EOF ( RdT ) THEN RETURN END (*IF*) 
        ; LChar := Rd . GetChar ( RdT )
        ; IF LChar # Text . GetChar ( FM3FileTag , LCharPos )
          THEN RETURN
          END (*IF*) 
        ; INC ( LCharPos ) 
        ; IF LCharPos >= TagLength THEN EXIT END (*IF*) 
        END (*LOOP*)

      (* Get Kind byte: *) 
      ; IF Rd . EOF ( RdT ) THEN RETURN END (*IF*) 
      ; Kind := Rd . GetChar ( RdT )

      (* Verify magic number: *) 
      ; LCharPos := 0
      ; LOOP
          IF Rd . EOF ( RdT ) THEN RETURN END (*IF*) 
        ; LChar := Rd . GetChar ( RdT )
        ; IF LChar # FM3Magic [ LCharPos ] THEN RETURN END (*IF*) 
        ; INC ( LCharPos ) 
        ; IF LCharPos >= TagLength THEN EXIT END (*IF*)   
        END (*LOOP*)

      ; IsOK := TRUE 
      ; RETURN
      EXCEPT
      | Rd . Failure , Thread . Alerted
      => RETURN 
      END (*EXCEPT*)
    END ReadPrefix

(*EXPORTED*) 
; PROCEDURE OpenRd
    ( FileName , PathName , Note1 , Note2 : TEXT := "" ) : Rd . T

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  
  ; BEGIN
      LFullFileName := Libm3Pathname . Join ( PathName , FileName ) 
    ; TRY 
       LRdT := FileRd . Open ( LFullFileName ) 
      EXCEPT
      | OSError . E ( EMsg ) 
      => RaiseFatal
           ( CatStrings 
               ( "Unable to open " , Note1 , Note2 , LFullFileName
               , ": OSError.E(" , AtomListToText ( EMsg ) , ")."
               )
           )
      END (*EXCEPT*)
    ; RETURN LRdT 
    END OpenRd

(*EXPORTED*) 
; PROCEDURE OpenResourceRd
    ( FileName : TEXT := "" ; ExpectedFileKind : FileKindTyp ) 
  : Rd . T
  RAISES { FatalError } 

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LFileKind : FileKindTyp 
  ; VAR LIsOK : BOOLEAN 
  
  ; BEGIN
      LRdT := OpenRd ( FileName , GResourcePathName )
    ; ReadPrefix ( LRdT , (*OUT*) LFileKind , (*OUT*) LIsOK )  
    ; IF LFileKind # ExpectedFileKind
      THEN (* Wrong kind. *)
        (* Oh WTH, just do this twice: *) 
        LFullFileName := Libm3Pathname . Join ( GResourcePathName , FileName ) 
      ; RaiseFatal
          ( CatStrings 
              ( "Resource file " , LFullFileName 
              , " has wrong kind: " , FileKindImage ( LFileKind )  
              , ", expecting " , FileKindImage ( ExpectedFileKind )  
              )
          ) 
      END (*IF*)
    ; RETURN LRdT 
    END OpenResourceRd

; BEGIN
  END FM3SharedUtils 
.
