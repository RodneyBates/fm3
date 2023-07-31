
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
; IMPORT Pickle2 
; IMPORT Rd
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextWr 
; IMPORT Thread  
; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Base
; IMPORT FM3LexTable 

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
  (* Convenience procedure for catchers of FatalError.
     Just write to stderror.
  *) 

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
        ; IF LCharPos >= NUMBER ( FM3Magic )  THEN EXIT END (*IF*)   
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
    ( FileName , PathName , Note1 , Note2 : TEXT := "" )
  : Rd . T
  RAISES { FatalError } 

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
  RAISES { FatalError , Thread . Alerted } 

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LFileKind : FileKindTyp 
  ; VAR LIsOK : BOOLEAN 
  
  ; BEGIN
      LRdT := OpenRd ( FileName , ResourcePathName )
    ; ReadPrefix ( LRdT , (*OUT*) LFileKind , (*OUT*) LIsOK )  
    ; IF LFileKind # ExpectedFileKind
      THEN (* Wrong kind. *)
        (* Oh WTH, just do this twice: *) 
        LFullFileName := Libm3Pathname . Join ( ResourcePathName , FileName ) 
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

(*EXPORTED*) 
; PROCEDURE ReadPickle
    ( FileName : TEXT ; ExpectedKind : FileKindTyp )
  : REFANY
  RAISES { FatalError , Thread . Alerted } 

  = VAR LRdT : Rd . T 
  ; VAR LResult : REFANY 

  ; BEGIN
      LRdT := OpenResourceRd ( FileName , ExpectedKind )
    ; TRY (*EXCEPT*)
        LResult := Pickle2 . Read ( LRdT ) 
      EXCEPT
      | Pickle2 . Error ( EMsg )
      => RaiseFatal
           ( CatStrings
               ( "Unable to unpickle file \""
               , FileName
               , "\", Pickle2.Error ("
               , EMsg
               , ")"
               )
           )
      | Rd . Failure ( EAtomList )
      => RaiseFatal
           ( CatStrings
               ( "Unable to unpickle file \""
               , FileName
               , "\", Rd.Failure ("
               , AtomListToText ( EAtomList ) 
               , ")"
               )
           )
      | Rd . EndOfFile 
      => RaiseFatal
           ( CatStrings
               ( "Unable to unpickle file \""
               , FileName
               , "\", Rd.EndOfFile"
               )
           )
      END (*EXCEPT*)
    ; RETURN LResult 
    END ReadPickle

(*EXPORTED*) 
; PROCEDURE ReadFsm ( FileName : TEXT ; Kind : FileKindTyp )
  : FM3LexTable . T
  RAISES { Thread . Alerted } 


  = VAR LRef : REFANY 

  ; BEGIN
      LRef := ReadPickle ( FileName , Kind )
    ; TYPECASE LRef OF
      | NULL 
        => RaiseFatal
             ( CatStrings
                 ( "In ReadFsm, NIL pickle file \"" , FileName , "\"" ) )
      | FM3LexTable . T ( TLexTable ) => RETURN TLexTable 
      ELSE
        RaiseFatal
          ( CatStrings
              ( "In ReadFsm, not an FSM pickle file \"" , FileName , "\"" ) )
      END (*TYPECASE*)
    END ReadFsm

(* This is not being pickled.  That would be redundant to procedure
   Image, produced by gentok.  
; PROCEDURE ReadIntTextArray ( RdT : Rd . T ) : IntTextArray . T

  = BEGIN
      LRef := Pickle2 . Read ( LRdT ) 
    ; TYPECASE LRef OF
      | NULL 
        => RaiseFatal
             ( CatStrings
                 ( "In ReadIntTextArray, NIL pickle file \""
                 , FileName
                 , "\"" )
                 )
      | IntTextArray . T ( TResult ) => RETURN TResult  
      ELSE
        RaiseFatal
          ( CatStrings
              ( "In ReadIntTextArray, not a textArray pickle file \""
              , FileName
              , "\"" )
              )
      END (*TYPECASE*)
    END ReadIntTextArray
*) 

(*EXPORTED*) 
; PROCEDURE ReadSets
    ( FileName : TEXT
    ; Kind : FileKindTyp
    ; VAR Temp : IntSets . T 
    ; VAR Patch : IntSets . T 
    ; VAR Arg1 : IntSets . T 
    ; VAR Arg2 : IntSets . T 
    ; VAR Arg3 : IntSets . T 
    )
  RAISES { FatalError , Thread . Alerted } 

  = VAR LRdT : Rd . T

  ; PROCEDURE RsReadIntSet ( RdT : Rd . T ) : IntSets . T
    RAISES { FatalError } 

    = VAR LRef : REFANY

    ; BEGIN
        LRef := Pickle2 . Read ( LRdT ) 
      ; TYPECASE LRef OF
        | NULL 
          => RaiseFatal
               ( CatStrings
                   ( "In RsReadIntSet, NIL pickle file \""
                   , FileName
                   , "\"" )
                   )
        | IntSets . T ( TResult ) => RETURN TResult  
        ELSE
          RaiseFatal
            ( CatStrings
                ( "In RsReadIntSet, not a textArray pickle file \""
                , FileName
                , "\"" )
                )
        END (*TYPECASE*)
      END RsReadIntSet 

  ; BEGIN
      LRdT :=  OpenResourceRd ( FileName , FM3FileKindTokSetsPkl )
    ; Temp := RsReadIntSet ( LRdT ) 
    ; Patch := RsReadIntSet ( LRdT ) 
    ; Arg1 := RsReadIntSet ( LRdT ) 
    ; Arg2 := RsReadIntSet ( LRdT ) 
    ; Arg3 := RsReadIntSet ( LRdT ) 
    END ReadSets 

(*EXPORTED*) 
; PROCEDURE IntHash ( Val : INTEGER ) : FM3Base . HashTyp

  = BEGIN
      RETURN VAL ( Val , FM3Base . HashTyp ) 
    END IntHash 

; BEGIN
  END FM3SharedUtils 
.
