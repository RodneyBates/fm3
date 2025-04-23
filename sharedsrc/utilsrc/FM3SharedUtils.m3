
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3SharedUtils

; IMPORT Atom 
; IMPORT AtomList
; IMPORT File 
; IMPORT FileRd
; IMPORT Fmt 
; IMPORT FS 
; IMPORT OSError
; IMPORT Params 
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
; IMPORT FM3SharedGlobals
; IMPORT FM3UnsafeUtils 

(*EXPORTED*) 
; PROCEDURE LongHexImage ( Value : LONGINT ) : TEXT 

  = BEGIN
      RETURN
        "16_" & Fmt . Pad
                  ( Fmt . LongUnsigned ( Value , base := 16 )
                  , length := 16
                  , padChar := '0'
                  , align:= Fmt . Align . Right
                  )
    END LongHexImage 

(*EXPORTED*) 
; <*INLINE*> PROCEDURE RefanyImage ( Value : REFANY ) : TEXT

  = BEGIN
      RETURN LongHexImage ( FM3UnsafeUtils . RefanyToLongInt ( Value ) ) 
    END RefanyImage 

(*EXPORTED*) 
; PROCEDURE PluralSuffix ( Value : INTEGER ) : TEXT 

  = BEGIN
      IF Value = 1 THEN RETURN "" END (*IF*)
    ; RETURN "s"
    END PluralSuffix

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

(* EXPORTED: *) 
; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT 

  = VAR LResult : TEXT 

  ; BEGIN (* AbsFileName *) 
      IF Libm3Pathname . Absolute ( Name ) 
      THEN RETURN Name 
      ELSE 
        TRY 
          LResult := FS . GetAbsolutePathname ( Name ) 
        ; RETURN LResult
        EXCEPT OSError . E 
        => RETURN Name
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END AbsFileName 

(*EXPORTED*) 
; PROCEDURE SibDirectoryPath ( FileName : TEXT ; SibDirName : TEXT ) : TEXT
(* Absolute path name of "SibDir" directory beside parent directory of
   FileName.
*) 

  = VAR LFileAbs : TEXT
  ; VAR LDirAbs : TEXT 
  ; VAR LResult : TEXT 

  ; BEGIN 
      IF FileName = NIL THEN FileName := "" END (*IF*) 
    ; TRY LFileAbs := FS . GetAbsolutePathname ( FileName ) 
      EXCEPT OSError . E ( EMsg )
      =>
(*

(* FIXME: Use the multi-executable message system. *) 
         FM3Messages . FatalArr  
           ( ARRAY OF REFANY
               { " Unable to get absolute path of executable at: \""
               , PathName 
               , "\""
               , Wr . EOL
               , "    OSError.E("
               , EMsg
               , ")."
               }
           )
*) 
      END (*EXCEPT*)
    ; LDirAbs := Libm3Pathname . Prefix ( LFileAbs )
    ; LResult := LDirAbs & "/../" & SibDirName  
    ; RETURN LResult 
    END SibDirectoryPath 

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
    ; PutTextish ( LWrT , T0 ) 
    ; PutTextish ( LWrT , T1 ) 
    ; PutTextish ( LWrT , T2 ) 
    ; PutTextish ( LWrT , T3 ) 
    ; PutTextish ( LWrT , T4 ) 
    ; PutTextish ( LWrT , T5 ) 
    ; PutTextish ( LWrT , T6 ) 
    ; PutTextish ( LWrT , T7 ) 
    ; PutTextish ( LWrT , T8 )
    ; LMsg := TextWr . ToText ( LWrT )
    ; IF LMsg = NIL THEN LMsg := "" END (*IF*) (* Can this happen? *) 
    ; RETURN LMsg 
    END CatStrings  

(*EXPORTED*) 
; PROCEDURE CatArrT
    ( READONLY Arr : ARRAY OF REFANY ; T0 : TEXT := NIL ) : TEXT
  (* T0 and each Arr [ I ] can be TEXT, Atom.T, or AtomList.T. *)
  (* T0 will appear *left* of elements of Arr.  
     Although this signature order seems very peculiar, it allows message
     procedures to prepend a tag such as "error", in color, while
     allowing the rest to be passed multiple levels as one parameter.
  *) 
    
  = VAR LWrT : TextWr . T
  ; VAR LMsg : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; PutTextish ( LWrT , T0 ) 
    ; FOR RI := 0 TO LAST ( Arr )
      DO PutTextish ( LWrT , Arr [ RI ] )
      END (*FOR*) 
    ; LMsg := TextWr . ToText ( LWrT )
    ; IF LMsg = NIL THEN LMsg := "" END (*IF*) (* Can this happen? *) 
    ; RETURN LMsg 
    END CatArrT  

(*EXPORTED*)
; PROCEDURE PutPosImage ( WrT : Wr . T ; Position : FM3Base . tPosition )

  = BEGIN
      Wr.PutChar ( WrT , '(' )
    ; Wr.PutText ( WrT , Fmt . Int ( Position.Line ) )
    ; Wr.PutChar ( WrT , ',' )
    ; Wr.PutText ( WrT , Fmt . Int ( Position.Column ) )
    ; Wr.PutChar ( WrT , ')' )
    END PutPosImage

(*EXPORTED*) 
; PROCEDURE PutTextish ( WrT : Wr . T ; Textish : REFANY )
  (* Textish can be NIL, TEXT, Atom.T, AtomList.T, REF ARRAY OF CHAR,
     or REF ARRAY OF REFANY.
  *) 

  = BEGIN
      TYPECASE Textish OF
      | NULL =>
      | TEXT ( TTxt )
        => Wr . PutText ( WrT , TTxt )
      | Atom . T ( TAtom )
        => Wr . PutText ( WrT , Atom . ToText ( TAtom ) )
      | AtomList . T ( TAtom )
        => Wr . PutText ( WrT , AtomListToText ( TAtom ) )
      | REF ARRAY OF CHAR ( TOAChars )
        => FOR RI := 0 TO LAST ( TOAChars ^ )
           DO Wr . PutChar ( WrT , TOAChars ^ [ RI ] ) 
           END (*FOR*)
      | REF ARRAY OF REFANY ( TNestedArr )
        => PutTextishArr ( WrT , TNestedArr ^ ) 
      ELSE
      END (*TYPECASE*)
    END PutTextish

(*EXPORTED*) 
; PROCEDURE PutTextishArr ( WrT : Wr . T ; READONLY Arr : ARRAY OF REFANY )

  = BEGIN 
      IF NUMBER ( Arr ) <= 0 THEN RETURN END (*IF*)
    ; FOR RI := FIRST ( Arr ) TO LAST ( Arr )  
      DO PutTextish ( WrT , Arr [ RI ] ) 
      END (*FOR*)
    END PutTextishArr 

(*EXPORTED*) 
; PROCEDURE FileKindImage ( Kind : FM3SharedGlobals . FileKindTyp ) : TEXT

  = BEGIN
      RETURN "\'" & Text . FromChar ( Kind ) & "\'" 
    END FileKindImage

(*EXPORTED*) 
; PROCEDURE FilePrefixT
    ( Kind : FM3SharedGlobals . FileKindTyp
    ; Version : FM3SharedGlobals . FileVersionTyp
    )
  : TEXT 

  = VAR LTextWrT : TextWr . T
  ; VAR LResult : TEXT
  
  ; BEGIN
      LTextWrT := TextWr . New ( ) 
    ; Wr . PutString ( LTextWrT , FM3SharedGlobals . FM3FileTagA )
    ; Wr . PutChar ( LTextWrT , Kind )
    ; Wr . PutChar ( LTextWrT , Version )
    ; Wr . PutString ( LTextWrT , FM3SharedGlobals . FM3MagicA )
    ; LResult := TextWr . ToText ( LTextWrT ) 
    ; RETURN LResult 
    END FilePrefixT

; PROCEDURE FilePrefixA
    ( Kind : FM3SharedGlobals . FileKindTyp
    ; Version : FM3SharedGlobals . FileVersionTyp
    )
  : ARRAY [ 0 .. 7 ] OF CHAR  

  = VAR LResultA : ARRAY [ 0 .. 7 ] OF CHAR
  
  ; BEGIN
      SUBARRAY ( LResultA , 0 , 3 ) := FM3SharedGlobals . FM3FileTagA
    ; LResultA [ 3 ] := Kind
    ; LResultA [ 4 ] := Version
    ; SUBARRAY ( LResultA , 5 , 3 ) := FM3SharedGlobals . FM3MagicA
    ; RETURN LResultA 
    END FilePrefixA 

; PROCEDURE FilePrefixB
    ( Kind : FM3SharedGlobals . FileKindTyp
    ; Version : FM3SharedGlobals . FileVersionTyp
    )
  : ARRAY [ 0 .. 7 ] OF File . Byte 

  = VAR LResult : ARRAY [ 0 .. 7 ] OF File . Byte
  
  ; BEGIN
      LResult [ 0 ] := ORD ( FM3SharedGlobals . FM3FileTagA [ 0 ] ) 
    ; LResult [ 1 ] := ORD ( FM3SharedGlobals . FM3FileTagA [ 1 ] ) 
    ; LResult [ 2 ] := ORD ( FM3SharedGlobals . FM3FileTagA [ 2 ] ) 
    ; LResult [ 3 ] := ORD ( Kind ) 
    ; LResult [ 4 ] := ORD ( Version ) 
    ; LResult [ 5 ] := ORD ( FM3SharedGlobals . FM3MagicA [ 0 ] )
    ; LResult [ 6 ] := ORD ( FM3SharedGlobals . FM3MagicA [ 1 ] )
    ; LResult [ 7 ] := ORD ( FM3SharedGlobals . FM3MagicA [ 2 ] )
    ; RETURN LResult 
    END FilePrefixB 

(*EXPORTED*) 
; PROCEDURE ReadPrefix
    ( RdT : Rd . T
    ; VAR Kind : FM3SharedGlobals . FileKindTyp
    ; VAR Version : FM3SharedGlobals . FileVersionTyp
    ; VAR IsOK : BOOLEAN
    )

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
        ; IF LChar # FM3SharedGlobals . FM3FileTagA [ LCharPos ]
          THEN RETURN
          END (*IF*) 
        ; INC ( LCharPos ) 
        ; IF LCharPos >= NUMBER ( FM3SharedGlobals . FM3FileTagA )
          THEN EXIT
          END (*IF*) 
        END (*LOOP*)

      (* Get Kind byte: *) 
      ; IF Rd . EOF ( RdT ) THEN RETURN END (*IF*) 
      ; Kind := Rd . GetChar ( RdT )

      (* Get Version byte: *) 
      ; IF Rd . EOF ( RdT ) THEN RETURN END (*IF*) 
      ; Version := Rd . GetChar ( RdT )

      (* Verify magic number: *) 
      ; LCharPos := 0
      ; LOOP
          IF Rd . EOF ( RdT ) THEN RETURN END (*IF*) 
        ; LChar := Rd . GetChar ( RdT )
        ; IF LChar # FM3SharedGlobals . FM3MagicA [ LCharPos ]
          THEN RETURN
          END (*IF*) 
        ; INC ( LCharPos ) 
        ; IF LCharPos >= NUMBER ( FM3SharedGlobals . FM3MagicA )
          THEN EXIT
          END (*IF*)   
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
    ( DirName , FileName , Note1 , Note2 : TEXT := "" )
  : Rd . T
  RAISES { FatalError } 

  = VAR LFullFileName : TEXT
  ; VAR LRdT : Rd . T
  
  ; BEGIN
      LFullFileName := Libm3Pathname . Join ( DirName , FileName ) 
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
    ( FileName : TEXT := ""
    ; ExpectedFileKind : FM3SharedGlobals . FileKindTyp
    ) 
  : Rd . T
  RAISES { FatalError , Thread . Alerted } 

  = VAR LRelFileName : TEXT
  ; VAR LAbsFileName : TEXT
  ; VAR LRdT : Rd . T
  ; VAR LFileKind : FM3SharedGlobals . FileKindTyp 
  ; VAR LFileVersion : FM3SharedGlobals . FileVersionTyp 
  ; VAR LIsOK : BOOLEAN 
  
  ; BEGIN
      LRdT := OpenRd ( ResourceDirName , FileName )
    ; ReadPrefix
        ( LRdT , (*OUT*) LFileKind , (*OUT*) LFileVersion , (*OUT*) LIsOK )  
    ; IF NOT LIsOK OR LFileKind # ExpectedFileKind
      THEN (* Wrong kind. *) 
        LRelFileName := Libm3Pathname . Join ( ResourceDirName , FileName )
      ; LAbsFileName := AbsFileName ( LRelFileName )
      ; IF NOT LIsOK
        THEN
          RaiseFatal
            ( CatStrings 
                ( "Resource file "
                , LAbsFileName 
                , " is not an FM3 internal file." 
                )
            )
        ELSE 
          RaiseFatal
            ( CatStrings 
                ( "Resource file " 
                , LAbsFileName 
                , " has wrong kind: " 
                , FileKindImage ( LFileKind )  
                , ", expecting " 
                , FileKindImage ( ExpectedFileKind )  
                )
            )
        END (*IF*) 
      END (*IF*)
    ; RETURN LRdT 
    END OpenResourceRd

(*EXPORTED*) 
; PROCEDURE ReadPickle
    ( FileName : TEXT ; ExpectedKind : FM3SharedGlobals . FileKindTyp )
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
; PROCEDURE ReadFsm ( FileName : TEXT ; Kind : FM3SharedGlobals . FileKindTyp )
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
    ( FileName : TEXT (* Simple name. *)
    ; Kind : FM3SharedGlobals . FileKindTyp
    ; VAR Temp : IntSets . T 
    ; VAR Patch : IntSets . T 
    ; VAR Arg1 : IntSets . T 
    ; VAR Arg2 : IntSets . T 
    ; VAR Arg3 : IntSets . T 
    ; VAR Arg4 : IntSets . T 
    ; VAR Arg5 : IntSets . T 
    ; VAR Arg6 : IntSets . T 
    )
  RAISES { FatalError , Thread . Alerted }

(* TODO: Aside from LoadSets below, only DumpWork calls ReadSets.
         Dump and DumpWork will likely be eliminated, so if so,
         merge ReadSets into LoadSets.
*) 

  = VAR LRdT : Rd . T

  ; PROCEDURE RsReadIntSet ( RdT : Rd . T ) : IntSets . T
    RAISES { FatalError } 

    = VAR LRef : REFANY

    ; BEGIN
        LRef := Pickle2 . Read ( RdT ) 
      ; TYPECASE LRef OF
        | IntSets . T ( TResult ) (* NIL is valid here. *) 
        => RETURN TResult  
        ELSE
          RaiseFatal
            ( CatStrings
                ( "In RsReadIntSet, not an IntSets pickle file \""
                , FileName
                , "\"" )
                )
        END (*TYPECASE*)
      END RsReadIntSet 

  ; BEGIN
      LRdT :=  OpenResourceRd
        ( FileName , FM3SharedGlobals . FM3FileKindTokSetsPkl )
    ; Temp := RsReadIntSet ( LRdT ) 
    ; Patch := RsReadIntSet ( LRdT ) 
    ; Arg1 := RsReadIntSet ( LRdT ) 
    ; Arg2 := RsReadIntSet ( LRdT ) 
    ; Arg3 := RsReadIntSet ( LRdT ) 
    ; Arg4 := RsReadIntSet ( LRdT ) 
    ; Arg5 := RsReadIntSet ( LRdT ) 
    ; Arg6 := RsReadIntSet ( LRdT ) 
    END ReadSets 

(*EXPORTED*) 
; PROCEDURE LoadSets ( )
  RAISES { FatalError , Thread . Alerted } 

  = VAR LIntFilePrefix : TEXT 
  ; VAR LSetsName : TEXT 

  ; BEGIN
      IF NOT FM3SharedGlobals . GSetsLoaded
      THEN
        LIntFilePrefix := "FM3IntToks"
      ; LSetsName := FM3SharedGlobals . GIntFilePrefix & "Sets.pkl"
      ; ReadSets
          ( LSetsName (* Simple name.  *) 
          , FM3SharedGlobals . FM3FileKindTokSetsPkl
          , FM3SharedGlobals . GTokSetTemp
          , FM3SharedGlobals . GTokSetPatch
          , FM3SharedGlobals . GTokSetGE1Arg
          , FM3SharedGlobals . GTokSetGE2Args
          , FM3SharedGlobals . GTokSetGE3Args
          , FM3SharedGlobals . GTokSetGE4Args
          , FM3SharedGlobals . GTokSetGE5Args
          , FM3SharedGlobals . GTokSetGE6Args
          ) 
      ; FM3SharedGlobals . GSetsLoaded := TRUE 
      END (*IF*) 
    END LoadSets   

(*EXPORTED*) 
; PROCEDURE IntHash ( Val : INTEGER ) : FM3Base . HashTyp

  = BEGIN
      RETURN VAL ( Val * 13 , FM3Base . HashTyp ) 
    END IntHash
    
; CONST FixedLength = 85 
; CONST FixedBlanks = ARRAY [ 0 .. FixedLength - 1 ] OF CHAR { ' ' , .. } 

; VAR FixedText : TEXT 

(* EXPORTED: *) 
; PROCEDURE Blanks ( Length : INTEGER ) : TEXT 
  (* A TEXT of length Length, all blanks. *) 

  = VAR LRemaining : INTEGER  
  ; VAR LResult : TEXT 

(* TODO: Make this keep an array or text around, expanding when needed,
         and then available for the next time.
*) 
(* TODO: I think this is duplicated somewhere. *) 
  ; BEGIN (* Blanks *)
      IF Length <= 0 THEN RETURN "" END (* IF *) 
    ; IF Length <= FixedLength 
      THEN (* Fast path: *) 
        RETURN Text . FromChars ( SUBARRAY ( FixedBlanks , 0 , Length ) ) 
      ELSE 
        LRemaining := Length - FixedLength 
      ; LResult := FixedText 
      ; WHILE LRemaining >= FixedLength 
        DO LResult := LResult & FixedText 
        ; DEC ( LRemaining , FixedLength ) 
        END (* WHILE *) 
      ; RETURN 
          LResult 
          & Text . FromChars ( SUBARRAY ( FixedBlanks , 0 , LRemaining ) ) 
      END (* IF *) 
    END Blanks 

(* EXPORTED: *) 
; PROCEDURE DefaultResourceDirName ( ) : TEXT 

  = VAR LExeName : TEXT 
  ; VAR LResult : TEXT 

  ; BEGIN (*DefaultResourceDirName*)
      IF Params . Count > 0 
      THEN  
        LExeName := Params . Get ( 0 ) 
      ; LResult := SibDirectoryPath ( LExeName , "lib" ) 
      ELSE LResult := "../lib" (* Not very likely to work, but what else? *) 
      END (*IF*) 
    ; RETURN LResult 
    END DefaultResourceDirName

; PROCEDURE DeleteFile ( FileName : TEXT )
(* PRE noncondition: File does not necessarily exist. *) 

  = BEGIN
      TRY FS . DeleteFile ( FileName )
      EXCEPT OSError . E => (* It didn't exist. *) 
      END (*EXCEPT*)
    END DeleteFile

; VAR Dummy : CHAR 

; PROCEDURE IntSetsElemImage ( Elem : INTEGER ) : TEXT
 (* Why do I have to wrap Fmt.Int for this? *)

(*
; PROCEDURE IntSetsElemImage ( Elem : IntSets . ValidElemT ) : TEXT
(* TODO: Change to an OrdSets  ^ that expects OrdSets.ElemT here. *)
*)
 = BEGIN
      RETURN Fmt . Int ( Elem ) 
    END IntSetsElemImage

(*EXPORTED.*)
; PROCEDURE IntSetsImage
    ( Set : IntSets . T ; LinePrefix : TEXT ; MaxLine : CARDINAL ) : TEXT 

  = VAR LResult : TEXT 

  ; BEGIN (*IntSetsImage*)
      LResult
        := IntSets . Image ( Set , IntSetsElemImage , LinePrefix , MaxLine )
    ; RETURN LResult 
    END IntSetsImage

; BEGIN 
    ResourceDirName := DefaultResourceDirName ( )  
  END FM3SharedUtils 
.
