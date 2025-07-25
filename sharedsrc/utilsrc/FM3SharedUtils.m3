
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Utility things that may be used by multiple different main programs. *) 

MODULE FM3SharedUtils

; IMPORT Atom 
; IMPORT AtomList
; IMPORT Date 
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
; IMPORT Time 
; IMPORT Wr

; IMPORT IntSets 
; IMPORT Layout

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

(*EXPORTED:*)
; PROCEDURE CompareAToT
    ( READONLY Left : ARRAY OF CHAR ; Right : TEXT ) : FM3Base . CompareTyp

  = VAR LLeftTxt , LRightTxt : TEXT 

  ; BEGIN
      LLeftTxt := Text . FromChars ( Left )
    ; IF Right = NIL
      THEN LRightTxt := ""
      ELSE LRightTxt := Right
      END (*IF*) 
    ; RETURN Text . Compare ( LLeftTxt , LRightTxt ) 
    END CompareAToT 

(* EXPORTED: *) 
; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT 

  = VAR LResult : TEXT 

  ; BEGIN (* AbsFileName *) 
      IF Libm3Pathname . Absolute ( Name )
      (* ^ Is this really necessary? *) 
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
      => StandaloneFatalError
           ( CatArrT 
               ( ARRAY OF REFANY
                   { " Unable to get absolute path of executable: \""
                   , FileName 
                   , "\""
                   , Wr . EOL
                   , "    OSError.E("
                   , EMsg
                   , ")."
                   }
               )
           ) 
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
; PROCEDURE PutPosImage ( WrT : Wr . T ; Position : FM3Base . tPosition )

  = BEGIN
      Wr.PutChar ( WrT , '[' )
    ; Wr.PutText ( WrT , Fmt . Int ( Position.Line ) )
    ; Wr.PutChar ( WrT , ',' )
    ; Wr.PutText ( WrT , Fmt . Int ( Position.Column ) )
    ; Wr.PutChar ( WrT , ']' )
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
  (* Each element of Arr can be anything handled by PutTextish. *) 

  = BEGIN 
      IF NUMBER ( Arr ) <= 0 THEN RETURN END (*IF*)
    ; FOR RI := FIRST ( Arr ) TO LAST ( Arr )  
      DO PutTextish ( WrT , Arr [ RI ] ) 
      END (*FOR*)
    END PutTextishArr 

(*EXPORTED*) 
; PROCEDURE CatStrings
    ( T0 , T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : REFANY := NIL ) : TEXT
  (* Each Tn can be anything handled by PutTextish. *) 
    
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
  (* T0 and each Arr [ I ] can be anything handled by PutTextish. *)
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
; PROCEDURE FileKindImage ( Kind : FM3SharedGlobals . FileKindTyp ) : TEXT

  = BEGIN
      RETURN "\'" & Text . FromChar ( Kind ) & "\'" 
    END FileKindImage

(*EXPORTED*) 
; PROCEDURE FilePrefixT
    ( Kind : FM3SharedGlobals . FileKindTyp
    ; Version : FM3SharedGlobals . FileVersionTyp
        := FM3SharedGlobals . FM3FileVersion0 
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
        := FM3SharedGlobals . FM3FileVersion0
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
        := FM3SharedGlobals . FM3FileVersion0 
    )
  : PrefixBTyp 

  = VAR LResult : PrefixBTyp 
  
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
; PROCEDURE ReadPrefixR
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
    END ReadPrefixR

(*EXPORTED*) 
; PROCEDURE ParsePrefixB
    ( PrefixB : PrefixBTyp 
    ; VAR Kind : FM3SharedGlobals . FileKindTyp
    ; VAR Version : FM3SharedGlobals . FileVersionTyp 
    ; VAR IsOK : BOOLEAN
    )

  = VAR LPrefixPos : CARDINAL
  ; VAR LCheckPos : CARDINAL 
  ; VAR LByte : File . Byte 

  ; <*FATAL Rd . EndOfFile *>
    BEGIN
      IsOK := FALSE
    ; LPrefixPos := 0
    
    (* Verify tag: *)
    ; LCheckPos := 0 
    ; LOOP
        LByte := PrefixB [ LPrefixPos ]
      ; IF LByte # FM3SharedGlobals . FM3FileTagB [ LCheckPos ]
        THEN RETURN
        END (*IF*) 
      ; INC ( LPrefixPos ) 
      ; INC ( LCheckPos ) 
      ; IF LCheckPos >= NUMBER ( FM3SharedGlobals . FM3FileTagA )
        THEN EXIT
        END (*IF*) 
      END (*LOOP*)

    (* Get Kind byte: *) 
    ; Kind := VAL ( PrefixB [ LPrefixPos ] , CHAR ) 
    ; INC ( LPrefixPos )
    
   (* Get Version byte: *) 
    ; Version := VAL ( PrefixB [ LPrefixPos ] , CHAR )
    ; INC ( LPrefixPos ) 

    (* Verify magic number: *) 
    ; LCheckPos := 0
    ; LOOP
        LByte := PrefixB [ LPrefixPos ]
      ; IF LByte # FM3SharedGlobals . FM3MagicB [ LCheckPos ]
        THEN RETURN
        END (*IF*) 
      ; INC ( LPrefixPos ) 
      ; INC ( LCheckPos ) 
      ; IF LCheckPos >= NUMBER ( FM3SharedGlobals . FM3MagicA )
        THEN EXIT
        END (*IF*)   
      END (*LOOP*)

    ; IsOK := TRUE 
    ; RETURN
    END ParsePrefixB

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

(*EXPORTED.*)
; PROCEDURE CheckPrefix
    ( IsOK : BOOLEAN
    ; ActualFileKind , ExpectedFileKind : FM3SharedGlobals . FileKindTyp
    ; ActualFileVersion , ExpectedFileVersion : FM3SharedGlobals . FileVersionTyp
    ; FileTag , FileName : TEXT 
    )
  RAISES { FatalError } 

  = BEGIN (*CheckPrefix*)
      IF NOT IsOK
      THEN
        RaiseFatal
          ( CatStrings ( FileTag , FileName , " is not an FM3 internal file." ) )
      ELSIF ActualFileKind # ExpectedFileKind 
      THEN 
        RaiseFatal
          ( CatStrings 
              ( FileTag
              , FileName 
              , " has wrong kind: " 
              , FileKindImage ( ActualFileKind )  
              , ", expecting " 
              , FileKindImage ( ExpectedFileKind )  
              )
          )
      ELSIF ActualFileVersion # ExpectedFileVersion 
      THEN 
        RaiseFatal
          ( CatStrings 
              ( FileTag
              , FileName 
              , " has wrong version: " 
              , FileKindImage ( ActualFileVersion )  
              , ", expecting " 
              , FileKindImage ( ExpectedFileVersion )  
              )
          )
      END (*IF*)
    END CheckPrefix
      
(*EXPORTED*) 
; PROCEDURE OpenResourceRd
    ( FileName : TEXT := ""
    ; ExpectedFileKind : FM3SharedGlobals . FileKindTyp
    ; ExpectedFileVersion : FM3SharedGlobals . FileVersionTyp
        := FM3SharedGlobals . FM3FileVersion0
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
    ; ReadPrefixR
        ( LRdT , (*OUT*) LFileKind , (*OUT*) LFileVersion , (*OUT*) LIsOK )  
    ; LRelFileName := Libm3Pathname . Join ( ResourceDirName , FileName )
    ; LAbsFileName := AbsFileName ( LRelFileName )
    ; CheckPrefix
        ( LIsOK
        , LFileKind
        , ExpectedFileKind
        , LFileVersion
        , ExpectedFileVersion
        , "Resource file"
        , LAbsFileName
        )
      ; RETURN LRdT 
    END OpenResourceRd

(*EXPORTED*) 
; PROCEDURE ReadPickle
    ( FileName : TEXT
    ; ExpectedFileKind : FM3SharedGlobals . FileKindTyp
    ; ExpectedFileVersion : FM3SharedGlobals . FileVersionTyp
        := FM3SharedGlobals . FM3FileVersion0
    )
  : REFANY
  RAISES { FatalError , Thread . Alerted } 

  = VAR LRdT : Rd . T 
  ; VAR LResult : REFANY 

  ; BEGIN
      LRdT
        := OpenResourceRd ( FileName , ExpectedFileKind , ExpectedFileVersion )
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
; PROCEDURE ReadFsm
    ( FileName : TEXT
    ; ExpectedFileKind : FM3SharedGlobals . FileKindTyp
    ; ExpectedFileVersion : FM3SharedGlobals . FileVersionTyp 
        := FM3SharedGlobals . FM3FileVersion0
    )
  : FM3LexTable . T
  RAISES { Thread . Alerted } 

  = VAR LRef : REFANY 

  ; BEGIN
      LRef := ReadPickle ( FileName , ExpectedFileKind , ExpectedFileVersion )
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

(*EXPORTED.*)
; PROCEDURE CurrentYear ( ) : TEXT 

  = VAR LTimeT : Time.T
  ; VAR LDateT : Date . T
  ; VAR LResult : TEXT 

  ; BEGIN (*CurrentYear*)
      LTimeT := Time . Now ( )
    ; LDateT := Date . FromTime ( LTimeT )
    ; LResult := Fmt . Int ( LDateT . year ) 
    ; RETURN LResult 
    END CurrentYear

(*EXPORTED.*)
; PROCEDURE EmitCopyright ( WrT : Wr . T )

  = BEGIN
      Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText
        ( WrT 
        , "(* -----------------------------------------------------------------------1- *)"
        )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText
        ( WrT 
        , "(* This file is part of the FM3 Modula-3 compiler.                           *)"
        )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText
        ( WrT 
        , "(* Copyright "
        )
    ; Wr . PutText
        ( WrT , CurrentYear ( ) )
    ; Wr . PutText
        ( WrT , "        Rodney M. Bates.                                    *)" )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText
        ( WrT 
        , "(* rodney.m.bates@acm.org                                                    *)"
        )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText
        ( WrT 
        , "(* Licensed under the MIT License.                                           *)"
        )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText
        ( WrT 
        , "(* -----------------------------------------------------------------------2- *)"
        )
    ; Wr . PutText ( WrT , Wr . EOL )
    ; Wr . PutText ( WrT , Wr . EOL )
    END EmitCopyright

(*EXPORTED.*)
; PROCEDURE ArgListAsText ( ) : TEXT

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , Params . Get ( 0 ) ) 
    ; FOR RArgNo := 1 TO Params . Count - 1
      DO
        Wr . PutChar ( LWrT , ' ' )
      ; Wr . PutText ( LWrT , Params . Get ( RArgNo ) ) 
      END (*FOR*) 
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END ArgListAsText 

; BEGIN 
    ResourceDirName := DefaultResourceDirName ( )  
  END FM3SharedUtils 
.
