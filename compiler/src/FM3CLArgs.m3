       
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3CLArgs

; IMPORT Atom 
; IMPORT AtomList 
; IMPORT FileWr
; IMPORT FS 
; IMPORT OSError
; IMPORT Params
; IMPORT Pathname 
; IMPORT Rd 
; IMPORT Stdio
; IMPORT Text 
; IMPORT TextWr 
; IMPORT Wr

; IMPORT IntSets 

; IMPORT FM3Atom_Text 
; IMPORT FM3Base
; IMPORT FM3CLOptions 
; IMPORT FM3CLToks 
; IMPORT FM3CLToks AS Clt
; IMPORT FM3Files 
; IMPORT FM3Globals
; IMPORT FM3LexTable 
; IMPORT FM3Messages 
; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3TextColors
; IMPORT FM3Utils 
; IMPORT FM3Version  

; EXCEPTION TerminateCL ( TEXT (* Message. *) )

; PROCEDURE PrependTextToList ( VAR List : AtomList . T ; Txt : TEXT )

  = BEGIN (*PrependTextToList*) 
      List := AtomList . Cons ( Atom . FromText ( Txt ) , List )
    END PrependTextToList 

; PROCEDURE AssignOptionSetElem
    ( VAR Set : FM3CLOptions . OptionTokSetTyp
    ; Elem : FM3CLOptions . OptionTokTyp
    ; Value : BOOLEAN
    )

  = BEGIN (*AssignOptionSetElem*)
      IF Value
      THEN FM3CLOptions . InclOptionTok ( (*IN OUT*) Set , Elem ) 
      ELSE FM3CLOptions . ExclOptionTok ( (*IN OUT*) Set , Elem ) 
      END (*IF*) 
    END AssignOptionSetElem  

; PROCEDURE AlterOptionSet
    ( VAR Set : FM3CLOptions . OptionTokSetTyp
    ; Changes : FM3CLOptions . OptionTokSetTyp
    ; Include : BOOLEAN (* Otherwise, exclude. *) 
    )

  = BEGIN (*AlterOptionSet*)
      IF Include
      THEN FM3CLOptions . OptionTokSetUnion ( (*IN OUT*) Set , Changes ) 
      ELSE FM3CLOptions . OptionTokSetDiff ( (*IN OUT*) Set , Changes ) 
      END (*IF*) 
    END AlterOptionSet 

; PROCEDURE AlterPassNos
    ( VAR Set : FM3CLOptions . PassNoSetTyp
    ; Changes : FM3CLOptions . PassNoSetTyp
    ; Include : BOOLEAN (* Otherwise, exclude. *) 
    )

  = BEGIN (*AlterPassNos*)
      IF Include
      THEN FM3CLOptions . PassNoSetUnion ( (*IN OUT*) Set , Changes ) 
      ELSE FM3CLOptions . PassNoSetDiff ( (*IN OUT*) Set , Changes ) 
      END (*IF*) 
    END AlterPassNos 

; PROCEDURE SingleDigitParam ( Param : TEXT ) : INTEGER
  RAISES { TerminateCL } 

  = VAR LResult : INTEGER

  ; BEGIN (*SingleDigitParam*)
      IF Text . Length ( Param ) # 1
      THEN RAISE TerminateCL ( "Parameter must be a single digit." )
      END (*IF*)
    ; LResult := ORD ( Text . GetChar ( Param , 0 ) ) - ORD ( '0' )
    ; RETURN LResult  
    END SingleDigitParam

; TYPE DirListElemTyp
       = RECORD Atom : INTEGER ; Link : REF DirListElemTyp END 
; VAR DirsList : REF DirListElemTyp := NIL 
; VAR DirsAtomDict : FM3Atom_Text . T
; VAR DirsSet := IntSets . Empty ( )

; PROCEDURE DirElem ( DirPath : TEXT ; No : BOOLEAN )
  (* Call once for each directory argument. *) 

  = VAR LAtom : INTEGER 

  ; BEGIN
      LAtom
        := FM3Atom_Text . MakeAtom
             ( DirsAtomDict
             , DirPath
             , Hash := FM3Utils . HashOfText ( DirPath )
             )
    ; IF No
      THEN
        DirsSet := IntSets . Exclude ( DirsSet , LAtom ) 
      ELSE
        DirsList
          := NEW ( REF DirListElemTyp , Atom := LAtom , Link := DirsList )  
      ; DirsSet := IntSets . Include ( DirsSet , LAtom ) 
      END (*IF*) 
    END DirElem

; PROCEDURE DerivedDirs ( ) : REF ARRAY OF TEXT
  (* Call once for the whole list. *) 

  = VAR LArrRef : REF ARRAY OF TEXT 
  ; VAR LArrRefLong : REF ARRAY OF TEXT
  ; VAR LDirName : TEXT 
  ; VAR LCt : INTEGER
  ; VAR LNextIn : INTEGER
  ; VAR LDir : REF DirListElemTyp
  ; VAR LEmptyCt : INTEGER
  ; VAR LIsGoodDir : BOOLEAN 

  ; BEGIN
      LCt := IntSets . Card ( DirsSet )
    ; LArrRef := NEW ( REF ARRAY OF TEXT , LCt )
    ; LNextIn := 0
    ; LDir := DirsList 
    ; WHILE LDir # NIL
      DO IF IntSets . IsElement ( LDir ^ . Atom , DirsSet )
        THEN
          <* ASSERT FM3Atom_Text . Key
               ( DirsAtomDict , LDir ^ . Atom , (*OUT*) LDirName )
          *>
          LIsGoodDir := TRUE 
        ; TRY
            IF FS . Status ( LDirName ) . type # FS . DirectoryFileType
            THEN LIsGoodDir := FALSE
            END (*IF*) 
          EXCEPT OSError . E
          => LIsGoodDir := FALSE  
          END (*EXCEPT*) 
        ; IF LIsGoodDir
          THEN 
            LArrRef ^ [ LNextIn ] := LDirName
          ; INC ( LNextIn )
          END (*IF*) 
        ; DirsSet := IntSets . Exclude ( DirsSet , LDir ^ . Atom ) 
        END (*IF*)
      ; LDir := LDir ^ . Link 
      END (*WHILE*)
    ; <* ASSERT IntSets . IsEmpty ( DirsSet ) *>
      LEmptyCt := LCt - LNextIn 
    ; IF LEmptyCt > 0 
      THEN
        LArrRefLong := LArrRef
      ; LArrRef := NEW ( REF ARRAY OF TEXT , LNextIn )
      ; LArrRef ^ := SUBARRAY ( LArrRefLong ^ , 0 , LNextIn )
      END (*IF*) 
    ; RETURN LArrRef 
    END DerivedDirs

; PROCEDURE DerivedDirsMsg ( List : REF ARRAY OF TEXT ) : TEXT  

  = VAR LWrT : TextWr . T
  ; VAR LResult : TEXT 

  ; BEGIN
      LWrT := TextWr . New ( ) 
    ; IF List # NIL
      THEN 
        FOR RI := FIRST ( List ^ ) TO LAST ( List ^ )
        DO
          Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
        ; Wr . PutText ( LWrT , List ^ [ RI ] ) 
        END (*FOR*)
      END (*IF*) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END DerivedDirsMsg 

; PROCEDURE ParseArgs ( )
  RAISES { FM3SharedUtils . Terminate } 

  = VAR PaArgText : TEXT
  ; VAR PaPrevArgText : TEXT
  ; VAR PaArgCt : INTEGER
  ; VAR PaArgNo : INTEGER
  ; VAR PaArgLen : INTEGER
  ; VAR PaArgSs : INTEGER 
  ; VAR PaHasEqualSign : BOOLEAN

  ; PROCEDURE PaFetchArg ( MinLen : INTEGER := 1 ) RAISES { TerminateCL } 
    (* PRE: PaArgNo < Params . Count *)
    
    = BEGIN
        PaArgText := Params . Get ( PaArgNo )
      ; IF PaArgText = NIL (* Probably not possible. *)
        THEN PaArgText := ""
        END (*IF*)
      ; PaArgLen := Text . Length ( PaArgText )
      ; PaArgSs := 0 
      ; IF PaArgLen < MinLen
        THEN RAISE TerminateCL ( "Argument is too short.")
        END (*IF*)
      END PaFetchArg

  ; PROCEDURE PaFindParam ( ) RAISES { TerminateCL }
    (* PRE: A parameter is expected.  *) 
    (* POST: There is a param and it is in PaArgText, starting at PaArgSs. *) 

    = BEGIN
        IF PaArgSs < PaArgLen 
        THEN (* We're already at the beginning of a parameter inside
                the current arg in PaArgText. *) 
          PaPrevArgText := NIL  
        ELSE
          INC ( PaArgNo )
        ; IF PaArgNo >= PaArgCt
          THEN
            RAISE TerminateCL ( "Argument requires a parameter."  ) 
          END (*IF*) 
        ; PaPrevArgText := PaArgText 
        ; PaFetchArg ( MinLen := 1 )
        (* This entire arg is the parameter. *) 
        END (*IF*)
      END PaFindParam 

  ; PROCEDURE PaCheckValidSrcFile ( FileName : TEXT ) RAISES { TerminateCL }

    = BEGIN
        IF FM3Files . FileSuffix ( FileName ) = FM3Files . SuffixTyp . SfxNull
        THEN RAISE TerminateCL ( "Invalid source file name." )
        END (*IF*) 
      END PaCheckValidSrcFile 

  ; PROCEDURE PaNoNo ( No : BOOLEAN ) RAISES { TerminateCL } 

    = BEGIN (*PaNoNo*)
        IF No
        THEN
          PaArgSs := 2 
        ; RAISE
            TerminateCL
              ( "This argument does not support the \"no-\" prefix." )
        END (*IF*) 
      END PaNoNo
      
  ; PROCEDURE PaNoEqualSign ( ) RAISES { TerminateCL } 
  
    = BEGIN
        IF PaHasEqualSign 
        THEN
          RAISE TerminateCL ( "This argument does not take a parameter. " ) 
        END (*IF*) 
      END PaNoEqualSign 

  ; CONST SingleDigits = SET OF CHAR { '1' .. '9' }

  ; PROCEDURE PaPassNoSet
      ( VAR Set : FM3CLOptions . PassNoSetTyp
      ; Include : BOOLEAN (* Otherwise, exclude. *) 
      )
    RAISES { TerminateCL } 
  
    = VAR LPassNo : INTEGER
    ; VAR LChar : CHAR

    ; BEGIN (*PaPassNoSet*)
        WHILE PaArgSs < PaArgLen
        DO
          LChar := Text . GetChar ( PaArgText , PaArgSs )
        ; IF NOT LChar IN SingleDigits 
          THEN RAISE TerminateCL  ( "Pass number must be a digit." ) 
          END (*IF*)
        ; LPassNo := ORD ( LChar ) - ORD ( '0' )
        ; IF NOT LPassNo IN FM3CLOptions . PassNoSetValid
          THEN RAISE TerminateCL ( "Invalid pass number." ) 
          END (*IF*) 
        ; IF Include
          THEN FM3CLOptions . InclPassNo ( Set , LPassNo )
          ELSE FM3CLOptions . ExclPassNo ( Set , LPassNo )
          END (*IF*) 
        ; INC ( PaArgSs ) 
        END (*WHILE*)
      END PaPassNoSet

  ; PROCEDURE PaTwoHyphenArg ( ) RAISES { TerminateCL } 
    (* PRE: Arg starts with two hyphens, PaArgSs = 2. *) 

    = VAR LParam : TEXT
    ; VAR LLexState : FM3LexTable . StateNoTyp 
    ; VAR LLexValue : FM3LexTable . ValueTyp
    ; VAR LChar : CHAR 
    ; VAR LNo : BOOLEAN

    ; BEGIN
        IF PaArgLen >= 5
           AND Text . Equal ( Text . Sub ( PaArgText , 2 , 3 ) , "no-" )
        THEN
          LNo := TRUE (* Confusing? *)
        ; INC ( PaArgSs , 3 )  
        ELSE LNo := FALSE 
        END (*IF*)
      ; IF PaArgSs >= PaArgLen
        THEN RAISE TerminateCL  ( "Incomplete argument." )
        END (*IF*)
        
      ; IF FM3CLOptions . OptionsLexTable = NIL
        THEN
          FM3CLOptions . OptionsLexTable
            := FM3Files . ReadFsm
                 ( "Clt" , FM3SharedGlobals . FM3FileKindCltPkl )
        END (*IF*)

      ; PaHasEqualSign := FALSE 
      ; LLexState := FM3LexTable . IncrInit ( FM3CLOptions . OptionsLexTable )
      ; LOOP
          (* INVARIANT: Lex machine needs a(nother) character.
             The next one is at PaArgSs, but not fetched.
          *) 
          IF PaArgSs >= PaArgLen
          THEN LChar := FM3LexTable . NullChar
          ELSE
            LChar := Text . GetChar ( PaArgText , PaArgSs )
          ; IF LChar = '='
            THEN
              LChar := FM3LexTable . NullChar
            ; PaHasEqualSign := TRUE 
            END (*IF*) 
          ; INC ( PaArgSs )
          END (*IF*) 
        ; LLexValue
            := FM3LexTable . IncrNext
                 ( FM3CLOptions . OptionsLexTable
                 , LChar
                 , (*IN OUT*) LLexState
                 )
        ; IF LLexValue = FM3LexTable . ValueUnrecognized 
          THEN RAISE TerminateCL  ( "Unrecognized argument." )
          ELSIF LLexValue = FM3LexTable . ValueNull
          THEN (* LexTable wants more characters. *)
            (* Loop. *)
          ELSE (* A recognized option, but it might be only a proper prefix
                  of the command-line argument.
               *)
            (* This is made very messy by virtue that LexTable sometimes can
               and will recognize a string without seeing its successor, if
               any following character would make for an unrecognizable string.
               Otherwise, LexTable will need to receive a trailing NullChar
               before it recognizes the string.
            *) 
            IF LChar # FM3LexTable . NullChar
            THEN (* LexTable recognized the option from its last character. *)
              IF PaArgSs < PaArgLen
                 AND Text . GetChar ( PaArgText , PaArgSs ) = '='
              THEN (* Consume and note the equal sign. *) 
                PaHasEqualSign := TRUE 
              ; INC ( PaArgSs )
              END (*IF*) 
            END (*IF*) 
          ; IF PaHasEqualSign OR PaArgSs >= PaArgLen  
            THEN (* No more chars in PaArgText.  LLexValue is recognized. *)
              EXIT
            ELSE (* There is an extra trailing character, which will make 
                    argument LLexValue unrecognizable after all.
                 *) 
              RAISE TerminateCL  ( "Unrecognized argument." )
            END (*IF*) 
          END (*IF*) 
        END (*LOOP*) 

      ; CASE LLexValue 
        OF
        | Clt . CltVersion
        =>  PaNoEqualSign ( )
          ; IF NOT LNo
            THEN (* OK, so this is silly.  But as much on the user's part. *) 
              DisplayVersion ( )
            ; RAISE FM3SharedUtils . Terminate ( NIL )
            END (*IF*) 
        
        | Clt . CltHelp 
        =>  PaNoEqualSign ( )
          ; IF NOT LNo
            THEN (* OK, so this is silly.  But as much on the user's part. *) 
              DisplayVersion ( )
            ; DisplayHelp ( ) 
            ; RAISE FM3SharedUtils . Terminate ( NIL ) 
            END (*IF*) 
        
        | Clt . CltSrcFile  
        =>  PaNoNo ( LNo )
          ; PaFindParam ( )
          ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
          ; PaCheckValidSrcFile ( LParam ) 
          ; PrependTextToList ( FM3CLOptions . SourceFileNames , LParam )
          ; INC ( FM3CLOptions . SourceFileCt ) 
          ; FM3CLOptions . SrcFileName := LParam  
        
        | Clt . CltSrcDir  
        =>  PaNoNo ( LNo )
          ; PaFindParam ( )
          ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
          ; DirElem ( LParam , LNo ) 
        
        | Clt . CltImportDir  
        =>  PaNoNo ( LNo )
          ; PaFindParam ( )
          ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
          ; PrependTextToList ( FM3CLOptions . ImportDirNames , LParam ) 
          ; INC ( FM3CLOptions . ImportDirCt ) 
        
        | Clt . CltResourceDir  
        =>  PaNoNo ( LNo )
          ; PaFindParam ( )
          ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
          ; FM3CLOptions . ResourceDirName := LParam
        
        | Clt . CltBuildDir 
        =>  PaNoNo ( LNo )
          ; PaFindParam ( )
          ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
          ; FM3CLOptions . BuildDir := LParam
        
        | Clt . CltKeepPasses 
        =>  PaFindParam ( )
          ; PaPassNoSet ( FM3CLOptions . PassNosToKeep , NOT LNo ) 
        
        | Clt . CltKeep 
        =>  PaNoEqualSign ( )
          ; AlterPassNos
              ( FM3CLOptions . PassNosToKeep
              , FM3CLOptions . PassNoSetValid
              , NOT LNo
              ) 

        | Clt . CltDisAsmPasses 
        =>  PaFindParam ( )
          ; PaPassNoSet ( FM3CLOptions . PassNosToDisAsm , NOT LNo ) 
        
        | Clt . CltDisAsm 
        =>  PaNoEqualSign ( )
          ; AlterPassNos
              ( FM3CLOptions . PassNosToDisAsm
              , FM3CLOptions . PassNoSetValid
              , NOT LNo
              ) 
        
        (* Binary, parameterless options: *) 
        | Clt . CltStdErr  
        , Clt . CltStdOut  
        , Clt . CltFM3Log
        , Clt . CltUnitLog 
        =>  PaNoEqualSign ( )
          ; AssignOptionSetElem 
              ( FM3CLOptions . OptionTokSet , LLexValue , Value := NOT LNo ) 
        
        ELSE RAISE TerminateCL ( "Unrecognized argument." ) 
        END (*CASE*)
      END PaTwoHyphenArg
      
  ; PROCEDURE PaHyphenArg ( )
    RAISES { TerminateCL , FM3SharedUtils . Terminate } 
    (* PRE: Arg starts with one hyphen only. *) 

    = VAR LParam : TEXT
    ; VAR LArgLength : INTEGER 
    ; VAR LPassNo : INTEGER 
    ; VAR LArgChar : CHAR 

    ; BEGIN
        IF PaArgLen <= 1
        THEN RAISE TerminateCL ( "Missing argument." )
        END (*IF*)
      ; PaArgSs := 1
      ; LOOP (* Thru' multiple letters of this argument. *) 
          LArgChar := Text . GetChar ( PaArgText , PaArgSs )
        ; INC ( PaArgSs )
        ; CASE LArgChar OF
          | '='
          => RAISE TerminateCL
               ( "Equal sign not allowed in single-character argument." )
               
          | 'v'
          =>  DisplayVersion ( )
            ; RAISE FM3SharedUtils . Terminate ( NIL )
         
          | 'h'
          =>  DisplayVersion ( )
            ; DisplayHelp ( )
            ; RAISE FM3SharedUtils . Terminate ( NIL )
           
          | 's' (* Source file. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; PaCheckValidSrcFile ( LParam ) 
            ; PrependTextToList ( FM3CLOptions . SourceFileNames , LParam )
            ; INC ( FM3CLOptions . SourceFileCt ) 
            ; FM3CLOptions . SrcFileName := LParam 
            ; EXIT 

          | 'S' (* Source directory. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; DirElem ( LParam , No := FALSE ) 
            ; EXIT 
        
          | 'I'
          =>  PaFindParam ( ) 
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; PrependTextToList ( FM3CLOptions . ImportDirNames , LParam ) 
            ; INC ( FM3CLOptions . ImportDirCt ) 

          | 'B'
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs ) 
            ; FM3CLOptions . BuildDir := LParam
            ; EXIT 

          | 'k' (* Keep one pass. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
            ; LPassNo := SingleDigitParam ( LParam ) 
            ; IF NOT LPassNo IN FM3CLOptions . PassNoSetValid
              THEN RAISE TerminateCL ( "Invalid pass number." ) 
              END (*IF*) 
            ; FM3CLOptions . InclPassNo
               ( FM3CLOptions . PassNosToKeep , LPassNo )
            ; EXIT 

          | 'K' (* Keep, all passes. *) 
          => AlterPassNos
               ( FM3CLOptions . PassNosToKeep
               , FM3CLOptions . PassNoSetValid
               , Include := TRUE 
               ) 
                
          | 'd' (* Disassemble one pass. *) 
          =>  PaFindParam ( )
            ; LParam := Text . Sub ( PaArgText , PaArgSs , PaArgLen - PaArgSs )
            ; LPassNo := SingleDigitParam ( LParam )
            ; IF NOT LPassNo IN FM3CLOptions . PassNoSetValid
              THEN RAISE TerminateCL ( "Invalid pass number." ) 
              END (*IF*) 
            ; FM3CLOptions . InclPassNo
               ( FM3CLOptions . PassNosToDisAsm , LPassNo )
            ; EXIT 
               
          | 'D' (* Disassemble , all passes. *) 
          => AlterPassNos
               ( FM3CLOptions . PassNosToDisAsm
               , FM3CLOptions . PassNoSetValid
               , Include := TRUE 
               ) 
                
          ELSE RAISE TerminateCL  ( "Unrecognized argument." )
          END (*CASE*)
        ; INC ( PaArgSs )
        ; IF PaArgSs >= PaArgLen THEN EXIT END (*IF*) 
        END (*LOOP*) 
      END PaHyphenArg 

  ; BEGIN (* ParseArgs *) 
      PaArgCt := Params . Count 
    ; PaArgNo := 1

    ; TRY 
        WHILE PaArgNo < PaArgCt DO
          PaFetchArg ( MinLen := 1 )
        ; PaPrevArgText := NIL 
        ; IF PaArgLen >= 1 AND Text . GetChar ( PaArgText , 0 ) = '-'
          THEN
            IF PaArgLen >= 2 AND Text . GetChar ( PaArgText , 1 ) = '-' 
            THEN (* Two hyphens.*) 
              INC ( PaArgSs , 2 )
            ; PaTwoHyphenArg ( )
            ELSE (* One hyphen. *) 
              INC ( PaArgSs )
            ; PaHyphenArg ( )
            END (*IF*) 
          ELSE (* No hyphens. *)
            PaCheckValidSrcFile ( PaArgText ) 
          ; PrependTextToList ( FM3CLOptions . SourceFileNames , PaArgText )
          ; INC ( FM3CLOptions . SourceFileCt ) 
          ; FM3CLOptions . SrcFileName := PaArgText 
          END (*IF*) 
        ; INC ( PaArgNo )
        END (*WHILE*)
      EXCEPT TerminateCL  ( EMsg )
      =>   VAR LBlankCt : INTEGER
        ; BEGIN
            Wr . PutText ( Stdio . stderr , FM3TextColors . FGDkRed ) 
          ; Wr . PutText ( Stdio . stderr , "Command line error: " ) 
          ; Wr . PutText ( Stdio . stderr , FM3TextColors . Reset) 
          ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
          ; Wr . PutText ( Stdio . stderr , "    " )
          ; LBlankCt := 4 
          ; IF PaPrevArgText # NIL
            THEN
              Wr . PutText ( Stdio . stderr , PaPrevArgText ) 
            ; INC ( LBlankCt , Text . Length ( PaPrevArgText ) )
            ; Wr . PutChar ( Stdio . stderr , ' ' )
            ; INC ( LBlankCt ) 
            END (*IF*) 
          ; Wr . PutText ( Stdio . stderr , PaArgText ) 
          ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
          ; Wr . PutText
              ( Stdio . stderr
              , FM3SharedUtils . Blanks ( LBlankCt + PaArgSs ) 
              ) 
          ; Wr . PutChar ( Stdio . stderr , '^' )
          ; Wr . PutChar ( Stdio . stderr , ' ' )
          (* 
          ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
          ; Wr . PutText ( Stdio . stderr , "    " )
          *) 
          ; Wr . PutText ( Stdio . stderr , EMsg )
          ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
          ; Wr . Flush ( Stdio . stderr )  
          ; DisplayHelp ( ) 
          ; RAISE FM3SharedUtils . Terminate ( "FM3CLArgs" )
          END (*Block*) 
      END (*EXCEPT*)
    END ParseArgs

; PROCEDURE DisplayVersion ( )

  = BEGIN
      Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . PutText ( Stdio . stderr , FM3TextColors . FGDkGreen ) 
    ; Wr . PutText ( Stdio . stderr , "Running " ) 
    ; Wr . PutText ( Stdio . stderr , FM3TextColors . Reset) 
    ; Wr . PutText ( Stdio . stderr , Params . Get ( 0 ) ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . PutText
        ( Stdio . stderr , "    FM3 Modula-3 compiler, version " ) 
    ; Wr . PutText ( Stdio . stderr , FM3Version . VersionString ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL )
    ; Wr . Flush ( Stdio . stderr ) 
    END DisplayVersion

; CONST HelpTextSimpleName = "FM3HelpText"
    
; PROCEDURE DisplayHelp ( )

  = VAR LHelpRdT : Rd . T
  ; VAR LLine : TEXT
  ; VAR LReason : TEXT
  ; VAR LLength : INTEGER 
  ; VAR LOpenFailed : BOOLEAN 

  ; BEGIN
      LOpenFailed := FALSE 
    ; TRY (*EXCEPT*)
        LHelpRdT
          := FM3SharedUtils . OpenRd
               ( FM3CLOptions . ResourceDirName
               , HelpTextSimpleName
               , "help text"
               )
      EXCEPT
      | FM3SharedUtils . FatalError ( EMsg ) 
      => LReason 
           := FM3SharedUtils . CatArrT 
                ( ARRAY OF REFANY { "    (" , EMsg , ")" , Wr . EOL } ) 
      ; LOpenFailed := TRUE 
      ELSE 
        LReason := NIL  
      ; LOpenFailed := TRUE 
      END (*EXCEPT*) 
    ; IF LOpenFailed 
      THEN 
        Wr . PutText ( Stdio . stderr , "Unable to open help text file " )
      ; Wr . PutText 
          ( Stdio . stderr
          , Pathname . Join 
              ( FM3CLOptions . ResourceDirName ,  HelpTextSimpleName , NIL ) 
          ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL )  
      ; Wr . PutText ( Stdio . stderr , LReason )
      ; Wr . PutText ( Stdio . stderr , "    Try supplying " ) 
      ; Wr . PutText 
          ( Stdio . stderr , FM3CLToks . Image ( FM3CLToks . CltResourceDir ) )
      ; Wr . PutText ( Stdio . stderr , " argument first." ) 
      ; Wr . Flush ( Stdio . stderr ) 
      ELSE      
        WHILE NOT Rd . EOF ( LHelpRdT )
        DO
          LLine := Rd . GetLine ( LHelpRdT )
        ; LLength := Text . Length ( LLine )
        ; IF LLength >= 2
             AND Text . GetChar ( LLine , 0 ) = '$' 
             AND Text . GetChar ( LLine , 1 ) = 'Z'
          THEN (* Don't copy lines beginning with "$Z". *) 
          ELSE
            Wr . PutText ( Stdio . stderr , LLine )
          ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
          END (*IF*) 
        END (*WHILE*)
      ; Wr .Flush (Stdio . stderr )
      ; Rd . Close ( LHelpRdT )
      END (*IF*) 
    END DisplayHelp

; CONST OptionTokSetDefault 
          = FM3CLOptions . OptionTokSetTyp
             { Clt . CltStdErr 
             , Clt . CltFM3Log 
             , Clt . CltStdErr 
             , Clt . CltUnitLog
             }

; PROCEDURE SetDefaults ( )

  = VAR LExeName : TEXT
    
  ; BEGIN (*SetDefaults*) 
      FM3CLOptions . SrcDirList := NIL 
    ; FM3CLOptions . SourceFileNames := NIL
    ; FM3CLOptions . ImportDirNames := NIL
    ; FM3CLOptions . SourceFileCt := 0
    ; FM3CLOptions . ImportDirCt := 0
    ; LExeName := Params . Get ( 0 )
    ; FM3CLOptions . ResourceDirName
        := FM3SharedUtils  . DefaultResourceDirName ( ) 

    ; FM3CLOptions . BuildDirRelPath := "../build"
    
    ; FM3CLOptions . SrcFileName := "" 

    ; FM3CLOptions . OptionTokSet := OptionTokSetDefault 
             
    ; FM3CLOptions . PassNosToKeep := FM3CLOptions . PassNoSetEmpty 
    ; FM3CLOptions . PassNosToDisAsm := FM3CLOptions . PassNoSetEmpty 

  (* TEMPORARY: during development: *)

    ; FM3CLOptions . SrcFileName := "Main.m3" 

    (* Keep intermediate files. *) 
    ; AlterPassNos
        ( FM3CLOptions . PassNosToKeep
        , FM3CLOptions . PassNoSetValid
        , Include := TRUE 
        ) 

    (* Disassemble intermediate files. *)
    ; AlterPassNos
        ( FM3CLOptions . PassNosToDisAsm
        , FM3CLOptions . PassNoSetValid
        , Include := TRUE 
        ) 

    END SetDefaults

; PROCEDURE RemoveLeftoverFiles ( )

  = BEGIN
      (* This is currently being done elsewhere, e.g. Compile, Pass1, Pass2. *)
    END RemoveLeftoverFiles

; PROCEDURE ComputeDerivedInfo ( ) 

  = BEGIN
      FM3CLOptions . PassNoSetUnion
        ( (*IN OUT*) FM3CLOptions . PassNosToKeep
        , FM3CLOptions . PassNosToDisAsm
        ) 
      
    ; IF Clt . CltFM3Log IN FM3CLOptions . OptionTokSet 
      THEN 
        TRY FM3Messages . FM3LogFileWrT
              := FileWr . Open ( FM3Messages . FM3LogFileName ) 
        EXCEPT
        | OSError . E ( EAtoms )
        => FM3Messages . FM3LogArr
             ( ARRAY OF REFANY
               { "Unable to open FM3 log file "
               , FM3Messages . FM3LogFileName 
               , ": OSError.E(" 
               , EAtoms
               , ")"
               , FM3Messages . NLIndent 
               , "Will proceed without it." 
               }
             ) 
        ; FM3CLOptions . ExclOptionTok
            ( FM3CLOptions . OptionTokSet , Clt . CltFM3Log ) 
        END (*EXCEPT*)
      ELSE FM3SharedUtils . DeleteFile ( FM3Messages . FM3LogFileName ) 
      END (*IF*)
    ; FM3SharedUtils . ResourceDirName := FM3CLOptions . ResourceDirName
      (* Push this out so FM3SharedUtils need not import FM3CLOptions and thus
         can be used in other main programs that get their options other ways.
      *)

    ; FM3CLOptions . SrcDirList := DerivedDirs ( )
    ; FM3CLOptions . SrcDirMsg := DerivedDirsMsg ( FM3CLOptions . SrcDirList )  

(* TOTO: remove any leftover old versions of files not to be generated
         by this run.  Keep pass files, disasm files, logs.
*) 

    END ComputeDerivedInfo

(*EXPORTED*)
; PROCEDURE Process ( ) 
  RAISES { FM3SharedUtils . Terminate } 

  = BEGIN
      SetDefaults ( )
    ; ParseArgs ( )
    ; ComputeDerivedInfo ( ) 
    END Process 

(*EXPORTED*)
; PROCEDURE Cleanup ( )

  = BEGIN
      IF FM3Messages . FM3LogFileWrT # NIL
      THEN Wr . Close ( FM3Messages . FM3LogFileWrT )
      END (*IF*) 
    END Cleanup

(*EXPORTED*)
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
  END FM3CLArgs
.

