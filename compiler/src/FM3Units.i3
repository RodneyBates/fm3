
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Units

; IMPORT File
; IMPORT Pickle2 AS Pickle 
; IMPORT Wr

; IMPORT IntSets
; IMPORT Time 
; IMPORT UniRd 
; IMPORT VarArray_Int_ExpImpProxy  
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Atom_Text
; IMPORT FM3Base
; IMPORT FM3CLOptions
; IMPORT FM3ExpImpProxy
; IMPORT FM3Globals 
; IMPORT FM3OpenArray_Char
; IMPORT FM3SrcToks
; IMPORT FM3Utils 
; IMPORT RdBackFile

; TYPE UnitKindTyp
         = { UkNull
           , UkInterface
           , UkGenInterface
           , UkInstInterface
           , UkModule
           , UkGenModule
           , UkInstModule
           }

; CONST UnitKindSetInterface
    = SET OF UnitKindTyp
        { UnitKindTyp . UkInterface 
        , UnitKindTyp . UkGenInterface 
        , UnitKindTyp . UkInstInterface
        } 

; CONST UnitKindSetModule
    = SET OF UnitKindTyp
        { UnitKindTyp . UkModule 
        , UnitKindTyp . UkGenModule 
        , UnitKindTyp . UkInstModule
        }

; PROCEDURE UnitKindImage ( Kind : UnitKindTyp ) : TEXT

; PROCEDURE UnitKindSectionNo ( Kind : UnitKindTyp ) : TEXT

; TYPE UnitStateTyp
         = { UsNull
           , UsNotUsable 
           , UsExporting 
           , UsImporting 
           , UsCompiling
           , UsCompiled
           , UsLoaded
           }

; CONST UnitStateSetUsable
    = SET OF UnitStateTyp
        { UnitStateTyp . UsExporting
        , UnitStateTyp . UsImporting
        , UnitStateTyp . UsCompiling
        , UnitStateTyp . UsCompiled
        , UnitStateTyp . UsLoaded
        }

(* Persistent info about a unit. Pickled when compiled.
   Reloaded when exported or imported and exists and is current.
*) 
; CONST UnitRefBrand = "UnitRef0.1"

; REVEAL FM3Globals . UnitRefTyp = BRANDED UnitRefBrand REF UnitTyp 
; TYPE UnitRefTyp = FM3Globals . UnitRefTyp
(* ^Avoiding cyclic imports. *) 

; TYPE UnitTyp
    = RECORD
        UntSrcFileSimpleName : TEXT := NIL (* Simple name *) 
      ; UntSrcFilePath : TEXT := NIL
        (* ^ I.e, directory wherein UntSimpleSrcFileName lives. *)
      ; UntSrcFileTime : Time . T 
      ; UntUnitFileSimpleName : TEXT := NIL 
      ; UntLogSimpleName : TEXT := NIL 
      ; UntUnitIdent : FM3OpenArray_Char . T := NIL 
      ; UntUnitIdentPos : FM3Base . tPosition := FM3Base . PositionNull 
      ; UntBuildDirPath : TEXT := NIL 
      (* ^Same for pass1 output, patch stack, and pass2 output files. *)
      ; UntSrcTime : Time . T := FIRST ( Time . T ) 

      ; UntPassNosDisAsmed : FM3CLOptions . PassNoSetTyp
          := FM3CLOptions . PassNoSetEmpty 
      ; UntPassNosDumped : FM3CLOptions . PassNoSetTyp
          := FM3CLOptions . PassNoSetEmpty 
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL
          (* ^Identifiers occurring in the unit, but not reserved ids. *)   
      ; UntNumLitAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsLitAtomDict : FM3Atom_OAChars . T := NIL (* TEXT literals. *) 
      ; UntWCharsLitAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntDeclMap : FM3Base . MapTyp := NIL
          (* ^DeclNo to <: of FM3Decls.DeclRefTyp.  All the decls in this
              unit, but not erroneous duplicate decls.
          *) 
      ; UntExprMap : FM3Base . MapTyp := NIL
          (* ^ExprNo to ExprRef.  All the exprs in this unit. *) 
      ; UntScopeMap : FM3Base . MapTyp := NIL
          (* ScopeNo to ScopeRef.  All the scopes in this unit. *)
      ; UntExpImpIdSet : IntSets . T := NIL (* IntSets . Empty ( ) *) 
          (* ^Atoms of idents [ex/im]ported into this unit. *)  
      ; UntExpImpMap : VarArray_Int_ExpImpProxy . T 
          (* ^Unit Ident atom to ExpImpProxy.
             We fill this very early, during [ex|im]port processing of the unit,
             so exported and imported atoms will be compactly numbered.
          *)
          (* INVARIANT: Atom is in UntExpImpMap IFF in UntExpImpIdSet. *) 
      ; UntExpImpRefSetxxx : IntSets . T := NIL (* IntSets . Empty ( ) *) 
      ; UntScopeRef : FM3Globals . ScopeRefTyp := NIL  
          (* ^Contains IdAtoms of [ex|im]ports and decls known at
              unit's top level
          *)
      ; UntHash : FM3Utils . HashTyp := FM3Utils . HashNull 
      ; UntExpImpCt : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull 
      ; UntScanResult : INTEGER := - 1  
      ; UntParseResult : INTEGER (* Parse error count.*) 
      ; UntPass2Result : INTEGER := - 1
      ; UntPass3Result : INTEGER := - 1
      ; UntFirstTrueDeclNo : INTEGER := 1 
        (* ^In the unit's top-level scope.  As opposed to imported proxies,
            which are all lower-numbered. *) 
      ; UntStdTok : FM3SrcToks . TokTyp := FM3Base . TokNull
        (* ^Derived from unit file name. *)
      ; UntKind := UnitKindTyp . UkNull 
      ; UntState := UnitStateTyp . UsNull
      ; UntUnsafe : BOOLEAN := FALSE  
      ; UntInExpImpCycle : BOOLEAN := FALSE
      ; UntHasStdUnitPragma : BOOLEAN := FALSE (* Has the FM3_STDUNIT pragma. *)
      ; UntNextDeclNo : INTEGER := 1
      END (*UnitTyp*)

(* Transient info about a unit. Present while the unit is in compilation
   or loaded after being compiled by a prior run.
*) 
; CONST UnitTRefBrand = "UnitTRef0.1"

; REVEAL FM3Globals . UnitTRefTyp = BRANDED UnitTRefBrand REF UnitTTyp 
; TYPE UnitTRefTyp = FM3Globals . UnitTRefTyp
(* ^Avoiding cyclic imports. *)

; TYPE UnitTStateTyp
       = { UttsNull
         , UttsNew 
         , UttsNotFound   
         , UttsNotLoadable 
         , UttsLoaded
             (* obj file exists, was loaded earlier in this compile run,
                and is up to date WRC its src.
             *)
         , UttsCompiled
            (* obj file exists in the compile package, was compiled earlier in
              this compile run, and is thus up to date WRT src.
            *)
         } 

; PROCEDURE UnitTStateImage ( State : UnitTStateTyp ) : TEXT 

; TYPE UnitTTyp
    = RECORD
        UttStackLink : UnitTRefTyp := NIL
      ; UttSrcFilePath : TEXT 
      ; UttUnitRef : UnitRefTyp := NIL
      ; UttLoadFileT : File . T
      ; UttPickleReader : Pickle . Reader 
      ; UttSrcUniRd : UniRd . T := NIL 
      ; UttLogSimpleName : TEXT := NIL 
      ; UttLogWrT : Wr . T := NIL
      ; UttPatchStackSimpleName : TEXT := NIL
      ; UttPatchStackRdBack : RdBackFile . T := NIL
      ; UttMaxPatchStackDepth : LONGINT := 0L
      ; UttPatchStackEmptyCoord : LONGINT := 0L
        (* ^Value of RdBackFile.LengthL when conceptually empty, but may be
           nonzero, on account of file tag, length, etc. *) 
      ; UttPatchStackTopCoord : LONGINT := 0L

(*TODO: box up pass-dependent groups like this one.  Maybe heap-allocate. *)  
      ; UttPass1OutSimpleName : TEXT := NIL
      ; UttPass1OutRdBack : RdBackFile . T := NIL
      ; UttPass1OutDataLength : LONGINT := 0L
        (* ^Excludng final boilerplate tokens. *) 
      ; UttMaxPass1OutLength : LONGINT := 0L 
      ; UttPass1OutEmptyCoord : LONGINT := 0L
      ; UttPass2OutSimpleName : TEXT := NIL 
      ; UttPass2OutRdBack : RdBackFile . T := NIL
      ; UttMaxPass2OutLength : LONGINT := 0L 
      ; UttPass2OutEmptyCoord : LONGINT := 0L

      ; UttPass3OutSimpleName : TEXT := NIL 
      ; UttPass3OutRdBack : RdBackFile . T := NIL
      ; UttMaxPass3OutLength : LONGINT := 0L 
      ; UttPass3OutEmptyCoord : LONGINT := 0L

      ; UttUnitOutSimpleName : TEXT := NIL 

      ; UttImportingUnitTRef : UnitTRefTyp := NIL 
          (* ^The unit this one is in process of [ex|im]porting. *) 
      ; UttRequestPosition : FM3Base . tPosition := FM3Base . PositionNull 
      ; UttExpUnitSet : IntSets . T := NIL (* IntSets . Empty ( ) *)
          (* Unit Nos of units exported by this unit. *) 
      ; UttSkipStackBase : INTEGER := 0 
      ; UttExprStackBaseCt : INTEGER := 0 
          (* TOS Subscript at beginning and end of unit compile. *) 
      ; UttScopeDeclStackBaseCt : INTEGER := 0 
          (* TOS Subscript at beginning and end of unit compile. *) 
      ; UttLookupScopeStackBaseCt : INTEGER := 0 
          (* TOS Subscript at beginning and end of unit compile. *) 
      ; UttStackDepth : INTEGER := 0
          (* ^Where on the units stack this UnitTRef is. *) 
      ; UttSelfUnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull
          (* ^Self-referential. *)
      ; UttState := UnitTStateTyp . UttsNull 
      ; UttIsUsable : BOOLEAN := FALSE 
      END (*UnitTTyp*)

; <*INLINE*>
  PROCEDURE UnitTRefOfUnitNo ( UnitNo : FM3Globals . UnitNoTyp ) : UnitTRefTyp
  (* Mainly for convenient calling by a debugger. *) 

; PROCEDURE UnitTRefImage ( UnitTRef : UnitTRefTyp ; ShowFields := FALSE )
    : TEXT 
  (* UnitNo, REF, and source file path. *) 
  
; VAR UnitsAtomDict : FM3Atom_Text . T
        (* ^Just one in entire compiler run.  Map source file simple names as 
            TEXTS directly to unit numbers, which will be compact. See
            comments in FM3Scope.i3.
        *) 
; VAR UnitsAtomInitSize := 50
; VAR UnitsTMap : VarArray_Int_Refany . T 
    (* Only one UnitsTMap in a compile.  Maps both IdAtoms from UnitsAtomDict
       and unit numbers (which the the same va;uie) directly into UnitTRefs.
    *)

; CONST ExpImpProxyNull
    = FM3ExpImpProxy . T
        { EipUnitNo := FM3Globals . UnitNoNull
          (* ^Null means present but not usable. *)
        , EipDeclNo := FM3Globals . DeclNoNull
        , EipImportingUnitNo := FM3Globals . UnitNoNull 
        , EipImportingUnitPosition := FM3Base . PositionNull
        } 

; PROCEDURE NewUnitTRef ( ) : UnitTRefTyp
  (* Allocate a UnitTTyp (transient), initialize non-constant fields, 
     give it a UnitNo, and put into UnitsTMap.
  *)

; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate a UnitTyp (persistent), & Initialize non-constant fields. *) 

; PROCEDURE UnitRefIdImage ( UnitRef : UnitRefTyp ) : TEXT 

; PROCEDURE AllocateDeclNos ( Count : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Count Decl numbers, unique
     within the current unit, and return thxe lowest number.
  *) 

; PROCEDURE IdAtomText ( IdAtom : FM3Base . AtomTyp ) : TEXT
  (* In the current unit. *) 

; VAR UnitTStackTopRef : UnitTRefTyp := NIL 
    (* One UnitStack in a run of the compiler. *)
    (* This is the Unit curently being worked-on. *) 
    
; PROCEDURE PushUnit ( UnitTRef : UnitTRefTyp ) 

; PROCEDURE PopUnit ( ) : UnitTRefTyp

; PROCEDURE CacheTopUnitValues ( )
  (* Cache some fields of top unit in global variables for faster access. *) 

; PROCEDURE CurrentUnitIsModule ( ) : BOOLEAN 

; PROCEDURE CharsOfIdentAtom
    ( UnitTRef : UnitTRefTyp ; Atom : FM3Base . AtomTyp )
  : FM3Atom_OAChars . KeyTyp (* Which is ARRAY OF CHAR. *) 
;
 END FM3Units


.

