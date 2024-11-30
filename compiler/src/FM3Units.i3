
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Units

; IMPORT Wr

; IMPORT IntSets 
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

; CONST UnitRefBrand = "UnitRef0.1"
; REVEAL FM3Globals . UnitRefTyp = BRANDED UnitRefBrand REF UnitTyp 
; TYPE UnitRefTyp = FM3Globals . UnitRefTyp 

; TYPE UnitTyp
    = RECORD
        UntStackLink : UnitRefTyp := NIL
      ; UntSrcFileSimpleName : TEXT := NIL (* Simple name *) 
      ; UntSrcFilePath : TEXT := NIL
        (* ^ I.e, directory wherein UntSimpleSrcFileName lives. *)
      ; UntSrcUniRd : UniRd . T 
      ; UntLogSimpleName : TEXT := NIL 
      ; UntLogWrT : Wr . T := NIL
      ; UntUnitIdent : FM3OpenArray_Char . T 
      ; UntUnitIdentPos : FM3Base . tPosition 
      ; UntBuildDirPath : TEXT := NIL 
      (* ^Same for pass1 output, patch stack, and pass2 output files. *)  
      ; UntPatchStackSimpleName : TEXT := NIL
      ; UntPatchStackRdBack : RdBackFile . T := NIL
      ; UntMaxPatchStackDepth : LONGINT := 0L
      ; UntPatchStackEmptyCoord : LONGINT := 0L
        (* ^Value of RdBackFile.LengthL when conceptually empty, but may be
           nonzero, on account of file tag, length, etc. *) 
      ; UntPatchStackTopCoord : LONGINT := 0L

(*TODO: box  up pass-dependent groups like this one.  Maybe heap-allocate. *)  
      ; UntPass1OutSimpleName : TEXT := NIL
      ; UntPass1OutRdBack : RdBackFile . T := NIL
      ; UntMaxPass1OutLength : LONGINT := 0L 
      ; UntPass1OutEmptyCoord : LONGINT := 0L
(**) 
      ; UntPass2OutSimpleName : TEXT := NIL (* Parse pass output file. *) 
      ; UntPass2OutRdBack : RdBackFile . T := NIL
      ; UntMaxPass2OutLength : LONGINT := 0L 
      ; UntPass2OutEmptyCoord : LONGINT := 0L

      ; UntPass3OutSimpleName : TEXT := NIL (* Parse pass output file. *) 
      ; UntPass3OutRdBack : RdBackFile . T := NIL
      ; UntMaxPass3OutLength : LONGINT := 0L 
      ; UntPass3OutEmptyCoord : LONGINT := 0L

      ; UntPassNosDisAsmed : FM3CLOptions . PassNoSetTyp
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL
          (* ^Identifiers occurring in the unit, except reserved ids. *)   
      ; UntNumLitAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsLitAtomDict : FM3Atom_OAChars . T := NIL (* TEXT literals. *) 
      ; UntWCharsLitAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntImportingUnitRef : UnitRefTyp
          (* ^The unit this one is in process of [ex|im]porting. *) 
      ; UntPositionOfImport : FM3Base . tPosition
          (* ^Of the being-[ex|im]ported identifier. *) 
      ; UntDeclMap : FM3Base . MapTyp := NIL
          (* ^DeclNo to and <: of FM3Decls.DeclRefTyp.  All the decls in this
              unit, but not duplicate decls.
          *) 
      ; UntExprMap : FM3Base . MapTyp := NIL
          (* ^ExprNo to ExprRef.  All the exprs in this unit. *) 
      ; UntScopeMap : FM3Base . MapTyp := NIL
          (* ScopeNo to ScopeRef.  All the scopes in this unit. *)
      ; UntExpUnitSet : IntSets . T := NIL (* IntSets . Empty ( ) *)
          (* Unit Nos of units exported by this unit. *) 
      ; UntExpImpIdSet : IntSets . T := NIL (* IntSets . Empty ( ) *) 
          (* ^Atoms of idents [ex/im]ported into this unit. *)  
      ; UntExpImpMap : VarArray_Int_ExpImpProxy . T 
          (* ^Unit Ident atom to ExpImpProxy.
             We fill this very early, during [ex|im]port processing of the unit,
             so the atoms will be compactly numbered.
          *)
          (* INVARIANT: Atom is in UntExpImpMap IFF in UntExpImpIdSet. *) 
      ; UntExpImpRefSet : IntSets . T := NIL (* IntSets . Empty ( ) *) 
      ; UntScopeRef : FM3Globals . ScopeRefTyp := NIL  
          (* ^Contains Atoms of [ex|im]ports and decls known at unit's top level *)
      ; UntExpImpCt : FM3Globals . DeclNoTyp := FM3Globals . DeclNoNull 
      ; UntSkipStackBase : INTEGER := 0 
          (* TOS Subscript at beginning and end of unit compile. *) 
      ; UntStackDepth : INTEGER := 0
          (* ^Where on the units stack this UnitRef is. *) 
      ; UntSelfUnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull
          (* ^Self-referential. *) 
      ; UntScanResult : INTEGER 
      ; UntParseResult : INTEGER 
      ; UntPass2Result : INTEGER
      ; UntPass3Result : INTEGER
      ; UntFirstTrueDeclNo : INTEGER := 1 
        (* ^In the unit's top-level scope.  As opposed to imported proxies,
            which are all lower-numbered. *) 
      ; UntNextDeclNo : INTEGER := 1
      ; UntKind := UnitKindTyp . UkNull 
      ; UntState := UnitStateTyp . UsNull
      ; UntUnsafe : BOOLEAN := FALSE  
      ; UntInExpImpCycle : BOOLEAN := FALSE
      ; UntIsPredefUnit : BOOLEAN := FALSE 
      END (*UnitTyp*)

; VAR UnitsAtomDict : FM3Atom_Text . T
        (* ^Just one in entire compler run.  Map source file simple names as TEXTS
            directly to unit numbers, which will be compact. See comments
            in FM3Scope.i3.
        *) 
; VAR UnitsAtomInitSize := 50
; VAR UnitsMap : VarArray_Int_Refany . T 
    (* Only one UnitsMap in a compile.  Maps both Atoms from UnitsAtomDict
       and unit numbers (which are the same) directly into UnitRefs.
    *)

; CONST ExpImpProxyNull
    = FM3ExpImpProxy . T
        { EipUnitNo := FM3Globals . UnitNoNull (* Null means present but not usable. *)
        , EipDeclNo := FM3Globals . DeclNoNull
        , EipImportingUnitNo := FM3Globals . UnitNoNull 
        , EipImportingUnitPosition := FM3Base . PositionNull
        } 

; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitsMap. *)

; PROCEDURE AllocateDeclNos ( Count : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Count Decl numbers, unique
     within the current unit, and return thxe lowest number.
  *) 

; PROCEDURE TextOfIdAtom ( IdAtom : FM3Base . AtomTyp ) : TEXT
  (* In the current unit. *) 

; VAR UnitStackTopRef : UnitRefTyp := NIL 
    (* One UnitStack in a run of the compiler. *)
    (* This is the Unit curently being worked-on. *) 
    
; PROCEDURE PushUnit ( UnitRef : UnitRefTyp ) 

; PROCEDURE PopUnit ( ) : UnitRefTyp

; PROCEDURE CacheTopUnitValues ( )
  (* Cache some fields of top unit in global variables for faster access. *) 

; PROCEDURE CurrentUnitIsModule ( ) : BOOLEAN 

; END FM3Units


.

