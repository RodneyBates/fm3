
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

; PROCEDURE UnitKindImage ( Kind : UnitKindTyp ) : TEXT

; PROCEDURE UnitKindSectionNo ( Kind : UnitKindTyp ) : TEXT

; TYPE UnitRefTyp = REF UnitTyp
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
        (* ^The patch coordinate argument of what is conceptually the top token
           on the patch stack is actually kept, decompressed, in
           UntPatchStackTopCoord, for easy access.  The token itself and its
           other operands are on the RdBackFile proper.  For deeper tokens,
           the coordinate is kept on top of the token, opposite of the usual
           order, on top of its other operands. 
        *)

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
      ; UntPassNosDisAsmed : FM3CLOptions . PassNoSetTyp
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL (* Identifiers. *)   
      ; UntNumLitAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsLitAtomDict : FM3Atom_OAChars . T := NIL (* TEXT literals. *) 
      ; UntWCharsLitAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntUnitRefDoingImporting : UnitRefTyp
          (* ^The unit this one is in process of [ex|im]porting. *) 
      ; UntPositionOfImport : FM3Base . tPosition
          (* ^Of the being-[ex|im]ported identifier. *) 
      ; UntDeclMap : FM3Base . MapTyp := NIL
          (* ^DeclNo to DeclRef.  All the true decls in this unit. *) 
      ; UntScopeMap : FM3Base . MapTyp := NIL
          (* ScopeNo to ScopeRef.  All the scopes in this unit. *)
      ; UntExpUnitSet : IntSets . T := IntSets . Empty ( )
          (* Unit Nos of units exported by this unit. *) 
      ; UntExpImpIdSet : IntSets . T := IntSets . Empty ( ) 
          (* ^Atoms of idents [ex/im]ported into this unit. *)  
      ; UntExpImpMap : VarArray_Int_ExpImpProxy . T 
          (* ^DeclNo to ExpImpProxy. *)
      ; UntExpImpRefSet : IntSets . T := IntSets . Empty ( ) 
      ; UntDeclScopeRefd : FM3Base . ScopeRefTyp := NIL  
          (* ^Contains Atoms of idents and imports known at unit's top level *)
      ; UntExpImpCt : FM3Base . DeclNoTyp := FM3Base . DeclNoNull 
      ; UntSkipStackBase : INTEGER := 0 
          (* TOS Subscript at beginning and end of unit compile. *) 
      ; UntStackDepth : INTEGER := 0
          (* ^Where on the units stack this UnitRef is. *) 
      ; UntUnitNo : FM3Base . UnitNoTyp := FM3Base . UnitNoNull
          (* ^Self-referential. *) 
      ; UntScanResult : INTEGER 
      ; UntParseResult : INTEGER 
      ; UntPass2Result : INTEGER
      ; UntFirstTrueDeclNo : INTEGER := 1 
        (* ^In the unit's top-level scope.  as opposed to imported proxies,
            which are all lower-numbered. *) 
      ; UntNextDeclNo : INTEGER := 1 
      ; UntKind := UnitKindTyp . UkNull 
      ; UntState := UnitStateTyp . UsNull
      ; UntUnsafe : BOOLEAN := FALSE  
      ; UntInCycle : BOOLEAN := FALSE  
      END (*UnitTyp*)

; VAR UnitsAtomDict : FM3Atom_Text . T
        (* ^Just one in entire compler run.  See comments in FM3Scope.i3. *) 
; VAR UnitsAtomInitSize := 50
; VAR UnitsMap : VarArray_Int_Refany . T 
    (* Only one UnitsMap in a compile.  Maps both Atoms from UnitsAtomDict
       and unit numbers (which are the same) directly into UnitRefs.
    *)

; CONST ExpImpProxyNull
    = FM3ExpImpProxy . T
        { EipUnitNo := FM3Base . UnitNoNull 
        , EipDeclNo := FM3Base . DeclNoNull
        , EipImportingUnitNo := FM3Base . UnitNoNull 
        , EipImportingUnitPosition := FM3Base . PositionNull
        } 

; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitsMap. *)

; PROCEDURE AllocateDeclNos ( Ct : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Ct Decl numbers, unique
     within the current unit, and return the lowest number.
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

