
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
; IMPORT VarArray_Int_ExpImpRef  
; IMPORT VarArray_Int_Refany 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Atom_Text
; IMPORT FM3Base
; IMPORT FM3CLOptions
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
        (* ^The patch coordinate of what is conceptually the top token on the
           patch stack is actually kept, decompressed, in UntPatchStackTopCoord,
           for easy access.  The token itself and its other operands are on the
           RdBackFile proper.  For deeper tokens, the coordinate is kept on top
           of the token, opposite of the usual order, on top of its other
           operands. 
        *) 
      ; UntPass1OutSimpleName : TEXT := NIL
      ; UntPass1OutRdBack : RdBackFile . T := NIL
      ; UntMaxPass1OutLength : LONGINT := 0L 
      ; UntPass1OutEmptyCoord : LONGINT := 0L
      ; UntPass2OutSimpleName : TEXT := NIL (* Parse pass output file. *) 
      ; UntPass2OutRdBack : RdBackFile . T := NIL
      ; UntMaxPass2OutLength : LONGINT := 0L 
      ; UntPass2OutEmptyCoord : LONGINT := 0L
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL (* Identifiers. *)   
      ; UntNumberAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsAtomDict : FM3Atom_OAChars . T := NIL (* TEXT literals. *) 
      ; UntWCharsAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntUnitRefImporting : UnitRefTyp
        (* The unit this one is in process of [ex|im]porting. *) 
      ; UntPositionOfImport : FM3Base . tPosition
        (* Of the being-[ex|im]ported identifier. *) 
      ; UntDeclMap : FM3Base . MapTyp := NIL (* All the decls in this unit. *) 
      ; UntScopeMap : FM3Base . MapTyp := NIL (* All the scopes in this unit. *)
      ; UntExpImpIdSet : IntSets . T 
        (* ^Atoms of idents [ex/im]ported into this unit. *)  
      ; UntExpImpMap : VarArray_Int_ExpImpRef . T 
          (* ^IdentAtom to ExpImpRef. *)
      ; UntDeclScopeRef : FM3Base . ScopeRefTyp := NIL  
        (* ^Contains Atoms of idents declared at the top level of this unit.
            These are disjoint from those in UntExpImpIdSet *)
      ; UntUnitNo : FM3Base . UnitNoTyp := FM3Base . UnitNoNull
          (* ^Self-referential. *) 
      ; UntStackDepth : INTEGER 
      ; UntScanResult : INTEGER 
      ; UntParseResult : INTEGER 
      ; UntPass2Result : INTEGER
      ; UntNextDeclNo : INTEGER := 1 
      ; UntKind := UnitKindTyp . UkNull 
      ; UntPassNosDisAsmed : FM3CLOptions . PassNoSetTyp
      ; UntState := UnitStateTyp . UsNull
      ; UntUnsafe : BOOLEAN := FALSE  
      ; UntInCycle : BOOLEAN := FALSE  
      END (*UnitTyp*)

; VAR UnitsAtomDict : FM3Atom_Text . T
; VAR UnitsAtomInitSize := 50
; VAR UnitsMap : VarArray_Int_Refany . T 
    (* Only one UnitsMap in a compile.  Maps Atoms from UnitsAtomDict
       into unit numbers.
    *) 

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
  (* Cache some fields of the top unit in global variables for faster access. *) 

; PROCEDURE UncacheTopUnitValues ( )
  (* Just set the globals to null/NIL values. *) 

; PROCEDURE CurrentUnitIsModule ( ) : BOOLEAN 

; END FM3Units


.

