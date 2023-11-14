
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Units

; IMPORT Wr 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
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

; TYPE UnitRefTyp = REF UnitTyp
; TYPE UnitTyp
    = RECORD
        UntStackLink : UnitRefTyp := NIL 
      ; UntSrcFileName : TEXT := NIL (* Simple name *) 
      ; UntSrcFilePath : TEXT := NIL (* I.e, directory wherein UntSrcFileName lives. *) 
      ; UntLogName : TEXT := NIL 
      ; UntLogWrT : Wr . T := NIL 
      ; UntBuildDirPath : TEXT := NIL 
      (* ^Same for unnest stack, patch stack, and parse pass output file. *)  
      ; UntPatchStackName : TEXT := NIL
      ; UntPatchStackRdBack : RdBackFile . T := NIL
      ; UntParsePassDisassRdBack : RdBackFile . T := NIL 
      ; UntMaxPatchStackDepth : LONGINT := 0L
      ; UntPatchStackEmpty : LONGINT := 0L
        (* ^Value of RdBackFile.LengthL when conceptually empty, but may be
           nonzero, on account of file tag, length, etc. *) 
      ; UntPatchStackTopCoord : LONGINT := 0L
      ; UntPatchStackTopToken : LONGINT := 0L
        (* ^The token and patch coordinate of the topmost item conceptually on
           the patch stack are kept, decompressed, in the above two fields,
           for easy access.  Any operands remain on the RdBack file.
        *) 
      ; UntUnnestStackName : TEXT := NIL
      ; UntUnnestStackRdBack : RdBackFile . T := NIL
      ; UntMaxUnnestStackDepth : LONGINT := 0L 
      ; UntUnnestStackEmpty : LONGINT := 0L
      ; UntParsePassName : TEXT := NIL (* Parse pass output file. *) 
      ; UntParsePassRdBack : RdBackFile . T := NIL
      ; UntParsePassEmpty : LONGINT := 0L
      ; UntIdentAtomDict : FM3Atom_OAChars . T := NIL (* Identifiers. *)   
      ; UntNumberAtomDict : FM3Atom_OAChars . T := NIL (* Numeric literals. *)  
      ; UntCharsAtomDict : FM3Atom_OAChars . T := NIL (* TEXT literals. *) 
      ; UntWCharsAtomDict : FM3Atom_OAWideChars . T := NIL
          (* ^Wide TEXT literals. *)
      ; UntDeclMap : FM3Base . MapTyp := NIL
      ; UntScopeMap : FM3Base . MapTyp := NIL 
      ; UntUnitNo : FM3Base . UnitNoTyp := FM3Base . UnitNoNull
      ; UntStackDepth : INTEGER 
      ; UntScanResult : INTEGER 
      ; UntParseResult : INTEGER 
      ; UntParsePassResult : INTEGER
      ; UntNextDeclNo : INTEGER 
      ; UntKind : UnitKindTyp 
        
      END (*UnitTyp*)

; TYPE UnitMapTyp = FM3Base . MapTyp
    (* Map UnitNoTyp to UnitRefTyp.  Only one UnitMap in a compile. *) 
; VAR UnitMap : UnitMapTyp

; PROCEDURE NewUnitMap ( InitUnitCt : FM3Base . UnitNoTyp ) : UnitMapTyp
  (* One UnnitMap in a compile. *) 

; VAR UnitStackTopRef : UnitRefTyp := NIL 
    (* One UnitStack in a compile *)
    
; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitMap. *)

; PROCEDURE AllocateDeclNos ( Ct : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Ct Decl numbers, unique
     within the current unit, and return the lowest number.
  *) 

; PROCEDURE TextOfIdAtom ( IdAtom : FM3Base . AtomTyp ) : TEXT
  (* In the current unit. *) 

; PROCEDURE PushUnit ( UnitRef : UnitRefTyp ) 

; PROCEDURE PopUnit ( ) : UnitRefTyp  

; END FM3Units


.

