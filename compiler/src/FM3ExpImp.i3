
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3ExpImp

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Base
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3OpenArray_Char
; IMPORT FM3Scanner 
; IMPORT FM3Units 

; VAR CompAtomDict : FM3Atom_OAChars . T := NIL (* Interface names. *)
  (* Imported or exported interface names.  Only one of these for
     an entire compiler run.
  *) 

; UnitRefMap : FM3Base . MapTyp
  (* Interface name atom to UnitRef.  Only one of these for
     an entire compiler run.
  *) 

; PROCEDURE GetInterface
    ( IdentChars : FM3OpenArray_Char . T
    ; Position : FM3Base . tPosition
      (* ^Of the [ex/im]port identifier in the current unit. *) 
    ; IsExport : BOOLEAN
    )
  : FM3Units . UnitRefTyp
    (* ^The interface unit to be [ex/im]ported, possibly NIL *)
  (* If not already done, compile or load the interface named by IdentChars. *)

; PROCEDURE CheckDuplicateExpImp 
    ( IntoUnitRef : FM3Units . UnitRefTyp
    ; NewIdentAtom : FM3Base . AtomTyp 
    ; ImportPosition : FM3Base . tPosition 
    ; IdentInserterText : TEXT 
    )
  : BOOLEAN (* Check passed. *) 

; PROCEDURE ImportDeclByNo
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; FromUnitDeclNo : FM3Globals . DeclNoTyp
    ; Position : FM3Base . tPosition
    ; Duplicator : TEXT  
    )
  : BOOLEAN (* Success. *)
  (* PRE: FromUnitDeclNo leads to a DeclRef in FromUnitRef^. *)

; PROCEDURE ImportDeclByIdent
    ( FromUnitRef : FM3Units . UnitRefTyp
    ; READONLY IdScanAttribute : FM3Scanner . tScanAttribute
      (* ^Containing info about the to-be-imported identifier. *) 
    )
  : BOOLEAN (* Success. *) 

; PROCEDURE ImportAS
    ( READONLY IntfScanAttr : FM3Scanner . tScanAttribute
    ; READONLY ASScanAttr : FM3Scanner . tScanAttribute
    )

; PROCEDURE CountDecls ( FromUnitRef :  FM3Units . UnitRefTyp )
    : INTEGER (* Number of decls in FromUnitRef^ *) 

; PROCEDURE ImportAllDecls
    ( FromUnitRef :  FM3Units . UnitRefTyp
    ; READONLY ExportPosition : FM3Base . tPosition
      (* ^Of the EXPORTS directive's identifier. *)
    )

; PROCEDURE Done ( ) 

; CONST NonTransitiveNote
    = FM3Messages . NLIndent
      & "(It's there via export/import,"
      & " but not transitively importable (2.5.1).)"

; END FM3ExpImp
.

