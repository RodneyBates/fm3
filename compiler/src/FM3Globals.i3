
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates                                     *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FM3Globals

; IMPORT TextIntTbl

; IMPORT RdBackFile
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *) 

; IMPORT FM3Atom_OAChars
; IMPORT FM3Atom_OAWideChars
; IMPORT FM3Base 
; IMPORT FM3Dict_OAChars_Int

(* Avoid cyclic imports of strongly-connected data structure interfaces.
   Opaque types are revealed in their respective interfaces.
 *)


; TYPE UnitRefTyp <: REFANY 
; TYPE UnitNoTyp = CARDINAL
; CONST UnitNoNull = FIRST ( CARDINAL )
; CONST UnitNoMax = LAST ( CARDINAL ) 
; CONST UnitNoFirstReal = UnitNoNull + 1 

; TYPE ScopeRefTyp <: REFANY 
; TYPE ScopeNoTyp = CARDINAL
; CONST ScopeNoNull = FIRST ( CARDINAL )
; CONST ScopeNoMax = LAST ( CARDINAL ) 
; CONST ScopeNoFirstReal = ScopeNoNull + 1

; TYPE DeclRefTyp <: REFANY 
; TYPE DeclNoTyp = INTEGER 
; CONST DeclNoNull = FIRST ( DeclNoTyp )
; CONST DeclNoMax = LAST ( DeclNoTyp )
; CONST DeclNoNotUseable = DeclNoNull + 1 
; CONST DeclNoFirstReal = DeclNoNotUseable + 1
; TYPE DeclRefListRefTyp = REF ARRAY OF DeclRefTyp 

; TYPE ExprTyp <: REFANY  

(* File names of internal and generated files: *)

(* Suffixes are W/O  '.', so can use Pathname.Join. *) 
; CONST PatchStackSuffix = "FM3Patch"
      (* The patch stack, used and emptied during parse pass 1. *)
; CONST PatchStackEmptySentinel = -1L
        (* ^An artificial coordinate to the patch file. *) 
; CONST Pass1OutSuffix = "FM3Pass1" (* Output of pass 1. *)
; CONST Pass2OutSuffix = "FM3Pass2" (* Output of pass 2. *) 
; CONST Pass3OutSuffix = "FM3Pass3" (* Output of pass 3. *)
; CONST CopyFileSuffix = "Copy" 
; CONST DisAsmFileSuffix = "DisAsm"
; CONST ExprsFileSuffix = "Exprs"
; CONST UnitLogSuffix = "Log" 

(* These are cached copies of Unt* fields of the current Unit, for faster
   access via static addressing.
*) 
; VAR P1RdBack : RdBackFile . T := NIL 
; VAR PatchRdBack : RdBackFile . T := NIL
; VAR P2RdBack : RdBackFile . T := NIL
; VAR P3RdBack : RdBackFile . T := NIL

(* One set of skip numbering should suffice for all units. *)
; VAR InitSkipStackCt := 255 (* Sometimes many compile errors? *) 
; VAR SkipNoStack : VarArray_Int_Int . T
; VAR NextSkipNo : INTEGER := 1 

; VAR M3RwDict : FM3Dict_OAChars_Int . GrowableTyp  
; VAR PgRwDict : FM3Dict_OAChars_Int . GrowableTyp 

; VAR IdentAtomInitSize := 1500
; VAR NumberAtomInitSize := 600
; VAR CharsAtomInitSize := 600
; VAR WideCharsAtomInitSize := 600

; VAR InitUnitsCt := 40
; VAR InitImportsCt := 100 
; VAR InitScopeCtPerUnit := 60 
; VAR InitDeclCtPerUnit := 100
; VAR InitDefCtPerUnit := 300

; PROCEDURE Init ( ) 

; PROCEDURE Finalize ( )

; END FM3Globals
.




