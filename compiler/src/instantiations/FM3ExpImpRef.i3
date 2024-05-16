
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3ExpImpRef
(* For instantiating. *)
(* A way to refer to a declaration [ex|im]ported from a remote unit. *) 

; IMPORT FM3Base

; CONST Brand = "FM3ExpImpRef-1.0" 

; TYPE T
    = RECORD
        EirUnitNo : FM3Base . UnitNoTyp (* The remote unit. *) 
      ; EirDeclNo : FM3Base . DeclNoTyp (* In the other unit. *)
        (* DeclNoNull, if this is a reference to the unit itself. *)
      ; EirImportingUnitNo : FM3Base . UnitNoTyp
      ; EirImportingUnitPosition : FM3Base . tPosition
        (* ^Of the EXPORTS or IMPORT that brought it in to the unit containing
           this export/import node.  Its position in the remote unit can be
           found in the remote unit's Decl node for it.
        *) 
      END 

; END FM3ExpImpRef 
.
