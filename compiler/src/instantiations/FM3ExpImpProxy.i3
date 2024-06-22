
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3ExpImpProxy
(* For instantiating. *)
(* A way to refer to a declaration [ex|im]ported from a remote unit. *) 

; IMPORT FM3Base

; CONST Brand = "FM3ExpImpProxy-1.0" 

; TYPE T
    = RECORD
        EipUnitNo : FM3Base . UnitNoTyp (* The remote unit. *) 
      ; EipDeclNo : FM3Base . DeclNoTyp (* In the remote unit. *)
        (* DeclNoNull, if this is a reference to the unit itself. *)
      ; EipImportingUnitNo : FM3Base . UnitNoTyp
      ; EipImportingUnitPosition : FM3Base . tPosition
        (* ^Of the EXPORTS or IMPORT ident that brought it in to the unit
           containing this export/import node.  Its position in the remote
           unit can be found in the remote unit's Decl node for it.
        *) 
      END
; END FM3ExpImpProxy 
.
