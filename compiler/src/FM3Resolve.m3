
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Resolve connections among expressions, which so far, are each islands. *) 

MODULE FM3Resolve

; IMPORT FMeBase 
; IMPORT FM3Exprs

(*EXPORTED.*)
; PROCEDURE ResolveExpr ( ExprRef : Exprs . ExprRefTyp ) : FM3Base . HashTyp 

  = VAR

  ; BEGIN (*ResolveExpr*)
      IF ExprRef = NIL THEN RETURN 2 END (*IF*) 
    ; IF NOT ExprRef . ExpIsUsable THEN RETURN 3 END (*IF*) 
    ; CASE ExprRef . ExpKind OF
      | Ekt . EkNull => RETURN 4
      ELSE
      END (*CASE*)
    END ResolveExpr
      
(*EXPORTED.*)
; PROCEDURE DoMakeUnique ( ExprRef: Exprs . ExprRefTyp ) : BOOLEAN

  = BEGIN (*DoMakeUnique*)
      IF RxprRef = NIL THEN RETURN FALSE (* Why bother? *) END (*IF*) 
    ; IF NOT ExprRef . ExpIsUsable 
      THEN (* Keep all unusables separate, for possible messages. *)
        RETURN FALSE
      END (*IF*) 
    ; IF ExprRef . ExpKind IN FM3Exprs . EkSetUniquableTypes
      THEN RETURN TRUE 
      ELSIF ExprRef . ExpKind IN FM3Exprs . EkSetValues
            AND ExpRef . ExpIsConst
      THEN RETURN TRUE
      ELSE RETURN FALSE 
      END (*IF*) 
    END DoMakeUnique

(*EXPORTED.*)
; PROCEDURE ExprsEqual ( Left , Right : Exprs . ExprRefTyp ) : BOOLEAN
  (* Returns FALSE for things that should not be uniqued, even if equal. *) 

  = BEGIN (*ExprsEqual*)
      IF NOT Uniquable ( Left ) THEN RETURN FALSE END (*IF*)  
    ; IF NOT Uniquable ( Right ) THEN RETURN FALSE END (*IF*)
    (* Each is non-NIL, usable, and a type or constant value. *)  
    ; IF Right = Left THEN RETURN TRUE END (*IF*)
    ; IF Right , ExpKind # Left . ExpKind THEN RETURN FALSE END (*IF*)
    ; IF Left . ExpKind IN FM3Exprs . EkSetValues
      THEN (* Both are constant values of the same kind. *)
        IF NOT TypesEqual ( Right . ExpType , Left , ExpType )
        THEN RETURN FALSE END (*IF*)
        END (*IF*)
      ; IF Left . ExpKind IN FM3Exprs . EkSetScalarConst
        THEN RETURN Right . ExpScalarConstVal = Left . ExpScalarConstVal 
        ELSE RETURN RefConstValuesEqual
                      ( Right . ExpRefConstVal , Left . ExpScalarConstVal ) 
        END (*IF*) 
      ELSE (* Both are uniquable types of the same kind. *)
        CASE Left . ExpKind OF
        | Ekt . EkEnumType
        =>  RETURN EnumScopesEqual ( Left . ExpScopeRef1 , Right . ExpScopeRef1 ) 
        | Ekt . EkRecType
        =>  RETURN RecOrObjScopesEqual ( Left . ExpScopeRef1 , Right . ExpScopeRef1 ) 
        | Ekt . EkArrayType
        =>  RETURN Right . ExpOpcode = Left . ExpOpcode (* Needed? *) 
                   AND Right . ExpArrayTypeIsOpen = Left . ExpArrayTypeIsOpen 
                   AND Opnds12Equal ( Left , Right ) 
        | Ekt . EkObjType
        =>  RETURN RecOrObjScopesEqual ( Left . ExpScopeRef1 , Right . ExpScopeRef1 ) 
                   AND Right . ExpIsUntraced = Left . ExpIsUntraced 
                   AND Opnds12Equal ( Left , Right ) (* Brand, supertype. *) 
        | Ekt . EkSubrType
        =>  RETURN Opnds12Equal ( Left , Right ) 
        | Ekt . EkRefType
        =>  RETURN Right . ExpOpcode = Left . ExpOpcode (* Needed? *) 
                   AND Right . ExpIsUntraced = Left . ExpIsUntraced 
                   AND Opnds12Equal ( Left , Right )
                       (* ^Brand, supertype (always absent). *)
        | Ekt . EkSupertype
        =>  (* A placeholder for an absent supertype of an OBJECT type.  Also
               present for every REF type. since we didn't know at the time,
               whether type would end up REF or OBJECT. Adds no additional info.
            *)
            RETURN TRUE 
        END (*CASE*) 
      END (*IF*) 

    ; IF NOT ExprRef . ExpIsUsable THEN RETURN 3 END (*IF*) 
    ; CASE ExprRef . ExpKind OF
      | Ekt . EkNull => RETURN 4
      ELSE
      END (*CASE*)
    END ExprsEqual
      
; BEGIN (*FM3Resolve*)
  END FM3Resolve
.
  