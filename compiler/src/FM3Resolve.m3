
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Resolve connections among expressions, which so far, are each islands. *) 

MODULE FM3Resolve

; IMPORT Text 

; IMPORT FM3Base
; IMPORT FM3Builtins 
; IMPORT FM3Exprs
; IMPORT FM3SrcToks 
; IMPORT FM3SrcToks AS Stk

; TYPE Ekt = FM3Exprs . ExprKindTyp 
; TYPE Est = FM3Exprs . ExprStateTyp 

(*EXPORTED.*)
; PROCEDURE ResolveExpr ( ExprRef : FM3Exprs . ExprTyp ) : FM3Base . HashTyp 

  = BEGIN (*ResolveExpr*)
      IF ExprRef = NIL THEN RETURN 2L END (*IF*) 
    ; IF NOT ExprRef . ExpIsUsable THEN RETURN 3L END (*IF*) 
    ; CASE ExprRef . ExpKind OF
      | Ekt . EkNull => RETURN 4L
      ELSE
      END (*CASE*)
    END ResolveExpr
      
; PROCEDURE Opnds12Equal ( Left , Right : FM3Exprs . ExprTyp ) : BOOLEAN

  = BEGIN (*Opnds12Equal*)
      IF NOT ExprsEqual ( Left . ExpOpnd1 , Right . ExpOpnd1 )
      THEN RETURN FALSE
      END (*IF*) 
    ; IF NOT ExprsEqual ( Left . ExpOpnd2 , Right . ExpOpnd2 )
      THEN RETURN FALSE
      END (*IF*)
    ; RETURN TRUE 
    END Opnds12Equal

(*EXPORTED.*)
; PROCEDURE ConstsEqual ( Left , Right : FM3Exprs . ExprTyp  ) : BOOLEAN
  (* PRE: Both are non-NIL, resolved, usable, constant values of equal types. *)  

  = BEGIN (*ConstsEqual*)
      IF Left . ExpType = FM3Builtins . BuiltinExpr ( Stk . RidTEXT )
      THEN (* TEXT is the only type that could be a reference constant. *)
        RETURN Text . Equal ( Right . ExpRefConstVal , Left . ExpRefConstVal ) 
      ELSE RETURN Right . ExpScalarConstVal = Left . ExpScalarConstVal 
      END (*IF*) 
    END ConstsEqual
    
(*EXPORTED.*)
; PROCEDURE IsUniquable ( ExprRef: FM3Exprs . ExprTyp ) : BOOLEAN
  (* And cache the result. *)
  (* POST: ExprRef is non-NIL, usable, a type or constant value. *) 

  = VAR BEGIN (*IsUniquable*)
      IF ExprRef = NIL THEN RETURN FALSE (* Why bother? *) END (*IF*)
    ; IF ExprRef . ExpState # Est . EsUnresolved
      THEN (* Was previously cached. *) 
        RETURN ExprRef . ExpIsUniquable
      END (*IF*) 
    ; IF NOT ExprRef . ExpIsUsable 
      THEN (* Keep all unusables separate, for possible messages. *)
        ExprRef . ExpIsUniquable := FALSE 
      ELSIF ExprRef . ExpKind IN FM3Exprs . EkSetUniquableTypes
      THEN ExprRef . ExpIsUniquable := TRUE  
      ELSIF ExprRef . ExpKind IN FM3Exprs . EkSetPossiblyConstants
            AND ExprRef . ExpIsConst
      THEN ExprRef . ExpIsUniquable := TRUE  
      ELSE ExprRef . ExpIsUniquable := FALSE 
      END(*IF*) 
    ; RETURN ExprRef . ExpIsUniquable 
    END IsUniquable

(*EXPORTED.*)
; PROCEDURE ExprsEqual ( Left , Right : FM3Exprs . ExprTyp ) : BOOLEAN
  (* Returns FALSE for things that should not be uniqued, even if equal. *) 

  = BEGIN (*ExprsEqual*)
      IF NOT IsUniquable ( Left ) THEN RETURN FALSE END (*IF*)  
    ; IF NOT IsUniquable ( Right ) THEN RETURN FALSE END (*IF*)
    (* Each is non-NIL, usable, and a type or constant value. *)  
    ; IF Right = Left THEN RETURN TRUE END (*IF*)
    ; IF Right . ExpKind # Left . ExpKind THEN RETURN FALSE END (*IF*)
    ; IF Left . ExpKind IN FM3Exprs . EkSetPossiblyConstants
      THEN (* Both are constant values of the same kind. *)
        IF NOT TypesEqual ( Right . ExpType , Left . ExpType )
        THEN RETURN FALSE 
        END (*IF*)
      ; RETURN ConstsEqual ( Left , Right ) 
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
               present but meaningless in a REF type. since we didn't know at
               parse time, whether type would end up REF or OBJECT. Adds no
               additional info.
            *)
            RETURN TRUE 
        END (*CASE*) 
      END (*IF*) 

    END ExprsEqual
      
; BEGIN (*FM3Resolve*)
  END FM3Resolve
.
  