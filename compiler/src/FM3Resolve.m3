
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
; IMPORT FM3Decls 
; IMPORT FM3Exprs
; IMPORT FM3Globals 
; IMPORT FM3SrcToks 
; IMPORT FM3SrcToks AS Stk
; IMPORT FM3Scopes 
; IMPORT FM3Units 
; IMPORT FM3Utils

; TYPE Ekt = FM3Exprs . ExprKindTyp 
; TYPE Est = FM3Exprs . ExprStateTyp
; TYPE Skt = FM3Scopes . ScopeKindTyp 

; PROCEDURE ResolveChild
    ( ParentRef :  FM3Exprs . ExprTyp ; ChildRef :  FM3Exprs . ExprTyp )

  = BEGIN (*ResolveChild*)
      ResolveExpr ( ChildRef )
    ; FM3Utils . ContribToHashL ( (*IN OUT*) ParentRef . ExpHash , ChildRef . ExpHash )
    END ResolveChild

(*EXPORTED.*)
; PROCEDURE ResolveExpr ( ExprRef : FM3Exprs . ExprTyp ) 

  = BEGIN (*ResolveExpr*)
      IF ExprRef = NIL THEN RETURN END (*IF*) 
    ; IF NOT ExprRef . ExpIsUsable THEN RETURN END (*IF*) 
    ; IF ExprRef . ExpState = Est . EsResolved THEN RETURN END (*IF*) 
    ; CASE ExprRef . ExpKind OF
      | Ekt . EkNull => 
      | Ekt . EkEnumType 
      , Ekt . EkRecType
      => (* Nothing synthesized. *) 
      | Ekt . EkObjType 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 ) (*Supertype.*) 
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 ) (*Brand.*)
      | Ekt . EkArrayType 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 ) (*Subscript type.*)
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 ) (*Element type.*)
      | Ekt . EkSubrType 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 ) (*Lo bound value.*)
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 ) (*Hi bound value.*)
      | Ekt . EkRefType 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 ) (*Brand.*)
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 ) (*Referent type.*)

      | Ekt . EkBrand 
      , Ekt . EkSupertype 
      =>  <* ASSERT FALSE , "should already be resolved." *> 

      | Ekt . EkBuiltin
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )  
      
      | Ekt . EkLiteral 
      =>  FM3Utils . ContribToHashL
            ( ExprRef . ExpHash , ExprRef . ExpScalarConstVal ) 
        ; FM3Utils . ContribToHashL
            ( ExprRef . ExpHash
            , VAL ( ORD ( ExprRef . ExpOpcode ) , FM3Base . HashTyp )
            ) 
      | Ekt . EkIdentRef 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkQualIdentRef 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )
      | Ekt . EkReservedIdent 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )
      | Ekt . EkRemoteRef 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )  
      | Ekt . EkUnOp 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkDot 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkBinOp 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkCall 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkSubscript 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkProc 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )  
      | Ekt . EkFunc 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )  
      | Ekt . EkValue 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 ) 
      | Ekt . EkRef 
      =>  ResolveChild ( ExprRef , ExprRef . ExpOpnd1 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd2 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd3 )
        ; ResolveChild ( ExprRef , ExprRef . ExpOpnd4 )  
      ELSE
      END (*CASE*)
    ; ExprRef . ExpState := Est . EsResolved 
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
; PROCEDURE ConstValuesAreEqual
    ( LeftExprRef , RightExprRef : FM3Exprs . ExprTyp )
  : BOOLEAN
  (* PRE: LeftExprRef & RightExprRef are non-NIL, value exprs
          of equal kind and equal type.
  *)
  
  (* An expr need not be M3 constant, but if it lacks a constant value known to
     the compiler, it will not be considered equal to anything.
  *) 

  = VAR LExprMap : FM3Base . MapTyp
  ; VAR LLeftExprRef : FM3Exprs . ExprTyp 
  ; VAR LRightExprRef : FM3Exprs . ExprTyp 

  ; BEGIN (*ConstValuesAreEqual*)
      IF NOT LeftExprRef . ExpConstValIsKnown THEN RETURN FALSE END (*IF*) 
    ; IF NOT RightExprRef . ExpConstValIsKnown THEN RETURN FALSE END (*IF*) 
    ; IF LeftExprRef . ExpType = FM3Builtins . BuiltinExpr ( Stk . RidTEXT )
      THEN (* TEXT is the only type that could be a reference constant. *)
(* FIXME: Not necessarily so. Value constructors can be constant. *) 
        RETURN Text . Equal
           ( RightExprRef . ExpRefConstVal , LeftExprRef . ExpRefConstVal ) 
      ELSE RETURN RightExprRef . ExpScalarConstVal = LeftExprRef . ExpScalarConstVal 
      END (*IF*) 
      
    END ConstValuesAreEqual

(*EXPORTED.*)
; PROCEDURE RepExprNo ( ExprNo : FM3Exprs . ExprNoTyp ) : FM3Exprs . ExprNoTyp
  (* POST: ExprNoNull if not an expr that can have equal duplicates. *) 

  = VAR LExprMap : FM3Base . MapTyp
  ; VAR LExprRef : FM3Exprs . ExprTyp 
  ; VAR LExprNo : FM3Exprs . ExprNoTyp 

  ; BEGIN (*RepExprNo*) 
      LExprMap := FM3Units . UnitStackTopRef ^ . UntExprMap 
    ; LExprNo := ExprNo 
    ; LOOP 
        IF LExprNo < FM3Exprs . ExprNoFirstReal 
        THEN RETURN FM3Exprs . ExprNoNull 
        END (*IF*) 
      ; LExprRef := FM3Exprs . ExprRefOfExprNo ( LExprNo ) 
      ; IF LExprRef . ExpRepExprNo = LExprNo  
        THEN RETURN LExprNo 
        END (*IF*) 
      ; LExprNo := LExprRef . ExpRepExprNo 
      END (*LOOP*) 
    END RepExprNo

(*EXPORTED.*)
; PROCEDURE TypeExprsEqual ( LeftExprRef , RightExprRef : FM3Exprs . ExprTyp )
  : BOOLEAN 

  = VAR LResult : BOOLEAN

  ; BEGIN (*TypeExprsEqual*)
      RETURN LResult 
    END TypeExprsEqual
      
(*EXPORTED.*)
; PROCEDURE EnumScopeRefsEqual
    ( LeftScopeRef , RightScopeRef : FM3Scopes . ScopeRefTyp )
  : BOOLEAN

  = VAR LLeftUnitRef : FM3Units . UnitRefTyp
  ; VAR LRightUnitRef : FM3Units . UnitRefTyp
  ; VAR LLeftDeclMap : FM3Decls . DeclMapTyp
  ; VAR LRightDeclMap : FM3Decls . DeclMapTyp
  ; VAR LLeftDeclNo : FM3Globals . DeclNoTyp
  ; VAR LRightDeclNo : FM3Globals . DeclNoTyp
  ; VAR LDeclCt : INTEGER 
  ; VAR LResult : BOOLEAN

  ; BEGIN (*EnumScopeRefsEqual*)
      IF LeftScopeRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF RightScopeRef = NIL THEN RETURN FALSE END (*IF*)
    ; LLeftUnitRef := LeftScopeRef ^ . ScpOwningUnitRef 
    ; LRightUnitRef := RightScopeRef ^ . ScpOwningUnitRef 
    ; IF LeftScopeRef = RightScopeRef THEN (*Identical*) RETURN TRUE END (*IF*)
    ; IF LeftScopeRef ^ . ScpKind # RightScopeRef ^ . ScpKind 
      THEN RETURN FALSE
      END (*IF*)
    ; IF LeftScopeRef ^ . ScpDeclCt # RightScopeRef ^ . ScpDeclCt
      THEN RETURN FALSE
      END (*IF*)
    ; IF NOT LeftScopeRef ^ . ScpKind IN FM3Scopes . ScopeKindSetTypeDef
      THEN
        LLeftDeclMap := LLeftUnitRef ^ . UntDeclMap  
      ; LRightDeclMap := LRightUnitRef ^ . UntDeclMap 
        
      ; LLeftDeclNo := LeftScopeRef ^ . ScpMinDeclNo 
      ; LRightDeclNo := RightScopeRef ^ . ScpMinDeclNo
      ; LDeclCt := LeftScopeRef ^ . ScpDeclCt 
      ; LOOP
          IF LDeclCt <= 0 THEN RETURN TRUE END (*IF*) 
        ; IF LeftScopeRef ^ . ScpIdentAtom # RightScopeRef ^ . ScpIdentAtom 
          THEN RETURN FALSE
          END (*IF*) 
        ; IF LeftScopeRef ^ . ScpKind = Skt . SkEnum
          THEN 
          ELSE
          END (*IF*) 
        ; INC ( LLeftDeclNo )  
        ; INC ( LRightDeclNo )
        ; DEC ( LDeclCt ) 
        END (*LOOP*) 
      ELSE
      END (*IF*)
      
    ; RETURN LResult 
    END EnumScopeRefsEqual

(*EXPORTED.*)
; PROCEDURE RecOrObjScopesEqual 
    ( LeftScopeRef , RightScopeRef : FM3Scopes . ScopeRefTyp )
  : BOOLEAN 

  = VAR LResult : BOOLEAN

  ; BEGIN (*RecOrObjScopesEqual*)
      RETURN LResult 
    END RecOrObjScopesEqual


(*EXPORTED.*)
; PROCEDURE ExprsEqual
    ( LeftExprRef , RightExprRef : FM3Exprs . ExprTyp ) : BOOLEAN
  (* Returns FALSE for things that should not be uniqued, even if equal. *) 

  = VAR LExprMap : FM3Base . MapTyp
  ; VAR LRightRepExprRef : FM3Exprs . ExprTyp 
  ; VAR LLeftRepNo : FM3Exprs . ExprNoTyp 
  ; VAR LRightRepNo : FM3Exprs . ExprNoTyp 
  ; VAR LResult : BOOLEAN 

  ; BEGIN (*ExprsEqual*)
      IF LeftExprRef = NIL THEN RETURN FALSE END (*IF*) 
    ; IF RightExprRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF RightExprRef = LeftExprRef THEN RETURN TRUE END (*IF*)
    ; IF LeftExprRef . ExpRepExprNo < FM3Exprs . ExprNoFirstReal
      THEN RETURN FALSE
      END (*IF*) 
    ; IF RightExprRef . ExpRepExprNo < FM3Exprs . ExprNoFirstReal
      THEN RETURN FALSE
      END (*IF*)
    ; IF LeftExprRef . ExpHash # RightExprRef . ExpHash THEN RETURN FALSE END (*IF*)
    ; LLeftRepNo := RepExprNo ( LeftExprRef . ExpSelfExprNo )
    ; LRightRepNo := RepExprNo ( RightExprRef . ExpSelfExprNo )
    ; IF LRightRepNo = LLeftRepNo AND LLeftRepNo # FM3Exprs . ExprNoNull
      THEN RETURN TRUE
      END (*IF*)
    ; IF RightExprRef . ExpKind # LeftExprRef . ExpKind THEN RETURN FALSE END (*IF*)

    (* No shortcuts.  Do a brute-force recursive comparison. *)
    ; IF LeftExprRef . ExpKind IN FM3Exprs . EkSetPossiblyConstants
      THEN (* Both are constant values of the same kind. *)
        IF NOT TypeExprsEqual ( RightExprRef . ExpType , LeftExprRef . ExpType )
        THEN LResult := FALSE 
        END (*IF*)
      ; LResult := ConstValuesAreEqual ( LeftExprRef , RightExprRef ) 
      ELSE (* Both are uniqualble types of the same kind. *)
        CASE LeftExprRef . ExpKind OF
        | Ekt . EkEnumType
        =>  LResult 
              := EnumScopeRefsEqual 
                   ( LeftExprRef . ExpScopeRef1 , RightExprRef . ExpScopeRef1 ) 
        | Ekt . EkRecType
        =>  LResult 
              := RecOrObjScopesEqual 
                   ( LeftExprRef . ExpScopeRef1 , RightExprRef . ExpScopeRef1 ) 
        | Ekt . EkArrayType
        =>  LResult := RightExprRef . ExpOpcode = LeftExprRef . ExpOpcode 
(* Needed? ------------^ *) 
                       AND RightExprRef . ExpArrayTypeIsOpen 
                           = LeftExprRef . ExpArrayTypeIsOpen 
                       AND Opnds12Equal ( LeftExprRef , RightExprRef ) 
        | Ekt . EkObjType
        =>  LResult 
              := RecOrObjScopesEqual 
                   ( LeftExprRef . ExpScopeRef1 , RightExprRef . ExpScopeRef1 ) 
                 AND RightExprRef . ExpIsUntraced = LeftExprRef . ExpIsUntraced 
                 AND Opnds12Equal 
                       ( LeftExprRef , RightExprRef ) (* Brand, supertype. *) 
        | Ekt . EkSubrType
        =>  LResult := Opnds12Equal ( LeftExprRef , RightExprRef ) 
        | Ekt . EkRefType
        =>  LResult := RightExprRef . ExpOpcode = LeftExprRef . ExpOpcode 
(* Needed? ------------^ *) 
                       AND RightExprRef . ExpIsUntraced 
                           = LeftExprRef . ExpIsUntraced 
                       AND Opnds12Equal ( LeftExprRef , RightExprRef )
                           (* ^Brand, supertype (always absent). *)
        | Ekt . EkSupertype
        =>  (* A placeholder for an absent supertype of an OBJECT type.  Also
               present but meaningless in a REF type. since we didn't know at
               parse time, whether type would end up REF or OBJECT. Adds no
               additional info.
            *)
            LResult := TRUE 
        END (*CASE*) 
      END (*IF*)
 
    ; IF LResult 
      THEN (* Discovered a new structural equality.  Record it for posterity. *) 
        LExprMap := FM3Units . UnitStackTopRef ^ . UntExprMap 
      ; LRightRepExprRef := FM3Exprs . ExprRefOfExprNo ( LRightRepNo ) 
      ; LRightRepExprRef . ExpRepExprNo := LLeftRepNo  
      END (*IF*) 
    ; RETURN LResult 
    END ExprsEqual
      
; BEGIN (*FM3Resolve*)
  END FM3Resolve
.
  