
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

; TYPE Dkt = FM3Decls . DeclKindTyp 
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
      ELSE
      END (*CASE*)
    ; ExprRef . ExpState := Est . EsResolved 
    END ResolveExpr

(* Check forms of structural expression equality that can be
   treated as equal.
*) 
      
; PROCEDURE Opnds1And2Equal ( Left , Right : FM3Exprs . ExprTyp ) : BOOLEAN

  = BEGIN (*Opnds1And2Equal*)
      IF NOT ExprRefsEqual ( Left . ExpOpnd1 , Right . ExpOpnd1 )
      THEN RETURN FALSE
      ELSIF NOT ExprRefsEqual ( Left . ExpOpnd2 , Right . ExpOpnd2 )
      THEN RETURN FALSE
      ELSE RETURN TRUE  
      END (*IF*)
    END Opnds1And2Equal

(*EXPORTED.*)
; PROCEDURE ConstValuesEqual
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

  ; BEGIN (*ConstValuesEqual*)
      IF NOT LeftExprRef . ExpConstValIsKnown THEN RETURN FALSE END (*IF*) 
    ; IF NOT RightExprRef . ExpConstValIsKnown THEN RETURN FALSE END (*IF*) 
    ; IF LeftExprRef . ExpType = FM3Builtins . BuiltinExpr ( Stk . RidTEXT )
      THEN (* TEXT is the only type that could be a reference constant. *)
(* FIXME: Not necessarily so. Value constructors can be constant. *) 
        RETURN Text . Equal
           ( RightExprRef . ExpRefConstVal , LeftExprRef . ExpRefConstVal ) 
      ELSE RETURN RightExprRef . ExpScalarConstVal = LeftExprRef . ExpScalarConstVal 
      END (*IF*) 
      
    END ConstValuesEqual

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

; PROCEDURE DeclRefsEqual
    ( LeftDeclRef , RightDeclRef : FM3Decls . DeclRefTyp )
  : BOOLEAN

  = BEGIN (* DeclRefsEqual *)

      IF LeftDeclRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF RightDeclRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF NOT LeftDeclRef ^ . DclIsUsable THEN RETURN FALSE END (*IF*)
    ; IF NOT RightDeclRef ^ . DclIsUsable THEN RETURN FALSE END (*IF*)
    ; IF LeftDeclRef = RightDeclRef THEN (*Identical*) RETURN TRUE END (*IF*)
    ; IF LeftDeclRef ^ . DclKind # RightDeclRef ^ . DclKind 
      THEN RETURN FALSE
      END (*IF*)

    ; IF LeftDeclRef ^ . DclIdAtom # RightDeclRef ^ . DclIdAtom
      THEN
(*  REVIEW: Does this test duplicate one in the caller? *) 
        RETURN FALSE
      END (*IF*)
    ; CASE LeftDeclRef ^ . DclKind OF 
      | Dkt . DkEnumLit
      => (* Mere presence of the enumlit's Ident is enough. *)
         RETURN TRUE
      | Dkt . DkType                        
      , Dkt . DkConst
      , Dkt . DkVar
      , Dkt . DkVALUEFormal
      , Dkt . DkVARFormal
      , Dkt . DkROFormal
      , Dkt . DkRecField
      , Dkt . DkObjField
      , Dkt . DkMethod
      =>  IF NOT ExprRefsEqual
                   ( LeftDeclRef ^ . DclDefType , RightDeclRef ^ . DclDefType ) 
          THEN RETURN FALSE
          ELSIF
            NOT ExprRefsEqual
                  ( LeftDeclRef ^ . DclDefValue , RightDeclRef ^ . DclDefValue )
          THEN RETURN FALSE
          ELSE RETURN TRUE 
          END (*IF*)
      | Dkt . DkOverride
      =>  IF NOT ExprRefsEqual
                    ( LeftDeclRef ^ . DclDefValue , RightDeclRef ^ . DclDefValue ) 
          THEN RETURN FALSE
          ELSE RETURN TRUE 
          END (*IF*)
      ELSE (* Other decl kinds are never equal, but we probably
              won't see them.
           *)  
        RETURN FALSE
      END (*CASE*)
    END DeclRefsEqual
    
(*EXPORTED.*)
; PROCEDURE TypeScopeRefsEqual
    ( LeftScopeRef , RightScopeRef : FM3Scopes . ScopeRefTyp )
  : BOOLEAN
  (* Only for scopes that are part of a type expression: enum, record,
     object, formals.
  *) 

  = VAR LLeftUnitRef : FM3Units . UnitRefTyp
  ; VAR LRightUnitRef : FM3Units . UnitRefTyp
  ; VAR LLeftDeclMap : FM3Decls . DeclMapTyp
  ; VAR LRightDeclMap : FM3Decls . DeclMapTyp
  ; VAR LLeftDeclNo : FM3Globals . DeclNoTyp
  ; VAR LRightDeclNo : FM3Globals . DeclNoTyp
  ; VAR LLeftDeclRef : FM3Decls . DeclRefTyp 
  ; VAR LRightDeclRef : FM3Decls . DeclRefTyp 
  ; VAR LDeclCt : INTEGER 

  ; BEGIN (*TypeScopeRefsEqual*)
      IF LeftScopeRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF RightScopeRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF LeftScopeRef = RightScopeRef THEN (*Identical*) RETURN TRUE END (*IF*)
    ; IF LeftScopeRef ^ . ScpKind # RightScopeRef ^ . ScpKind 
      THEN RETURN FALSE
      END (*IF*)
    ; IF NOT LeftScopeRef ^ . ScpKind IN FM3Scopes . ScopeKindSetTypeDef
      THEN (* Others are never equal, but this probably won't happen. *) 
        RETURN FALSE
      END (*IF*) 
    ; IF LeftScopeRef ^ . ScpDeclCt # RightScopeRef ^ . ScpDeclCt
      THEN RETURN FALSE
      END (*IF*)

    (* Go thru' the matching decls in the scopes. *) 
    ; LLeftUnitRef := LeftScopeRef ^ . ScpOwningUnitRef 
    ; LRightUnitRef := RightScopeRef ^ . ScpOwningUnitRef 
    ; LLeftDeclMap := LLeftUnitRef ^ . UntDeclMap  
    ; LRightDeclMap := LRightUnitRef ^ . UntDeclMap 

    ; LLeftDeclNo := LeftScopeRef ^ . ScpMinDeclNo 
    ; LRightDeclNo := RightScopeRef ^ . ScpMinDeclNo
    ; LDeclCt := LeftScopeRef ^ . ScpDeclCt 
    ; LOOP
        IF LDeclCt <= 0 THEN RETURN TRUE END (*IF*)
      ; LLeftDeclRef
          := FM3Decls . DeclRefOfDeclNo ( LLeftDeclNo , LLeftUnitRef )
      ; LRightDeclRef
          := FM3Decls . DeclRefOfDeclNo ( LRightDeclNo , LRightUnitRef )
      ; IF NOT DeclRefsEqual ( LLeftDeclRef , LRightDeclRef ) 
        THEN RETURN FALSE
        END (*IF*)
      ; INC ( LLeftDeclNo )  
      ; INC ( LRightDeclNo )
      ; DEC ( LDeclCt ) 
      END (*LOOP*) 
    END TypeScopeRefsEqual

; PROCEDURE OverrideListsEqual
    ( LeftList , RightList : FM3Globals . DeclRefListTyp )
  : BOOLEAN 

  = BEGIN (*OverrideListsEqual*)
      IF LeftList = RightList THEN RETURN TRUE END (*IF*)
         (* Works for both NIL, meaning empty lists. *)
    ; IF LeftList = NIL THEN RETURN FALSE END (*IF*)
    ; IF RightList = NIL THEN RETURN FALSE END (*IF*)
    ; IF NUMBER ( LeftList ^ ) # NUMBER ( RightList ^ )
      THEN RETURN FALSE
      END (*IF*)
    ; FOR RI := 0 TO LAST ( LeftList ^ )
      DO
        IF NOT DeclRefsEqual ( LeftList ^ [ RI ] , LeftList ^ [ RI ] )
        THEN RETURN FALSE
        ELSIF LeftList ^ [ RI ] . DclKind # Dkt . DkOverride 
        THEN RETURN FALSE
        END (*IF*) 
      END (*FOR*)
    ; RETURN TRUE 
    END OverrideListsEqual

(*EXPORTED.*)
; PROCEDURE ExprRefsEqual
    ( LeftExprRef , RightExprRef : FM3Exprs . ExprTyp ) : BOOLEAN
  (* Returns FALSE for things that should not be uniqued, even if equal. *) 

  = VAR LExprMap : FM3Base . MapTyp
  ; VAR LRightRepExprRef : FM3Exprs . ExprTyp 
  ; VAR LLeftRepNo : FM3Exprs . ExprNoTyp 
  ; VAR LRightRepNo : FM3Exprs . ExprNoTyp 
  ; VAR LResult : BOOLEAN 

  ; BEGIN (*ExprRefsEqual*)
      IF LeftExprRef = NIL THEN RETURN FALSE END (*IF*) 
    ; IF RightExprRef = NIL THEN RETURN FALSE END (*IF*)
    ; IF RightExprRef = LeftExprRef THEN RETURN TRUE END (*IF*)
    ; IF LeftExprRef . ExpRepExprNo < FM3Exprs . ExprNoFirstReal
      THEN RETURN FALSE
      END (*IF*) 
    ; IF RightExprRef . ExpRepExprNo < FM3Exprs . ExprNoFirstReal
      THEN RETURN FALSE
      END (*IF*)
    ; IF LeftExprRef . ExpHash # RightExprRef . ExpHash
      THEN RETURN FALSE
      END (*IF*)
    ; LLeftRepNo := RepExprNo ( LeftExprRef . ExpSelfExprNo )
    ; LRightRepNo := RepExprNo ( RightExprRef . ExpSelfExprNo )
    ; IF LRightRepNo = LLeftRepNo AND LLeftRepNo # FM3Exprs . ExprNoNull
      THEN RETURN TRUE
      END (*IF*)
    ; IF RightExprRef . ExpKind # LeftExprRef . ExpKind
      THEN RETURN FALSE
      END (*IF*)

    (* No shortcuts.  Do a brute-force recursive comparison. *)
    ; IF LeftExprRef . ExpKind IN FM3Exprs . EkSetPossiblyConstants
      THEN (* Both are constant values of the same kind. *)
        IF NOT ExprRefsEqual ( RightExprRef . ExpType , LeftExprRef . ExpType )
        THEN LResult := FALSE 
        END (*IF*)
      ; LResult := ConstValuesEqual ( LeftExprRef , RightExprRef ) 
      ELSE (* Both are types of the same kind. *)
        CASE LeftExprRef . ExpKind OF
        | Ekt . EkEnumType
        =>  LResult 
              := TypeScopeRefsEqual 
                   ( LeftExprRef . ExpScopeRef1 , RightExprRef . ExpScopeRef1 ) 
        | Ekt . EkRecType
        =>  LResult 
              := TypeScopeRefsEqual 
                   ( LeftExprRef . ExpScopeRef1 , RightExprRef . ExpScopeRef1 ) 
        | Ekt . EkArrayType
        =>  LResult := RightExprRef . ExpOpcode = LeftExprRef . ExpOpcode 
(* Needed? ------------^ *)
          ; LResult
              := LResult 
                 AND RightExprRef . ExpArrayTypeIsOpen 
                     = LeftExprRef . ExpArrayTypeIsOpen 
          ; LResult
              := LResult 
                 AND Opnds1And2Equal ( LeftExprRef , RightExprRef ) 
        | Ekt . EkObjType
        =>  LResult
              := RightExprRef . ExpIsUntraced = LeftExprRef . ExpIsUntraced 
          ; LResult
              := LResult 
                 AND Opnds1And2Equal 
                       ( LeftExprRef , RightExprRef ) (* Brand, supertype. *)
          ; LResult 
              := TypeScopeRefsEqual 
                   ( LeftExprRef . ExpScopeRef1 , RightExprRef . ExpScopeRef1 ) 
          ; LResult 
              := OverrideListsEqual 
                   ( LeftExprRef . ExpObjOverrides
                   , RightExprRef . ExpObjOverrides
                   ) 
        | Ekt . EkSubrType
        =>  LResult := Opnds1And2Equal ( LeftExprRef , RightExprRef ) 
        | Ekt . EkRefType
        =>  LResult := RightExprRef . ExpOpcode = LeftExprRef . ExpOpcode 
(* Needed? ------------^ *) 
          ; LResult
              := LResult 
                 AND RightExprRef . ExpIsUntraced 
                     = LeftExprRef . ExpIsUntraced 
          ; LResult
              := LResult 
                 AND Opnds1And2Equal ( LeftExprRef , RightExprRef )
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
      THEN
        (* Discovered a new structural equality.  Record it for posterity. *) 
        LExprMap := FM3Units . UnitStackTopRef ^ . UntExprMap 
      ; LRightRepExprRef := FM3Exprs . ExprRefOfExprNo ( LRightRepNo ) 
      ; LRightRepExprRef . ExpRepExprNo := LLeftRepNo  
      END (*IF*) 
    ; RETURN LResult 
    END ExprRefsEqual
      
; BEGIN (*FM3Resolve*)
  END FM3Resolve
.
  