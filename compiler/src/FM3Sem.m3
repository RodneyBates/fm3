
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* So far, just a holding place for stuff that might eventually go here. *)  

MODULE FM3Sem

; PROCEDURE MaybeConvertCallToOperator ( CallExpr : FM3Exprs . ExprRefTyp )
  : FM3Exprs . ExprRefTyp
  (* If conditions are met, return an expression that is the
     conversion of OrigExpr from a call to a builtin operator.
  *) 

  = VAR LCallExpr : FM3Exprs . ExprRefTyp 
  ; VAR LProcExpr : FM3Exprs . ExprRefTyp
  ; VAR LOpnd1 , LOpnd2 : FM3Exprs . ExprRefTyp 
  ; VAR LNewExpr : FM3Exprs . ExprRefTyp
  ; VAR LPluralSuffix : TEXT
  ; VAR LOpcode : FM3Exprs . OpcodeTyp 
  ; VAR LActualsCt : INTEGER
  ; VAR LUnitTok , LDeclTok : FM3SrcToks . TokTyp := FM3Base . TokNull 

  ; BEGIN
      IF CallExpr = NIL THEN RETURN NIL END (*IF*)
(* COMPLETEME: *) 
    ; RETURN CallExpr (* Disable this for now. *)
    ; IF CallExpr ^ . ExpKind # Ekt . EkCall THEN RETURN CallExpr END (*IF*) 
    ; IF NOT CallExpr ^ . ExpIsUsable THEN RETURN CallExpr END (*IF*) 
    ; IF CallExpr ^ . ExpArgsList = NIL THEN RETURN CallExpr END (*IF*)
    ; LActualsCt := NUMBER ( CallExpr ^ . ExpArgsList ^ )
    ; LProcExpr := CallExpr ^ . ExpArgPrefix 
    ; IF LProcExpr = NIL
      THEN <* ASSERT FALSE , "NIL ExpArgPrefix of Expr of kind EkCall" *>
      END (*IF*)
    ; IF LProcExpr ^ . ExpKind = Ekt . EkIdentRef
         AND IntSets . IsElement ( LProcExpr ^ . ExpOpcode , FM3Std . ProcSet )
      THEN LOpcode := LProcExpr ^ . ExpOpcode
      ELSE
        CASE LProcExpr ^ . ExpKind OF 
        | Ekt . EkIdentRef 
        =>  GetStdUnitNDeclToks 
              ( FM3Units . UnitStackTopRef ^ . UntSelfUnitNo 
              , LProcExpr ^ . ExpIdentDeclNo 
              , (*OUT*) LUnitTok 
              , (*OUT*) LDeclTok 
              ) 
        | Ekt . EkRemoteRef 
        =>  GetStdUnitNDeclToks 
              ( CallExpr ^ . ExpRemoteUnitNo 
              , CallExpr ^ . ExpRemoteDeclNo 
              , (*OUT*) LUnitTok 
              , (*OUT*) LDeclTok 
              )
        ELSE (* No more-complex prefix can denote an operation. *) 
          RETURN CallExpr 
        END (*CASE*)  
      ; IF LUnitTok = FM3Base . TokNull AND LDeclTok = FM3Base . TokNull 
        THEN (* Call is not on anything standard. *)
          RETURN CallExpr
        END (*IF*) 

      ; LDeclTok := DisambiguateStdDeclTok ( LUnitTok , LDeclTok ) 
        (* ^LDeclTok now belongs to only one unit . *) 
      ; IF NOT FM3Builtins . IsOperationTok ( LDeclTok ) 
        THEN (* It's a non-callable standard (type, constant, etc.) . *) 
          FM3Messages . ErrorArr 
            ( ARRAY OF REFANY 
                { FM3SrcToks . Image ( LDeclTok ) , " is not callable." }
            , LProcExpr ^ . ExpPosition 
            ) 
        ; CallExpr ^ . ExpIsUsable := FALSE 
        ; RETURN CallExpr 
        END (*IF*)
      (* It's a callable, standard operation. *) 
      ; LOpcode := LDeclTok 
      END (*IF*) 

    ; LNewExpr
        := FM3Builtins . BuiltinExpr
             ( LDeclTok , LProcExpr ^ . ExpPosition )
      (* ^This will be a changeable, fresh copy. *) 
    ; IF LNewExpr ^ . ExpBuiltinOpActualsCt
         = FM3Builtins . ActualsCtAtLeastOne  
         (* NEW is the only case here. *)
      THEN IF LActualsCt < 1
        THEN 
          FM3Messages . ErrorArr 
            ( ARRAY OF REFANY 
                { "NEW requires one or more actual parameters." }
            , CallExpr ^ . ExpPosition 
            ) 
        ; CallExpr ^ . ExpIsUsable := FALSE
        ; RETURN CallExpr
        END (*IF*) 
      ELSIF LActualsCt # LNewExpr ^ . ExpBuiltinOpActualsCt 
      THEN (* Wrong number of actual parameters. *) 
        IF LNewExpr ^ . ExpBuiltinOpActualsCt = 1 THEN LPluralSuffix := "."  
        ELSE LPluralSuffix := "s." 
        END (*IF*)
      ; FM3Messages . ErrorArr 
          ( ARRAY OF REFANY 
              { FM3SrcToks . Image ( LDeclTok )
              , " requires "
              , Fmt . Int ( LNewExpr ^ . ExpBuiltinOpActualsCt )
              , " actual parameter"
              , LPluralSuffix
              }
          , CallExpr ^ . ExpPosition 
          ) 
      ; CallExpr ^ . ExpIsUsable := FALSE 
      ; RETURN CallExpr
      END (*IF*)

    (* Conditions met, do the conversion using LOpcode. *) 
    ; LNewExpr ^ . ExpPosition := CallExpr ^ . ExpPosition
    ; LNewExpr ^ . ExpOpcode := LOpcode 
    ; LOpnd1 := CallExpr ^ . ExpArgsList ^ [ 0 ]
    ; LNewExpr ^ . ExpOpnd1 := LOpnd1
    ; LNewExpr ^ . ExpIsUsable
        := CallExpr ^ . ExpIsUsable AND LOpnd1 ^ . ExpIsUsable
    ; IF LActualsCt = 1
      THEN
        LNewExpr ^ . ExpReachedDeclNoSet := LOpnd1 ^ . ExpReachedDeclNoSet 
      ELSE
        LOpnd2 := CallExpr ^ . ExpArgsList ^ [ 1 ]
      ; LNewExpr ^ . ExpOpnd2 := LOpnd2
      ; LNewExpr ^ . ExpIsUsable
          := LNewExpr ^ . ExpIsUsable AND LOpnd2 ^ . ExpIsUsable
      ; LNewExpr ^ . ExpReachedDeclNoSet
        := IntSets . Union
             ( LOpnd1 ^ . ExpReachedDeclNoSet 
             , LOpnd2 ^ . ExpReachedDeclNoSet
             )
      END (*IF*) 

    ; EVAL CheckOperation ( LNewExpr )
    ; RETURN LNewExpr
    END MaybeConvertCallToOperator



; BEGUIN
  END FM3SWem
.
