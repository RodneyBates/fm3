
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Exprs

; IMPORT IntRanges 
; IMPORT VarArray_Int_Refany

; IMPORT FM3Base
; IMPORT FM3Globals 
; IMPORT FM3Units

; TYPE Es = ExprStateTyp

(* Types in the compiled code, not in the compiler. *)

(* EXPORTED.*) 
; PROCEDURE ResolveNow
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp
  = BEGIN
      IF Expr = NIL THEN RETURN Es . EsUnknown END (*IF*)
    ; CASE Expr . ExpState OF 
      | Es . EsUnresolved  
      => RETURN Expr . resolve ( ExprKind )
      | Es . EsResolving
      => <* ASSERT FALSE , "Illegal recursive declaration." *>
      ELSE RETURN Expr . ExpState
      END (*CASE*) 
    END ResolveNow

(* EXPORTED.*) 
; PROCEDURE ResolveEventually
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp 

  = BEGIN
      IF Expr = NIL THEN RETURN Es . EsUnknown END (*IF*)
    ; IF Expr . ExpState = Es . EsUnresolved  
      THEN RETURN Expr . resolve ( ExprKind )
      ELSE RETURN Expr . ExpState
      END (*IF*) 
    END ResolveEventually 

; PROCEDURE ResolveREFType ( Def : ExprTyp ) : BOOLEAN (* Is resolved. *)
  (* PRE : Def # NIL. *) 

  = BEGIN 
    END ResolveREFType


(*

         FM3Defs . ExpStackTopObj 
         := NEW ( FM3Defs . ExpREFTypeTyp


                , DefLink := FM3Defs . ExpStackTopObj 
                , DefExpKind := FM3Defs . ExpStackTopObj . ExpExpKind 
                )
*)


(* 
           WITH WReferent = FM3Exprs . GetAnPopExprStackTopRef
                , WREFType = FM3Exprs . TopExprStackTopRef 
           DO IF NOT WReferent . TgIsUsable
             THEN
               WREFType . TcReferent := NIL
             ELSIF NOT FM3Exprs . IsType ( WReferent )
             THEN
               FM3Messages . Error
                 ( "\"Referent\" of a REF type must be a type. (2.2.7). "
                 , WReferent . TcPosition
                 )
             ; WREFType . TcReferent := NIL
             ELSE
               WREFType . TcReferent := WReferent
             ; WReferent . TgIsUsable := TRUE
             ; WREFType . TcIsLegalRecursive L= TRUE 
             END (*IF*)
           END (*WITH*) 
*)

(*EXPORTED*) 
; PROCEDURE NewExprMap ( InitExprCt : FM3Globals . ExprNoTyp ) : ExprMapTyp
  (* One of these per Unit. *) 

  = BEGIN
      RETURN
        VarArray_Int_Refany . New
          ( NIL , IntRanges . RangeTyp {  0 , InitExprCt - 1 } )
    END NewExprMap

(*EXPORTED*) 
; PROCEDURE PushExprStack ( NewExpr : ExprTyp )

  = BEGIN
      NewExpr . ExpStackLink := ExprStackTopObj
    ; ExprStackTopObj := NewExpr 
    END PushExprStack 

(*EXPORTED*) 
; PROCEDURE PopExprStack ( ) : ExprTyp 

  = VAR LResult : ExprTyp 

  ; BEGIN
      LResult := ExprStackTopObj
    ; IF LResult # NIL
      THEN ExprStackTopObj := ExprStackTopObj . ExpStackLink
      END (*IF*) 
    ; RETURN LResult 
    END PopExprStack

(*EXPORTED*) 
; PROCEDURE IsNumericType ( Expr : ExprTyp ) : BOOLEAN 

  = BEGIN
      TYPECASE Expr OF
      | NULL => RETURN FALSE
      | ExprSubrTypeTyp ( TSubr )
      => RETURN IsNumericType ( TSubr . ExpRangeBase )
      | ExprLongTypeTyp
      , ExprIntTypeTyp
      , ExprRealTypeTyp
      , ExprLongRealTypeTyp
      , ExprExtendedTypeTyp
      => RETURN TRUE
      ELSE RETURN FALSE
      END (* TYPECASE*)
    END IsNumericType 
      

; BEGIN
  END FM3Exprs
.

