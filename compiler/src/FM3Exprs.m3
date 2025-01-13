
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Exprs

; IMPORT IntRanges
; IMPORT TextWr 
; IMPORT VarArray_Int_Refany
; IMPORT Wr 

; IMPORT FM3Globals


(* EXPORTED.*) 
; PROCEDURE ExprKindMessage ( Kind : ExprKindTyp ) : TEXT
  (* These are for constructing user messages. *) 

  = BEGIN
      CASE Kind OF 
      | Ekt . EkNull
       => RETURN "<null>" 
      | Ekt . EkType 
       => RETURN "type" 
      | Ekt . EkProc 
       => RETURN "procedure" 
      | Ekt . EkFunc 
       => RETURN "function" 
      | Ekt . EkValue 
       => RETURN "value" 
      | Ekt . EkConst 
       => RETURN "constant" 
      | Ekt . EkRef 
       => RETURN "reference" 
      ELSE RETURN "<unknown>"
      END (*CASE *) 
    END ExprKindMessage 

(* EXPORTED.*) 
; PROCEDURE KindSetCard ( KindSet : ExprKindSetTyp ) : INTEGER 

  = VAR LCt : INTEGER

  ; BEGIN
      LCt := 0 
    ; FOR RK := FIRST ( ExprKindTyp ) TO LAST ( ExprKindTyp )
      DO IF RK IN KindSet THEN INC ( LCt ) END (*IF*) 
      END (*FOR*)
    ; RETURN LCt
    END KindSetCard

(* EXPORTED.*) 
; PROCEDURE ExprKindSetMessage ( KindSet : ExprKindSetTyp ) : TEXT

  = VAR LWrT : Wr . T
  ; VAR LResult : TEXT 
  ; VAR LCt , LNo : INTEGER

  ; BEGIN 
      LCt := KindSetCard ( KindSet ) 
    ; IF LCt = 0 THEN RETURN "<nothing>" END (*IF*) 
    ; LWrT := NEW ( TextWr . T )
    ; Wr . PutText ( LWrT , "one of " ) 
    ; FOR RK := FIRST ( ExprKindTyp ) TO LAST ( ExprKindTyp )
      DO IF RK IN KindSet
        THEN
          IF LCt = 1 THEN RETURN "a " & ExprKindMessage ( RK ) END (*IF*)
        ; LNo := ORD ( RK )
        ; IF LNo + 1 <= LCt THEN Wr . PutText ( LWrT , ", " ) END (*IF*) 
        ; IF LNo + 1 = LCt THEN Wr . PutText ( LWrT , "or " ) END (*IF*)
        ; Wr . PutText ( LWrT , ExprKindMessage ( RK ) )
        END (*IF*) 
      END (*FOR*)
    ; LResult := TextWr . ToText ( LWrT)
    ; RETURN LResult
    END ExprKindSetMessage 

(* Types in the compiled code, not in the compiler. *)

(* EXPORTED.*) 
; PROCEDURE ResolveNow
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp
  = BEGIN
      IF Expr = NIL THEN RETURN Est . EsUnknown END (*IF*)
    ; CASE Expr . ExpState OF 
      | Est . EsUnresolved  
      => RETURN Expr . resolve ( ExprKind )
      | Est . EsResolving
      => <* ASSERT FALSE , "Illegal recursive declaration." *>
      ELSE RETURN Expr . ExpState
      END (*CASE*) 
    END ResolveNow

(* EXPORTED.*) 
; PROCEDURE ResolveEventually
    ( Expr : ExprTyp ; ExprKind : ExprKindTyp ) : ExprStateTyp 

  = BEGIN
      IF Expr = NIL THEN RETURN Est . EsUnknown END (*IF*)
    ; IF Expr . ExpState = Est . EsUnresolved  
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
      | ExprIntTypeTyp
      , ExprAddrTypeTyp (* Can happen in UNSAFE units. *) 
      , ExprFloatTypeTyp
      => RETURN TRUE
      ELSE RETURN FALSE
      END (* TYPECASE*)
    END IsNumericType 
      

; BEGIN
  END FM3Exprs
.

