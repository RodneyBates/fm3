
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2025        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

(* Things to help looking at internal operations.
   Suitable for calling within a debugger.
*)

(* W A R N I N G ! ! -------------------------
  The code in this file is intended to be called by m3gdb commands.
  There may not be any calls on it in compiled code.  But if it is not
  named in any IMPORT in the export/import closure, the compiler will
  not set up global variable addressing for it in the way that m3gdb
  depends on.  As of now 2025-3-20, the necessary IMPORT is in FM3.m3.
  Don't delete it even though It is not needed for compiling.
*) 

MODULE FM3Introspection

; IMPORT Fmt 
; IMPORT Stdio
; IMPORT Text 
; IMPORT TextWr  
; IMPORT Wr 

; IMPORT IntSets 
; IMPORT VarArray_Int_Refany

; IMPORT FM3Base
; IMPORT FM3Decls
; IMPORT FM3Globals
; IMPORT FM3Graph
; IMPORT FM3Scopes
; IMPORT FM3SharedUtils 
; IMPORT FM3Units 

(*EXPORTED.*)
; PROCEDURE CurrentSrcFileName ( ) : TEXT 

  = VAR LUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN (*CurrentSrcFileName*)
      LUnitRef := FM3Units . UnitStackTopRef 
    ; IF LUnitRef = NIL
      THEN RETURN "No current unit." 
      ELSE RETURN LUnitRef ^ . UntSrcFileSimpleName
      END (*IF*) 
    END CurrentSrcFileName 

(*EXPORTED.*)
; PROCEDURE DeclNoRef
    ( DeclNo : FM3Globals . DeclNoTyp 
    ; UnitNo : FM3Globals . UnitNoTyp := FM3Globals . UnitNoNull 
      (* ^UnitNoNull implies current unit. *) 
    )
  : FM3Decls . DeclRefTyp

  = VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LDeclMap : FM3Base . MapTyp
  ; VAR LDeclRef : FM3Decls . DeclRefTyp

  ; BEGIN (*DeclNoRef*)
      IF UnitNo = FM3Globals . UnitNoNull 
      THEN LUnitRef := FM3Units . UnitStackTopRef
      ELSE LUnitRef := FM3Units . UnitNoRef ( UnitNo )
      END (*IF*) 
    ; IF LUnitRef = NIL
      THEN RETURN NIL  
      END (*IF*)
    ; LDeclMap := LUnitRef ^ . UntDeclMap 
    ; IF LDeclMap = NIL
      THEN RETURN NIL  
      END (*IF*)
    ; LDeclRef(* Implicit NARROW. *) 
        := VarArray_Int_Refany . Fetch
             ( FM3Units . UnitStackTopRef ^ . UntDeclMap , DeclNo )
    ; RETURN LDeclRef 
    END DeclNoRef

(*EXPORTED.*)
; PROCEDURE DeclNoRefImage ( DeclNo : FM3Globals . DeclNoTyp ) : TEXT  

  = VAR LDeclRef : FM3Decls . DeclRefTyp
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclNoRefImage*)
      LDeclRef := DeclNoRef ( DeclNo ) 
    ; LResult := FM3Decls . DeclNoImage ( LDeclRef )
    ; Wr . PutText ( Stdio . stderr , LResult )
    (* ^Because m3gdb will escape the NLs and run all together. *) 
    ; RETURN LResult 
    END DeclNoRefImage

(*EXPORTED.*)
; PROCEDURE ScopeDeclsInfo ( ) : TEXT
  (* For the current Decl scope. *) 

  = VAR LUnitRef : FM3Units .UnitRefTyp
  ; VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LResult : TEXT 

  ; BEGIN (*ScopeDeclsInfo*)
      LUnitRef := FM3Units . UnitStackTopRef
    ; IF LUnitRef = NIL THEN RETURN "<NoUnitRef>" END (*IF*)
    ; LScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; IF LScopeRef = NIL THEN RETURN "<NIL Scope>" END (*IF*)
    ; FOR RDeclNo
            := LScopeRef ^ . ScpMinDeclNo
               TO LScopeRef ^ . ScpMinDeclNo + LScopeRef ^ . ScpDeclCt - 1
      DO
        Wr . PutText ( Stdio . stderr , "Decl No " ) 
      ; Wr . PutText ( Stdio . stderr , Fmt . Int ( RDeclNo ) ) 
      ; Wr . PutText ( Stdio . stderr , " " ) 
      ; Wr . PutText
          ( Stdio . stderr
          , FM3Decls . DeclInfoImage
              ( VarArray_Int_Refany . Fetch
                  ( LUnitRef ^ . UntDeclMap , RDeclNo )
              )
          )
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      END (*FOR*) 
    ; Wr . Flush ( Stdio . stderr ) 
    ; RETURN LResult 
    END ScopeDeclsInfo

(*EXPORTED.*)
; PROCEDURE DeclNoTypImage ( DeclNo : FM3Globals . DeclNoTyp ) : TEXT 

  = VAR LDeclRef : FM3Decls . DeclRefTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (*DeclNoTypImage*)
      LDeclRef := DeclNoRef ( DeclNo ) 
    ; LResult := FM3Decls . DeclTypImage ( LDeclRef )
    ; Wr . PutText ( Stdio . stderr , LResult ) 
    (* ^Because m3gdb will escape the NLs and run the lines together. *) 
    ; RETURN ""
    END DeclNoTypImage

(*EXPORTED.*)
; PROCEDURE GraphArc ( Pred , Succ : INTEGER ) : INTEGER
  (* In the graph of the current decl scope. *) 
  (* Scope-relative Pred & Succ. *) 
  (* < 0 => something was invalid. *) 

  = VAR LScope : FM3Scopes . ScopeRefTyp
  ; VAR LResult : INTEGER 

  ; BEGIN (*GraphArc*)
      LScope := FM3Scopes . DeclScopeStackTopRef
    ; IF LScope = NIL THEN RETURN -1 END (*IF*)
    ; TRY 
        LResult := FM3Graph . MakeArc ( LScope ^ . ScpDeclGraph , Pred , Succ )
      EXCEPT FM3Graph . BadNodeNo
      => RETURN -2
      END (*EXCEPT*)
    ; RETURN LResult 
    END GraphArc

; PROCEDURE RangeCheckGraphNodeNo
    ( ScopeRef : FM3Scopes . ScopeRefTyp ; GraphNodeNo : INTEGER ; Tag : TEXT  )
  : TEXT
  (* "" means it's OK, otherwise it's a message. *) 

  = VAR LScopeNodeNo : INTEGER
  ; LResult : TEXT 

  ; BEGIN (*RangeCheckGraphNodeNo*)
      LScopeNodeNo := GraphNodeNo + ScopeRef ^ . ScpMinDeclNo 
    ; IF LScopeNodeNo < ScopeRef ^ . ScpMinDeclNo
         OR LScopeNodeNo > ScopeRef ^ . ScpMinDeclNo + ScopeRef ^ . ScpDeclCt
      THEN LResult := FM3SharedUtils . CatArrT
             ( ARRAY OF REFANY
                 { "(Scope-relative) "
                 , Tag
                 , " node no "
                 , Fmt . Int ( LScopeNodeNo )
                 , " out of range ["
                 , Fmt . Int ( ScopeRef ^ . ScpMinDeclNo ) 
                 , ".." 
                 , Fmt . Int
                     ( ScopeRef ^ . ScpMinDeclNo + ScopeRef ^ . ScpDeclCt ) 
                 , "] " 
                 }
             ) 
      ; RETURN LResult
      ELSE RETURN ""
      END (*IF*) 
    END RangeCheckGraphNodeNo

(*EXPORTED.*)
; PROCEDURE GraphArcImage ( Arc : FM3Graph . ArcTyp ) : TEXT 
  (* In the current open scope. *) 

  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LPredNodeNo : INTEGER (* Graph-relative *)   
  ; VAR LSuccNodeNo : INTEGER (* Graph-relative *) 
  ; VAR LPredDeclNo : INTEGER (* Unit-relative *)   
  ; VAR LSuccDeclNo : INTEGER (* Unit-relative *)
  ; VAR LPredDeclRef : FM3Decls . DeclRefTyp 
  ; VAR LSuccDeclRef : FM3Decls . DeclRefTyp
  ; VAR LPredImage : TEXT 
  ; VAR LSuccImage : TEXT 
  ; VAR LResult : TEXT

  ; BEGIN (*GraphArcImage*)
      LScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; IF LScopeRef = NIL THEN RETURN "<NIL Scope>" END (*IF*)
    ; LPredNodeNo := FM3Graph . PredNodeNo ( LScopeRef . ScpDeclGraph , Arc ) 
    ; LSuccNodeNo := FM3Graph . SuccNodeNo ( LScopeRef . ScpDeclGraph , Arc ) 
    ; LResult := RangeCheckGraphNodeNo ( LScopeRef , LPredNodeNo , "Pred" ) 
    ; LResult 
         := LResult & RangeCheckGraphNodeNo ( LScopeRef , LSuccNodeNo , "Succ" )
    ; IF Text . Equal ( LResult , "" ) 
      THEN
        LPredDeclNo := LPredNodeNo + LScopeRef ^ . ScpMinDeclNo 
      ; LSuccDeclNo := LSuccNodeNo + LScopeRef ^ . ScpMinDeclNo 
      ; LPredDeclRef := DeclNoRef ( LPredDeclNo )
      ; LSuccDeclRef := DeclNoRef ( LSuccDeclNo ) 
      ; IF LPredDeclRef = NIL
        THEN LPredImage := Fmt . Int ( LPredDeclNo )
        ELSE LPredImage := FM3Decls . DeclInfoImage ( LPredDeclRef )
        END (*IF*) 
      ; IF LSuccDeclRef = NIL
        THEN LSuccImage := Fmt . Int ( LSuccDeclNo )
        ELSE LSuccImage := FM3Decls . DeclInfoImage ( LSuccDeclRef )
        END (*IF*) 
      ; LResult := FM3SharedUtils . CatArrT
          ( ARRAY OF REFANY
              { "("
              , LPredImage  
              , " ----> " 
              , LSuccImage 
              , ")"
              }
          )
      END (*IF*) 
    ; RETURN LResult 
    END GraphArcImage

(*EXPORTED.*)
; PROCEDURE GraphImage ( ) : TEXT 
  (* Of the current open scope. *) 

  = VAR LScopeRef : FM3Scopes . ScopeRefTyp
  ; VAR LGraphRef : FM3Graph . GraphTyp
  ; VAR LTextWrT : TextWr . T
  ; VAR GiPrefix : TEXT 
  ; VAR LResult : TEXT

  ; PROCEDURE GiVisit ( Arc : FM3Graph . ArcTyp )

    = BEGIN
        Wr . PutText ( LTextWrT , GiPrefix ) 
      ; Wr . PutText ( LTextWrT , GraphArcImage ( Arc ) ) 
      ; Wr . PutText ( LTextWrT , Wr . EOL ) 
      ; GiPrefix := ", "
      END GiVisit 

  ; BEGIN (*GraphImage*)
      LScopeRef := FM3Scopes . OpenScopeStackTopRef
    ; IF LScopeRef = NIL THEN LResult := "<NIL OpenScope>"
      ELSE 
        LGraphRef := LScopeRef ^ . ScpDeclGraph
      ; IF LGraphRef = NIL THEN LResult := "NIL"
        ELSIF FM3Graph . ArcCt ( LGraphRef ) = 0
        THEN LResult := "{ }"
        ELSE 
          LTextWrT := TextWr . New ( )
        ; GiPrefix := "{ "
        ; FM3Graph . ForAllArcsDo ( LGraphRef , GiVisit )  
        ; Wr . PutText ( LTextWrT , "}" ) 
        ; Wr . PutText ( LTextWrT , Wr . EOL ) 
        ; LResult := TextWr . ToText ( LTextWrT )
        END (*IF*)
      END (*IF*) 
    ; Wr . PutText ( Stdio . stderr , LResult )
    (* ^Because m3gdb will escape the NLs and run all together. *) 
    ; RETURN ""
    END GraphImage

(*EXPORTED.*)
; PROCEDURE IntSetsImage ( Set : IntSets . T  ) : TEXT

  = VAR LResult : TEXT 

  ; BEGIN (*IntSetsImage*)
      LResult
        := FM3SharedUtils . IntSetsImage
             ( Set , LinePrefix := "" , MaxLine := 72 )
    ; Wr . PutText  ( Stdio . stderr , LResult )
    ; RETURN ""
    END IntSetsImage

; BEGIN (*FM3Introspection*)
  END FM3Introspection
  .


