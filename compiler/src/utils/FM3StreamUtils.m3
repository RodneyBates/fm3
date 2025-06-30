
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3StreamUtils

; IMPORT FM3Base 
; IMPORT FM3Compress 
; IMPORT FM3Decls
; IMPORT FM3Globals
; IMPORT FM3Parser 

; IMPORT RdBackFile 

(*EXPORTED.*)
; PROCEDURE PutBwdAtom ( RdBack : RdBackFile . T ; Atom : FM3Base . AtomTyp )

(* Sheesh.  Is this one really worth it? *) 
  = BEGIN (*PutBwdAtom*)
      FM3Compress . PutBwd ( RdBack , VAL ( Atom , LONGINT ) ) 
    END PutBwdAtom

(*EXPORTED.*)
; PROCEDURE PutBwdPos
    ( RdBack : RdBackFile . T ; Position : FM3Base . tPosition )

  = BEGIN (*PutBwdPos*)
      FM3Compress . PutBwd ( RdBack , VAL ( Position . Column , LONGINT ) ) 
    ; FM3Compress . PutBwd ( RdBack , VAL ( Position . Line , LONGINT ) ) 
    END PutBwdPos

(*EXPORTED.*)
; PROCEDURE GetBwdInt ( RdBack : RdBackFile. T ) : INTEGER  

  = VAR LResult : INTEGER 

  ; BEGIN (*GetBwdInt*)
      LResult := VAL ( FM3Compress . GetBwd ( RdBack ) , INTEGER ) 
    ; RETURN LResult 
    END GetBwdInt
    
(*EXPORTED.*)
; PROCEDURE GetBwdScopeNo ( RdBack : RdBackFile . T ) : FM3Globals . ScopeNoTyp 

  = VAR LResult : FM3Globals . ScopeNoTyp 

  ; BEGIN (*GetBwdScopeNo*)
      LResult
        := VAL ( FM3Compress . GetBwd ( RdBack ) , FM3Globals . ScopeNoTyp ) 
    ; RETURN LResult 
    END GetBwdScopeNo 

(*EXPORTED.*)
; PROCEDURE GetBwdAtom ( RdBack : RdBackFile. T ) : FM3Base . AtomTyp 

  = VAR LResult : FM3Base . AtomTyp 

  ; BEGIN (*GetBwdAtom*)
      LResult := VAL ( FM3Compress . GetBwd ( RdBack ) , FM3Base . AtomTyp ) 
    ; RETURN LResult 
    END GetBwdAtom

; VAR GLastPos : FM3Base . tPosition 
    
(*EXPORTED.*)
; PROCEDURE GetBwdPos ( RdBack : RdBackFile . T ) : FM3Base . tPosition 

  = VAR LResult : FM3Base . tPosition

  ; BEGIN (*GetBwdPos*)
      LResult . Line
        := VAL ( FM3Compress . GetBwd ( RdBack ) , FM3Base . M2SHORTCARD )  
    ; LResult . Column 
        := VAL ( FM3Compress . GetBwd ( RdBack ) , FM3Base . M2SHORTCARD )
    ; GLastPos := LResult 
    ; RETURN LResult 
    END GetBwdPos
    
(*EXPORTED.*)
; PROCEDURE GetBwdBool ( RdBack : RdBackFile . T ) : BOOLEAN 

  = VAR LValueL : LONGINT 

  ; BEGIN (*GetBwdBool*)
      LValueL := FM3Compress . GetBwd ( RdBack )
    ; IF LValueL = 0L THEN RETURN FALSE
      ELSE RETURN TRUE
      END (*IF*) 
    END GetBwdBool
    
(*EXPORTED.*)
; PROCEDURE GetBwdDeclKind ( RdBack : RdBackFile . T ) : FM3Decls . DeclKindTyp 

  = VAR LResult : FM3Decls . DeclKindTyp 

  ; BEGIN (*GetBwdDeclKind*)
      LResult := VAL ( FM3Compress . GetBwd ( RdBack ) , FM3Decls . DeclKindTyp )
    ; RETURN LResult 
    END GetBwdDeclKind 

(*EXPORTED.*)
; PROCEDURE GetBwdBrandKind ( RdBack : RdBackFile . T )
  : FM3Parser . BrandKindTyp 

  = VAR LResult : FM3Parser . BrandKindTyp 

  ; BEGIN (*GetBwdBrandKind*)
      LResult
        := VAL ( FM3Compress . GetBwd ( RdBack ) , FM3Parser . BrandKindTyp )
    ; RETURN LResult 
    END GetBwdBrandKind 

; BEGIN (*FM3StreamUtils*)
  END FM3StreamUtils
.
  