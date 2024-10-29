
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Reading and writing compressed internal token streams. *) 

INTERFACE FM3StreamUtils

; IMPORT RdBackFile 
; IMPORT FM3Base 
; IMPORT FM3Decls
; IMPORT FM3Globals 

; PROCEDURE PutBwdAtom ( RdBack : RdBackFile . T ; Atom : FM3Base . AtomTyp )

; PROCEDURE PutBwdPos
    ( RdBack : RdBackFile . T ; Position : FM3Base . tPosition )

; PROCEDURE GetBwdInt ( RdBack : RdBackFile. T ) : INTEGER  

; PROCEDURE GetBwdScopeNo ( RdBack : RdBackFile . T ) : FM3Globals . ScopeNoTyp 

; PROCEDURE GetBwdAtom ( RdBack : RdBackFile. T ) : FM3Base . AtomTyp 

; PROCEDURE GetBwdPos ( RdBack : RdBackFile . T ) : FM3Base . tPosition 

; PROCEDURE GetBwdDeclKind ( RdBack : RdBackFile . T ) : FM3Decls . DeclKindTyp 

; END FM3StreamUtils
.
  