
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3ParsePass

; IMPORT FM3Base
; IMPORT FM3Decls 
; IMPORT FM3Units 
; IMPORT FM3Scanner
; IMPORT FM3Scopes 
; IMPORT FM3IntToks AS Itk  

  (* Lalr mandates this type, by name, and its 'Scan' field, Q.V. *) 
; TYPE tParsAttribute
    = RECORD
        Scan : FM3Scanner . tScanAttribute
      ; PaUnnestCoord : LONGINT  
      ; PaLong : LONGINT 
      ; PaConstructNo : INTEGER
      ; PaListItemNo : INTEGER
      ; PaInt : INTEGER
      ; PaTok : FM3Base . TokTyp 
      ; PaByte : [ 0 .. 16_FF ] 
      ; PaBool : BOOLEAN 
      END (* tParsAttribute *)

; CONST ParsAttrNull
    = tParsAttribute
        { Scan := FM3Scanner . ScanAttrNull
        , PaUnnestCoord := FIRST ( LONGINT ) 
        , PaLong := FIRST ( LONGINT )
        , PaConstructNo := FIRST ( INTEGER ) 
        , PaListItemNo := FIRST ( INTEGER )
        , PaInt := FIRST ( INTEGER )
        , PaTok := FM3Base . TokNull
        , PaByte := 16_FF 
        , PaBool := FALSE
        }

(* ---------------------------- Unnest stack ------------------------ *)

; PROCEDURE StartSkipping ( ) : CARDINAL (* depth after. *)

; PROCEDURE StopSkipping ( ) : CARDINAL (* depth before. *)

; PROCEDURE UnnestCoord ( ) : LONGINT
  (* Of the current unit. *)
  
; PROCEDURE PushUnnestStk ( READONLY ParsAttr : tParsAttribute )
  
; PROCEDURE PushUnnest ( Value : INTEGER )
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  (* Zero args. *)

; PROCEDURE Push_ListSepPatchPos
    ( ListT : Itk . TokTyp ; C : LONGINT ; Position : FM3Scanner . tPosition )

(* Here is the naming code, of namings of these Push_<x> procedures:

   Each of the Push_<x> procedures pushes a list of things on the unnest stack.
   The letters after the "_" encode what that is.  The letters in the code and
   the parameters are in left-to-right order, for ease of thought but the actual
   pushing is in the reverse order, because this stuff needs to be popped
   backwards.

   A capital letter denotes a value that is passed to the Push_ procedure as
   a parameter.  The parameters are in the same order as the capital letters.
   A lower case letter denotes a value derived from the previous case-
   homonym of itself.

   For a capital-letter token, the token parameter is always the left token
   of a group of related tokens.  For a lower-case-letter token, the procedure
   uses the previously passed in token as a base, converting as for a capital.

   The specific letter denotes which member of the token group, as follows: 

     L,l  Left token
     R,r  Right token
     E,e  1st infix token
     Z,z  2nd infix token
     D,d  3rd infix token
     V,v  4th infix token
     F,f  5th infix token

   A 'C' or 'c' denotes a patch coordinate, passed in ('C'), or copied from
   from the previous coordinate ('c').  If present, it must immediately follow
   a token, and it causes the push procedure to convert the preceding token
   to its patch counterpart.

   A 'P' or 'p' denotes a position in the source code.  When passed in, it
   is a single parameter, of type tPosition, a two-field record of line-number
   and column=number.  The procedure pushes it as two separate numbers on the
   unnest stack, column number deeper.

   A "I' or 'i' is an integer value. A 'B' or 'b' is a boolean value.
*) 

; PROCEDURE Push_L ( T : Itk . TokTyp )

; PROCEDURE Push_LP ( T : Itk . TokTyp ; Position : FM3Scanner . tPosition )

; PROCEDURE Push_LCr ( T : Itk . TokTyp ; C : LONGINT )

; PROCEDURE Push_LCP_rp
   ( T : Itk . TokTyp ; C : LONGINT ; Position : FM3Scanner . tPosition )

; PROCEDURE Push_LCPI_rpi
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; Position : FM3Scanner . tPosition 
    ; I : INTEGER 
    )

; PROCEDURE Push_LCPeCrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; PositionLt : FM3Scanner . tPosition
   ; COne : LONGINT
   ; PositionRt : FM3Scanner . tPosition
   )

; PROCEDURE Push_LCP_eCP_zCP_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; PL : FM3Scanner . tPosition
   ; Ce : LONGINT
   ; Pe : FM3Scanner . tPosition
   ; Cz : LONGINT
   ; Pz : FM3Scanner . tPosition
   ; Pr : FM3Scanner . tPosition
   )

; PROCEDURE Push_LCPeCprp
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; CInfix : LONGINT 
   ; PositionInfix : FM3Scanner . tPosition
   )

; PROCEDURE Push_ECPrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; PositionOne : FM3Scanner . tPosition
   ; PositionRt : FM3Scanner . tPosition
   )

; PROCEDURE Push_LCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

; PROCEDURE Push_LCIri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

; PROCEDURE Push_LI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

; PROCEDURE Push_LI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

; PROCEDURE Push_LCeCr ( T : Itk . TokTyp ; Ct , Co : LONGINT )

; PROCEDURE Push_LCIeCri
    ( T : Itk . TokTyp ; Ct : LONGINT ; I : INTEGER ; Co : LONGINT )

; PROCEDURE PushEXPORTSMain  ( READONLY Position : FM3Scanner . tPosition )

; PROCEDURE Pop4 ( )

; PROCEDURE Pop8 ( )

; PROCEDURE RequireTypeAndOrValue 
    ( Position : FM3Scanner . tPosition
    ; HasType : BOOLEAN 
    ; HasValue : BOOLEAN
    ) 
  (* Anything that requires a type and/or value: 
     variable , formal, field.  Gets DeclKnd from FM3Decls.TopDeclInfo. *) 

; PROCEDURE MakeList
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY ElemsAttr : tParsAttribute
      (* ^For unnest coordinate and elem count. *) 
    )

; PROCEDURE MakeListPos
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; Position : FM3Scanner . tPosition 
    ; READONLY ElemsAttr : tParsAttribute 
    )

; PROCEDURE MakeListPatch
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; PatchCoord : LONGINT
    ; ElemCt : INTEGER 
    )

(* ----------------------------- Parsing actions -------------------------- *)

; PROCEDURE ImportsLt (  )

; PROCEDURE ImportsRt (  )

; PROCEDURE Import
    ( Atom : FM3Base . AtomTyp ; Pos : FM3Base . tPosition ) 

; PROCEDURE FromImport
    ( IntfAtom : FM3Base . AtomTyp
    ; InftPos : FM3Base . tPosition
    ; DeclAtom : FM3Base . AtomTyp
    ; DeclPos : FM3Base . tPosition
    )

; PROCEDURE BeginBlock ( )

; PROCEDURE ScopeEmpty ( ScopeKind : FM3Scopes . ScopeKindTyp )

; PROCEDURE ScopeLtL2R
    ( ScopeKind : FM3Scopes . ScopeKindTyp ; Position : FM3Base . tPosition )
  : FM3Base . ScopeNoTyp (* Scope no. that was created. *) 

; PROCEDURE DeclIdL2R ( READONLY Attribute : tParsAttribute )
  : BOOLEAN (* Use this declared id.  It's not a duplicate in current scope. *)
  (* PRE: Attribute is for an identifier in a declaration context. *) 

(* Needed?
; PROCEDURE RefIdL2R
    ( RefIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
*)

; PROCEDURE ScopeRtL2R ( )
  (* Create an identifier-to-declNo dictionary for the scope, of
     exactly the needed size, and load it up with DeclIdAtom to
     DeclNo mappings, using the idents declared in the scope and
     a contiguously-numbered range of DeclNos.
  *) 

(* Not exported: 
; PROCEDURE ScopeRtR2L ( ScopeNo : FM3Base . ScopeNoTyp )

; PROCEDURE DuplDeclR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Base . DeclNoTyp 

; PROCEDURE DeclRtR2L
    ( DeclIdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition
    ; DeclKind : FM3Decls . DeclKindTyp 
    )
  : FM3Base . DeclNoTyp

; PROCEDURE DeclLtR2L
    ( DeclIdAtom : FM3Base . AtomTyp
    ; Position : FM3Base . tPosition
    ; DeclKind : FM3Decls . DeclKindTyp 
    )
  : FM3Base . DeclNoTyp
  (* May be 1st or a later duplicate decl of DeclIdAtom *)

; PROCEDURE RefIdR2L
    ( RefIdAtom : FM3Base . AtomTyp ; Position : FM3Base . tPosition )
  : FM3Base . DeclNoTyp 

; PROCEDURE ScopeLtR2L ( ScopeNo : FM3Base . ScopeNoTyp )

*) 

(* These are not called. 
; PROCEDURE PushUnnestTokPatch0 ( Token : LONGINT )
  (* To be patched.  No additional args. *) 

; PROCEDURE PushUnnestTok1 ( Token : LONGINT )
  (* One arg. *) 

; PROCEDURE PushUnnestTokPatch1 ( Token : LONGINT )
  (* To be patched.  One additional arg. *) 

; PROCEDURE PushTok ( Tok : Itk . TokTyp ; Arg0 : LONGINT )

; PROCEDURE PushTokPatch ( Tok : Itk . TokTyp ; Arg0 , Arg1 : LONGINT )
*) 

; PROCEDURE Run ( ) 

; END FM3ParsePass
.


