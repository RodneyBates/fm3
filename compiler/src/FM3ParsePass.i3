
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2024  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3ParsePass

; IMPORT FM3Base
; FROM FM3Base IMPORT tPosition 
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
      ; PaTok1 : FM3Base . TokTyp 
      ; PaTok2 : FM3Base . TokTyp 
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
        , PaTok1 := FM3Base . TokNull
        , PaTok2 := FM3Base . TokNull
        , PaByte := 16_FF 
        , PaBool := FALSE
        }

(* ------------------------ Skipping erroneous code ----------------- *)

; VAR SkipDepth : INTEGER := 0
  (* Zero means we are not skipping *) 

; PROCEDURE StartSkipping ( ) : CARDINAL (* depth after. *)

; PROCEDURE StopSkipping ( ) : CARDINAL (* depth before. *)

(* ---------------------------- Unnest stack ------------------------ *)

; PROCEDURE CheckUnitEndId
    ( READONLY StartAttr : tParsAttribute 
    ; VAR EndAttr : tParsAttribute 
    ; UnitKind : FM3Units . UnitKindTyp
    )

; PROCEDURE UnnestCoord ( ) : LONGINT
  (* Of the current unit. *)
  
; PROCEDURE PushUnnestStk ( READONLY ParsAttr : tParsAttribute )
  
; PROCEDURE PushUnnest ( Value : INTEGER )
; PROCEDURE PushUnnestLong ( Value : LONGINT )
  (* Zero args. *)

; PROCEDURE Push_ListSepPatchPos
    ( ListTokLt : Itk . TokTyp
    ; C : LONGINT
    ; ElemNo : INTEGER
    ; READONLY Position : tPosition
    )

(* Here is the naming code, of namings of these Push_<x> procedures:

   Each of the Push_<x> procedures pushes a list of things on the unnest stack.
   The letters after the "_" encode what that is.  The letters in the code and
   the parameters are in left-to-right order, for ease of thought, but the actual
   pushing is temporarily in the reverse order, because this stuff needs to be
   popped backwards.

   A capital letter denotes a value that is passed to the Push_<x> procedure as
   a parameter.  The parameters are in the same order as the capital letters.
   A lower case letter denotes a value derived from the previous case-
   homonym of itself.

   For a capital-letter token, the token parameter is always the left token
   of a group of related tokens.  For a lower-case-letter token, the procedure
   uses the previously passed in token as a base, converting as for a capital.

   The specific letter denotes which member of the token group, as follows: 

     L,l  Left token
     R,r  Right token
     E,e  1st infix token or list separator token
     Z,z  2nd infix token
     D,d  3rd infix token
     V,v  4th infix token
     F,f  5th infix token

   'C' or 'c' denotes a patch coordinate, passed in ('C'), or copied from
   from the previous coordinate ('c').  If present, it must immediately follow
   a token, and it causes the push procedure to convert the preceding token
   to its patch counterpart.

   'P' or 'p' denotes a position in the source code.  When passed in, it
   is a single parameter, of type tPosition, a two-field record of line-number
   and column-number.  The procedure pushes it as two separate numbers on the
   unnest stack, column number deeper.

   'I' or 'i' is an integer value.  'B' or 'b' is a boolean value.
*) 

; PROCEDURE Push_L ( T : Itk . TokTyp )

; PROCEDURE Push_LP ( T : Itk . TokTyp ; READONLY Position : tPosition )

; PROCEDURE Push_LIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

; PROCEDURE Push_LIP_rip
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

; PROCEDURE Push_EIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

; PROCEDURE Push_ECIP
    ( T : Itk . TokTyp
    ; Coord : LONGINT
    ; I : INTEGER
    ; READONLY Position : tPosition
    )

; PROCEDURE Push_LCr ( T : Itk . TokTyp ; C : LONGINT )

; PROCEDURE Push_LCP_rp
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY Position : tPosition )

; PROCEDURE Push_LCIP_rip
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I : INTEGER 
    ; READONLY Position : tPosition 
    )

; PROCEDURE Push_LCPI_rpi
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; READONLY Position : tPosition 
    ; I : INTEGER 
    )

; PROCEDURE Push_LCP_eCP_rP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionLt : tPosition
   ; CEins : LONGINT
   ; READONLY PositionEins : tPosition
   ; READONLY PositionRt : tPosition
   )

; PROCEDURE Push_LCP_eCP_zCP_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; READONLY PL : tPosition
   ; Ce : LONGINT
   ; READONLY Pe : tPosition
   ; Cz : LONGINT
   ; READONLY Pz : tPosition
   ; READONLY Pr : tPosition
   )

; PROCEDURE Push_LCPeCprp
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; CInfix : LONGINT 
   ; READONLY PositionInfix : tPosition
   )

; PROCEDURE Push_ECIP_riP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; I : INTEGER 
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

; PROCEDURE Push_ECPrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

; PROCEDURE Push_LCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

; PROCEDURE Push_LCIri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

; PROCEDURE Push_LI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

; PROCEDURE Push_LI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

; PROCEDURE Push_LCeCr ( T : Itk . TokTyp ; Ct , Co : LONGINT )

; PROCEDURE Push_LCIeCri
    ( T : Itk . TokTyp ; Ct : LONGINT ; I : INTEGER ; Co : LONGINT )

; PROCEDURE Pop4 ( )

; PROCEDURE Pop8 ( )

; PROCEDURE RequireTypeAndOrValue 
    ( READONLY Position : tPosition
    ; HasType : BOOLEAN 
    ; HasValue : BOOLEAN
    ) 
  : BOOLEAN (* OK *)
  (* Anything that requires a type and/or value: variable , formal, field. 
     Check and maybe emit message. 
     Gets DeclKind from FM3Decls.TopDeclInfo. 
  *) 

; PROCEDURE MakeListPos
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY Position : tPosition 
    ; READONLY ElemsAttr : tParsAttribute 
    )

(* ----------------------------- Parsing actions -------------------------- *)

; PROCEDURE ImportsLt (  )

; PROCEDURE ImportsRt (  )

; PROCEDURE Import
    ( Atom : FM3Base . AtomTyp ; READONLY Position : tPosition ) 

; PROCEDURE FromImport
    ( IntfAtom : FM3Base . AtomTyp
    ; READONLY InftPos : tPosition
    ; DeclAtom : FM3Base . AtomTyp
    ; READONLY DeclPos : tPosition
    )

; PROCEDURE BeginBlock ( ) : FM3Base . ScopeNoTyp (* Created. *) 

; PROCEDURE EndBlock ( )

; PROCEDURE ScopeEmpty ( ScopeKind : FM3Scopes . ScopeKindTyp )

; PROCEDURE ScopeLtL2R
    ( ScopeKind : FM3Scopes . ScopeKindTyp ; READONLY Position : tPosition )
  : FM3Base . ScopeNoTyp (* Scope no. that was created. *) 

; PROCEDURE DeclIdL2R 
    ( DeclIdTok : Itk . TokTyp
    ; DeclKind : FM3Decls . DeclKindTyp  
    ; READONLY IdAttribute : tParsAttribute
    ; SepTok : Itk . TokTyp := Itk . ItkNull
                            (* ^Implies single decl id, not in a list. *)  
    ; READONLY SepPosition : tPosition := FM3Base . PositionNull 
    ; PriorIdCt : INTEGER := 0 (* Number of ids to left of this one. *)
    )
  : BOOLEAN (* Use this declared id.  (It's not predefined and not a duplicate
               in current scope.) *)
  (* PRE: IdAttribute is for an identifier in a declaration context. *) 

; PROCEDURE IdentRefL2R ( READONLY StkIdAttribute : tParsAttribute )

; PROCEDURE QualIdentL2R
    ( READONLY StkLtAttribute , StkRtAttribute : tParsAttribute ) 

; PROCEDURE ScopeRtL2R ( ScopeNo : FM3Scopes . ScopeNoTyp )
  (* Create an identifier-to-declNo dictionary for the scope, of
     exactly the needed size, and load it up with DeclIdAtom to
     DeclNo mappings, using the idents declared in the scope and
     a contiguously-numbered range of DeclNos.
  *) 

(* Not exported: 
; PROCEDURE ScopeRtR2L ( ScopeNo : FM3Base . ScopeNoTyp )

; PROCEDURE DuplDeclR2L
    ( DeclIdAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
  : FM3Base . DeclNoTyp 

; PROCEDURE DeclRtR2L
    ( DeclIdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ; DeclKind : FM3Decls . DeclKindTyp 
    )
  : FM3Base . DeclNoTyp

; PROCEDURE DeclLtR2L
    ( DeclIdAtom : FM3Base . AtomTyp
    ; READONLY Position : tPosition
    ; DeclKind : FM3Decls . DeclKindTyp 
    )
  : FM3Base . DeclNoTyp
  (* May be 1st or a later duplicate decl of DeclIdAtom *)

; PROCEDURE IdentRefR2L
    ( IdentRefAtom : FM3Base . AtomTyp ; READONLY Position : tPosition )
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


