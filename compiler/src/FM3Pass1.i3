
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

INTERFACE FM3Pass1

; IMPORT FM3Base
; FROM FM3Base IMPORT tPosition 
; IMPORT FM3Decls 
; IMPORT FM3Globals 
; IMPORT FM3Units 
; IMPORT FM3Scanner
; IMPORT FM3Scopes 
; IMPORT FM3IntToks AS Itk  

  (* Lalr mandates this type, by name, and its 'Scan' field, Q.V. *)
(*FIXME: But why is it here? *) 
; TYPE tParsAttribute
    = RECORD
        Scan : FM3Scanner . tScanAttribute
      ; PaRefany : REFANY 
      ; PaPass1Coord : LONGINT  
      ; PaLong : LONGINT
      ; PaAtom : FM3Base . AtomTyp 
      ; PaConstructNo : INTEGER
      ; PaListItemNo : INTEGER
      ; PaInt : INTEGER
      ; PaPos : FM3Base . tPosition 
      ; PaTok1 : FM3Base . TokTyp 
      ; PaTok2 : FM3Base . TokTyp 
      ; PaByte : [ 0 .. 16_FF ] 
      ; PaBool : BOOLEAN 
      END (* tParsAttribute *)

; CONST ParsAttrNull
    = tParsAttribute
        { Scan := FM3Scanner . ScanAttrNull
        , PaRefany := NIL 
        , PaPass1Coord := FIRST ( LONGINT ) 
        , PaLong := FIRST ( LONGINT )
        , PaAtom := FM3Base . AtomNull 
        , PaConstructNo := FIRST ( INTEGER ) 
        , PaListItemNo := FIRST ( INTEGER )
        , PaInt := FIRST ( INTEGER )
        , PaPos := FM3Base . PositionNull 
        , PaTok1 := FM3Base . TokNull
        , PaTok2 := FM3Base . TokNull
        , PaByte := 16_FF 
        , PaBool := FALSE
        }

(* --------------------------- Signature kinds ---------------------- *)

; TYPE SigKindType
    = { SgkTypeDef
      , SgkMethodDef
      , SgkInterfaceProcDecl
      , SgkModuleProcDecl 
      } 

; PROCEDURE InterfaceId
    ( UnitRef : FM3Units . UnitRefTyp
    ; READONLY IdScanAttribute : FM3Scanner . tScanAttribute 
    )
  (*PRE: UnitRef # NIL *) 

; PROCEDURE ModuleId
    ( UnitRef : FM3Units . UnitRefTyp
    ; READONLY IdScanAttribute : FM3Scanner . tScanAttribute
    )
  (*PRE: UnitRef # NIL *) 
    

; PROCEDURE CheckUnitFinalId
    ( UnitRef : FM3Units . UnitRefTyp
    ; READONLY EndIdScanAttribute : FM3Scanner . tScanAttribute 
    ; UnitKind : FM3Units . UnitKindTyp
    )
    
(* ------------------------- Pass1 output file ---------------------- *)

; PROCEDURE Coord ( ) : LONGINT
  (* Of the current unit. *)
  
; PROCEDURE PutBwd_TextLit ( VAR(*READONLY*) ParsAttr : tParsAttribute )

; PROCEDURE PutBwd_WideTextLit ( VAR(*READONLY*) ParsAttr : tParsAttribute )

; PROCEDURE PutBwd_Attribute ( READONLY ParsAttr : tParsAttribute )
  
; PROCEDURE PutBwdInt ( Value : INTEGER )
; PROCEDURE PutBwdLong ( Value : LONGINT )
  (* Zero args. *)

; PROCEDURE PutBwd_ListSepPatchPos
    ( ListTokLt : Itk . TokTyp
    ; C : LONGINT
    ; ElemNo : INTEGER
    ; READONLY Position : FM3Base . tPosition
    )

(* Here is the naming code, of namings of these PutBwd_<x> procedures:

   Each of the PutBwd_<x> procedures pushes a list of things on the pass1
   output file.  The letters after the "_" encode what that is.  The letters
   in the naming code and the parameters are in left-to-right order, for
   ease of thought, but the actual pushing is in the reverse order, because
   this stuff will need to be popped backwards in pass 2.

   A capital letter denotes a value that is passed to the PutBwd_<x> procedure
   as a parameter.  The parameters are in the same order as the capital letters.
   A lower case letter denotes a value derived from the previous case-
   homonym of itself.

   For a capital-letter token, the token parameter is always the left token of
   a (possibly singleton) group of related tokens.  For a lower-case-letter
   token, the PutBwd_* procedure  uses the previously passed in token as a base,
   converting as for a capital.

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
   pass1 output file, column number deeper.

   pl means reuse the first position passed in.

   'I' or 'i' is an integer value.
   'B' or 'b' is a boolean value.
   'N' or 'n' is a LONGINT value. 
*) 

; PROCEDURE PutBwd_L ( T : Itk . TokTyp )

; PROCEDURE PutBwd_LP ( T : Itk . TokTyp ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LNP
    ( T : Itk . TokTyp ; N : LONGINT ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LNNP
    ( T : Itk . TokTyp ; N1 , N2 : LONGINT ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LP_rp
    ( T : Itk . TokTyp ; READONLY Position : tPosition )

; PROCEDURE PutBwd_RP ( T : Itk . TokTyp ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LI ( T : Itk . TokTyp ; I : INTEGER )

; PROCEDURE PutBwd_LIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LIIP
    ( T : Itk . TokTyp ; I1 , I2 : INTEGER ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LIP_rip
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

; PROCEDURE PutBwd_EIP
    ( T : Itk . TokTyp ; I : INTEGER ; READONLY Position : tPosition )

; PROCEDURE PutBwd_ECIP
    ( T : Itk . TokTyp
    ; Coord : LONGINT
    ; I : INTEGER
    ; READONLY Position : tPosition
    )

; PROCEDURE PutBwd_LCr ( T : Itk . TokTyp ; C : LONGINT )

; PROCEDURE PutBwd_LCP_rp
   ( T : Itk . TokTyp ; C : LONGINT ; READONLY Position : tPosition )

; PROCEDURE PutBwd_LCP_eCp_rp
   ( T : Itk . TokTyp
   ; C1 : LONGINT
   ; READONLY Position : tPosition
   ; C2 : LONGINT
   )

; PROCEDURE PutBwd_LCIP_rip
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I : INTEGER 
    ; READONLY Position : tPosition 
    )

; PROCEDURE PutBwd_LCIIP_riip
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I1 , I2 : INTEGER 
    ; READONLY Position : tPosition 
    )

; PROCEDURE PutBwd_LCNP_rnp
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; I : LONGINT 
    ; READONLY Position : tPosition 
    )

; PROCEDURE PutBwd_LCIP_eCiP_riP
    ( T : Itk . TokTyp 
    ; LC : LONGINT 
    ; I : INTEGER 
    ; READONLY LPos : tPosition
    ; EC : LONGINT 
    ; READONLY EPos : tPosition
    ; READONLY RPos : tPosition
    )
    
; PROCEDURE PutBwd_LCIP_eCip_rip
    ( T : Itk . TokTyp 
    ; LC : LONGINT 
    ; I : INTEGER 
    ; READONLY LPos : tPosition
    ; EC : LONGINT 
    )

; PROCEDURE PutBwd_LCPI_rpi
    ( T : Itk . TokTyp 
    ; C : LONGINT 
    ; READONLY Position : tPosition 
    ; I : INTEGER 
    )

; PROCEDURE PutBwd_LCP_eCP_rP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionLt : tPosition
   ; CEins : LONGINT
   ; READONLY PositionEins : tPosition
   ; READONLY PositionRt : tPosition
   )

; PROCEDURE PutBwd_LCP_eCP_zCP_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; READONLY PL : tPosition
   ; Ce : LONGINT
   ; READONLY Pe : tPosition
   ; Cz : LONGINT
   ; READONLY Pz : tPosition
   ; READONLY Pr : tPosition
   )

; PROCEDURE PutBwd_LCP_eCPB_zCP_rP
   ( L : Itk . TokTyp
   ; CL : LONGINT
   ; READONLY PL : tPosition
   ; Ce : LONGINT
   ; READONLY Pe : tPosition
   ; Be : BOOLEAN 
   ; Cz : LONGINT
   ; READONLY Pz : tPosition
   ; READONLY Pr : tPosition
   )

; PROCEDURE PutBwd_LCPeCprp
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; CInfix : LONGINT 
   ; READONLY PositionInfix : tPosition
   )

; PROCEDURE PutBwd_ECIP_riP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; I : INTEGER 
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

; PROCEDURE PutBwd_ECPrP
   ( T : Itk . TokTyp
   ; CLt : LONGINT
   ; READONLY PositionOne : tPosition
   ; READONLY PositionRt : tPosition
   )

; PROCEDURE PutBwd_LCBr ( T : Itk . TokTyp ; C : LONGINT ; B : BOOLEAN )

; PROCEDURE PutBwd_LCI_ri ( T : Itk . TokTyp ; C : LONGINT ; I : INTEGER )

; PROCEDURE PutBwd_LI3 ( T : Itk . TokTyp ; I0 , I1 , I2 : INTEGER )

; PROCEDURE PutBwd_LI6
    ( T : Itk . TokTyp ; I0 , I1 , I2 , I3 , I4 , I5 : INTEGER )

; PROCEDURE PutBwd_LC_eC_r ( T : Itk . TokTyp ; Ct , Co : LONGINT )

; PROCEDURE PutBwd_LCIeCri
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

; PROCEDURE MakeListEmpty
    ( VAR LHSAttr : tParsAttribute
    ; TokLt : Itk . TokTyp
    ; READONLY Position : tPosition
    )

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

; PROCEDURE BeginBlock ( ) : FM3Globals . ScopeNoTyp (* Created. *) 

; PROCEDURE EndBlock ( )

; PROCEDURE ScopeEmpty
    ( ScopeKind : FM3Scopes . ScopeKindTyp ; Position : FM3Base . tPosition )
  : FM3Scopes . ScopeRefTyp 

; PROCEDURE FlagReservedIdent
    ( READONLY IdAttr : tParsAttribute ; ContextTag := "used in this context" )

; PROCEDURE DeclIdL2R 
    ( DeclKind : FM3Decls . DeclKindTyp  
    ; READONLY IdAttribute : tParsAttribute
    ; SepTok : Itk . TokTyp := Itk . ItkNull
                            (* ^Implies single decl id, not in a list. *)  
    ; READONLY SepPosition : tPosition := FM3Base . PositionNull 
    ; PriorIdCt : INTEGER := 0 (* Number of ids to left of this one. *)
    )
  : BOOLEAN (* Use this declared id.  (It's not reserved and not a duplicate
               in current scope.) *)
  (* PRE: IdAttribute is for an identifier in a declaration context. *) 

; PROCEDURE IdentRefL2R ( READONLY IdAttribute : tParsAttribute )
  (* Possibly a reserved Id. *) 

; PROCEDURE RecognizedPragma ( READONLY PragmaAttr : tParsAttribute )

; PROCEDURE UnrecognizedPragma ( READONLY IdAttr : tParsAttribute )

; PROCEDURE OverrideIdentRefL2R ( READONLY IdAttribute : tParsAttribute )
  : BOOLEAN (* It's OK so far. *) 
  (* Disallows reserved Id. *) 

; PROCEDURE QualIdentL2R
    ( READONLY LtAttribute , RtAttribute : tParsAttribute )
  (* Handles either/both idents reserved. *) 
    
; PROCEDURE BuiltinNoSelectorAllowed
    ( READONLY IdAttribute , SelectorAttribute : tParsAttribute
    ; SelectedTag : TEXT
    )

; PROCEDURE BuiltinWithNoSelector
( READONLY IdAttribute : tParsAttribute )
  (* PRE: IdAttribute . Scan . SaStdTok # FM3Base , TokNull. *) 
  (* Builtin ident that has no selector. *) 

; PROCEDURE BuiltinIdentActualsL2R
    ( READONLY IdAttribute , ActualsAttribute : tParsAttribute )
  (* PRE: IdAttribute . Scan . SaStdTok # FM3Base , TokNull. *) 
  (* PRE: IdAttribute is for the builtin ident only. *) 
  (* PRE: ActualsAttribute is for an actual parameter list. *) 

; PROCEDURE BuiltinOtherSelector
    ( READONLY IdAttribute , SelectorAttribute : tParsAttribute ; Tag : TEXT )
  (* A builtin id with either a dot-selection or subscript(s).
     No builtin allows either of these. *)
  (* PRE: IdAttribute . Scan . SaStdTok # FM3Base , TokNull. *) 

; PROCEDURE DeclScopeRtL2R ( ScopeRef : FM3Scopes . ScopeRefTyp )
  (* Create an IdAtom-to-declNo, fixed-size dictionary for the scope, of
     exactly the needed size, and load it up with mappings of the idents
     declared in the scope, using a contiguously-numbered range of DeclNos.
  *) 

; PROCEDURE DisAsmPass1 ( UnitRef : FM3Units . UnitRefTyp )

; PROCEDURE RunPass1 ( ) 

; END FM3Pass1
.

