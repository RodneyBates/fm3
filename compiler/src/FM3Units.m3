
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2026  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *) 

MODULE FM3Units

; IMPORT Text
; IMPORT Fmt 

; IMPORT IntSets 
; IMPORT IntIntVarArray AS VarArray_Int_Int (* FM3's naming convention. *)

; IMPORT FM3Atom_OAChars 
; IMPORT FM3Atom_OAWideChars 
; IMPORT FM3Atom_Text  
; IMPORT FM3Base 
; IMPORT FM3Decls
; IMPORT FM3Exprs 
; IMPORT FM3Units
; IMPORT FM3Globals 
; IMPORT FM3Messages 
; IMPORT FM3Scopes
; IMPORT FM3SharedUtils
; IMPORT FM3SrcToks 
; IMPORT FM3Utils 
; IMPORT Ranges_Int
; IMPORT VarArray_Int_ExpImpProxy  
; IMPORT VarArray_Int_Refany 

; VAR NextUnitNo : INTEGER := 1

; TYPE Ukt = UnitKindTyp 

(*EXPORTED*) 
; PROCEDURE UnitKindImage ( Kind : Ukt ) : TEXT

  = BEGIN
      CASE Kind OF 
      | Ukt . UkNull => RETURN "<NullUnitKind>"
      | Ukt . UkInterface => RETURN "INTERFACE"
      | Ukt . UkGenInterface => RETURN "GENERIC INTERFACE"
      | Ukt . UkInstInterface => RETURN "INTERFACE"
      | Ukt . UkModule => RETURN "MODULE"
      | Ukt . UkGenModule => RETURN "GENERIC MODULE"
      | Ukt . UkInstModule => RETURN "MODULE"
      END (*CASE*) 
    END UnitKindImage 

(*EXPORTED*) 
; PROCEDURE UnitKindSectionNo ( Kind : Ukt ) : TEXT

  = BEGIN
      CASE Kind OF 
      | Ukt . UkNull => RETURN ""
      | Ukt . UkInterface => RETURN "2.5.2"
      | Ukt . UkGenInterface => RETURN "2.5.5"
      | Ukt . UkInstInterface => RETURN "2.5.5"
      | Ukt . UkModule => RETURN "2.5.3"
      | Ukt . UkGenModule => RETURN "2.5.5"
      | Ukt . UkInstModule => RETURN "2.5.5"
      END (*CASE*) 
    END UnitKindSectionNo  

; PROCEDURE NewUnitsMap
    ( InitUnitCt : FM3Globals . UnitNoTyp ) : VarArray_Int_Refany . T
  (* PRE: InitUnitCt > 0. *) 
  (* One UnitsMap in a compile. *) 

  = VAR LResult : VarArray_Int_Refany . T 

  ; BEGIN
      LResult
        := VarArray_Int_Refany . New
             ( NIL , Ranges_Int . RangeTyp {  0 , InitUnitCt - 1 } )
    ; VarArray_Int_Refany . Touch
        ( LResult , Ranges_Int . RangeTyp {  0 , 0  } )
    ; RETURN LResult 
          
    END NewUnitsMap

(*EXPORTED.*)
; <*INLINE*>
  PROCEDURE UnitNoRef ( UnitNo : FM3Globals . UnitNoTyp ) : UnitRefTyp 

  = VAR LUnitTRef : UnitRefTyp 

  ; BEGIN (*UnitNoRef*)
      LUnitTRef 
        := NARROW
             ( VarArray_Int_Refany . Fetch ( FM3Units . UnitsMap , UnitNo )
             , UnitRefTyp
             ) 
    ; RETURN LUnitTRef 
    END UnitNoRef
      
(*EXPORTED.*)
; PROCEDURE UnitRefImage ( UnitTRef : UnitRefTyp ) : TEXT 
  (* UnitNo, REF, and sourceFileName. *) 
  
  = VAR LResult : TEXT

  ; BEGIN (*UnitRefImage*)
      IF UnitTRef = NIL THEN RETURN "NIL" END (*IF*)
    ; LResult := FM3SharedUtils . CatArrT
        ( ARRAY OF REFANY
            { "UnitNo " 
            , Fmt . Int ( UnitTRef ^ . UttSelfUnitNo ) 
            , " at " 
            , FM3Utils . RefanyImage ( UnitTRef )
            , " "
            , UnitTRef ^ . UntSrcFileSimpleName 
            }
        ) 
    ; RETURN LResult 
    END UnitRefImage

(*EXPORTED*) 
; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate, low-level initialize, give it a UnitNo, and put into UnitsMap. *)

  = VAR LUnitTRef : UnitRefTyp
  ; VAR LUnitNo : FM3Globals . UnitNoTyp 

  ; BEGIN
      LUnitTRef := NEW ( UnitRefTyp )
    ; IF LUnitTRef = NIL
      THEN
        FM3Messages  . FatalArr
          ( ARRAY OF REFANY { "Allocation of a FM3Units.UnitRefTyp failed." } )
      ; RAISE FM3SharedUtils . AllocationFailure ( "allocating a UnitRef" ) 
      END 

    ; LUnitNo := NextUnitNo
    ; INC ( NextUnitNo )
(* TODO: Either complete the list of constant-initialized fields, or bite
         nails and rely on the declaration.
*) 
    ; LUnitTRef ^ . UttStackLink := NIL 
    ; LUnitTRef ^ . UttStackDepth := 0
    ; LUnitTRef ^ . UttSelfUnitNo := LUnitNo
    ; LUnitTRef ^ . UntSrcFileSimpleName := NIL 
    ; LUnitTRef ^ . UntSrcFilePath := NIL
    ; LUnitTRef ^ . UntBuildDirPath := NIL
    ; LUnitTRef ^ . UttPatchStackSimpleName := NIL
    ; LUnitTRef ^ . UttPatchStackRdBack := NIL
    ; LUnitTRef ^ . UttMaxPatchStackDepth := 0L 
    ; LUnitTRef ^ . UttImportingUnitRef := NIL 
    ; LUnitTRef ^ . UttPositionOfImport := FM3Base . PositionNull  
    ; LUnitTRef ^ . UttPass1OutSimpleName := NIL
    ; LUnitTRef ^ . UttPass1OutRdBack := NIL
    ; LUnitTRef ^ . UttMaxPass1OutLength := 0L 
    ; LUnitTRef ^ . UttPass2OutSimpleName := NIL
    ; LUnitTRef ^ . UttPass2OutRdBack := NIL
    ; LUnitTRef ^ . UntScopeRef := NIL
    ; LUnitTRef ^ . UntExpImpCt := FM3Globals . DeclNoNull 
    ; LUnitTRef ^ . UttSkipStackBase := 0 
    ; LUnitTRef ^ . UntUnitIdent := NIL 
    ; LUnitTRef ^ . UntUnitIdentPos := FM3Base . PositionNull
    ; LUnitTRef ^ . UntState := UnitStateTyp . UsNull
    ; LUnitTRef ^ . UntUnsafe := FALSE 
    ; LUnitTRef ^ . UntInExpImpCycle := FALSE
    ; LUnitTRef ^ . UntHasStdUnitPragma := FALSE 
    ; LUnitTRef ^ . UntIdentAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . IdentAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAChars 
             , DoReverseMap := TRUE
             )
    ; LUnitTRef ^ . UntNumLitAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . NumberAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAChars 
             , DoReverseMap := TRUE
             )
    ; LUnitTRef ^ . UntCharsLitAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . CharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAChars 
             , DoReverseMap := TRUE
             )
    ; LUnitTRef ^ . UntWCharsLitAtomDict 
        := FM3Atom_OAWideChars . New
             ( FM3Globals . WideCharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAWChars
             , DoReverseMap := TRUE
             )
    ; LUnitTRef ^ . UntScopeMap
        := FM3Scopes . NewScopeMap ( FM3Globals . InitScopeCtPerUnit )
    ; VarArray_Int_Refany . Touch
        ( LUnitTRef ^ .  UntScopeMap , Ranges_Int . RangeTyp { 0 , 0 } )
    ; LUnitTRef ^ . UntExpImpIdSet := IntSets . Empty ( )
    ; LUnitTRef ^ . UntExpImpMap
        := VarArray_Int_ExpImpProxy . New
             ( ExpImpProxyNull
             , Ranges_Int . RangeTyp { 0 , FM3Globals . InitImportsCt - 1 } 
             )
    ; VarArray_Int_ExpImpProxy . Touch
        ( LUnitTRef ^ .  UntExpImpMap , Ranges_Int . RangeTyp { 0 , 0 } )
    ; LUnitTRef ^ . UttNextDeclNo := 1 
    ; LUnitTRef ^ . UntStdTok := FM3Base . TokNull  
    ; LUnitTRef ^ . UntDeclMap 
        := FM3Decls . NewDeclMap ( FM3Globals . InitDeclCtPerUnit ) 
    ; VarArray_Int_Refany . Touch
        ( LUnitTRef ^ .  UntDeclMap , Ranges_Int . RangeTyp { 0 , 0 } )
    ; LUnitTRef ^ . UntExprMap 
        := FM3Exprs . NewExprMap ( FM3Globals . InitDefCtPerUnit ) 
    ; VarArray_Int_Refany . Touch
        ( LUnitTRef ^ .  UntExprMap
        , Ranges_Int . RangeTyp
            { FM3Exprs . ExprNoNull , FM3Exprs . ExprNoFirstReal - 1 }
        )
    ; LUnitTRef ^ . UttNextDeclNo := 1
    ; LUnitTRef ^ . UntFirstTrueDeclNo := 1
    ; LUnitTRef ^ . UttSkipStackBase
        := VarArray_Int_Int . TouchedRange ( FM3Globals . SkipNoStack ) . Hi
    ; LUnitTRef ^ . UttExprStackBaseCt := 0 
    ; LUnitTRef ^ . UttScopeDeclStackBaseCt := 0 
    ; LUnitTRef ^ . UttLookupScopeStackBaseCt := 0 
    ; VarArray_Int_Refany . Assign ( UnitsMap , LUnitNo , LUnitTRef )
    ; RETURN LUnitTRef 
    END NewUnitRef

(*EXPORTED.*)
; PROCEDURE UnitRefIdImage ( UnitTRef : UnitRefTyp ) : TEXT 

  = VAR LName : TEXT 

  ; BEGIN (*UnitRefIdImage*)
      IF UnitTRef = NIL THEN RETURN "<NIL_Unit>" END (*IF*)
    ; LName := UnitTRef ^ . UntSrcFileSimpleName 
    ; IF LName = NIL THEN RETURN "<NIL_SourceFileName>" END (*IF*)
    ; RETURN LName  
    END UnitRefIdImage

    
(*EXPORTED.*)
; PROCEDURE AllocateDeclNos ( Count : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Count Decl numbers, unique
     within the current scope, and return the lowest number.
  *) 

  = VAR LResult : INTEGER 

  ; BEGIN (*AllocateDeclNos*)
      IF UnitTStackTopRef = NIL THEN RETURN FM3Globals . DeclNoNull END (*IF*)
    ; LResult := UnitTStackTopRef ^ . UttNextDeclNo
    ; INC ( UnitTStackTopRef ^ . UttNextDeclNo , Count )
    ; RETURN LResult 
    END AllocateDeclNos
    
(*EXPORTED.*)
; PROCEDURE IdAtomText ( IdentAtom : FM3Base . AtomTyp ) : TEXT 
  (* In the current unit. *) 

  = VAR LOACharsRef : FM3Atom_OAChars . KeyTyp
  ; VAR LIdentText : TEXT 

  ; BEGIN (*IdAtomText*)
      IF IdentAtom = FM3Base . AtomNull
      THEN LIdentText := "<Null>" 
      ELSIF IdentAtom < 0
      THEN LIdentText := FM3SrcToks . Image ( - IdentAtom ) 
      ELSE 
        IF NOT FM3Atom_OAChars . Key 
                 ( UnitTStackTopRef ^ . UntIdentAtomDict
                 , IdentAtom
                 , (*OUT*) LOACharsRef
                 )
        THEN LIdentText := "<NotFound>"
        ELSIF LOACharsRef = NIL
        THEN LIdentText := "<NILSpelling>"
        ELSE LIdentText := Text . FromChars ( LOACharsRef ^ )
        END (*IF*) 
      END (*IF*)
    ; RETURN LIdentText 
    END IdAtomText

(*EXPORTED.*)
; PROCEDURE PushUnit ( UnitTRef : UnitRefTyp ) 

  = VAR LBeneathUnitRef : UnitRefTyp

  ; BEGIN (*PushUnit*)
      IF UnitTRef = NIL THEN RETURN END (*IF*) 
    ; <* ASSERT UnitTRef . UttStackDepth = 0 *> (* Not already on stack. *)
      LBeneathUnitRef := UnitTStackTopRef 
    ; IF LBeneathUnitRef = NIL
      THEN UnitTRef . UttStackDepth := 1
      ELSE UnitTRef . UttStackDepth := LBeneathUnitRef . UttStackDepth + 1
      END (*IF*)
    ; UnitTRef ^ . UttStackLink := LBeneathUnitRef  
    ; UnitTStackTopRef := UnitTRef 
    END PushUnit

(*EXPORTED.*)
; PROCEDURE CacheTopUnitValues ( )

  = BEGIN
      IF UnitTStackTopRef = NIL 
      THEN
        FM3Globals . P1RdBack := NIL 
      ; FM3Globals . PatchRdBack := NIL 
      ; FM3Globals . P2RdBack := NIL 
      ELSE 
        FM3Globals . P1RdBack := UnitTStackTopRef . UttPass1OutRdBack 
      ; FM3Globals . PatchRdBack := UnitTStackTopRef . UttPatchStackRdBack 
      ; FM3Globals . P2RdBack := UnitTStackTopRef . UttPass2OutRdBack
      ; FM3Globals . P3RdBack := UnitTStackTopRef . UttPass3OutRdBack
      END (*IF*)    
    END CacheTopUnitValues 

(*EXPORTED.*)
; PROCEDURE PopUnit ( ) : UnitRefTyp  

  = VAR LPoppedUnitTRef : UnitRefTyp

  ; BEGIN (*PopUnit*)
      LPoppedUnitTRef := UnitTStackTopRef  
    ; <* ASSERT LPoppedUnitTRef # NIL *>
      UnitTStackTopRef := LPoppedUnitTRef ^ . UttStackLink
    ; IF UnitTStackTopRef = NIL
      THEN <* ASSERT LPoppedUnitTRef ^ . UttStackDepth = 1 *> 
      ELSE 
        <* ASSERT
             UnitTStackTopRef ^ . UttStackDepth
             = LPoppedUnitTRef ^ . UttStackDepth - 1
        *>
      END (*IF*)
    ; LPoppedUnitTRef . UttStackDepth := 0
      (* ^Note that it's no longer on the unit stack. *)  
    ; RETURN LPoppedUnitTRef
    END PopUnit

(*EXPORTED.*)
; PROCEDURE CurrentUnitIsModule ( ) : BOOLEAN

  = BEGIN 
      RETURN UnitTStackTopRef ^ . UntKind IN UnitKindSetModule  
    END CurrentUnitIsModule

(*EXPORTED.*)
; PROCEDURE CharsOfIdentAtom
    ( UnitTRef : UnitRefTyp ; Atom : FM3Base . AtomTyp )
  : FM3Atom_OAChars . KeyTyp (* Which is ARRAY OF CHAR. *) 

  = VAR LIdentChars : FM3Atom_OAChars . KeyTyp 

  ; BEGIN 
      IF NOT FM3Atom_OAChars . Key 
               ( UnitTRef ^ . UntIdentAtomDict , Atom , (*OUT*) LIdentChars )
      THEN LIdentChars := NIL
      END (*IF*)
    ; RETURN LIdentChars
    END CharsOfIdentAtom 
    
; BEGIN
    UnitsAtomDict  
      := FM3Atom_Text . New
           ( FM3Globals . InitUnitsCt 
           , FM3Base . AtomFirstReal
           , HashFunc := FM3Utils . HashOfText 
           , DoReverseMap := TRUE (* Needed? *) 
           )
           
  ; UnitsMap := NewUnitsMap ( FM3Globals . InitUnitsCt - 1 )
  ; NextUnitNo := 1
  ; UnitTStackTopRef := NIL 
  END FM3Units
.

