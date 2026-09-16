
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

; PROCEDURE NewUnitsTMap
    ( InitUnitCt : FM3Globals . UnitNoTyp ) : VarArray_Int_Refany . T
  (* PRE: InitUnitCt > 0. *) 
  (* One UnitsTMap in a compile. *) 

  = VAR LResult : VarArray_Int_Refany . T 

  ; BEGIN
      LResult
        := VarArray_Int_Refany . New
             ( NIL , Ranges_Int . RangeTyp {  0 , InitUnitCt - 1 } )
    ; VarArray_Int_Refany . Touch
        ( LResult , Ranges_Int . RangeTyp {  0 , 0  } )
    ; RETURN LResult 
    END NewUnitsTMap

(*EXPORTED.*)
; <*INLINE*>
  PROCEDURE UnitTRefOfUnitNo ( UnitNo : FM3Globals . UnitNoTyp ) : UnitTRefTyp 

  = VAR LUnitTRef : UnitTRefTyp 

  ; BEGIN (*UnitTRefOfUnitNo*)
      LUnitTRef 
        := NARROW
             ( VarArray_Int_Refany . Fetch ( FM3Units . UnitsTMap , UnitNo )
             , UnitTRefTyp
             ) 
    ; RETURN LUnitTRef 
    END UnitTRefOfUnitNo
      
(*EXPORTED.*)
; PROCEDURE UnitTRefImage ( UnitTRef : UnitTRefTyp ; ShowFields := FALSE )
    : TEXT 
  (* UnitNo, REF, and source file path. *) 
  
  = VAR LFields : TEXT
  ; VAR LResult : TEXT

  ; BEGIN (*UnitTRefImage*)
      IF UnitTRef = NIL THEN RETURN "NIL" END (*IF*)
    ; LFields := ""
    ; IF ShowFields
      THEN 
(* COMPLETEME*)
      END (*IF*) 
    ; LResult := FM3SharedUtils . CatArrT
        ( ARRAY OF REFANY
            { "UnitNo " 
            , Fmt . Int ( UnitTRef ^ . UttSelfUnitNo ) 
            , " at " 
            , FM3Utils . RefanyImage ( UnitTRef )
            , " "
            , UnitTRef ^ . UttSrcFilePath
            , LFields 
            }
        ) 
    ; RETURN LResult 
    END UnitTRefImage

(*EXPORTED.*)
; PROCEDURE UnitTStateImage ( State : UnitTStateTyp ) : TEXT 

  = BEGIN (*UnitTStateImage*)
      CASE State OF
      | UnitTStateTyp . UttsNull => RETURN "UttsNull" 
      | UnitTStateTyp . UttsNew => RETURN "UttsNew" 
      | UnitTStateTyp . UttsNotFound => RETURN "UttsNotFound" 
      | UnitTStateTyp . UttsNotLoadable => RETURN "UttsNotLoadable" 
      | UnitTStateTyp . UttsLoaded => RETURN "UttsLoaded" 
      | UnitTStateTyp . UttsCompiled => RETURN "UttsCompiled" 
      END (*CASE*)   
   END UnitTStateImage

(*EXPORTED.*)
; PROCEDURE UnitReqKindImage ( Kind : UnitReqKindTyp ) : TEXT

  = BEGIN (*UnitReqKindImage*) 
      CASE Kind OF
      | UnitReqKindTyp . UttrNull => RETURN "UttrNull"
      | UnitReqKindTyp . UttrCL => RETURN "UttrCL"  
      | UnitReqKindTyp . UttrExport => RETURN "UttrExport"
      | UnitReqKindTyp . UttrImport => RETURN "UttrImport" 
      | UnitReqKindTyp . UttrGenActual => RETURN "UttrGenActual"
      END (*CASE*) 
    END UnitReqKindImage 

(*EXPORTED.*)
; PROCEDURE UnitReqKindTag ( Kind : UnitReqKindTyp ) : TEXT

  = BEGIN (*UnitReqKindTag*) 
      CASE Kind OF
      | UnitReqKindTyp . UttrNull => RETURN ""
      | UnitReqKindTyp . UttrCL => RETURN "command line"  
      | UnitReqKindTyp . UttrExport => RETURN "export"
      | UnitReqKindTyp . UttrImport => RETURN "import" 
      | UnitReqKindTyp . UttrGenActual => RETURN "generic actual"
      END (*CASE*) 
    END UnitReqKindTag 

(*EXPORTED*) 
; PROCEDURE NewUnitTRef ( ) : UnitTRefTyp
  (* Allocate a UnitTTyp (transient), initialize non-constant fields, 
     give it a UnitNo, and put into UnitsTMap.
  *)

  = VAR LUnitTRef : UnitTRefTyp
  ; VAR LUnitNo : FM3Globals . UnitNoTyp 

  ; BEGIN
      LUnitTRef := NEW ( UnitTRefTyp )
    ; IF LUnitTRef = NIL
      THEN
        FM3Messages  . FatalArr
          ( ARRAY OF REFANY { "Allocation of a FM3Units.UnitTRefTyp failed." } )
      ; RAISE FM3SharedUtils . AllocationFailure ( "allocating a UnitRef" ) 
      END 
    ; LUnitNo := NextUnitNo
    ; INC ( NextUnitNo )

    (* Non-constant field initializations. *) 
    ; LUnitTRef ^ . UttSelfUnitNo := LUnitNo
    ; LUnitTRef ^ . UttExpUnitSet := IntSets . Empty ( ) 
    ; VarArray_Int_Refany . Assign ( UnitsTMap , LUnitNo , LUnitTRef )

    ; RETURN LUnitTRef 
    END NewUnitTRef

(*EXPORTED*) 
; PROCEDURE NewUnitRef ( ) : UnitRefTyp
  (* Allocate a UnitTyp, & Initialize non-constant fields. *) 

  = VAR LUnitRef : UnitRefTyp

  ; BEGIN (* NewUnitRef *) 
      LUnitRef := NEW ( UnitRefTyp )
    ; IF LUnitRef = NIL
      THEN
        FM3Messages  . FatalArr
          ( ARRAY OF REFANY { "Allocation of a FM3Units.UnitRefTyp failed." } )
      ; RAISE FM3SharedUtils . AllocationFailure ( "allocating a UnitRef" ) 
      END
    
    (* Non-constant field initializations. *) 
    ; LUnitRef ^ . UntIdentAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . IdentAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAChars 
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntNumLitAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . NumberAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAChars 
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntCharsLitAtomDict 
        := FM3Atom_OAChars . New
             ( FM3Globals . CharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAChars 
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntWCharsLitAtomDict 
        := FM3Atom_OAWideChars . New
             ( FM3Globals . WideCharsAtomInitSize
             , FM3Base . AtomFirstReal
             , HashFunc := FM3Utils . HashOfOAWChars
             , DoReverseMap := TRUE
             )
    ; LUnitRef ^ . UntScopeMap
        := FM3Scopes . NewScopeMap ( FM3Globals . InitScopeCtPerUnit )
    ; VarArray_Int_Refany . Touch
        ( LUnitRef ^ .  UntScopeMap , Ranges_Int . RangeTyp { 0 , 0 } )
    ; LUnitRef ^ . UntExpImpIdSet := IntSets . Empty ( )
    ; LUnitRef ^ . UntExpImpMap
        := VarArray_Int_ExpImpProxy . New
             ( ExpImpProxyNull
             , Ranges_Int . RangeTyp { 0 , FM3Globals . InitImportsCt - 1 } 
             )
    ; VarArray_Int_ExpImpProxy . Touch
        ( LUnitRef ^ .  UntExpImpMap , Ranges_Int . RangeTyp { 0 , 0 } )
    ; LUnitRef ^ . UntDeclMap 
        := FM3Decls . NewDeclMap ( FM3Globals . InitDeclCtPerUnit ) 
    ; VarArray_Int_Refany . Touch
        ( LUnitRef ^ .  UntDeclMap , Ranges_Int . RangeTyp { 0 , 0 } )
    ; LUnitRef ^ . UntExprMap 
        := FM3Exprs . NewExprMap ( FM3Globals . InitDefCtPerUnit ) 
    ; VarArray_Int_Refany . Touch
        ( LUnitRef ^ . UntExprMap
        , Ranges_Int . RangeTyp
            { FM3Exprs . ExprNoNull , FM3Exprs . ExprNoFirstReal - 1 }
        )
    ; RETURN LUnitRef 
    END NewUnitRef

(*EXPORTED.*)
; PROCEDURE UnitRefIdImage ( UnitRef : UnitRefTyp ) : TEXT 

  = VAR LName : TEXT 

  ; BEGIN (*UnitRefIdImage*)
      IF UnitRef = NIL THEN RETURN "<NIL_Unit>" END (*IF*)
    ; LName := UnitRef ^ . UntSrcFileSimpleName 
    ; IF LName = NIL THEN RETURN "<NIL_SourceFileName>" END (*IF*)
    ; RETURN LName  
    END UnitRefIdImage
    
(*EXPORTED.*)
; PROCEDURE AllocateDeclNos ( Count : INTEGER ) : INTEGER 
  (* Allocate a contiguous range of Count Decl numbers, unique
     within the current scope, and return the lowest number.
  *) 

  = VAR LResult : INTEGER
  ; VAR LUnitRef : UnitRefTyp 

  ; BEGIN (*AllocateDeclNos*)
      IF UnitTStackTopRef = NIL THEN RETURN FM3Globals . DeclNoNull END (*IF*)
    ; LUnitRef := UnitTStackTopRef ^ . UttUnitRef 
    ; LResult := LUnitRef ^ . UntNextDeclNo
    ; INC ( LUnitRef ^ . UntNextDeclNo , Count )
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
                 ( UnitTStackTopRef ^ . UttUnitRef ^. UntIdentAtomDict
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
; PROCEDURE PushUnitT ( UnitTRef : UnitTRefTyp ) 

  = VAR LBeneathUnitTRef : UnitTRefTyp

  ; BEGIN (*PushUnitT*)
      IF UnitTRef = NIL THEN RETURN END (*IF*) 
    ; <* ASSERT UnitTRef . UttStackDepth = 0 *> (* Not already on stack. *)
      LBeneathUnitTRef := UnitTStackTopRef 
    ; IF LBeneathUnitTRef = NIL
      THEN UnitTRef . UttStackDepth := 1
      ELSE UnitTRef . UttStackDepth := LBeneathUnitTRef . UttStackDepth + 1
      END (*IF*)
    ; UnitTRef ^ . UttStackLink := LBeneathUnitTRef  
    ; UnitTStackTopRef := UnitTRef 
    END PushUnitT

(*EXPORTED.*)
; PROCEDURE CacheTopUnitValues ( )

  = BEGIN
      IF UnitTStackTopRef = NIL 
      THEN
        FM3Globals . P1RdBack := NIL 
      ; FM3Globals . PatchRdBack := NIL 
      ; FM3Globals . P2RdBack := NIL 
      ; FM3Globals . P3RdBack := NIL 
      ELSE 
        FM3Globals . P1RdBack := UnitTStackTopRef . UttPass1OutRdBack 
      ; FM3Globals . PatchRdBack := UnitTStackTopRef . UttPatchStackRdBack 
      ; FM3Globals . P2RdBack := UnitTStackTopRef . UttPass2OutRdBack
      ; FM3Globals . P3RdBack := UnitTStackTopRef . UttPass3OutRdBack
      END (*IF*)    
    END CacheTopUnitValues 

(*EXPORTED.*)
; PROCEDURE PopUnitT ( ) : UnitTRefTyp  

  = VAR LPoppedUnitTRef : UnitTRefTyp

  ; BEGIN (*PopUnitT*)
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
    END PopUnitT

(*EXPORTED.*)
; PROCEDURE CurrentUnitIsModule ( ) : BOOLEAN

  = BEGIN 
      RETURN UnitTStackTopRef ^ . UttUnitRef ^ . UntKind IN UnitKindSetModule  
    END CurrentUnitIsModule

(*EXPORTED.*)
; PROCEDURE CharsOfIdentAtom
    ( UnitTRef : UnitTRefTyp ; Atom : FM3Base . AtomTyp )
  : FM3Atom_OAChars . KeyTyp (* Which is ARRAY OF CHAR. *) 

  = VAR LIdentChars : FM3Atom_OAChars . KeyTyp 

  ; BEGIN 
      IF NOT FM3Atom_OAChars . Key 
               ( UnitTRef ^ . UttUnitRef ^ . UntIdentAtomDict
               , Atom
               , (*OUT*) LIdentChars
               )
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
           
  ; UnitsTMap := NewUnitsTMap ( FM3Globals . InitUnitsCt - 1 )
  ; NextUnitNo := 1
  ; UnitTStackTopRef := NIL 
  END FM3Units
.

