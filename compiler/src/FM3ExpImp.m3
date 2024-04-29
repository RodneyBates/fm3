
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3ExpImp

; IMPORT Pathname 
; IMPORT Text
; IMPORT TextWr
; IMPORT Wr 

; IMPORT FM3Base
; IMPORT FM3Compile 
; IMPORT FM3Messages
; IMPORT FM3OpenArray_Char
; IMPORT FM3Units 
; IMPORT FM3Utils

; TYPE Us = FM3Units . UnitStateTyp 

; PROCEDURE ReportCyclic
    ( UnitRef : FM3Units . UnitRefTyp ; Position : FM3Base . tPosition )
  (* PRE: ExpImportUnitRef is both the bottom and top unit in the cycle. *)

  = VAR LWrT : TextWr . T
  ; VAR LUnitRef : FM3Units . UnitRefTyp 
  ; VAR LNextUnitRef : FM3Units . UnitRefTyp 

  ; BEGIN
      LUnitRef := UnitRef (* Start at bottom. *)  
    ; LWrT := TextWr . New ( )
    ; Wr . PutText ( LWrT , "Cyclic import of" )
    ; LOOP 
        Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , FM3Messages . NLIndent ) 
      ; Wr . PutText
          ( LWrT
          , Pathname . Join
              ( LUnitRef ^ . UntSrcFilePath
              , LUnitRef ^ . UntSrcFileSimpleName
              )
          )
      ; LNextUnitRef := LUnitRef . UntImportingUnitRef 
      ; LUnitRef . UntImportingUnitRef := NIL  
      ; LUnitRef ^ . UntInCycle := TRUE 
      ; IF LUnitRef = FM3Units . UnitStackTopRef 
        THEN (* The unit containing the cycle-closing [ex/im]port clause. *) 
          Wr . PutText ( LWrT , Wr . EOL ) 
        ; EXIT
        ELSE 
          Wr . PutText ( LWrT , ", which at " ) 
        ; Wr . PutText
            ( LWrT
            , FM3Utils . PositionImage ( LUnitRef . UntImportingPosition )
            )
        ; Wr . PutText ( LWrT , ", imports" )
        ; LUnitRef := LNextUnitRef 
        END (*IF*) 
      END (*LOOP*) 

    ; FM3Messages . ErrorArr
        ( ARRAY OF REFANY { TextWr . ToText ( LWrT ) }
        , Pos := Position 
        ) 
    END ReportCyclic

(*EXPORTED*) 
; PROCEDURE Interface
    ( IntfNameOA : FM3OpenArray_Char . T
    ; IntfHash : FM3Base . HashTyp 
    ; AsNameOA : FM3OpenArray_Char . T
    ; AsHash : FM3Base . HashTyp 
    ; Position : FM3Base . tPosition
    ; Exports : BOOLEAN 
    )
  : FM3Units . UnitRefTyp
    (* ^The interface unit that was [ex/im]ported, possibly NIL *) 
  (* PRE: IntfNameOA and AsNameOA are Modula-3 identifiers, thus ocntain no
          file name suffix.
  *) 

  = VAR LIntfUnitRef : FM3Units . UnitRefTyp 
  ; VAR LSrcFileName : TEXT 
  ; VAR LAdjective : TEXT 

  ; BEGIN
      IF IntfNameOA = NIL OR NUMBER ( IntfNameOA ^ ) = 0
      THEN RETURN NIL
      END (*IF*)
    ; LSrcFileName
        := Pathname . Join ( NIL , Text . FromChars ( IntfNameOA ^ ) , ".i3" )  
    ; IF AsNameOA = NIL
      THEN AsNameOA := IntfNameOA ; AsHash := IntfHash
      END (*IF*)
    ; LIntfUnitRef := FM3Compile . UnitOfFileName ( LSrcFileName )  
    ; IF LIntfUnitRef . UntState = Us . UsNull 
      THEN (* Haven't seen this unit yet. *)
      (* Compile it. *)
(*TODO: Or load it. *)
        IF Exports
        THEN LAdjective := "exported "
        ELSE LAdjective := "imported " 
        END (*IF*) 

      (* Compare this to similar code in FM3Compile.CompileOrLoadCLUnit. *) 
      ; IF FM3Compile . FindAndOpenUnitSrcFile
             ( LIntfUnitRef , Adjective := LAdjective )
        THEN 
          FM3Units . UnitStackTopRef ^ . UntImportingUnitRef := LIntfUnitRef 
        ; FM3Units . UnitStackTopRef ^ . UntImportingPosition := Position
        ; LIntfUnitRef ^ . UntState := Us . UsImporting 
        ; FM3Units . PushUnit ( LIntfUnitRef ) 
        ; FM3Compile . CompileUnitFromSrc ( LIntfUnitRef ) 
        ; <* ASSERT FM3Units . PopUnit ( ) = LIntfUnitRef *>
          RETURN LIntfUnitRef 
        ELSE RETURN LIntfUnitRef 
        END (*IF*) 
      ELSE
        IF LIntfUnitRef . UntImportingUnitRef # NIL 
        THEN (* Cyclic imports/exports. *)
          ReportCyclic  ( LIntfUnitRef , Position )
        ; RETURN NIL
        ELSE RETURN LIntfUnitRef 
        END (*IF*)
      END (*IF*)
    ; (* Do the actual [ex/im]port. *) 
    END Interface 

; BEGIN (*FM3ExpImp*)
  END FM3ExpImp
.
