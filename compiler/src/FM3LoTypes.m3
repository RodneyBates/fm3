
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FM3LoTypes

; IMPORT FM3Target
; IMPORT VarArray_Int_Refany

; BEGIN

    LoTypeMap := VarArray_Int_Refany . New ( NIL ) 

(* These are the same for all targets: *) 
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoNull
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoNull 
            , TiNatSize := 0
            , TiMinSize := 0
            , TiAlign := 0
            , TiSigned := FALSE 
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoI64
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoI64
            , TiNatSize := 64
            , TiMinSize := 64
            , TiAlign := 64
            , TiSigned := TRUE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoI32
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoI32
            , TiNatSize := 32
            , TiMinSize := 32
            , TiAlign := 32
            , TiSigned := TRUE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoI16
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoI16
            , TiNatSize := 16
            , TiMinSize := 16
            , TiAlign := 16
            , TiSigned := TRUE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoI8
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoI8
            , TiNatSize := 8
            , TiMinSize := 8
            , TiAlign := 8
            , TiSigned := TRUE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoU64
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoU64
            , TiNatSize := 64
            , TiMinSize := 64
            , TiAlign := 64
            , TiSigned := FALSE 
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoU32
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoU32
            , TiNatSize := 32
            , TiMinSize := 32
            , TiAlign := 32
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoU16
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoU16
            , TiNatSize := 16
            , TiMinSize := 16
            , TiAlign := 16
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoU8
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoU8
            , TiNatSize := 8
            , TiMinSize := 8
            , TiAlign := 8
            , TiSigned := FALSE 
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoAddr32
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoAddr32
            , TiNatSize := 32
            , TiMinSize := 32
            , TiAlign := 32
            , TiSigned := FALSE 
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoAddr64
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoAddr64
            , TiNatSize := 64
            , TiMinSize := 64
            , TiAlign := 64
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoReal
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoReal
            , TiNatSize := 32
            , TiMinSize := 32
            , TiAlign := 32
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoLongReal
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoLongReal
            , TiNatSize := 64
            , TiMinSize := 64
            , TiAlign := 64
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoExtended
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoExtended
            , TiNatSize := 64
            , TiMinSize := 64
            , TiAlign := 64
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoMultiU64
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoMultiU64
            , TiNatSize := 64 (*?*) 
            , TiMinSize := 64 (*?*)
            , TiAlign := 64
            , TiSigned := FALSE
            )
      )
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap , LoTypeNoMultiU32
      , NEW ( LoTypeInfoRefTyp
            , TiLoTypeNo := LoTypeNoMultiU32
            , TiNatSize := 32 (*?*)
            , TiMinSize := 32 (*?*)
            , TiAlign := 32
            , TiSigned := FALSE
            )
      )

(* These will vary according to the target: *) 

  ; VarArray_Int_Refany . Assign
      ( LoTypeMap
      , LoTypeNoLong
      , VarArray_Int_Refany . Fetch ( LoTypeMap , FM3Target . LoTypeNoLong )
      ) 
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap
      , LoTypeNoInt
      , VarArray_Int_Refany . Fetch ( LoTypeMap , FM3Target . LoTypeNoInt )
      ) 
  ; VarArray_Int_Refany . Assign
      ( LoTypeMap
      , LoTypeNoWideChar
      , VarArray_Int_Refany . Fetch ( LoTypeMap , FM3Target . LoTypeNoWideChar )
      )
   ; VarArray_Int_Refany . Assign
      ( LoTypeMap
      , LoTypeNoAddr
      , VarArray_Int_Refany . Fetch ( LoTypeMap , FM3Target . LoTypeNoAddr )
      ) 


  END FM3LoTypes
. 
