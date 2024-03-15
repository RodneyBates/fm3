        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE  FM3Compile

; IMPORT Pathname
; IMPORT FileWr
; IMPORT Wr 

; IMPORT FM3DisAsm 
; IMPORT FM3Globals 
; IMPORT FM3Units
; IMPORT RdBackFile 

(*EXPORTED*) 
; PROCEDURE DisAsm
    ( UnitRef : FM3Units . UnitRefTyp ; RdBackFileName : TEXT )
  (*PRE: RdBackFile.Copy is closed. *) 
  (*POST: RdBackFile.Copy is reclosed. *) 

  = VAR LFullDisAsmFileName : TEXT
  ; VAR LFullRdBackFileName : TEXT
  ; VAR LDisAsmWrT : Wr . T
  ; VAR LRdBack : RdBackFile . T
  
  ; BEGIN
      LFullDisAsmFileName
        := Pathname . Join
             ( NIL , RdBackFileName , FM3Globals . DisAsmFileSuffix ) 
    ; LFullRdBackFileName
        := Pathname . Join
             ( NIL , RdBackFileName , FM3Globals . CopyFileSuffix )
    ; LRdBack := RdBackFile . Open ( LFullRdBackFileName )
    ; LDisAsmWrT := FileWr . Open ( LFullDisAsmFileName )
    ; FM3DisAsm . DisAsmWOperandsBwd ( LRdBack , LDisAsmWrT )
    ; RdBackFile . Close ( LRdBack , - 1L )      
    ; Wr . Close ( LDisAsmWrT ) 
    END DisAsm

; BEGIN (*FM3Compile*)
  END FM3Compile
.

