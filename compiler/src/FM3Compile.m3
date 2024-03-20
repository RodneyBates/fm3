        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2024        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE  FM3Compile

; IMPORT OSError 
; IMPORT Pathname
; IMPORT FileWr
; IMPORT FS 
; IMPORT Wr 

; IMPORT FM3Base
; IMPORT FM3CLArgs 
; IMPORT FM3DisAsm 
; IMPORT FM3CLOptions  
; IMPORT FM3Globals
; IMPORT FM3Messages 
; IMPORT FM3Pass1 
; IMPORT FM3Pass2 
; IMPORT FM3Units
; IMPORT RdBackFile 

(*EXPORTED*) 
; PROCEDURE MakePassFileCopy
    ( UnitRef : FM3Units . UnitRefTyp
    ; PassFileSuffix : TEXT
    ; RdBackFileT : RdBackFile . T  
    )
  (* This has to be done early, while the pass's RdBack file is
     at its right end.  This is before we know whether the copy
     will be needed.
  *) 

  = VAR LPassFileSimpleName : TEXT 
  ; VAR LPassFileFullName : TEXT 
  ; VAR LCopyFullName : TEXT
  
  ; BEGIN
      LPassFileSimpleName
        := Pathname . Join
             ( NIL , UnitRef ^ . UntSrcFileSimpleName , PassFileSuffix )
    ; LPassFileFullName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath , LPassFileSimpleName , NIL )
    ; LCopyFullName 
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
    ; RdBackFile . Copy 
        ( RdBackFileT , LCopyFullName , - 1L )
    END MakePassFileCopy 

(*EXPORTED*) 
; PROCEDURE DisAsmPassFile
    ( UnitRef : FM3Units . UnitRefTyp ; PassFileSuffix : TEXT )
  (* PRE: A dispensible .Copy file exists in the build directory. *)
  (* POST: The disassembly file has been written in the build directory. *)
  (* POST: The copy file has been removed. *) 

  = VAR LPassFileFullName : TEXT 
  ; VAR LCopyFileFullName : TEXT
  ; VAR LDisAsmFileFullName : TEXT
  ; VAR LDisAsmWrT : Wr . T
  ; VAR LRdBack : RdBackFile . T
  
  ; BEGIN
      LPassFileFullName
        := Pathname . Join
             ( UnitRef ^ . UntBuildDirPath
             , UnitRef ^ . UntSrcFileSimpleName 
             , PassFileSuffix
             )
    ; LCopyFileFullName 
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
    ; LDisAsmFileFullName
        := Pathname . Join
             ( NIL , LPassFileFullName , FM3Globals . DisAsmFileSuffix ) 
    ; LDisAsmWrT := FileWr . Open ( LDisAsmFileFullName )
    ; LRdBack := RdBackFile . Open ( LCopyFileFullName )

    ; FM3DisAsm . DisAsmWOperandsBwd ( LRdBack , LDisAsmWrT )
    
    ; RdBackFile . Close ( LRdBack , - 1L )      
    ; Wr . Close ( LDisAsmWrT ) 

    ; TRY FS . DeleteFile ( LCopyFileFullName )
      EXCEPT OSError . E => (* It didn't exist.  Shouldn't happen. *) 
      END (*EXCEPT*)

    END DisAsmPassFile

(*EXPORTED*) 
; PROCEDURE CleanPassFilesAndCopies ( UnitRef : FM3Units . UnitRefTyp )
  (* Only after all passes have been run do we know what pass file
     copies are still hanging around.  Delete them.  Also, the existence
     of a copy file implies that no disassembly file was written during
     this pass, which in turn implies an existing disassembly file is
     a leftover from a previous compile.  Delete it too.
  *) 

  = VAR LPassFileFullName : TEXT
  ; VAR LCopyFileFullName : TEXT

  ; BEGIN (*CleanPassFilesAndCopies*)
      FOR RPassNo := FIRST ( FM3Base . PassNoRangeTyp ) 
                  TO LAST ( FM3Base . PassNoRangeTyp )
      DO 
        IF FM3Base . PassNo2 IN UnitRef ^ . UntPassNosDisAsmed
        THEN
          LPassFileFullName
            := Pathname . Join
                 ( UnitRef ^ . UntBuildDirPath
                 , UnitRef ^ . UntSrcFileSimpleName 
                 , FM3Globals . PassNoSuffixes [ RPassNo ] 
                 )
        ; LCopyFileFullName 
            := Pathname . Join
                 ( NIL , LPassFileFullName , FM3Globals . CopyFileSuffix ) 
        ; IF NOT RPassNo IN FM3CLOptions . PassNosToKeep  
          THEN
            TRY FS . DeleteFile ( LPassFileFullName )
            EXCEPT OSError . E => (* It didn't exist.  Could happen. *) 
            END (*EXCEPT*)
          END (*IF*) 
        ; TRY FS . DeleteFile ( LCopyFileFullName )
          EXCEPT OSError . E => (* It didn't exist.  Shouldn't happen. *) 
          END (*EXCEPT*)
        END (*IF*) 
      END (*FOR*) 
    END CleanPassFilesAndCopies

(*EXPORTED*)
; PROCEDURE CompileSrcFile ( SrcFileName : TEXT )

  = VAR LUnitRef : FM3Units . UnitRefTyp
  ; VAR LPoppedUnitRef : FM3Units . UnitRefTyp

  ; BEGIN (*CompileSrcFile*)
      FM3Pass1 . RunPass1 ( FM3CLOptions . SrcFileName )
      (* ^POST:  A UnitRef is pushed. *)
    ; LUnitRef := FM3Units . UnitStackTopRef 
    ; FM3Pass2 . RunPass2 ( LUnitRef )

    ; RdBackFile . Close 
        ( LUnitRef ^ . UntPass2OutRdBack , - 1L (* Leave full length. *) )
      (* ^When the next pass is implemented, don't do this. *)

    ; CleanPassFilesAndCopies ( LUnitRef ) 
    ; LPoppedUnitRef := FM3Units . PopUnit ( )
    ; <* ASSERT LPoppedUnitRef = LUnitRef *> 

      FM3Messages . FM3LogArr
        ( ARRAY OF REFANY
            { "Finished compiling " , LUnitRef ^ . UntSrcFileSimpleName , "." }
        )
    ; FM3Messages . EndUnit ( LPoppedUnitRef ^ . UntSrcFileSimpleName ) 

    END CompileSrcFile 

; BEGIN (*FM3Compile*)
  END FM3Compile
.

