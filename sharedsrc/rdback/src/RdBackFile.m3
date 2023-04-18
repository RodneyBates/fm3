        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A file that can be written (one byte at at time) normally, or
   read backwards.  Writes and reads can be interspersed, in which
   case, it behaves abstractly as a stack.
   
   It is maintained on a disk file during use, thus could grow
   very large, letting the OS deal with space, possibly in memory.
   
   It also can be closed and reopened later.
*) 

MODULE RdBackFile

; IMPORT Atom 
; IMPORT AtomList 
; IMPORT FS 
; IMPORT File
; IMPORT OSError
; IMPORT RegularFile 
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Wr

; CONST Beginning = RegularFile . Origin . Beginning

; CONST BlockSize = 16_400 
; CONST BlockSizeL = VAL ( BlockSize , LONGINT ) 

; TYPE BlockSsTyp = [ 0 .. BlockSize - 1 ] 
; TYPE BlockTyp = ARRAY BlockSsTyp OF ByteTyp 

; REVEAL T
  = BRANDED "RdBackFile.T" REF RECORD
      RbLengthL : LONGCARD := 0L (* Byte count of current abstract contents. *) 
    ; RbDiskLengthL : LONGCARD := 0L (* Byte count of the disk file. *)   
    ; RbBlockNo : INTEGER := - 1 (* Current block number. *)
    ; RbBlockCt : INTEGER := 0 
    ; RbRMBlockByteNo : INTEGER 
    ; RbNextIn : INTEGER 
    ; RbBlockNextIn : INTEGER := 0
    ; RbFileName : TEXT := ""
    ; RbFileHandle : RegularFile . T 
    ; RbIsOpen : BOOLEAN 
    ; RbBuffer : BlockTyp (* In-memory contents of current block. *) 
    END (*T*)

(* The file is handled in fixed-size blocks of size BlockSize.  RbBuffer is
   an in-memory copy of block number RbBlockNo, which is not necessarily
   up-to-date on disk.  RbBlockNo is the rightmost that contains meaningful
   data, which occupy bytes [0..RbBlockNextIn-1] of RbBuffer.  RbBlockNextIn
   is the subscript where the next byte should go into RbBuffer, but it can
   be one beyond the end of RbBuffer, i.e. equal to BlockSize.  If a Put
   occurs in this state, the RbBuffer must first be moved to the block to the
   right.  RbBlockNextIn-1 is the subscript of the next byte to be returned,
   but analogously, it can be one below the beginning of RbBuffer, i.e., -1,
   with an analogous requirement to move to the block to the left, should a
   GetBack occur in this state.
*)

; PROCEDURE Raise
    ( T1 , T2 , T3 , T4 , T5 : TEXT := NIL ; Code : AtomList . T := NIL )
  RAISES { OSError . E } 

  = VAR LMsg : TEXT
  ; VAR LWrT : Wr . T
  ; VAR LNewAtom : Atom . T
  ; VAR LNewList : AtomList . T  

  ; <*FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      LWrT := NEW ( TextWr . T )
    ; Wr . PutText ( LWrT , "RdBackFile." ) 
    ; IF T1 # NIL THEN Wr . PutText ( LWrT , T1) END (*IF*)
    ; IF T2 # NIL THEN Wr . PutText ( LWrT , T2) END (*IF*)
    ; IF T3 # NIL THEN Wr . PutText ( LWrT , T3) END (*IF*)
    ; IF T4 # NIL THEN Wr . PutText ( LWrT , T4) END (*IF*)
    ; IF T5 # NIL THEN Wr . PutText ( LWrT , T5) END (*IF*)
    ; LMsg := TextWr . ToText ( LWrT )
    ; LNewAtom := Atom . FromText ( LMsg )
    ; LNewList := AtomList . Cons ( LNewAtom , Code )
    ; RAISE OSError . E ( LNewList ) 
    END Raise 

; PROCEDURE InnerOpen 
    ( FileName : TEXT
    ; MsgLabel : TEXT
    ; Truncate : BOOLEAN (* To empty. *)
    ; MustBeEmpty : BOOLEAN
    )
  : T
  RAISES { OSError . E }   

  = VAR LResult : T
  ; VAR LStatus : File . Status 

  ; BEGIN
      IF FileName = NIL THEN FileName := "" END (*IF*) 
    ; IF FileName = NIL
      THEN Raise ( MsgLabel , ", empty file name." ) 
      END (*IF*) 
    ; LResult := NEW ( T )
    ; LResult . RbFileName := FileName 
    ; TRY
        TYPECASE  
          FS . OpenFile
            ( FileName
            , truncate := Truncate
            , create := FS . CreateOption . Ok
              (* Open if exists.  Create if not. *) 
            , access := FS . AccessOption . Default
            )
        OF NULL
        => Raise ( MsgLabel , ", FS.OpenFile failed for " , FileName , "." )
        | RegularFile . T (  RegFile ) 
        => LStatus := RegFile . status ( ) 
         ; IF MustBeEmpty AND LStatus . size > 0L
           THEN
             TRY LResult . RbFileHandle . close ( ) 
             EXCEPT OSError . E => (* Let's just ignore this one. *) 
             END (*EXCEPT*)
           ; Raise ( MsgLabel , " file " , FileName , " not empty." )
           END (*IF*)
         ; LResult . RbFileHandle := RegFile
         ; LResult . RbDiskLengthL := LStatus . size 
         ; LResult . RbLengthL := LStatus . size 
        ELSE
          Raise ( MsgLabel , FileName , " is not a regular file.")
        END (*TYPECASE*)
      EXCEPT OSError . E ( Code )
      => Raise ( MsgLabel , ", unable to open " , FileName , "." , Code := Code )
      END (*EXCEPT*)

    ; LResult . RbIsOpen := TRUE
    ; RETURN LResult 
    END InnerOpen  

(*EXPORTED*)
; PROCEDURE Create
    ( Filename : TEXT ; Truncate (* To empty. *) := FALSE ) : T
  RAISES { OSError . E }   

  = VAR LResult : T

  ; BEGIN
      LResult 
        := InnerOpen ( Filename , "Create" , Truncate , MustBeEmpty := TRUE )
    ; LResult . RbBlockNo := 0 
    ; LResult . RbBlockCt := 0 
    ; LResult . RbLengthL := 0L 
    ; LResult . RbDiskLengthL := 0L
    ; LResult . RbNextIn := 0 
    ; RETURN LResult 
    END Create  

(*EXPORTED*)
; PROCEDURE Open ( Filename : TEXT ) : T RAISES { OSError . E }   

  = VAR LResult : T 

  ; BEGIN
      LResult 
        := InnerOpen 
             ( Filename , "Open" , Truncate := FALSE , MustBeEmpty := FALSE )
    ; LResult . RbBlockNo
        := VAL ( LResult . RbLengthL DIV BlockSizeL , INTEGER ) 
    ; LResult . RbNextIn
        := VAL ( LResult . RbLengthL MOD BlockSizeL , INTEGER )
    ; LResult . RbBlockCt
        := LResult . RbBlockNo + ORD ( LResult . RbNextIn > 0 )  
    ; RETURN LResult 
    END Open  

(*EXPORTED*)
; PROCEDURE LengthL ( RbFile : T ) : LONGCARD RAISES { OSError . E }

  = BEGIN
      IF RbFile = NIL THEN Raise ( "LengthL, NIL ReadBackFile." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "LengthL, " , RbFile . RbFileName , "not open." )
      END (*IF*) 
    ; RETURN RbFile . RbLengthL  
    END LengthL 

(*EXPORTED*)
; PROCEDURE IsEmpty ( RbFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than Length(F)=0L. *) 

  = BEGIN
      IF RbFile = NIL THEN Raise ( "IsEmpty, NIL ReadBackFile." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "IsEmpty" , RbFile . RbFileName , "not open." )
      END (*IF*) 
    ; RETURN RbFile . RbLengthL <= 0L
    END IsEmpty
    
; PROCEDURE Seek
    ( RbFile : T ; SeekToL : LONGINT ; MsgTag : TEXT ) RAISES { OSError . E }
    
  = VAR LGotL : LONGINT
  (* The following should be LONGINT, but seek has not been updated.*)
  ; VAR LGot : INTEGER 
  ; VAR LSeekTo : INTEGER
  
  ; BEGIN
      TRY
        LSeekTo := VAL ( SeekToL , INTEGER ) 
      ; LGot := RbFile . RbFileHandle . seek ( Beginning , LSeekTo )
      ; LGotL := VAL ( LGot , LONGINT ) 
      ; IF LGotL # SeekToL
        THEN
          Raise
            ( MsgTag , ", wrong seek length for " , RbFile . RbFileName , "'" )
        END (*IF*) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", seek failure for " , RbFile . RbFileName , "'"
           , Code := Code
           ) 
      END (*EXCEPT*) 
    END Seek 

; PROCEDURE ReadBuffer
    ( RbFile : T ; Length : INTEGER ; MsgTag : TEXT ) RAISES { OSError . E } 

  = VAR LSeekToL : LONGINT
  ; VAR LGotL : LONGINT
  ; VAR LGot : INTEGER

  ; BEGIN 
      LSeekToL := VAL ( RbFile . RbBlockNo , LONGINT ) * BlockSizeL
    ; Seek ( RbFile , LSeekToL , MsgTag )
    ; TRY
        LGot
          := RbFile . RbFileHandle . read 
               ( (*OUT*) SUBARRAY ( RbFile . RbBuffer , 0 , Length )
               , mayBlock := FALSE
               )
      ; LGotL := VAL ( LGot , LONGINT ) 
      ; IF LGotL # LSeekToL
        THEN
          Raise
            ( MsgTag , ", wrong read length for " , RbFile . RbFileName , "'" )
        END (*IF*) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", failure reading " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*) 
    END ReadBuffer

; PROCEDURE WriteBuffer
    ( RbFile : T 
    ; READONLY Buffer : BlockTyp
      (* ^So this can be used for file copy without extra buffer copy. *) 
    ; Length : INTEGER
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LSeekToL : LONGINT 

  ; BEGIN
      IF Length <= 0 THEN RETURN END (*IF*) 
    ; LSeekToL := VAL ( RbFile . RbBlockNo , LONGINT ) * BlockSizeL 
    ; Seek ( RbFile , LSeekToL , MsgTag )
    ; TRY RbFile . RbFileHandle . write 
            ( SUBARRAY ( Buffer , 0 , Length ) ) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", failure writing " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*)
    ; RbFile . RbDiskLengthL
        := MAX ( RbFile . RbDiskLengthL
               , LSeekToL + VAL ( Length , LONGINT )
               ) 
    END WriteBuffer

; PROCEDURE SimpleClose ( RbFile : T ) RAISES { OSError . E }

  = BEGIN
      TRY RbFile . RbFileHandle . close ( ) 
      EXCEPT OSError . E => (* Let's just ignore this one. *) 
      END (*EXCEPT*)
    END SimpleClose

; PROCEDURE TempFileName ( OrigFileName : TEXT ) : TEXT

  = BEGIN
      RETURN OrigFileName & "tempA3v6JT9" 
    END TempFileName 

(*EXPORTED*)
; PROCEDURE Close ( RbFile : T ) RAISES { OSError . E }

  = VAR LTempFile : T
  ; VAR LFileName : TEXT
  ; VAR LTempFileName : TEXT
  ; VAR LFullBlockCt : INTEGER
  ; VAR LTotalBlockCt : INTEGER
  ; VAR LPartialBlockSize : INTEGER

  ; BEGIN
      IF RbFile = NIL THEN RETURN END (*IF*)
    ; IF NOT RbFile . RbIsOpen THEN RETURN  END (*IF*)
    ; IF RbFile . RbNextIn > 0
      THEN
        WriteBuffer ( RbFile , RbFile . RbBuffer , RbFile . RbNextIn , "Close" )
      END (*IF*) 

(* HELP!  How can we truncate a RegularFile?  *)

    ; IF FALSE (* Disable this for now, because: *) 
               (* But we don't have even a full Truncate library call. *)
         AND RbFile . RbLengthL = 0L
      THEN (* Make the disk file empty too. *)
        SimpleClose ( RbFile ) 
      ELSIF RbFile . RbDiskLengthL = RbFile . RbLengthL
      THEN (* Disk is the exact length, no truncation needed. *)
        SimpleClose ( RbFile ) 

      ELSE (* Must partially truncate the disk file. *)

      (* The Modula-3 libraries provide no way to truncate a RegularFile,
         which is needed here.  Adding this would entail OS-dependent
         versions for POSIX and WIN32 and make this module dependent on having
         the latest RegularFile in the CM3 installation.  I don't what to
         get into that now, so am resorting to doing a partial copy of the
         disk file.  Yes, it's crude, inefficient, and entails a lot of
         code to acheive such a simple result, but it's the path of least
         resistance now. 
      *)
      
        LTempFileName := TempFileName ( RbFile . RbFileName ) 
      ; LTempFile := Create ( LTempFileName , Truncate := FALSE )
      ; LTempFile . RbBlockNo := 0 
      ; RbFile . RbBlockNo := 0 
      ; LFullBlockCt
          := VAL ( RbFile . RbLengthL DIV BlockSizeL , INTEGER ) 
      ; LPartialBlockSize 
          := VAL ( RbFile . RbLengthL MOD BlockSizeL , INTEGER )
      ; LTotalBlockCt
          := RbFile . RbBlockNo + ORD ( RbFile . RbNextIn > 0 )  
      ; FOR RBlockNo := 0 TO LFullBlockCt - 1
        DO
          ReadBuffer ( RbFile , BlockSize , "Close" )
        ; WriteBuffer ( LTempFile , RbFile . RbBuffer , BlockSize , "Close" ) 
        END (*FOR*)
      ; IF LPartialBlockSize > 0
        THEN (* Copy partial block. *) 
          ReadBuffer ( RbFile , LPartialBlockSize , "Close" )
        ; WriteBuffer
            ( LTempFile , RbFile . RbBuffer , LPartialBlockSize , "Close" ) 
        END (*IF*)
      ; LFileName := RbFile . RbFileName (* Just paranoia. *) 
      ; SimpleClose ( RbFile ) 
      ; TRY FS . DeleteFile ( LFileName )
        EXCEPT OSError . E ( Code ) 
        => Raise
             ( "Close", ", failure deleting " , LFileName , " for rename."
             , Code := Code
             ) 
        END (*EXCEPT*) 
      ; SimpleClose ( RbFile ) 
      ; TRY FS . Rename ( LTempFileName , LFileName )
        EXCEPT OSError . E ( Code ) 
        => Raise
             ( "Close, failure renaming " , LTempFileName
             , " to " , LFileName , "."
             , Code := Code
             ) 
        END (*EXCEPT*) 
      END (*IF*) 

    END Close 

(*EXPORTED*)
; PROCEDURE Put ( RbFile : T ; Value : ByteTyp )  RAISES { OSError . E }  

  = BEGIN
      IF RbFile = NIL THEN Raise ( "Put" , ", NIL ReadBackFile." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "Put" , RbFile . RbFileName , "not open." )
      END (*IF*) 

    ; IF RbFile . RbBlockNextIn >= BlockSize
      THEN (* Right of the block in RbBuffer, if any. *)
        IF RbFile . RbBlockNo >= 0 (* Block RbBlockNo exists. *) 
        THEN (* Write to disk. *)
          WriteBuffer ( RbFile , RbFile . RbBuffer , BlockSize , "Put" ) 
        END (*IF*)
      (* Moving rightward to a new block number. It will, extremely soon,
         contain an unstored byte. *)
      ; INC ( RbFile . RbBlockNo ) 
      ; RbFile . RbBlockNextIn := 0 
      END (*IF*)
      
    (* Store the new byte. *) 
    ; RbFile . RbBuffer [ RbFile . RbBlockNextIn ] := Value
    ; INC ( RbFile . RbBlockNextIn )
    ; INC ( RbFile . RbLengthL ) 
    END Put 

(*EXPORTED*)
; PROCEDURE GetBwd ( RbFile : T ) : ByteTyp  RAISES { BOF , OSError . E }  

  = VAR LSeekToL : LONGINT
  ; VAR LLength : INTEGER
  ; VAR LResult : ByteTyp 

  ; BEGIN
      IF RbFile = NIL THEN Raise ( "GetBwd" , ", NIL ReadBackFile." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "GetBwd" , RbFile . RbFileName , "not open." )
      END (*IF*) 

    ; IF RbFile . RbLengthL = 0L THEN RAISE BOF END (* IF *)

    (* If necessary, read block RbBlockNo into RbBuffer. *) 
    ; IF RbFile . RbBlockNextIn = 0 (* Left of the block in RbBuffer. *)
         AND RbFile . RbBlockNo >= 0 (* Block to left exists. *) 
      THEN
        DEC ( RbFile . RbBlockNo )


(* Seeks? *) 
      ; LLength 
          := MIN ( VAL ( RbFile . RbDiskLengthL - LSeekToL , INTEGER )
                 , BlockSize
                 ) 
      ; ReadBuffer ( RbFile , LLength , "GetBwd" )
      ; RbFile . RbBlockNextIn := BlockSize 
      END (*IF*)
      
    (* Fetch the desired byte. *) 
    ; DEC ( RbFile . RbBlockNextIn )
    ; LResult := RbFile . RbBuffer [ RbFile . RbBlockNextIn ] 
    ; DEC ( RbFile . RbLengthL )
    ; RETURN LResult 
    END GetBwd 

; BEGIN
  END RdBackFile
.

