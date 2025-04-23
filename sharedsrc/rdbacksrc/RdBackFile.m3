        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023..2025  Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A file that can be written (one byte at at time) normally, or
   read backwards, either way, only at the right end.  Writes and
   reads can be interspersed, in which case, it behaves abstractly
   as a stack.
   
   It is maintained on a disk file during use, thus could grow very
   large, letting the OS deal with space management, possibly in memory.
   
   It also can be closed and reopened later.  When closed, its contents
   are indistinguishable from and interchangable with a plain file, thus
   handled by FileRd or FileWr.
(* Is this really true? *) 

*)

(* TODO:

   1) remove checks for an open file from several procs and just make
      it a precondition.

   2) Integrate the logging with FM3's system, while not pulling FM3
      itself.

   3) Figure out an EOF sentinal or something to avoid the whole copy
      business during close, just to denote the end.

   4) Add a Flush.
*)

MODULE RdBackFile

; IMPORT Atom 
; IMPORT AtomList 
; IMPORT File
; IMPORT FS 
; IMPORT OSError
; IMPORT RegularFile
; IMPORT Stdio 
; IMPORT Text
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Wr

; IMPORT FM3UnsafeUtils

; CONST Beginning = RegularFile . Origin . Beginning
; CONST Current = RegularFile . Origin . Current 

; CONST BlockSize = 16_400 
; CONST BlockSizeL = VAL ( BlockSize , LONGINT ) 

; TYPE BlockSsTyp = [ 0 .. BlockSize - 1 ] 
; TYPE BlockTyp = ARRAY BlockSsTyp OF File . Byte

; TYPE HeaderTyp
    = RECORD
        HdrPrefix : ARRAY [ 0 .. 7 ] OF File . Byte 
        (* Same as FM3SharedGlobals . PrefixTyp *) 
      ; HdrLengthL : LONGINT := 0L    (* These are relative to the byte no ... *)
      ; HdrMaxLengthL : LONGINT := 0L (* immediately following the header. *)
        (* ^N.B. This is not the length of the header.  It is stored in the
           header and gives the number of active data (i.e. non-header) bytes.
        *) 
      END

; CONST HeaderSize = BYTESIZE ( HeaderTyp )
; CONST HeaderSizeL = VAL ( HeaderSize , LONGINT ) 

(* All lengths and offsets within the file are relative to after the prefix. *) 

; REVEAL T
  = BRANDED "RdBackFile.T" REF RECORD 
      RbDiskLengthL : LONGCARD := 0L (* Byte count of the disk file. *)   
    ; RbBlockNo : INTEGER := - 1 (* Current block number. *)
    ; RbBlockCt : INTEGER := 0 
    ; RbBlockNextIn : INTEGER := 0
    ; RbFileName : TEXT := ""
    ; RbFileHandle : RegularFile . T 
    ; RbIsOpen : BOOLEAN
    ; RbIsDirty : BOOLEAN 
    ; RbCheckLastTok : INTEGER
    ; RbCheckArgCt : INTEGER
(* TODO: The above two fields do not belong in this abstraction, but it's a lot
   of work to put them into a subtype.
*)
    ; RbForceAlign64 : LONGINT 
    ; RbHeader : HeaderTyp (* In-memory contents of file header. *) 
    ; RbBuffer : BlockTyp  (* In-memory contents of current block. *) 
    END (*T*)

(* The file is handled in fixed-sized blocks of size BlockSize.  RbBuffer is
   an in-memory copy of block number RbBlockNo, which is not necessarily
   up-to-date on disk.  RbBlockNo denotes the rightmost block that contains
   meaningful current data, which occupy bytes [0..RbBlockNextIn-1] of RbBuffer.
   RbBlockNextIn is the subscript where the next byte would go into RbBuffer,
   but it can be one beyond the end of RbBuffer, i.e. equal to BlockSize
   (if the block is full) or one greater than the rightmost occupied byte.
   If a Put occurs in this state, RbBuffer must first be changed to contain
   the block to the right, after flushing its old contents to disk.
   RbBlockNextIn-1 is the subscript of the next byte to be removed, but
   analogous to the above, it can be one to the left of the beginning of
   RbBuffer, i.e., -1, with an analogous requirement to flush and change
   the block to the left, should a GetBwd occur in this state.  Thus there
   is hysteresis, avoiding rapid jitter in block-shifting.
*)

; VAR GDoStderr := TRUE 
; VAR GDoLog := TRUE
; VAR GLogWrT : Wr . T := NIL 

; PROCEDURE LogMsg ( Msg : TEXT )
(* TODO: Move this somewhere universal and provide a way to set its options. *) 
  = <*FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      IF GDoLog AND GLogWrT # NIL AND NOT Wr . Closed ( GLogWrT ) 
      THEN (* Write to log file. *) 
        Wr . PutText ( GLogWrT , Msg ) 
      ; Wr . PutText ( GLogWrT , Wr . EOL )
      END (*IF*)
    ; IF GDoStderr
      THEN 
        Wr . PutText ( Stdio . stderr , Msg ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL )
      END (*IF*) 
    END LogMsg

; PROCEDURE Raise
    ( T1 , T2 , T3 , T4 , T5 : TEXT := NIL ; Code : AtomList . T := NIL )
  RAISES { OSError . E } 

  = VAR LMsg : TEXT
  ; VAR LWrT : Wr . T
  ; VAR LNewAtom : Atom . T
  ; VAR LNewList : AtomList . T  

  ; <*FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      LWrT := TextWr . New ( ) 
    ; Wr . PutText ( LWrT , "RdBackFile." ) 
    ; IF T1 # NIL THEN Wr . PutText ( LWrT , T1) END (*IF*)
    ; IF T2 # NIL THEN Wr . PutText ( LWrT , T2) END (*IF*)
    ; IF T3 # NIL THEN Wr . PutText ( LWrT , T3) END (*IF*)
    ; IF T4 # NIL THEN Wr . PutText ( LWrT , T4) END (*IF*)
    ; IF T5 # NIL THEN Wr . PutText ( LWrT , T5) END (*IF*)
    ; LMsg := TextWr . ToText ( LWrT )
    ; LogMsg ( LMsg ) 
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

  ; BEGIN (*InnerOpen*) 
      IF FileName = NIL THEN Raise ( MsgLabel, ", NIL FileName." ) END (*IF*) 
    ; IF Text . Equal ( FileName , "" )
      THEN Raise ( MsgLabel, ", empty FileName." )
      END (*IF*) 
    ; LResult := NEW ( T )
    ; LResult . RbFileName := FileName 
    ; TRY
        TYPECASE  
          FS . OpenFile
            ( FileName
            , truncate := Truncate
            , create := FS . CreateOption . Ok
              (* ^Open if file exists.  Create if not. *) 
            , access := FS . AccessOption . Default
            )
        OF NULL
        => Raise ( MsgLabel , ", FS.OpenFile failed for " , FileName , "." )
        | RegularFile . T ( TRegFile ) 
        => LResult . RbFileHandle := TRegFile
         ; LStatus := TRegFile . status ( ) 
         ; IF MustBeEmpty AND LStatus . size > 0L
           THEN
             TRY LResult . RbFileHandle . close ( ) 
             EXCEPT OSError . E 
             => Raise ( MsgLabel , ", file " , FileName , " is not empty." )
             END (*EXCEPT*)
           END (*IF*)
         ; LResult . RbDiskLengthL := LStatus . size 
         ; ReadDisk
             ( LResult
             , 0L
             , HeaderSize
             , (*OUT*) LResult . RbHeader
             , MsgLabel
             ) 

         ELSE
           Raise ( MsgLabel , ", " , FileName , " is not a regular file." )
        END (*TYPECASE*)
      EXCEPT OSError . E ( Code )
      => Raise
           ( MsgLabel , ", unable to open " , FileName , "." , Code := Code )
      END (*EXCEPT*)

    ; LResult . RbIsOpen := TRUE
    ; RETURN LResult 
    END InnerOpen  

(*EXPORTED*)
; PROCEDURE Create
    ( Filename : TEXT ; Truncate (* To empty. *) := FALSE ) : T
(*TODO: Eliminate Truncate and treat as always TRUE.  Creating without
         truncating makes no sense.
*)
  RAISES { OSError . E }   

  = VAR LResult : T

  ; BEGIN
      LResult 
        := InnerOpen ( Filename , "Create" , Truncate , MustBeEmpty := TRUE )
    ; LResult . RbBlockNo := 0 
    ; LResult . RbBlockCt := 0 
    ; LResult . RbHeader . HdrLengthL := 0L 
    ; LResult . RbHeader . HdrMaxLengthL := 0L
    ; LResult . RbDiskLengthL := 0L
    ; LResult . RbBlockNextIn := 0 
    ; RETURN LResult 
    END Create  

(*EXPORTED*)
; PROCEDURE Open ( Filename : TEXT ) : T RAISES { OSError . E }   

  = VAR LResult : T 

  ; BEGIN
      LResult 
        := InnerOpen 
             ( Filename , "Open" , Truncate := FALSE , MustBeEmpty := FALSE )
    ; IF LResult . RbHeader . HdrLengthL <= 0L
      THEN 
        LResult . RbBlockNo := 0 
      ; LResult . RbBlockCt := 0 
      ; LResult . RbBlockNextIn := 0
      (* And RbBuffer is garbage. *)
      ELSE 
        LResult . RbBlockNo
          := VAL ( ( LResult . RbHeader . HdrLengthL - 1L ) DIV BlockSizeL
                 , INTEGER
                 ) 
      ; LResult . RbBlockNextIn
          := VAL
               ( ( ( LResult . RbHeader . HdrLengthL -1L ) MOD BlockSizeL ) + 1L
               , INTEGER
               )
      ; LResult . RbBlockCt := LResult . RbBlockNo + 1
      ; ReadDataBlock
          ( LResult
          , LResult . RbBlockNo
          , LResult . RbBlockNextIn
          , LResult . RbBuffer
          , "Open"
          )
      ; LResult . RbIsDirty := FALSE 
      END (*IF*) 
    ; RETURN LResult 
    END Open

(*EXPORTED*)
; PROCEDURE FileName ( RbFile : T ) : TEXT (* Never NIL.  Possibly empty. *) 

  = BEGIN
      IF RbFile = NIL THEN RETURN "" END (*IF*)
    ; IF RbFile . RbFileName = NIL THEN RETURN "" END (*IF*)
    ; RETURN RbFile . RbFileName 
    END FileName 

(*EXPORTED*)
; PROCEDURE LengthL ( RbFile : T ) : LONGCARD
  RAISES { OSError . E }

  = BEGIN
      IF RbFile = NIL THEN Raise ( "LengthL, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "LengthL, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbHeader . HdrLengthL  
    END LengthL 

(*EXPORTED*)
; PROCEDURE MaxLengthL ( RbFile : T ) : LONGCARD
  RAISES { OSError . E }

  = BEGIN
      IF RbFile = NIL THEN Raise ( "MaxLengthL, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "MaxLengthL, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbHeader . HdrMaxLengthL  
    END MaxLengthL 

(*EXPORTED*)
; PROCEDURE IsEmpty ( RbFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than Length(F)=0L. *) 

  = BEGIN
      IF RbFile = NIL THEN Raise ( "IsEmpty, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "IsEmpty, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbHeader . HdrLengthL <= 0L
    END IsEmpty
    
; PROCEDURE InnerSeek
    ( RbFile : T
    ; DiskAddrL : LONGINT (* Absolute in disk file. *) 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }
    
  = VAR LGot : INTEGER 
    (* ^This should be LONGINT, but RegularFile.T.read is behind the times.*)
  ; VAR LCurrent : INTEGER 
  ; VAR LDiskAddr : INTEGER
  (* ^These two should be LONGINT, but RegularFile.T.seek is behind the times.*)
  
  ; BEGIN
      LDiskAddr := VAL ( DiskAddrL , INTEGER )
    ; TRY
        LCurrent := RbFile . RbFileHandle . seek ( Current , 0 )
        (* ^Just returs current location. *) 
      ; IF LCurrent # LDiskAddr
        THEN 
          LGot
             := RbFile . RbFileHandle . seek
                  ( (* Relative to:*) Beginning , LDiskAddr )
        ; IF LGot # LDiskAddr
          THEN
            Raise
              ( MsgTag , ", wrong seek length for " , RbFile . RbFileName , "." )
          END (*IF*) 
        END (*IF*) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", seek failure for " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*) 
    END InnerSeek 

; PROCEDURE ReadDisk
    ( RbFile : T
    ; DiskAddrL : LONGINT 
    ; Length : INTEGER
    ; VAR Buffer : BlockTyp
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LGot : INTEGER
  (* ^This should be LONGINT, but RegularFile.T.read is behind the times.*)

  ; BEGIN 
      InnerSeek ( RbFile , DiskAddrL , MsgTag )
    ; TRY
        LGot
          := RbFile . RbFileHandle . read 
               ( (*OUT*) SUBARRAY ( Buffer , 0 , Length )
               , mayBlock := TRUE 
               )
      ; IF LGot # Length
        THEN
          Raise
            ( MsgTag , ", wrong read length for " , RbFile . RbFileName , "." )
        END (*IF*) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", failure reading " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*) 
    END ReadDisk

; PROCEDURE ReadDataBlock
    ( RbFile : T
    ; BlockNo : INTEGER 
    ; Length : INTEGER
    ; VAR Buffer : BlockTyp
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LDiskAddr : LONGINT


  ; BEGIN 
      LDiskAddr
        := VAL ( BlockNo , LONGINT ) * BlockSizeL + VAL ( HeaderSize , LONGINT ) 
    ; ReadDisk ( RbFile , LDiskAddr , Length , Buffer , MsgTag ) 
    END ReadDataBlock

; PROCEDURE WriteDisk
    ( RbFile : T 
    ; DiskAddrL : LONGINT 
    ; Length : INTEGER
    ; READONLY Buffer : ARRAY OF CHAR 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LFileCoord : LONGINT 

  ; BEGIN
      IF Length <= 0 THEN RETURN END (*IF*)
    ; InnerSeek ( RbFile , DiskAddrL , MsgTag )
    ; TRY RbFile . RbFileHandle . write ( SUBARRAY ( Buffer , 0 , Length ) ) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", failure writing " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*)
    ; RbFile . RbDiskLengthL
        := MAX ( RbFile . RbDiskLengthL
               , LFileCoord + VAL ( Length , LONGINT )
               ) 
    END WriteDisk

; PROCEDURE WriteDataBlock
    ( RbFile : T 
    ; BlockNo : INTEGER 
    ; Length : INTEGER
    ; READONLY Buffer : BlockTyp
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LDiskAddrL : LONGINT 

  ; BEGIN
      IF Length <= 0 THEN RETURN END (*IF*)
    ; IF BlockNo < 0 THEN RETURN END (*IF*)
    ; LDiskAddrL
        := VAL ( BlockNo , LONGINT ) * BlockSizeL + VAL ( HeaderSize , LONGINT )
    ; WriteDisk ( RbFile , LDiskAddrL , Length , Buffer , MsgTag )
    ; RbFile . RbDiskLengthL
        := MAX ( RbFile . RbDiskLengthL , LDiskAddrL + VAL ( Length , LONGINT ) )
    END WriteDataBlock

; PROCEDURE CloseDisk ( RbFile : T ) 
  = BEGIN
      TRY RbFile . RbFileHandle . close ( ) 
      EXCEPT OSError . E 
      => (* Let's log this one but not raise an exception. *) 
        LogMsg ( "Close of " & RbFile . RbFileName & " failed." )  
      END (*EXCEPT*)
    ; RbFile . RbIsOpen := FALSE 
    END CloseDisk

; PROCEDURE TempFileName ( OrigFileName : TEXT ) : TEXT

  = BEGIN
      RETURN OrigFileName & "_tempA3v6JT9"
(* TODO: Get a non-constant temp file name suffix, or maybe MUTEX
         the copy operation. *) 
    END TempFileName 

; PROCEDURE InnerCopy ( RbFile : T ; CopyFile : T ; TruncTo : LONGINT ) 
  (* PRE: RbFile is open. *)
  (* PRE: TruncTo IN [ 0L .. RbFile . RbMaxLenghtL ] *) 
  (* POST: CopyFile is open. *) 

  = VAR LFullBlockCt : INTEGER (*I.e., count of blocks that are full. *)
  ; VAR LTotalBlockCt : INTEGER
  ; VAR LPartialBlockSize : INTEGER
  ; VAR LBuffer : BlockTyp  

  ; BEGIN (*InnerCopy*)
      LFullBlockCt := VAL ( TruncTo DIV BlockSizeL , INTEGER ) 
    ; LPartialBlockSize := VAL ( TruncTo MOD BlockSizeL , INTEGER )
    ; LTotalBlockCt
        := LFullBlockCt + ORD ( LPartialBlockSize > 0 )  
    ; FOR RBlockNo := 0 TO LFullBlockCt - 1
      DO
        IF RbFile . RbBlockNo = RBlockNo
        THEN (* Write from in-memory block. *) 
          WriteDataBlock
            ( CopyFile , RBlockNo , BlockSize , RbFile . RbBuffer , "CopyMem" )
        ELSE (* Copy block from source file. *) 
          ReadDataBlock ( RbFile , RBlockNo , BlockSize , LBuffer , "Copy" )
        ; WriteDataBlock
            ( CopyFile , RBlockNo , BlockSize , LBuffer , "CopyFile" )
        END (*IF*) 
      END (*FOR*)
    ; IF LPartialBlockSize > 0
      THEN (* Copy partial block. *)
        IF RbFile . RbBlockNo = LFullBlockCt
        THEN
          WriteDataBlock
            ( CopyFile
            , LFullBlockCt
            , LPartialBlockSize
            , RbFile . RbBuffer
            , "CopyMemPartial"
            )
        ELSE 
          ReadDataBlock
            ( RbFile
            , LFullBlockCt
            , LPartialBlockSize
            , LBuffer
            , "CopyPartial"
            )
        ; WriteDataBlock
            ( CopyFile
            , LFullBlockCt
            , LPartialBlockSize
            , LBuffer
            , "CopyFilePartial"
            )
        END (*IF*) 
      END (*IF*)
    END InnerCopy
      
(*EXPORTED*)
; PROCEDURE Copy
    ( RbFile : T ; CopyFileName : TEXT ; TruncTo : LONGINT )
  (* TruncTo < 0 means max length. *)
  (* Does not alter RbFile *) 
  (* PRE & POST: RbFile is open. *)
  (* POST: The copy is closed. *)

  = VAR LTruncTo : LONGINT
  ; VAR LCopyFile : T 

  ; BEGIN (*Copy*)
      LCopyFile := Create ( CopyFileName , Truncate := TRUE )
    ; IF TruncTo < 0L THEN LTruncTo := RbFile . RbHeader . HdrMaxLengthL 
      ELSE LTruncTo := MIN ( TruncTo , RbFile . RbHeader . HdrMaxLengthL )
      END (*IF*) 
    ; InnerCopy ( RbFile , LCopyFile , LTruncTo ) 
    ; CloseDisk ( LCopyFile ) 
    END Copy
    
(*EXPORTED.*)
; PROCEDURE Seek ( RbFile : T ; LocationL : LONGINT ) 
  : LONGINT (* Actually positioned here. *) 

  = VAR LLocationL : LONGINT
  ; LNewBlockNo : INTEGER 
  ; LNewNextIn : INTEGER 
  ; LLength : INTEGER

  ; BEGIN (*Seek*)
      IF LocationL < 0L THEN LLocationL := 0L
      ELSIF LocationL > RbFile . RbHeader . HdrMaxLengthL
      THEN LLocationL := RbFile . RbHeader . HdrMaxLengthL
      ELSE LLocationL := LocationL
      END (*IF*) 
    ; IF LLocationL = RbFile . RbHeader . HdrLengthL 
      THEN RETURN LLocationL
      END (*IF*) 
    ; LNewBlockNo := VAL ( LocationL DIV BlockSizeL , INTEGER ) 
    ; LNewNextIn := VAL ( ( ( LocationL -1L ) MOD BlockSizeL ) + 1L , INTEGER )
    ; IF LNewBlockNo # RbFile . RbBlockNo
      THEN
        IF RbFile . RbIsDirty
        THEN 
          IF RbFile . RbBlockNo = RbFile . RbBlockCt - 1 
          THEN
            LLength
              := MIN ( RbFile . RbHeader . HdrLengthL  
                       - VAL ( RbFile . RbBlockNo , LONGINT ) * BlockSizeL
                     , BlockSizeL
                     )
          ELSE LLength := BlockSize
          END (*IF*) 
        ; WriteDataBlock
            ( RbFile 
            , RbFile . RbBlockNo
            , LLength 
            , RbFile . RbBuffer
            , "Seek"
            )
        END (*IF*) 
      ; IF LNewBlockNo = RbFile . RbBlockCt - 1 
        THEN
          LLength
            := MIN ( RbFile . RbHeader . HdrLengthL 
                     - VAL ( LNewBlockNo , LONGINT ) * BlockSizeL  
                   , BlockSizeL
                   )
        ELSE LLength := BlockSize
        END (*IF*) 
      ; ReadDataBlock
          ( RbFile , RbFile . RbBlockNo
          , LLength
          , RbFile . RbBuffer
          , "Copy"
          )
      ; RbFile . RbIsDirty := FALSE
      END (*IF*) 
    ; RbFile . RbIsDirty := FALSE
    ; RbFile . RbBlockNextIn := LNewNextIn 
    END Seek

; PROCEDURE InnerFlushDataBlock ( RbFile : T ; MsgTag : TEXT )

  = BEGIN (*InnerFlushDataBlock*)
      WriteDataBlock
        ( RbFile
        , RbFile . RbBlockNo
        , MIN ( BlockSize
              , VAL ( RbFile . RbHeader . HdrMaxLengthL , INTEGER ) 
                - RbFile . RbBlockNo * BlockSize
              ) 
        , RbFile . RbBuffer
        , MsgTag
        )
    ; RbFile . RbIsDirty := FALSE 
    END InnerFlushDataBlock
      
; PROCEDURE InnerFlushHeader ( RbFile : T ; MsgTag : TEXT )

  = BEGIN (*InnerFlushHeader*)
      WriteDisk
        ( RbFile
        , 0L
        , HeaderSize
        , RbFile . RbHeader
        , MsgTag
        )
    END InnerFlushHeader

(*EXPORTED.*)
; PROCEDURE Flush ( RbFile : T )

  = BEGIN (*Flush*)
      InnerFlushDataBlock ( RbFile , "Flush" ) 
    ; InnerFlushHeader ( RbFile , "Flush" ) 
    END Flush
      

(*EXPORTED*)
; PROCEDURE Close ( RbFile : T ; TruncToL : LONGINT )
  (* TruncTo < 0 means max length. *) 

  = VAR LTruncDiskToL : LONGINT 
  ; VAR LTempFile : T
  ; VAR LRbFileName : TEXT
  ; VAR LTempFileName : TEXT

  ; BEGIN
      IF RbFile = NIL THEN RETURN END (*IF*)
    ; IF NOT RbFile . RbIsOpen THEN RETURN END (*IF*)
    ; InnerFlushDataBlock ( RbFile , "Close" ) 
    ; IF TruncToL <= 0L 
      THEN
        TruncToL := 0L
      ; RbFile . RbHeader . HdrMaxLengthL := 0L 
      ; LTruncDiskToL := HeaderSizeL
      ELSIF TruncToL > RbFile . RbHeader . HdrMaxLengthL
      THEN
        TruncToL := RbFile . RbHeader . HdrMaxLengthL
      ; LTruncDiskToL := TruncToL + HeaderSizeL
      ELSE 
        RbFile . RbHeader . HdrMaxLengthL := TruncToL 
      ; LTruncDiskToL := TruncToL + HeaderSizeL
      END (*IF*)
    ; InnerFlushHeader ( RbFile , "Close" ) 
    ; IF TruncToL = RbFile . RbHeader . HdrMaxLengthL
      THEN (* No actual truncation needed. *) 
        CloseDisk ( RbFile )
      ; RETURN
      END (*IF*) 

    ; IF LTruncDiskToL < RbFile . RbDiskLengthL
      THEN (* Partially truncate the disk file. *)
      (* The Modula-3 libraries provide no way to truncate a RegularFile,
         which is needed here.  Adding this would entail OS-dependent
         versions for POSIX and WIN32 and make this module dependent on having
         the latest RegularFile in the CM3 installation.  I don't what to
         get into that now, so am resorting to doing a partial copy of the
         disk file.  Yes, it's crude, inefficient, and entails a lot of
         code to achieve such a simple result, but it's the path of least
         resistance now.

         So do it brute force by copying. 
      *)
      
        LRbFileName := RbFile . RbFileName (* Just paranoia. *) 
      ; LTempFileName := TempFileName ( LRbFileName ) 
      ; LTempFile := Create ( LTempFileName , Truncate := FALSE )
      ; InnerCopy ( RbFile , LTempFile , LTruncDiskToL ) 
      ; CloseDisk ( LTempFile ) (* So it's safe if anything fails later. *) 
      ; CloseDisk ( RbFile ) 
      ; TRY FS . DeleteFile ( LRbFileName )
        EXCEPT OSError . E ( Code ) 
        => Raise
             ( "Close", ", failure deleting " , LRbFileName , " for rename."
             , Code := Code
             ) 
        END (*EXCEPT*) 
      ; TRY FS . Rename ( LTempFileName , LRbFileName )
        EXCEPT OSError . E ( Code ) 
        => Raise
             ( "Close, failure renaming " , LTempFileName
             , " to " , LRbFileName , "."
             , Code := Code
             ) 
        END (*EXCEPT*) 
      END (*IF*) 
    END Close 

(*EXPORTED*)
; PROCEDURE Put ( RbFile : T ; Value : ByteTyp )
  RAISES { OSError . E }  

  = BEGIN
      IF RbFile = NIL THEN Raise ( "Put" , ", NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "Put, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 

    ; IF RbFile . RbBlockNextIn >= BlockSize
      THEN (* RbBlockNextIn is right of the block in RbBuffer, if any. *)
        IF RbFile . RbBlockNo >= 0 (* Block RbBlockNo exists. *)
           AND RbFile . RbIsDirty
        THEN (* Write to disk. *)
          WriteDataBlock
             ( RbFile
             , RbFile . RbBlockNo
             , BlockSize
             , RbFile . RbBuffer
             , "Put"
             ) 
        END (*IF*)
      (* Moving rightward to a new block number.  It will, extremely soon,
         contain a byte not written to disk. *)
      ; INC ( RbFile . RbBlockNo ) 
      ; RbFile . RbBlockNextIn := 0
      ; IF RbFile . RbHeader . HdrMaxLengthL
           > RbFile . RbHeader . HdrLengthL + 1L 
        THEN (* Byte(s) previously stored in the new disk block to the right
                will be needed, if they are neither overlaid, nor truncated
                when closing. *) 
          ReadDataBlock
            ( RbFile
            , RbFile . RbBlockNo
            , BlockSize
            , RbFile . RbBuffer
            , "Put"
            )
        ; RbFile . RbIsDirty := FALSE 
        END (*IF*) 
      END (*IF*)
      
    (* Store the new byte. *) 
    ; RbFile . RbBuffer [ RbFile . RbBlockNextIn ] := Value
    ; RbFile . RbIsDirty := TRUE 
    ; INC ( RbFile . RbBlockNextIn )
    ; INC ( RbFile . RbHeader . HdrLengthL )
    ; RbFile . RbHeader . HdrMaxLengthL
        := MAX ( RbFile . RbHeader . HdrMaxLengthL
               , RbFile . RbHeader . HdrLengthL
               )   
    END Put 

(*EXPORTED*)
; PROCEDURE GetBwd
    ( RbFile : T ; Consume := TRUE ) : ByteTyp RAISES { OSError . E , BOF }

  = VAR LResult : ByteTyp

  ; BEGIN
      IF RbFile = NIL THEN Raise ( "GetBwd" , ", NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "GetBwd, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 

    ; IF RbFile . RbHeader . HdrLengthL = 0L
      THEN RAISE BOF
      END (*IF*)

    (* If necessary, make RbBuffer contain the block to the left. *) 
    ; IF RbFile . RbBlockNextIn = 0 (* We are left of the block in RbBuffer. *)
         (* But RbFile not empty, => Block to left exists. *) 
      THEN
        IF RbFile . RbHeader . HdrMaxLengthL
           > RbFile . RbHeader . HdrLengthL + 1L
        THEN (* Byte(s) in the current buffer will be needed on disk,
                if they are neither overlaid, nor truncated when closing. *)
          WriteDataBlock
            ( RbFile
            , RbFile . RbBlockNo
            , BlockSize
            , RbFile . RbBuffer
            , "GetBwd"
            )
        END (*IF*) 
      ; DEC ( RbFile . RbBlockNo )

      ; ReadDataBlock
          ( RbFile
          , RbFile . RbBlockNo
          , BlockSize
          , RbFile . RbBuffer
          , "GetBwd"
          )
      ; RbFile . RbIsDirty := FALSE 
      ; RbFile . RbBlockNextIn := BlockSize 
      END (*IF*)
      
    (* Fetch the desired byte. *) 
    ; DEC ( RbFile . RbBlockNextIn )
    ; LResult := RbFile . RbBuffer [ RbFile . RbBlockNextIn ]
    ; IF Consume
      THEN DEC ( RbFile . RbHeader . HdrLengthL )
      ELSE INC ( RbFile . RbBlockNextIn ) (* Undo the above. *) 
      END (*IF*) 
    ; RETURN LResult 
    END GetBwd

(*EXPORTED.*)
; PROCEDURE CheckStatic ( )

  = VAR LT : T
  ; VAR LOk : BOOLEAN 

  ; BEGIN (*CheckStatic*)
      LT := NEW ( T )
    ; LOk := ( FM3UnsafeUtils . AddrToLongInt ( ADR ( LT ^ . RbHeader ) ) )
             MOD VAL ( BYTESIZE ( LONGINT ) , LONGINT ) = 0L
    ; <* ASSERT LOk*> 
      LOk := ADR ( LT ^ . RbBuffer ) MOD BYTESIZE ( LONGINT ) = 0L
    ; <* ASSERT LOk *> 
    END CheckStatic
      
; BEGIN (*RdBackFile*) 
    CheckStatic ( ) 

(* Do all this elsewhere.  
  ; GLogWrT := NIL
  ; GDoStderr := TRUE 
  ; GDoLog := FALSE
  ; TRY 
      GLogWrT := FileWr . Open ( "FM3Log")
    EXCEPT OSError . E ( EMsg )
    => FM3Messages . Log (* Oh the irony. *) 
         ( " Unable to open log file " , FM3Messages . LogFileName
         , ", OSError.E(" , FM3SharedUtils . AtomListToText ( EMsg ) , ")."
         )
    END (*EXCEPT*)
*) 
  END RdBackFile
.

