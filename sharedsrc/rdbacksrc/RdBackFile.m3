        
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

*)

MODULE RdBackFile

; IMPORT Atom 
; IMPORT AtomList
; IMPORT Fmt 
; IMPORT File
; IMPORT FS 
; IMPORT OSError
; IMPORT RegularFile
; IMPORT Stdio 
; IMPORT Text
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Wr

; IMPORT FM3SharedGlobals 
; IMPORT FM3SharedUtils 
; IMPORT FM3UnsafeUtils

; CONST Beginning = RegularFile . Origin . Beginning
; CONST Current = RegularFile . Origin . Current 

; CONST BlockSize = 16_400 
; CONST BlockSizeL = VAL ( BlockSize , LONGINT ) 

; TYPE BlockSsTyp = [ 0 .. BlockSize - 1 ] 
; TYPE BlockTyp = ARRAY BlockSsTyp OF File . Byte

; TYPE Bytes8 = ARRAY [ 0 .. 7 ] OF File . Byte
; CONST PrefixSize = BYTESIZE ( Bytes8 ) 
; CONST PrefixSizeL = VAL ( PrefixSize , LONGINT ) 

; CONST HeaderSize = PrefixSize + 2 * BYTESIZE ( LONGINT )
; CONST HeaderSizeL = VAL ( HeaderSize , LONGINT ) 

(* All lengths and offsets within the file are relative to after the prefix. *) 

; REVEAL T
  = BRANDED "RdBackFile.T" REF RECORD 
      RbDiskLengthL : LONGCARD := 0L (* Byte count of the disk file. *)   
    ; RbLengthL : LONGINT := 0L    (* These are relative to the byte no ... *)
    ; RbMaxLengthL : LONGINT := 0L (* immediately following the header. *)
    ; RbFileName : TEXT := ""
    ; RbFileHandle : RegularFile . T 
    ; RbBlockNo : INTEGER := 0 (* Current block number. *)
    ; RbBlockCt : INTEGER := 0 
    ; RbBlockNextIn : INTEGER := 0
    ; RbIsOpen : BOOLEAN
    ; RbIsDirty : BOOLEAN 
    ; RbCheckLastTok : INTEGER
    ; RbCheckArgCt : INTEGER
(* TODO: The above two fields do not belong in this abstraction, but it's a lot
   of work to put them into a subtype.
*)
    ; RbPrefix : ARRAY [ 0 .. 7 ] OF File . Byte 
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
    ( T1 , T2 , T3 , T4 , T5 , T6 , T7 , T8 : TEXT := NIL
    ; Code : AtomList . T := NIL
    )
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
    ; IF T6 # NIL THEN Wr . PutText ( LWrT , T6) END (*IF*)
    ; IF T7 # NIL THEN Wr . PutText ( LWrT , T7) END (*IF*)
    ; IF T8 # NIL THEN Wr . PutText ( LWrT , T8) END (*IF*)
    ; LMsg := TextWr . ToText ( LWrT )
    ; LogMsg ( LMsg ) 
    ; LNewAtom := Atom . FromText ( LMsg )
    ; LNewList := AtomList . Cons ( LNewAtom , Code )
    ; RAISE OSError . E ( LNewList ) 
    END Raise

; PROCEDURE ReadHeader ( RbFile : T ; MsgTag : TEXT )

  = VAR LBytes8 : Bytes8 

  ; BEGIN (*ReadHeader*)
      ReadDisk
        ( RbFile
        , 0L
        , PrefixSize
        , (*OUT*) RbFile . RbPrefix
        , MsgTag & " read prefix"
        )
    ; ReadDisk
        ( RbFile
        , PrefixSizeL
        , BYTESIZE ( Bytes8 )
        , (*OUT*) LBytes8
        , MsgTag & " read length"
        )
    ; RbFile . RbLengthL := FM3UnsafeUtils . Bytes8ToLongInt ( LBytes8 ) 
    ; ReadDisk
        ( RbFile
        , VAL ( PrefixSize + BYTESIZE ( Bytes8 ) , LONGINT )  
        , BYTESIZE ( Bytes8 )
        , (*OUT*) LBytes8
        , MsgTag & " read max length" 
        )
    ; RbFile . RbMaxLengthL := FM3UnsafeUtils . Bytes8ToLongInt ( LBytes8 ) 
    END ReadHeader

; PROCEDURE WriteHeader ( RbFile : T ; MsgTag : TEXT )

  = VAR LBytes8 : Bytes8 

  ; BEGIN (*WriteHeader*)
      WriteDisk
        ( RbFile
        , 0L
        , PrefixSize
        , RbFile . RbPrefix
        , MsgTag & " write prefix"
        ) 
    ; LBytes8 := FM3UnsafeUtils . LongIntToBytes8 ( RbFile . RbLengthL ) 
    ; WriteDisk
        ( RbFile
        , PrefixSizeL
        , BYTESIZE ( Bytes8 )
        , LBytes8
        , MsgTag & " write length"
        )
    ; LBytes8 := FM3UnsafeUtils . LongIntToBytes8 ( RbFile . RbMaxLengthL) 
    ; WriteDisk
        ( RbFile
        , VAL ( PrefixSize + BYTESIZE ( Bytes8 ) , LONGINT )  
        , BYTESIZE ( Bytes8 )
        , LBytes8
        , MsgTag & " write max length" 
        )
    END WriteHeader

; PROCEDURE InnerOpen 
    ( FileName : TEXT
    ; MsgLabel : TEXT
    ; Truncate : BOOLEAN (* To empty. *)
    ; MustBeEmpty : BOOLEAN
    )
  : T
  RAISES { OSError . E }   

  = VAR LRbFile : T
  ; VAR LStatus : File . Status 

  ; BEGIN (*InnerOpen*) 
      IF FileName = NIL THEN Raise ( MsgLabel, ", NIL FileName." ) END (*IF*) 
    ; IF Text . Equal ( FileName , "" )
      THEN Raise ( MsgLabel, ", empty FileName." )
      END (*IF*) 
    ; LRbFile := NEW ( T )
    ; LRbFile . RbFileName := FileName 
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
        => LRbFile . RbFileHandle := TRegFile
         ; LStatus := TRegFile . status ( ) 
         ; IF MustBeEmpty AND LStatus . size > 0L
           THEN
             TRY LRbFile . RbFileHandle . close ( ) 
             EXCEPT OSError . E 
             => Raise ( MsgLabel , ", file " , FileName , " is not empty." )
             END (*EXCEPT*)
           END (*IF*)
         ; LRbFile . RbDiskLengthL := LStatus . size
         ELSE
           Raise ( MsgLabel , ", " , FileName , " is not a regular file." )
        END (*TYPECASE*)
      EXCEPT OSError . E ( Code )
      => Raise
           ( MsgLabel , ", unable to open " , FileName , "." , Code := Code )
      END (*EXCEPT*)

    ; LRbFile . RbIsOpen := TRUE
    ; RETURN LRbFile 
    END InnerOpen  

(*EXPORTED*)
; PROCEDURE Create ( Filename : TEXT ) : T
  RAISES { OSError . E }   

  = VAR LRbFile : T

  ; BEGIN
      LRbFile 
        := InnerOpen
             ( Filename , "Create" , Truncate := TRUE , MustBeEmpty := TRUE )
    ; LRbFile . RbBlockNo := 0 
    ; LRbFile . RbBlockCt := 0 
    ; LRbFile . RbLengthL := 0L 
    ; LRbFile . RbMaxLengthL := 0L
    ; LRbFile . RbDiskLengthL := 0L
    ; LRbFile . RbBlockNextIn := 0
    ; LRbFile . RbPrefix
        := FM3SharedUtils . FilePrefixB
             ( FM3SharedGlobals .  FM3FileKindRdBack
             , FM3SharedGlobals .  FM3FileVersion0
             ) 
    ; WriteHeader ( LRbFile , "Create" ) 
    ; RETURN LRbFile 
    END Create  

(*EXPORTED*)
; PROCEDURE Open ( Filename : TEXT ) : T
  RAISES { FM3SharedUtils . FatalError , OSError . E }

  = VAR LRbFile : T 
  ; VAR LFileKind : FM3SharedGlobals . FileKindTyp 
  ; VAR LFileVersion : FM3SharedGlobals . FileVersionTyp 
  ; VAR LIsOK : BOOLEAN 

  ; BEGIN
      LRbFile 
        := InnerOpen 
             ( Filename , "Open" , Truncate := FALSE , MustBeEmpty := FALSE )
    ; ReadHeader ( LRbFile , "Open" ) 
    ; IF LRbFile . RbLengthL <= 0L
      THEN 
        LRbFile . RbBlockNo := 0 
      ; LRbFile . RbBlockCt := 0 
      ; LRbFile . RbBlockNextIn := 0
      (* And RbBuffer is uninitialized. *)
      ELSE
        ReadHeader ( LRbFile , "Open" )
      ; FM3SharedUtils . ParsePrefixB
          ( LRbFile . RbPrefix
          , (*OUT*) LFileKind
          , (*OUT*) LFileVersion
          , (*OUT*) LIsOK
          )  
      ; FM3SharedUtils .CheckPrefix
          ( LIsOK
          , LFileKind
          , FM3SharedGlobals .  FM3FileKindRdBack 
          , LFileVersion
          , FM3SharedGlobals .  FM3FileVersion0
          , "Readbackfile"
          , LRbFile . RbFileName  
          ) 

      ; LRbFile . RbBlockNo
          := VAL ( ( LRbFile . RbLengthL - 1L ) DIV BlockSizeL
                 , INTEGER
                 ) 
      ; LRbFile . RbBlockNextIn
          := VAL
               ( ( ( LRbFile . RbLengthL -1L ) MOD BlockSizeL ) + 1L
               , INTEGER
               )
      ; LRbFile . RbBlockCt
         := VAL ( ( LRbFile . RbMaxLengthL - 1L ) DIV BlockSizeL + 1L
                , INTEGER
                ) 
      ; ReadDataBlock
          ( LRbFile
          , LRbFile . RbBlockNo
          , LRbFile . RbBlockNextIn
          , LRbFile . RbBuffer
          , "Open"
          )
      ; LRbFile . RbIsDirty := FALSE 
      END (*IF*) 
    ; RETURN LRbFile 
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
    ; RETURN RbFile . RbLengthL  
    END LengthL 

(*EXPORTED*)
; PROCEDURE MaxLengthL ( RbFile : T ) : LONGCARD
  RAISES { OSError . E }

  = BEGIN
      IF RbFile = NIL THEN Raise ( "MaxLengthL, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "MaxLengthL, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbMaxLengthL  
    END MaxLengthL 

(*EXPORTED*)
; PROCEDURE IsEmpty ( RbFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than LengthL(F)=0L. *) 

  = BEGIN
      IF RbFile = NIL THEN Raise ( "IsEmpty, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "IsEmpty, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbLengthL <= 0L
    END IsEmpty
    
; PROCEDURE SeekDisk
    ( RbFile : T
    ; DiskAddrL : LONGINT (* Absolute in disk file. *) 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }
    
  = VAR LGot : INTEGER 
    (* ^This should be LONGINT, but RegularFile.T.read is behind the times.*)
  ; VAR LCurrent : INTEGER 
  ; VAR LDiskAddr : INTEGER
  (* ^These 3 should be LONGINT, but RegularFile.T.seek is behind the times.*)
  
  ; BEGIN
      LDiskAddr := VAL ( DiskAddrL , INTEGER )
    ; TRY
        LCurrent
           := RbFile . RbFileHandle . seek ( (* Relative to: *) Current , 0 )
        (* ^Just returns current location. *) 
      ; IF LCurrent # LDiskAddr
        THEN 
          LGot
             := RbFile . RbFileHandle . seek
                  ( (* Relative to:*) Beginning , LDiskAddr )
        ; IF LGot # LDiskAddr
          THEN
            Raise
              ( MsgTag
              , ", wrong seek address for "
              , RbFile . RbFileName
              , ", expected "
              , Fmt . Int ( LDiskAddr )
              , ", got "
              , Fmt . Int ( LGot )
              , "."
              )
          END (*IF*) 
        END (*IF*) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", seek failure for " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*) 
    END SeekDisk 

; PROCEDURE ReadDisk
    ( RbFile : T
    ; DiskAddrL : LONGINT 
    ; Length : INTEGER
    ; VAR Buffer : ARRAY OF File . Byte 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LGot : INTEGER
  (* ^This should be LONGINT, but RegularFile.T.read is behind the times.*)

  ; BEGIN 
      SeekDisk ( RbFile , DiskAddrL , MsgTag )
    ; TRY
        LGot
          := RbFile . RbFileHandle . read 
               ( (*OUT*) SUBARRAY ( Buffer , 0 , Length )
               , mayBlock := TRUE 
               )
      ; IF LGot # Length
        THEN
          Raise
            ( MsgTag
            , ", wrong read length for "
            , RbFile . RbFileName
            , ", expected "
            , Fmt . Int ( Length )
            , ", got "
            , Fmt . Int ( LGot )
            , "."
            )
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
    ; VAR Buffer : ARRAY OF File . Byte 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LDiskAddr : LONGINT


  ; BEGIN 
      LDiskAddr := VAL ( BlockNo , LONGINT ) * BlockSizeL + HeaderSizeL  
    ; ReadDisk ( RbFile , LDiskAddr , Length , Buffer , MsgTag ) 
    END ReadDataBlock

; PROCEDURE WriteDisk
    ( RbFile : T 
    ; DiskAddrL : LONGINT 
    ; Length : INTEGER
    ; READONLY Buffer : ARRAY OF File . Byte 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = BEGIN
      IF Length <= 0 THEN RETURN END (*IF*)
    ; SeekDisk ( RbFile , DiskAddrL , MsgTag )
    ; TRY RbFile . RbFileHandle . write ( SUBARRAY ( Buffer , 0 , Length ) ) 
      EXCEPT OSError . E ( Code ) 
      => Raise
           ( MsgTag , ", failure writing " , RbFile . RbFileName , "."
           , Code := Code
           ) 
      END (*EXCEPT*)
    ; RbFile . RbDiskLengthL
        := MAX ( RbFile . RbDiskLengthL , DiskAddrL + VAL ( Length , LONGINT ) ) 
    END WriteDisk

; PROCEDURE WriteDataBlock
    ( RbFile : T 
    ; BlockNo : INTEGER 
    ; Length : INTEGER
    ; READONLY Buffer : ARRAY OF File . Byte 
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LDiskAddrL : LONGINT 

  ; BEGIN
      IF Length <= 0 THEN RETURN END (*IF*)
    ; IF BlockNo < 0 THEN RETURN END (*IF*)
    ; LDiskAddrL := VAL ( BlockNo , LONGINT ) * BlockSizeL + HeaderSizeL 
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

; PROCEDURE BlockLength ( RbFile : T ; BlockNo : INTEGER ) : INTEGER

  = BEGIN
      RETURN  
        MIN ( BlockSize
            , VAL ( RbFile . RbMaxLengthL , INTEGER ) 
              - BlockNo * BlockSize
            )
    END BlockLength 
      
(*EXPORTED.*)
; PROCEDURE Seek ( RbFile : T ; LocationL : LONGINT ) 
  : LONGINT (* Actually positioned here. *) 

  = VAR LLocationL : LONGINT
  ; LNewBlockNo : INTEGER 
  ; LNewNextIn : INTEGER 

  ; BEGIN (*Seek*)
      IF LocationL < 0L THEN LLocationL := 0L
      ELSIF LocationL > RbFile . RbMaxLengthL
      THEN LLocationL := RbFile . RbMaxLengthL
      ELSE LLocationL := LocationL
      END (*IF*) 
    ; IF LLocationL = RbFile . RbLengthL 
      THEN RETURN LLocationL
      END (*IF*) 
    ; LNewBlockNo := VAL ( LLocationL DIV BlockSizeL , INTEGER ) 
    ; LNewNextIn := VAL ( ( ( LLocationL -1L ) MOD BlockSizeL ) + 1L , INTEGER )
    ; IF LNewBlockNo # RbFile . RbBlockNo
      THEN
        IF RbFile . RbIsDirty
        THEN 
          WriteDataBlock
            ( RbFile 
            , RbFile . RbBlockNo
            , BlockLength ( RbFile , LNewBlockNo ) 
            , RbFile . RbBuffer
            , "Seek"
            )
        END (*IF*) 
      ; ReadDataBlock
          ( RbFile
          , LNewBlockNo
          , BlockLength ( RbFile , LNewBlockNo ) 
          , RbFile . RbBuffer
          , "Seek"
          )
      ; RbFile . RbIsDirty := FALSE
      END (*IF*)
    ; RbFile . RbLengthL := LLocationL 
    ; RbFile . RbBlockNo := LNewBlockNo 
    ; RbFile . RbBlockNextIn := LNewNextIn 
    ; RbFile . RbIsDirty := FALSE
    ; RETURN LLocationL
    END Seek

; PROCEDURE InnerCopy ( RbFile : T ; CopyFile : T ; TruncTo : LONGINT ) 
  (* PRE: RbFile is open. *)
  (* PRE: TruncTo IN [ 0L .. RbFile . RbMaxLenghtL ] *) 
  (* POST: CopyFile is open. *) 

  = VAR LFullBlockCt : INTEGER (*I.e., count of blocks that are full. *)
  ; VAR LTotalBlockCt : INTEGER
  ; VAR LPartialBlockSize : INTEGER
  ; VAR LBuffer : BlockTyp  

  ; BEGIN (*InnerCopy*)
      TruncTo := MIN ( TruncTo , RbFile . RbDiskLengthL ) 
    ; LFullBlockCt := VAL ( TruncTo DIV BlockSizeL , INTEGER ) 
    ; LPartialBlockSize := VAL ( TruncTo MOD BlockSizeL , INTEGER )
    ; LTotalBlockCt := LFullBlockCt + ORD ( LPartialBlockSize > 0 )  
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
    ; WriteHeader ( RbFile , "Copy" ) (* Just for examination at this point. *)  
    ; CopyFile . RbLengthL := RbFile . RbLengthL 
    ; CopyFile . RbMaxLengthL := RbFile . RbMaxLengthL 
    ; CopyFile . RbDiskLengthL := RbFile . RbDiskLengthL 
    ; WriteHeader ( CopyFile , "Copy" ) 
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
      LCopyFile := Create ( CopyFileName )
    ; IF TruncTo < 0L THEN LTruncTo := RbFile . RbMaxLengthL 
      ELSE LTruncTo := MIN ( TruncTo , RbFile . RbMaxLengthL )
      END (*IF*) 
    ; InnerCopy ( RbFile , LCopyFile , LTruncTo ) 
    ; CloseDisk ( LCopyFile ) 
    END Copy
    
; PROCEDURE InnerFlushDataBlock ( RbFile : T ; MsgTag : TEXT )

  = BEGIN (*InnerFlushDataBlock*)
      WriteDataBlock
        ( RbFile
        , RbFile . RbBlockNo
        , BlockLength  ( RbFile , RbFile . RbBlockNo )
        , RbFile . RbBuffer
        , MsgTag
        )
    ; RbFile . RbIsDirty := FALSE 
    END InnerFlushDataBlock

(*EXPORTED.*)
; PROCEDURE Flush ( RbFile : T )

  = BEGIN (*Flush*)
      InnerFlushDataBlock ( RbFile , "Flush data" ) 
    ; WriteHeader ( RbFile , "Flush" ) 
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
      ; RbFile . RbMaxLengthL := 0L 
      ; LTruncDiskToL := HeaderSizeL
      ELSIF TruncToL > RbFile . RbMaxLengthL
      THEN
        TruncToL := RbFile . RbMaxLengthL
      ; LTruncDiskToL := TruncToL + HeaderSizeL
      ELSE 
        RbFile . RbMaxLengthL := TruncToL 
      ; LTruncDiskToL := TruncToL + HeaderSizeL
      END (*IF*)
    ; WriteHeader ( RbFile , "Close" ) 
    ; IF TruncToL = RbFile . RbMaxLengthL
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
      ; LTempFile := Create ( LTempFileName )
      ; InnerCopy ( RbFile , LTempFile , LTruncDiskToL ) 
      ; CloseDisk ( LTempFile ) (* So it's safe if anything fails later. *) 
      ; CloseDisk ( RbFile ) 
      ; TRY FS . DeleteFile ( LRbFileName )
        EXCEPT OSError . E ( Code ) 
        => Raise
             ( "Close, failure deleting " , LRbFileName , " for rename."
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
      THEN (* RbBlockNextIn is right of the current block. *)
           (* Move to the block to right. *) 
        IF RbFile . RbBlockNo >= 0 (* Block RbBlockNo exists. *)
           AND RbFile . RbIsDirty
        THEN (* Byte(s) in current buffer need to be preserved. *)
          WriteDataBlock
             ( RbFile
             , RbFile . RbBlockNo
             , BlockSize
             , RbFile . RbBuffer
             , "Put"
             ) 
        END (*IF*)
      (* Moving rightward to a new block number.  It will soon be dirty. *)
      ; INC ( RbFile . RbBlockNo ) 
      ; RbFile . RbBlockNextIn := 0
      ; IF RbFile . RbMaxLengthL
           > RbFile . RbLengthL + 1L 
        THEN (* Byte(s) previously stored in the new disk block to the right
                will be needed, in case they are neither overlaid, nor truncated
                when closing. *) 
          ReadDataBlock
            ( RbFile
            , RbFile . RbBlockNo
            , BlockSize
            , RbFile . RbBuffer
            , "Put"
            )
     (* ELSE we are moving into a previously non-existing blocl. *)        
        END (*IF*) 
      END (*IF*)
      
    (* Store the new byte. *) 
    ; RbFile . RbBuffer [ RbFile . RbBlockNextIn ] := Value
    ; RbFile . RbIsDirty := TRUE 
    ; INC ( RbFile . RbBlockNextIn )
    ; INC ( RbFile . RbLengthL )
    ; RbFile . RbMaxLengthL
        := MAX ( RbFile . RbMaxLengthL
               , RbFile . RbLengthL
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

    ; IF RbFile . RbLengthL <= (* < shouldn't happen. *) 0L
      THEN RAISE BOF
      END (*IF*)

    (* If necessary, make RbBuffer contain the block to the left. *) 
    ; IF RbFile . RbBlockNextIn <= (* < shouldn't happen. *) 0
         (* We are left of the current block. *)
         (* But not at BOF, => Block to left exists.  Move to it. *) 
      THEN
        IF RbFile . RbMaxLengthL
           > RbFile . RbLengthL + 1L
           AND RbFile . RbIsDirty
        THEN (* Byte(s) in current buffer other than #0 need to be preserved,
                in case they are neither overlaid, nor truncated when closing.
             *)
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
      THEN DEC ( RbFile . RbLengthL )
      ELSE INC ( RbFile . RbBlockNextIn ) (* Undo the above. *) 
      END (*IF*) 
    ; RETURN LResult 
    END GetBwd

; BEGIN (*RdBackFile*) 

(* Do all this elsewhere.  
    GLogWrT := NIL
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

