        
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

; CONST Beginning = RegularFile . Origin . Beginning
; CONST Current = RegularFile . Origin . Current 

; CONST BlockSize = 16_400 
; CONST BlockSizeL = VAL ( BlockSize , LONGINT ) 

; TYPE BlockSsTyp = [ 0 .. BlockSize - 1 ] 
; TYPE BlockTyp = ARRAY BlockSsTyp OF File . Byte 

; REVEAL T
  = BRANDED "RdBackFile.T" REF RECORD 
      RbLengthL : LONGCARD := 0L (* Byte count of current abstract contents. *) 
    ; RbMaxLengthL : LONGCARD := 0L (* Max occurring during period of openness. *)
    ; RbDiskLengthL : LONGCARD := 0L (* Byte count of the disk file. *)   
    ; RbBlockNo : INTEGER := - 1 (* Current block number. *)
    ; RbBlockCt : INTEGER := 0 
    ; RbBlockNextIn : INTEGER := 0
    ; RbFileName : TEXT := ""
    ; RbFileHandle : RegularFile . T 
    ; RbIsOpen : BOOLEAN
    ; RbCheckLastTok : INTEGER
    ; RbCheckArgCt : INTEGER
(* TODO: These two fields do not belong in this abstraction, but it's a lot
   of work to put them into a subtype.
*) 
    ; RbBuffer : BlockTyp (* In-memory contents of current block. *) 
    END (*T*)

(* The file is handled in fixed-sized blocks of size BlockSize.  RbBuffer is
   an in-memory copy of block number RbBlockNo, which is not necessarily
   up-to-date on disk.  RbBlockNo denotes the rightmost block that contains
   meaningful current data, which occupy bytes [0..RbBlockNextIn-1] of RbBuffer.
   RbBlockNextIn is the subscript where the next byte would go into RbBuffer,
   but it can be one beyond the end of RbBuffer, i.e. equal to BlockSize.
   If a Put occurs in this state, RbBuffer must first be changed to contain
   the block to the right, after flushing its old contents to disk.
   RbBlockNextIn-1 is the subscript of the next byte to be removed, but
   analogous to the above, it can be one to the left of the beginning of
   RbBuffer, i.e., -1, with an analogous requirement to flush and change
   the block to the left, should a GetBwd occur in this state. Thus there
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

  ; BEGIN

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
         ; LResult . RbLengthL := LStatus . size
         ; LResult . RbMaxLengthL := LStatus . size
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
    ; LResult . RbLengthL := 0L 
    ; LResult . RbMaxLengthL := 0L
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
    ; IF LResult . RbLengthL <= 0L
      THEN 
        LResult . RbBlockNo := 0 
      ; LResult . RbBlockCt := 0 
      ; LResult . RbBlockNextIn := 0
      (* And RbBuffer is garbage. *)
      ELSE 
        LResult . RbBlockNo
          := VAL ( ( LResult . RbLengthL - 1L ) DIV BlockSizeL , INTEGER ) 
      ; LResult . RbBlockNextIn
          := VAL ( ( ( LResult . RbLengthL -1L ) MOD BlockSizeL ) + 1L , INTEGER )
      ; LResult . RbBlockCt := LResult . RbBlockNo + 1
      ; ReadBuffer
          ( LResult
          , LResult . RbBlockNo
          , LResult . RbBlockNextIn
          , LResult . RbBuffer
          , "Open"
          )
      END (*IF*) 
    ; RETURN LResult 
    END Open

(*EXPORTED*)
; PROCEDURE FileName ( RbFile : T ) : TEXT 
  (* Never NIL.  Possibly empty. *) 

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
; PROCEDURE MaxLengthL ( RbFile : T ) : LONGCARD RAISES { OSError . E }
  (* Max LengthL ever was since Create or Open. *) 

  = BEGIN
      IF RbFile = NIL THEN Raise ( "LengthL, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "MaxLengthL, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbMaxLengthL  
    END MaxLengthL 

(*EXPORTED*)
; PROCEDURE IsEmpty ( RbFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than Length(F)=0L. *) 

  = BEGIN
      IF RbFile = NIL THEN Raise ( "IsEmpty, NIL file." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "IsEmpty, " , RbFile . RbFileName , " is not open." )
      END (*IF*) 
    ; RETURN RbFile . RbLengthL <= 0L
    END IsEmpty
    
; PROCEDURE Seek
    ( RbFile : T ; SeekToL : LONGINT ; MsgTag : TEXT ) RAISES { OSError . E }
    
  = VAR LGotL : LONGINT
  ; VAR LGot : INTEGER 
  ; VAR LCurrent : INTEGER 
  ; VAR LSeekTo : INTEGER
  (* ^These two should be LONGINT, but RegularFile.T.seek is behind the times.*)
  
  ; BEGIN
      LSeekTo := VAL ( SeekToL , INTEGER )
    ; TRY
        LCurrent := RbFile . RbFileHandle . seek ( Current , 0 )
      ; IF LCurrent # LSeekTo
        THEN 
          LGot := RbFile . RbFileHandle . seek ( Beginning , LSeekTo )
        ; LGotL := VAL ( LGot , LONGINT ) 
        ; IF LGotL # SeekToL
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
    END Seek 

; PROCEDURE ReadBuffer
    ( RbFile : T
    ; BlockNo : INTEGER 
    ; Length : INTEGER
    ; VAR Buffer : BlockTyp
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LSeekToL : LONGINT
  ; VAR LGot : INTEGER
  (* ^This should be LONGINT, but RegularFile.T.read is behind the times.*)

  ; BEGIN 
      LSeekToL := VAL ( BlockNo , LONGINT ) * BlockSizeL
    ; Seek ( RbFile , LSeekToL , MsgTag )
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
    END ReadBuffer

; PROCEDURE WriteBuffer
    ( RbFile : T 
    ; BlockNo : INTEGER 
    ; Length : INTEGER
    ; READONLY Buffer : BlockTyp
    ; MsgTag : TEXT
    )
  RAISES { OSError . E }

  = VAR LSeekToL : LONGINT 

  ; BEGIN
      IF Length <= 0 THEN RETURN END (*IF*)
    ; IF BlockNo < 0 THEN RETURN END (*IF*)
    ; LSeekToL := VAL ( BlockNo , LONGINT ) * BlockSizeL 
    ; Seek ( RbFile , LSeekToL , MsgTag )
    ; TRY RbFile . RbFileHandle . write ( SUBARRAY ( Buffer , 0 , Length ) ) 
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

; PROCEDURE SimpleClose ( RbFile : T ) 
  = BEGIN
      TRY RbFile . RbFileHandle . close ( ) 
      EXCEPT OSError . E 
      => (* Let's log this one but not raise an exception. *) 
        LogMsg ( "Close of " & RbFile . RbFileName & " failed." )  
      END (*EXCEPT*)
    ; RbFile . RbIsOpen := FALSE 
    END SimpleClose

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

  = VAR LFullBlockCt : INTEGER (*I.e., count of full blocks. *)
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
        THEN
          WriteBuffer
            ( CopyFile , RBlockNo , BlockSize , RbFile . RbBuffer , "CopyMem" )
        ELSE 
          ReadBuffer ( RbFile , RBlockNo , BlockSize , LBuffer , "Copy" )
        ; WriteBuffer ( CopyFile , RBlockNo , BlockSize , LBuffer , "CopyFile" )
        END (*IF*) 
      END (*FOR*)
    ; IF LPartialBlockSize > 0
      THEN (* Copy partial block. *)
        IF RbFile . RbBlockNo = LFullBlockCt
        THEN
          WriteBuffer
            ( CopyFile
            , LFullBlockCt
            , LPartialBlockSize
            , RbFile . RbBuffer
            , "CopyMemPartial"
            )
        ELSE 
          ReadBuffer
            ( RbFile
            , LFullBlockCt
            , LPartialBlockSize
            , LBuffer
            , "CopyPartial"
            )
        ; WriteBuffer
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
    ; IF TruncTo < 0L THEN LTruncTo := RbFile . RbMaxLengthL
      ELSE LTruncTo := MIN ( TruncTo , RbFile . RbMaxLengthL )
      END (*IF*) 
    ; InnerCopy ( RbFile , LCopyFile , LTruncTo ) 
    ; SimpleClose ( LCopyFile ) 
    END Copy

(*EXPORTED*)
; PROCEDURE Close ( RbFile : T ; TruncTo : LONGINT )
  (* TruncTo < 0 means max length. *) 

  = VAR LTruncTo : LONGINT
  ; VAR LTempFile : T
  ; VAR LFileName : TEXT
  ; VAR LTempFileName : TEXT

  ; BEGIN
      IF RbFile = NIL THEN RETURN END (*IF*)
    ; IF NOT RbFile . RbIsOpen THEN RETURN  END (*IF*)
    ; WriteBuffer
        ( RbFile
        , RbFile . RbBlockNo
        , MIN ( BlockSize
              , VAL ( RbFile . RbMaxLengthL , INTEGER ) 
                - RbFile . RbBlockNo * BlockSize
              ) 
        , RbFile . RbBuffer
        , "Close"
        )

    ; IF TruncTo < 0L THEN LTruncTo := RbFile . RbMaxLengthL
      ELSE LTruncTo := MIN ( TruncTo , RbFile . RbMaxLengthL )
      END (*IF*) 

    ; IF FALSE (* Disable this for now, because: *) 
               (* But we don't have even a full Truncate library call. *)
         AND LTruncTo = 0L
      THEN (* Make the disk file empty too. *)
(* TODO: Get an OS truncate function and use it. *) 
        SimpleClose ( RbFile ) 
      ELSIF LTruncTo = RbFile . RbDiskLengthL 
      THEN (* Disk is the exact length, no truncation needed. *)
        SimpleClose ( RbFile ) 

      ELSE (* Must partially truncate the disk file. *)

      (* The Modula-3 libraries provide no way to truncate a RegularFile,
         which is needed here.  Adding this would entail OS-dependent
         versions for POSIX and WIN32 and make this module dependent on having
         the latest RegularFile in the CM3 installation.  I don't what to
         get into that now, so am resorting to doing a partial copy of the
         disk file.  Yes, it's crude, inefficient, and entails a lot of
         code to achieve such a simple result, but it's the path of least
         resistance now. 
      *)
      
        LFileName := RbFile . RbFileName (* Just paranoia. *) 
      ; LTempFileName := TempFileName ( LFileName ) 
      ; LTempFile := Create ( LTempFileName , Truncate := FALSE )
      ; InnerCopy ( RbFile , LTempFile , LTruncTo ) 
      ; SimpleClose ( RbFile ) 
      ; TRY FS . DeleteFile ( LFileName )
        EXCEPT OSError . E ( Code ) 
        => Raise
             ( "Close", ", failure deleting " , LFileName , " for rename."
             , Code := Code
             ) 
        END (*EXCEPT*) 
      ; SimpleClose ( LTempFile ) 
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
        THEN (* Write to disk. *)
          WriteBuffer
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
      ; IF RbFile . RbMaxLengthL > RbFile . RbLengthL + 1L 
        THEN (* Byte(s) previously stored in the new disk block to the right
                will be needed, if they are neither overlaid, nor truncated
                when closing. *) 
          ReadBuffer
            ( RbFile
            , RbFile . RbBlockNo
            , BlockSize
            , RbFile . RbBuffer
            , "Put"
            )
        END (*IF*) 
      END (*IF*)
      
    (* Store the new byte. *) 
    ; RbFile . RbBuffer [ RbFile . RbBlockNextIn ] := Value
    ; INC ( RbFile . RbBlockNextIn )
    ; INC ( RbFile . RbLengthL )
    ; RbFile . RbMaxLengthL
        := MAX ( RbFile . RbMaxLengthL , RbFile . RbLengthL )   
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

    ; IF RbFile . RbLengthL = 0L
      THEN RAISE BOF
      END (*IF*)

    (* If necessary, make RbBuffer contain the block to the left. *) 
    ; IF RbFile . RbBlockNextIn = 0 (* Left of the block in RbBuffer. *)
         (* But RbFile not empty, => Block to left exists. *) 
      THEN
        IF RbFile . RbMaxLengthL > RbFile . RbLengthL + 1L
        THEN (* Byte(s) in the current buffer will be needed on disk,
                if they are neither overlaid, nor truncated when closing. *)
          WriteBuffer
            ( RbFile
            , RbFile . RbBlockNo
            , BlockSize
            , RbFile . RbBuffer
            , "GetBwd"
            )
        END (*IF*) 
      ; DEC ( RbFile . RbBlockNo )

      ; ReadBuffer
          ( RbFile
          , RbFile . RbBlockNo
          , BlockSize
          , RbFile . RbBuffer
          , "GetBwd"
          )
      ; RbFile . RbBlockNextIn := BlockSize 
      END (*IF*)
      
    (* Fetch the desired byte. *) 
    ; DEC ( RbFile . RbBlockNextIn )
    ; LResult := RbFile . RbBuffer [ RbFile . RbBlockNextIn ]
    ; IF Consume
      THEN DEC ( RbFile . RbLengthL )
      ELSE INC ( RbFile . RbBlockNextIn )
      END (*IF*) 
    ; RETURN LResult 
    END GetBwd 

; BEGIN

    GLogWrT := NIL
  ; GDoStderr := TRUE 
  ; GDoLog := FALSE

(* TODO: Use FM3Messages here. *) 
(* 
    TRY 
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

