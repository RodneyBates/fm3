        
(* -----------------------------------------------------------------------1- *)
(* This file is part of the FM3 Modula-3 compiler.                           *)
(* Copyright 2023        Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A file that can be written (one byte at at time) normally, or
   read backwards.  Writes and reads can be interspersed, in which
   case, it behaves abstractly as a stack.
   It is maintained on a disk file.
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

; TYPE Bst = { BstBehind , BstEqual , BstAhead } 

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
    ; RbBufferState : Bst 
    ; RbBuffer : BlockTyp (* In-memory contents of current block. *) 
    END (*T*) 

(* The file is handled in fixed-size blocks of size BlockSize.  RbBuffer usually
   contains, in bytes [0..RbBlockNextIn-1], the current contents of the rightmost
   occupied block of the abstract file contents, thius being block number RbBlockNo. 
   If a Put happens next, the byte will be inserted at RbBuffer[RbBlockNextIn].
   If a Get happens next, the byte will be taken from RbBuffer[RbBlockNextIn-1]
   There is a transitional case involving a pair of adjacent blocks, when
   RbBlockNextIn=0.  In this case, RbBlockNo denotes the left of these two blocks.  
   If RbBufferState, RbBuffer has the contents of block RbBlockNo.  Otherwise,
   RbBuffer has no valid contents, but the block's contents are the disk.
   The right block is empty, and, although the disk may have contents for its
   block number, they are leftover from previous Get(s), and meaningless. 
*) 

; PROCEDURE Raise
    ( T1 , T2 , T3 , T4 : TEXT := NIL ; Code : AtomList . T := NIL )
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
    ; LResult . RbBufferState := Bst . BstAhead 
    ; RETURN LResult 
    END Create  

(*EXPORTED*)
; PROCEDURE Open ( Filename : TEXT ) : T RAISES { OSError . E }   

  = VAR LResult : T 

  ; BEGIN
      LResult 
        := InnerOpen 
             ( Filename , "Open" , Truncate := FALSE , MustBeEmpty := FALSE )
    ; LResult . RbBlockNo := VAL ( LResult . RbLengthL DIV BlockSizeL , INTEGER ) 
    ; LResult . RbNextIn := VAL ( LResult . RbLengthL MOD BlockSizeL , INTEGER )
    ; LResult . RbBlockCt := LResult . RbBlockNo + ORD ( LResult . RbNextIn > 0 )  
    ; LResult . RbBufferState := Bst . BstBehind 
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
  ; VAR LGot : INTEGER (* These should be LONGINT, but seek has not been updated.*)
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
    ( RbFile : T ; BlockNo : INTEGER ; MsgTag : TEXT ) RAISES { OSError . E } 

  = VAR LSeekToL : LONGINT
  ; VAR LGotL : LONGINT
  ; VAR LGot : INTEGER
  ; VAR LLength : INTEGER 

  ; BEGIN 
      LSeekToL := VAL ( BlockNo , LONGINT ) * BlockSizeL
    ; Seek ( RbFile , LSeekToL , MsgTag )
    ; LLength
       := MIN ( VAL ( RbFile . RbDiskLengthL - LSeekToL , INTEGER ) , BlockSize )
    ; TRY
        LGot
          := RbFile . RbFileHandle . read 
               ( (*OUT*) SUBARRAY ( RbFile . RbBuffer , 0 , LLength )
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
    ( RbFile : T ; BlockNo : INTEGER ; Length : INTEGER ; MsgTag : TEXT )
  RAISES { OSError . E }

  = VAR LSeekToL : LONGINT 

  ; BEGIN
      IF Length <= 0 THEN RETURN END (*IF*) 
    ; LSeekToL := VAL ( BlockNo , LONGINT ) * BlockSizeL 
    ; Seek ( RbFile , LSeekToL , MsgTag )
    ; TRY RbFile . RbFileHandle . write 
            ( SUBARRAY ( RbFile . RbBuffer , 0 , Length ) ) 
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

(*EXPORTED*)
; PROCEDURE Close ( RbFile : T ) RAISES { OSError . E }

  = BEGIN
      IF RbFile = NIL THEN RETURN  END (*IF*)
    ; IF NOT RbFile . RbIsOpen THEN RETURN  END (*IF*)
    ; IF RbFile . RbNextIn > 0
      THEN WriteBuffer ( RbFile , RbFile . RbBlockNo , RbFile . RbNextIn , "Close" )
      END (*IF*) 

(* HELP!  How can we truncate a RegularFile?  *)

    ; TRY RbFile . RbFileHandle . close ( ) 
      EXCEPT OSError . E => (* Let's just ignore this one. *) 
      END (*EXCEPT*)
    END Close 

(*EXPORTED*)
; PROCEDURE Put ( RbFile : T ; Value : ByteTyp )  RAISES { OSError . E }  

  = BEGIN
      IF RbFile = NIL THEN Raise ( "Put" , ", NIL ReadBackFile." ) END (*IF*) 
    ; IF NOT RbFile . RbIsOpen
      THEN Raise ( "Put" , RbFile . RbFileName , "not open." )
      END (*IF*) 

    ; IF RbFile . RbBlockNextIn >= BlockSize (* Right of the block in RbBuffer. *)
      THEN 
      (* If necessary, write block RbBlockNo from RbBuffer to disk. *) 
        IF RbFile . RbBlockNo >= 0 (* Block RbBlockNo exists. *) 
           AND RbFile . RbBufferState = Bst . BstAhead 
           (* Its contents are in RbBuffer (but not on disk.) *) 
        THEN (* Write to disk. *)
          WriteBuffer ( RbFile , RbFile . RbBlockNo , BlockSize , "Put" ) 
        END (*IF*)
      (* Moving rightward to a new block number. It will, extremely soon,
         contain an unstored byte. *)
      ; INC ( RbFile . RbBlockNo ) 
      ; RbFile . RbBufferState := Bst . BstAhead  
      ; RbFile . RbBlockNextIn := 0 
      END (*IF*)
      
    (* Store the new byte. *) 
    ; RbFile . RbBuffer [ RbFile . RbBlockNextIn ] := Value
    ; INC ( RbFile . RbBlockNextIn )
    ; INC ( RbFile . RbLengthL ) 

    (* If just filled RbBlockNo, we are now straddling.  Note that it
       is unstored. *) 
    ; IF RbFile . RbBlockNextIn = BlockSize  
      THEN (* Now straddling blocks. *)
        RbFile . RbBufferState := Bst . BstAhead (* This could already be so. *) 
      END (*IF*)
    END Put 

(*EXPORTED*)
; PROCEDURE GetBwd ( RbFile : T ) : ByteTyp  RAISES { BOF , OSError . E }  

  = VAR LResult : ByteTyp 

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
      ; ReadBuffer ( RbFile , RbFile . RbBlockNo , "GetBwd" )
      ; RbFile . RbBlockNextIn := BlockSize 
      ; RbFile . RbBufferState := Bst . BstAhead  
      END (*IF*)
      
    (* Fetch the desired byte. *) 
    ; DEC ( RbFile . RbBlockNextIn )
    ; LResult := RbFile . RbBuffer [ RbFile . RbBlockNextIn ] 
    ; DEC ( RbFile . RbLengthL )

    (* If moved into a different block to the left, note it is unfetched. *) 
    ; IF RbFile . RbBlockNextIn = BlockSize
      THEN (* Now straddling blocks. *)
        RbFile . RbBufferState := Bst . BstBehind 
      END (*IF*)
      
    ; RETURN LResult 
    END GetBwd 

; BEGIN
  END RdBackFile
.

