        
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

(*
; FROM FM3SharedGlobals
  IMPORT FM3TagLt , FM3TagRtBwd , FM3TagRdBackLt , FM3TagRdBackRt 
; CONST LeftTagLen
    = BYTESIZE ( FM3TagLeft )
    + BYTESIZE ( FM3TagRdBackBOF )
    + BYTESIZE ( TagVersion ) 
*)

; TYPE ShouldBeLONGINT = INTEGER
  (* File.T.read and RegularFile.T.seek should have been updated
     to LONGINT, but not so.  So this is here for now.
  *) 

; CONST Beginning = RegularFile . Origin . Beginning

; CONST BlockSize = 16_400 
; CONST BlockSizeL = VAL ( BlockSize , LONGINT ) 

; TYPE BlockSsTyp = [ 0 .. BlockSize - 1 ] 
; TYPE BlockTyp = ARRAY BlockSsTyp OF ByteTyp 

(* Linear linked list of blocks, optionally in memory, linked right-to-left. 
; TYPE BlockdescrRefTyp = REF BlockDescrTyp
; TYPE BlockDescrTyp
    = RECORD
        BdLink : BlockDescrRefTyp
      ; BdOccupiedCt : CARDINAL
      ; BdBuffer : BlockRefTyp (* NIL if not in memory, *)
      END (*BlockDescrTyp*)
*) 

; TYPE Bst = { BstBehind , BstEqual , BstAhead } 

; REVEAL T
  = BRANDED "RdBackFile.T" REF RECORD
      FbLengthL : LONGCARD := 0L (* Byte cound of current contents. *) 
    ; FbFileLengthL : LONGCARD := 0L (* Byte count of the disk file. *)   
    ; FbBlockNo : INTEGER := - 1 (* Current block number. *)
    ; FbBlockCt : INTEGER := 0 
    ; FbNextIn : INTEGER 
    ; FbBlockNextIn : INTEGER := 0
    ; FbFileName : TEXT := ""
    ; FbFileHandle : RegularFile . T 
    ; FbIsOpen : BOOLEAN 
    ; FbBufferState : Bst 
    ; FbBuffer : BlockTyp (* In-memory contents of current block. *) 
    END (*T*) 

(* The file is handled in fixed-size blocks of size BlockSize.  FbBuffer usually
   contains, in bytes [0..FbBlockNextIn-1], the current contents of the rightmost
   occupied block of the abstract file contents, thius being block number FbBlockNo. 
   If a Put happens next, the byte will be inserted at FbBuffer[FbBlockNextIn].
   If a Get happens next, the byte will be taken from FbBuffer[FbBlockNextIn-1]
   There is a transitional case involving a pair of adjacent blocks, when
   FbBlockNextIn=0.  In this case, FbBlockNo denotes the left of these two blocks.  
   If FbBufferState, FbBuffer has the contents of block FbBlockNo.  Otherwise,
   FbBuffer has no valid contents, but the block's contents are the disk.
   The right block is empty, and, although the disk may have contents for its
   block number, they are leftover from previous Get(s), and meaningless. 
*) 

; PROCEDURE Raise
    ( RBFile : T ; Code : AtomList . T ; Prefix , Suffix : TEXT := NIL )
  RAISES { OSError . E } 

  = VAR LMsg : TEXT
  ; VAR LWrT : Wr . T
  ; VAR LNewAtom : Atom . T
  ; VAR LNewList : AtomList . T  

  ; <*FATAL Thread . Alerted , Wr . Failure *>
    BEGIN
      LWrT := NEW ( TextWr . T ) 
    ; IF Prefix # NIL THEN Wr . PutText ( LWrT , Prefix ) END (*IF*)
    ; Wr . PutText ( LWrT , RBFile . FbFileName ) 
    ; IF Suffix # NIL THEN Wr . PutText ( LWrT , Suffix ) END (*IF*)
    ; LMsg := TextWr . ToText ( LWrT )
    ; LNewAtom := Atom . FromText ( LMsg )
    ; LNewList := AtomList . Cons ( LNewAtom , Code )
    ; RAISE OSError . E ( LNewList ) 
    END Raise 

; PROCEDURE InnerOpen 
    ( FileName : TEXT
    ; MsgLabel : TEXT
    ; Truncate : BOOLEAN
    ; MustBeEmpty : BOOLEAN
    )
  : T
  RAISES { OSError . E }   

  = VAR LResult : T
  ; VAR LStatus : File . Status 

  ; BEGIN
      IF FileName = NIL THEN FileName := "" END (*IF*) 
    ; LResult := NEW ( T )
    ; LResult . FbFileName := FileName 
    ; TRY
        TYPECASE  
          FS . OpenFile
            ( FileName
            , truncate := Truncate
            , create := FS . CreateOption . Ok
            , access := FS . AccessOption . Default
            )
        OF NULL
        => Raise ( LResult , NIL , "RdBackFile." & MsgLabel & " failed for ")
        | RegularFile . T (  RegFile ) 
        => LStatus := RegFile . status ( ) 
         ; IF MustBeEmpty AND LStatus . size > 0L
           THEN
             Raise ( LResult , NIL , "RdBackFile." & MsgLabel & " file not empty: ")
           END (*IF*)
         ; LResult . FbFileHandle := RegFile
         ; LResult . FbFileLengthL := LStatus . size 
         ; LResult . FbLengthL := LStatus . size 
        ELSE
          Raise ( LResult , NIL , "RdBackFile." & MsgLabel & " not a regular file: ")
        END (*TYPECASE*)
      EXCEPT OSError . E ( Code )
      => Raise ( LResult , Code, "RdBackFile." & MsgLabel & " unable to open ")
      END (*EXCEPT*)

    ; LResult . FbIsOpen := TRUE
    ; RETURN LResult 
    END InnerOpen  

(*EXPORTED*)
; PROCEDURE Create
    ( Filename : TEXT ; Truncate := FALSE ) : T
  RAISES { OSError . E }   

  = VAR LResult : T

  ; BEGIN
      LResult 
        := InnerOpen ( Filename , "Create" , Truncate , MustBeEmpty := TRUE )

    ; LResult . FbBlockNo := - 1
    ; LResult . FbBlockCt := 0 
    ; LResult . FbLengthL := 0L 
    ; LResult . FbFileLengthL := 0L
    ; LResult . FbBufferState := Bst . BstAhead 
    ; LResult . FbNextIn := BlockSize 
    ; RETURN LResult 
    END Create  

(*EXPORTED*)
; PROCEDURE Open ( Filename : TEXT ) : T RAISES { OSError . E }   

  = VAR LResult : T 

  ; BEGIN
      LResult 
        := InnerOpen 
             ( Filename , "Open" , Truncate := FALSE , MustBeEmpty := FALSE )

    ; LResult . FbBlockNo := VAL ( LResult . FbLengthL DIV BlockSizeL , INTEGER ) 
    ; LResult . FbNextIn 
        := VAL ( LResult . FbLengthL MOD BlockSizeL , INTEGER )
    ; IF LResult . FbNextIn = 0
      THEN
        DEC ( LResult . FbBlockNo )
      ; LResult . FbNextIn := BlockSize
      END (*IF*) 
    ; LResult . FbBufferState := Bst . BstBehind 
    ; RETURN LResult 
    END Open  

(*EXPORTED*)
; PROCEDURE LengthL ( RBFile : T ) : LONGCARD RAISES { OSError . E }

  = BEGIN
      IF RBFile = NIL OR NOT RBFile . FbIsOpen
      THEN RAISE OSError . E ( NIL )
      END (*IF*)  
    ; RETURN RBFile . FbLengthL  
    END LengthL 

(*EXPORTED*)
; PROCEDURE IsEmpty ( RBFile : T ) : BOOLEAN RAISES { OSError . E }
  (* Possibly faster than Length(F)=0L. *) 

  = BEGIN
      IF RBFile = NIL OR NOT RBFile . FbIsOpen
      THEN RAISE OSError . E ( NIL )
      END (*IF*) 
    ; RETURN RBFile . FbLengthL <= 0L
    END IsEmpty  

; PROCEDURE ReadBuffer
    ( RBFile : T ; BlockNo : INTEGER ; MsgTag : TEXT ) RAISES { OSError . E } 

  = VAR LSeekToL : ShouldBeLONGINT 
  ; VAR LGotL : ShouldBeLONGINT 

  ; BEGIN 
        LSeekToL := VAL ( BlockNo , ShouldBeLONGINT ) * BlockSize(*L*) 
      ; TRY
          LGotL := RBFile . FbFileHandle . seek ( Beginning , LSeekToL )
        ; IF LGotL # LSeekToL
          THEN Raise ( RBFile , NIL , "RdBackFile." & MsgTag & ", wrong seek length " )
          END (*IF*) 
        EXCEPT OSError . E ( Code ) 
        => Raise ( RBFile , Code , "RdBackFile." & MsgTag & ", failure seeking " ) 
        END (*EXCEPT*) 
      ; TRY
          LGotL
            := RBFile . FbFileHandle . read 
                 ( (*OUT*) SUBARRAY ( RBFile . FbBuffer , 0 , BlockSize )
                 , mayBlock := FALSE
                 ) 
        EXCEPT OSError . E ( Code ) 
        => Raise ( RBFile , Code , "RdBackFile." & MsgTag & ", failure reading " ) 
        END (*EXCEPT*) 
    END ReadBuffer

; PROCEDURE WriteBuffer
    ( RBFile : T ; BlockNo : INTEGER ; MsgTag : TEXT ) RAISES { OSError . E }

  = VAR LSeekToL : ShouldBeLONGINT 
  ; VAR LGotL : ShouldBeLONGINT 

  ; BEGIN
      LSeekToL := VAL ( BlockNo , ShouldBeLONGINT ) * BlockSize(*L*) 
    ; TRY
        LGotL := RBFile . FbFileHandle . seek ( Beginning , LSeekToL )
      ; IF LGotL # LSeekToL
        THEN Raise ( RBFile , NIL , "RdBackFile." & MsgTag & ", wrong seek length " )
        END (*IF*) 
      EXCEPT OSError . E ( Code ) 
      => Raise ( RBFile , Code , "RdBackFile." & MsgTag & ", failure seeking " ) 
      END (*EXCEPT*) 
    ; TRY RBFile . FbFileHandle . write 
            ( (*OUT*) SUBARRAY ( RBFile . FbBuffer , 0 , BlockSize ) ) 
      EXCEPT OSError . E ( Code ) 
      => Raise ( RBFile , Code , "RdBackFile." & MsgTag & ", failure writing " ) 
      END (*EXCEPT*)
    END WriteBuffer 

(*EXPORTED*)
; PROCEDURE Close ( RBFile : T ) RAISES { OSError . E }

  = BEGIN
      IF RBFile = NIL THEN RETURN  END (*IF*)
    ; IF RBFile . FbIsOpen THEN RETURN  END (*IF*)
    ; IF RBFile . FbBufferState = Bst . BstBehind
      THEN WriteBuffer ( RBFile , RBFile . FbBlockNo , "Close" )
      END (*IF*) 
(* HELP!  How can we truncate a RegularFile?  *) 
    END Close 

(*EXPORTED*)
; PROCEDURE Put ( RBFile : T ; Value : ByteTyp )  RAISES { OSError . E }  

  = BEGIN
      IF RBFile = NIL THEN RAISE OSError . E ( NIL ) END (*IF*) 
    ; IF NOT RBFile . FbIsOpen
      THEN Raise ( RBFile , NIL , "RdBackFile.Put, not open: " )
      END (*IF*) 

    ; IF RBFile . FbBlockNextIn = BlockSize (* Straddling blocks. *)
      THEN 
      (* If necessary, write block FbBlockNo from FbBuffer to disk. *) 
        IF RBFile . FbBlockNo >= 0 (* Block to left exists. *) 
           AND RBFile . FbBufferState = Bst . BstAhead 
           (* Its contents are in FbBuffer (but not on disk.) *) 
        THEN (* Write to disk. *)
          WriteBuffer ( RBFile , RBFile . FbBlockNo , "Put" ) 

        END (*IF*)
      (* Moving rightward to a new block number. It will, extremely soon,
         contain an unstored byte. *)
      ; INC ( RBFile . FbBlockNo ) 
      ; RBFile . FbBufferState := Bst . BstAhead  
      ; RBFile . FbBlockNextIn := 0 
      END (*IF*)
      
    (* Store the new byte. *) 
    ; RBFile . FbBuffer [ RBFile . FbBlockNextIn ] := Value 
    ; INC ( RBFile . FbBlockNextIn )

    (* If just filled FbBlockNo, we are now straddling.  Note that it
       is unstored. *) 
    ; IF RBFile . FbBlockNextIn = BlockSize  
      THEN (* Now straddling blocks. *)
        RBFile . FbBufferState := Bst . BstAhead (* This could already be so. *) 
      END (*IF*)
    END Put 

(*EXPORTED*)
; PROCEDURE GetBwd ( RBFile : T ) : ByteTyp  RAISES { BOF , OSError . E }  

  = VAR LResult : ByteTyp 

  ; BEGIN
      IF RBFile = NIL THEN RAISE OSError . E ( NIL ) END (*IF*) 
    ; IF NOT RBFile . FbIsOpen
      THEN Raise ( RBFile , NIL , "RdBackFile.GetBwd, not open: " )
      END (*IF*) 
    ; IF RBFile . FbLengthL = 0L THEN RAISE BOF END (* IF *)

    (* If necessary, read block FbBlockNo into FbBuffer. *) 
    ; IF RBFile . FbBlockNextIn = BlockSize (* Straddling blocks. *)
         AND RBFile . FbBlockNo >= 0 (* Block to left exists. *) 
         AND NOT RBFile . FbBufferState = Bst . BstBehind 
      THEN (* Load FbBlockNo from disk. *)
        ReadBuffer ( RBFile , RBFile . FbBlockNo , "GetBwd" ) 
      ; RBFile . FbBufferState := Bst . BstAhead  
      END (*IF*)
      
    (* Fetch the desired byte. *) 
    ; DEC ( RBFile . FbBlockNextIn )
    ; LResult := RBFile . FbBuffer [ RBFile . FbBlockNextIn ] 

    (* If moved into a different block to the left, note it is unfetched. *) 
    ; IF RBFile . FbBlockNextIn = BlockSize
      THEN (* Now straddling blocks. *)
        DEC ( RBFile . FbBlockNo ) (* Could go negative. *) 
      ; RBFile . FbBufferState := Bst . BstBehind 
      END (*IF*)
      
    ; RETURN LResult 
    END GetBwd 

; BEGIN
  END RdBackFile
.

